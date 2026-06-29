# Generic Multi-Language Vibe Coding Evaluation MVP
# ------------------------------------------------
# Language-agnostic, file-based evaluation protocol
# Programs under test emit JSON facts; R computes metrics

library(jsonlite)
library(ollamar)
library(httr2)
library(base64enc)
library(openxlsx)

# ---- Configuration ----

TASK_DIR   <- "."        # contains taskN.txt
WORK_DIR   <- "./workspace"    # per-run execution dirs
RESULT_FILE <- "result.json" # contract file name
N_RUNS     <- 1
PYTHON_PATH <- "/mnt/scratch2/users/40290129/pyenv/bin/python" #can be replaced with any absolute path for testing in different environments
SCORER_MODEL <- "qwen2.5:14b"
VISION_MODEL <- "llama3.2-vision:11b"
SYSTEM_PROMPT <- paste(
  "You are a Python coding assistant.",
  "Respond ONLY with valid JSON.",
  "The JSON must contain exactly one field named \"code\".",
  "The value of \"code\" must be a complete Python script as a string.",
  "Do not include markdown, code fences, explanations, prose, or additional fields.",
  "Task:\n\n"
)

# ---- Task discovery ----

get_tasks <- function(root_dir) {
  files <- list.files(
    root_dir,
    pattern = "^task[0-9]+\\.txt$",
    #pattern = "^task1\\.txt$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  lapply(files, function(f) {
    parts <- strsplit(f, .Platform$file.sep)[[1]]
    
    list(
      id       = tools::file_path_sans_ext(basename(f)),
      prompt   = paste(readLines(f, warn = FALSE), collapse = "\n"),
      level    = parts[length(parts) - 1]
    )
  })
}


# ---- Model registry ----
# Models must return raw source code only

#NOTE! The data contained in these list entries are NOT the models themselves,
#but rather the means of reaching them!

models <- c(
  gemma       = "gemma3:12b",
  phi4        = "phi4:14b",
  mistral     = "mistral-nemo:12b",
  qwen_coder  = "qwen2.5-coder:14b"
)

# ---- Manual repair support ----

init_manual_repair <- function() {
  choice <- readline("Allow manual repair when parsing fails? 'y' for yes, 'n' or anything else for no.")
  tolower(trimws(choice)) == "y"
}

# ---- Scoring prompt standardisation

add_scoring_prompts <- function(tasks) {
  task_ids <- unique(vapply(tasks, function(task) task$id, character(1)))
  
  for (task_id in task_ids) {
    matching_tasks <- Filter(function(task) task$id == task_id, tasks)
    
    level_numbers <- vapply(matching_tasks, function(task) {
      suppressWarnings(as.integer(gsub("[^0-9]", "", task$level)))
    }, integer(1))
    
    baseline_prompt <- matching_tasks[[which.min(level_numbers)]]$prompt
    
    for (i in seq_along(tasks)) {
      if (identical(tasks[[i]]$id, task_id)) {
        tasks[[i]]$scoring_prompt <- baseline_prompt
      }
    }
  }
  
  tasks
}


# ---- Wrapping code ----

wrap_code <- function(code, result_path) {
  paste("import json",
        "import io",
        "import sys",
        "import types",
        "class TrackingDict(dict):",
        "   def __init__(self, history_size=5):",
        "      super().__init__()",
        "      self.history_size = history_size",
        "      self.history = []  # Stores (name, value) pairs",
        "",
        "   def __setitem__(self, key, value):",
        "      # Track only non-internal variables (skip __builtins__, etc.)",
        "      if (",
        '         not key.startswith("__")',
        "         and not isinstance(value, types.ModuleType)",
        "         and not isinstance(value, types.FunctionType)",
        "      ):",
        "         self.history.append((key, value))",
        "         if len(self.history) > self.history_size:",
        "            self.history.pop(0)",
        "      super().__setitem__(key, value)",
        "",
        "   def last_list(self):",
        '      """Return the last N assignments as a list of (name, value) pairs."""',
        "      return list(self.history)",
        "",
        "def make_json_safe(value):",
        "   if isinstance(value, (str, int, float, bool)) or value is None:",
        "      return value",
        "   elif isinstance(value, list):",
        "      return [make_json_safe(v) for v in value]",
        "   elif isinstance(value, dict):",
        "      return {str(k): make_json_safe(v) for k, v in value.items()}",
        "   else:",
        "      return repr(value)",
        "",
        "last_user_locals = []",
        "",
        "def trace_locals(frame, event, arg):",
        '   if event == "return":',
        "      global last_user_locals",
        "      frame_globals = frame.f_globals",
        "      frame_name = frame.f_code.co_name",
        "      if (",
        '         frame_globals.get("__name__") == "__main__"',
        '         and frame_name not in {"trace_locals", "make_json_safe", "__setitem__", "last_list"}',
        "      ):",
        "         current_locals = []",
        "         for name, value in frame.f_locals.items():",
        "            if (",
        '               not name.startswith("__")',
        '               and name not in {"self", "key", "value", "frame", "event", "arg", "frame_globals", "frame_name"}',
        "               and not isinstance(value, types.ModuleType)",
        "               and not isinstance(value, types.FunctionType)",
        "            ):",
        "               current_locals.append((name, make_json_safe(value)))",
        "         if len(current_locals) > 0:",
        "            last_user_locals = current_locals",
        "   return trace_locals",
        "",
        "",
        "old_stdout = sys.stdout",
        "sys.stdout = buffer = io.StringIO()",
        "# Create a tracking namespace",
        "tracker_ns = TrackingDict(history_size=3)",
        'tracker_ns["__name__"] = "__main__"',
        "",
        "# Example program code",
        paste0("program_code = ", jsonlite::toJSON(code, auto_unbox = TRUE)),
        paste0("result_path = ", jsonlite::toJSON(normalizePath(result_path, winslash = "/", mustWork = FALSE), auto_unbox = TRUE)),
        "",
        "listOfVariables = []",
        "localVariables = []",
        "status = 'executed_ok'",
        "try:",
        "   sys.settrace(trace_locals)",
        "   # Execute inside the tracking namespace",
        "   exec(program_code, tracker_ns)",
        "",
        "   # Retrieve the last tracked assignments",
        "   listOfVariables = [",
        "      (name, make_json_safe(value)) for name, value in tracker_ns.last_list()",
        "   ]",
        "   localVariables = list(last_user_locals)",
        "   executed = True",
        "except Exception as e:",
        "   executed = False",
        "   status = 'runtime_error'",
        "finally:",
        "   sys.settrace(None)",
        "   sys.stdout = old_stdout",
        "   stdout = buffer.getvalue()",
        "with open(result_path, 'w') as f:",
        "   json.dump({'executed': executed, 'variablesCreated': listOfVariables, 'localVariables': localVariables, 'stdout': stdout, 'status': status}, f)",
        sep = "\n"
  )
}

# ---- Execution ----

run_code <- function(code, model_name = "model", task = NULL, run_index = 1) {
  ## original_dir <- getwd()  # save current directory
  dir.create(WORK_DIR, showWarnings = FALSE)
  task_id <- if (!is.null(task)) task$id else "Not assigned"
  level <- if (!is.null(task)) task$level else "Not assigned"

  run_label <- paste(model_name, task_id, level, paste0("run", run_index), sep = "_")
  run_label <- gsub("[^A-Za-z0-9_-]", "_", run_label)

  run_dir <- file.path(WORK_DIR, run_label)

  # Avoid collision if rerunning without cleanup
  if (dir.exists(run_dir)) {
    suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
    run_dir <- file.path(WORK_DIR, paste0(run_label, "_", suffix))
  }

  dir.create(run_dir, recursive = TRUE)
  
  run_dir_absolute <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)

  src <- file.path(run_dir_absolute, "main.py")
  result_path <- file.path(run_dir_absolute, RESULT_FILE)

  wrapped_code <- wrap_code(code, result_path)
  
  writeLines(wrapped_code, src)

  src_absolute <- normalizePath(src, winslash = "/", mustWork = FALSE) #convert to absolute path
  run_dir_absolute <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)

  #print(paste("Executing:", src_absolute))
  #print(paste("Run dir:", run_dir_absolute))

  output <- tryCatch(
    system2(
      "bash",
      args = c(
        "-lc",
        shQuote(paste(
          "cd", shQuote(run_dir_absolute),
          "&&",
          shQuote(PYTHON_PATH),
          shQuote(src_absolute)
        ))
      ),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) e$message
  )
  
  #setwd(original_dir)  # restore original directory
  
  list(
    stdout = output,
    result_path = result_path,
    run_dir = run_dir
  )
}

# ---- Result ingestion ----

read_result <- function(path) {
  if (!file.exists(path)) return(list(executed = FALSE, status = "no_result_file"))
  
  tryCatch(
    jsonlite::fromJSON(path),
    error = function(e) list(
      executed = FALSE,
      status = "invalid_result_json",
      parse_error = e$message
    )
  )
}

# ---- Task categorisation ----
detect_output_types <- function(result, run_dir) {
  types <- character()
  
  image_files <- list.files(
    run_dir,
    pattern = "\\.(png|jpg|jpeg)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  excel_files <- list.files(
    run_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(image_files) > 0) types <- c(types, "vision")
  if (length(excel_files) > 0) types <- c(types, "excel")
  
  has_stdout <- !is.null(result$stdout) && nchar(trimws(result$stdout)) > 0
  has_vars <- !is.null(result$variablesCreated) && !identical(result$variablesCreated, "NA")
  
  if (has_stdout || has_vars) types <- c(types, "llm")
  
  if (length(types) == 0) types <- "none"
  
  unique(types)
}

# ---- Scoring (example invariant-based) ----

score_result <- function(result, task, run_dir) {
  output_types <- detect_output_types(result, run_dir)
  
  if ("vision" %in% output_types) {
    output_score <- score_output_vision(run_dir)
  } else if ("excel" %in% output_types) {
    output_score <- score_output_excel(run_dir)
  } else if ("llm" %in% output_types) {
    output_score <- score_output_llm(task$scoring_prompt, result$variablesCreated, result$stdout)
  } else {
    output_score <- list(passed = FALSE, reason = "no_observable_output")
  }
  
  list(
    output_passed = output_score$passed,
    output_reason = output_score$reason
  )
}

# ---- Functionality scoring ----

score_functionality <- function(exec, result, model, model_name, task) {
  # First attempt succeeded cleanly
  if (isTRUE(result$executed)) {
    return(list(score = 2, attempts = 1))
  }
  
  retry_context <- paste(exec$stdout, collapse = "\n")
  
  retry_prompt <- if (nchar(trimws(retry_context)) == 0) {
    paste(
      "In a previous session you did not solve a coding task correctly.",
      "Here is the full prompt you were sent which includes the formatting mandates and the task itself:\n",
      "\n",
      SYSTEM_PROMPT,
      task$prompt, "\n",
      "\n",
      "That concludes the prompt in its entirety.\n",
      "For some further context, you will be shown the results of your first attempt. Here were the variables observed during your code's execution, given either as a list with each entry being in the form [<variable name>, <value] or as a 'NA' string if none were observed:",
      result$variablesCreated, "\n",
      "Here is everything that was printed to the console during execution:", result$stdout, "\n",
      "\n",
      "Please try again and return a corrected response in the required format - ensure you follow the instructions carefully."
    )
  } else {
    paste(
      "In a previous session you did not solve a coding task correctly, and it produced this error:",
      retry_context,
      "Here is the full prompt you were sent which includes the formatting mandates and the task itself:\n",
      "\n",
      SYSTEM_PROMPT,
      task$prompt, "\n",
      "\n",
      "That concludes the prompt in its entirety.\n",
      "For some further context, you will be shown the results of your first attempt. Here were the variables observed during your code's execution, given either as a list with each entry being in the form [<variable name>, <value] or as a 'NA' string if none were observed:",
      result$variablesCreated, "\n",
      "Here is everything that was printed to the console during execution:", result$stdout, "\n",
      "\n",
      "Please try again and return a corrected response in the required format - ensure you follow the instructions carefully."
    )
  }
  
  res <- ollamar::chat(
    model = model,
    messages = list(
      list(role = "user", content = retry_prompt)
    ),
    format = "json"
  )
  
  parsed <- jsonlite::fromJSON(rawToChar(res$body))
  retry_content <- parsed$message$content
  
  # Strip markdown fences before parsing
  retry_content <- gsub("^```[a-zA-Z]*\\n?", "", retry_content)
  retry_content <- gsub("\\n?```\\s*$", "", retry_content)
  
  # Remove trailing full stops, if they exist
  retry_content <- trimws(retry_content)
  retry_content <- sub("\\.$", "", retry_content)
  
  response_json <- tryCatch(
    jsonlite::fromJSON(retry_content),
    error = function(e) NULL
  )
  
  if (is.null(response_json)) {
    start_pos <- regexpr("\\{", retry_content)[1]
    end_positions <- gregexpr("\\}", retry_content)[[1]]
    
    if (start_pos != -1 && end_positions[1] != -1) {
      end_pos <- tail(end_positions, 1)
      candidate_json <- substr(retry_content, start_pos, end_pos)
      
      response_json <- tryCatch(
        jsonlite::fromJSON(candidate_json),
        error = function(e) NULL
      )
    }
  }
  
  if (is.null(response_json) && ALLOW_MANUAL_REPAIR) {
    response_json <- manual_repair(retry_content)
  }
  
  retry_code <- if (
    !is.null(response_json) &&
    !is.null(response_json$code) &&
    is.character(response_json$code) &&
    length(response_json$code) == 1
  ) {
    response_json$code
  } else {
    NULL
  }
  
  if (is.null(retry_code) || nchar(trimws(retry_code)) == 0) {
    return(list(score = 0, attempts = 2))
  }
  
  retry_exec <- run_code(retry_code, model_name = model_name, task = task, run_index = 2)
  retry_result <- read_result(retry_exec$result_path)
  retry_result <- prepare_result_for_scoring(retry_result)
  
  if (isTRUE(retry_result$executed)) {
    return(list(score = 1, attempts = 2, retry_exec = retry_exec, retry_result = retry_result))
  }
  
  list(score = 0, attempts = 2, retry_exec = retry_exec, retry_result = retry_result)
}

# ---- Output scoring - LLM based ----
score_output_llm <- function(task_prompt, variablesCreated, stdout) {
  if((is.null(variablesCreated) || identical(variablesCreated, "NA") || length(variablesCreated) == 0) && (is.null(stdout) || nchar(trimws(stdout)) == 0)) {
    return(list(passed = FALSE, reason = "no_output"))
  }
  
  scoring_prompt <- paste(
    "Below are three fields - 'Task', 'Created variables' and 'Console output.'",
    "Task is a job given to another LLM to carry out through a coding solution.",
    "Created variables refers to the variables observed during execution of the solution, and it is represented as a list with each entry being in the form [<variable name>, <value>]. If no variables were created, you will instead see a string 'NA' in its place.",
    "Console output refers to any information displayed on the console as a result of execution of the solution (e.g print statements).\n",
    "\n",
    "Using your knowledge of what variables were made and what was printed to the console, decide whether or not the task was successfully fulfilled.",
    "Reply ONLY with a JSON object: {\"passed\": true} (if you believe it succeeded) or {\"passed\": false} (if you believe it failed).",
    "Do not include any explanation, additional fields, or any punctuation (i.e a full-stop/period) outside the context of the JSON object. Here are the three fields in question:\n",
    "\n",
    "Task: ", task_prompt,
    "Created variables: ", variablesCreated,
    "Console output: ", stdout
  )
  
  res <- tryCatch(
    ollamar::chat(
        model = SCORER_MODEL,
        messages = list(list(role = "user", content = scoring_prompt)),
        format = "json"
    ),
    error = function(e) list(error = TRUE, message = e$message)
  )

  if (isTRUE(res$error)) {
    return(list(passed = FALSE, reason = paste("scorer_chat_failed:", res$message)))
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(rawToChar(res$body)),
    error = function(e) NULL
  )

  if (is.null(parsed) || is.null(parsed$message$content)) {
    return(list(passed = FALSE, reason = "scorer_response_parse_failed"))
  }

  scorer_content <- parsed$message$content
  # Strip markdown fences first
  scorer_content <- gsub("^```[a-zA-Z]*\\n?", "", scorer_content)
  scorer_content <- gsub("\\n?```\\s*$", "", scorer_content)
  
  #Remove trailing full stops, if they exist.
  scorer_content <- trimws(scorer_content)
  scorer_content <- sub("\\.$", "", scorer_content)
  
  result <- tryCatch(
    jsonlite::fromJSON(scorer_content),
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    json_match <- regmatches(
      scorer_content,
      regexpr("\\{[[:space:][:print:]]*\"passed\"[[:space:]]*:[[:space:]]*(true|false)[[:space:]]*\\}", scorer_content, ignore.case = TRUE)
    )
    
    if (length(json_match) > 0 && nzchar(json_match)) {
      json_match <- tolower(json_match)

      result <- tryCatch(
        jsonlite::fromJSON(json_match),
        error = function(e) NULL
      )
    }
  }
  
  if (is.null(result) && ALLOW_MANUAL_REPAIR) {
    result <- manual_repair(scorer_content, "passed")
  }
  
  if (is.null(result) || is.null(result$passed)) {
    return(list(
      passed = FALSE,
      reason = paste("scorer_failed_raw:", substr(scorer_content, 1, 200))
    ))
  }
  
  list(
    passed = result$passed,
    reason = if (result$passed) "correct" else "incorrect"
  )
}

# ---- Output scoring - vision based ----
score_output_vision <- function(run_dir) {
  image_files <- list.files(
    run_dir,
    pattern = "\\.(png|jpg|jpeg)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(image_files) == 0) {
    return(list(passed = FALSE, reason = "no_image_file"))
  }
  
  image_path <- normalizePath(image_files[1], winslash = "/", mustWork = TRUE)
  
  res <- tryCatch(
    ollamar::chat(
      model = VISION_MODEL,
      messages = list(
        list(
          role = "user",
          content = "Evaluate whether or not this image depicts a straight line graph. I must insist that you only reply with a single word, that being 'True' if it does or 'False' if it does not. No explanations, just your answer that correctly describes the truth of the statement, with no punctuation (i.e no full stops/periods etc.).",
          images = list(image_path)
        )
      )
    ),
    error = function(e) {
      return(list(error = TRUE, message = e$message))
    }
  )

  if(isTRUE(res$error)) {
  return(list(passed = FALSE, reason = paste("vision_model_failed:", res$message)))
  }
  parsed <- jsonlite::fromJSON(rawToChar(res$body))
  response <- trimws(tolower(parsed$message$content))
  
  list(
    passed = grepl("\\btrue\\b", response),
    reason = if (grepl("\\btrue\\b", response)) "correct" else "incorrect"
  )
}

# ---- Output scoring - Excel based ----
score_output_excel <- function(run_dir) {
  excel_files <- list.files(
    run_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(excel_files) == 0) {
    return(list(passed = FALSE, reason = "no_excel_file"))
  }
  
  excel_path <- normalizePath(excel_files[1], winslash = "/", mustWork = TRUE)
  file_size <- file.info(excel_path)$size
  
  tryCatch({
    wb <- openxlsx::read.xlsx(excel_path, colNames = FALSE)
    cell_value <- wb[1, 1]
    cell_value <- as.character(cell_value)
    
    passed <- !is.na(cell_value) && identical(cell_value, "Excel sucks")
    
    list(
      passed = passed,
      reason = if (passed) "correct" else paste("incorrect_content:", cell_value),
      file = excel_path,
      file_size = file_size
    )
  }, error = function(e) {
    list(
      passed = FALSE,
      reason = paste("excel_read_failed:", e$message),
      file = excel_path,
      file_size = file_size
    )
  })
}



# ---- Result post-processing ----
prepare_result_for_scoring <- function(result) {
  vars_created <- result$variablesCreated
  local_vars <- result$localVariables
  
  if (is.null(vars_created)) vars_created <- list()
  if (is.null(local_vars)) local_vars <- list()
  
  chosen_vars <- if (length(vars_created) > 0) vars_created else local_vars
  
  result$variablesCreated <- if (length(chosen_vars) == 0) {
    "NA"
  } else {
    paste(capture.output(str(chosen_vars)), collapse = " ")
  }
  
  result
}
# ---- Manual JSON repair fallback ----
manual_repair <- function(raw_content, target_field = "code") {
  cat("\n--- MANUAL REPAIR REQUIRED ---\n")
  cat("The model response could not be repaired automatically.\n")
  cat("Do NOT improve, rewrite, or reinterpret the underlying content.\n")
  cat("If the repaired input is unusable, this sample will be marked inconclusive.\n\n")
  
  cat("Raw response:\n")
  cat(raw_content, "\n\n")
  
  if (target_field == "code") {
    cat("Only fix formatting, escaping, or fencing issues needed to recover the intended code.\n")
    cat("Paste the repaired code below.\n")
    cat("You may paste either raw code or a quoted string containing literal \\\\n escapes.\n")
    cat("Do NOT fix logic errors or rename variables unless you have explicitly decided to allow that.\n")
    cat("Finish with a line containing only END\n")
    
    lines <- character()
    repeat {
      line <- readline()
      if (identical(trimws(line), "END")) break
      lines <- c(lines, line)
    }
    
    repaired_code <- paste(lines, collapse = "\n")
    repaired_code <- trimws(repaired_code)
    
    if (nchar(repaired_code) == 0) {
      cat("Manual repair failed: no code was provided.\n")
      return(NULL)
    }
    
    # If the whole pasted repair is wrapped in matching quotes, strip them.
    if (
      nchar(repaired_code) >= 2 &&
      (
        (startsWith(repaired_code, "\"") && endsWith(repaired_code, "\"")) ||
        (startsWith(repaired_code, "'") && endsWith(repaired_code, "'"))
      )
    ) {
      repaired_code <- substr(repaired_code, 2, nchar(repaired_code) - 1)
    }
    
    # Normalize common escaped forms if the user pasted code as a JSON-style string.
    repaired_code <- gsub("\\\\n", "\n", repaired_code)
    repaired_code <- gsub("\\\\t", "\t", repaired_code)
    repaired_code <- gsub('\\\\\"', '"', repaired_code)
    repaired_code <- gsub("\\\\'", "'", repaired_code)
    repaired_code <- trimws(repaired_code)
    
    if (nchar(repaired_code) == 0) {
      cat("Manual repair failed: repaired code became empty after normalization.\n")
      return(NULL)
    }
    
    return(list(code = repaired_code))
  }
  
  if (target_field == "passed") {
    cat("Type only true or false, then press Enter.\n")
    answer <- readline()
    answer <- tolower(trimws(answer))
    
    if (identical(answer, "true")) {
      return(list(passed = TRUE))
    }
    
    if (identical(answer, "false")) {
      return(list(passed = FALSE))
    }
    
    cat("Manual repair failed: input must be exactly true or false.\n")
    return(NULL)
  }
  
  cat("Manual repair failed: unsupported target field.\n")
  return(NULL)
}

# ---- Single evaluation ----

evaluate_once <- function(model, model_name, task) {
  combined_prompt <- paste(SYSTEM_PROMPT, task$prompt)
  res <- tryCatch(
    ollamar::chat(
      model = model,
      messages = list(
        #list(role = "system", content = SYSTEM_PROMPT),
        list(role = "user", content = combined_prompt)
      ),
      format = "json"
    ),
    error = function(e)  list(error = TRUE, message = e$message)
  )

  if (isTRUE(res$error)) {
    return(list(
      model = model_name,
      task = task$id,
      level = task$level,
      format_valid = FALSE,
      syntax_valid = "no_code",
      functionality = NA,
      attempts = 1,
      output_passed = NA,
      output_reason = paste("generation_chat_failed:", res$message),
      status = "generation_chat_failed"
    ))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(rawToChar(res$body)),
    error = function(e) NULL
  )

  if (is.null(parsed) || is.null(parsed$message$content)) {
    return(list(
      model = model_name,
      task = task$id,
      level = task$level,
      format_valid = FALSE,
      syntax_valid = "no_code",
      functionality = NA,
      attempts = 1,
      output_passed = NA,
      output_reason = "generation_response_parse_failed",
      status = "generation_response_parse_failed"
    ))
  }


  # Extract model content
  content <- parsed$message$content
  
  # Strip markdown fences first
  content <- gsub("^```[a-zA-Z]*\\n?", "", content)
  content <- gsub("\\n?```\\s*$", "", content)
  
  #Remove trailing full stops, if they exist.
  content <- trimws(content)
  content <- sub("\\.$", "", content)
  
  response_json <- tryCatch(
    jsonlite::fromJSON(content),
    error = function(e) NULL
  )
  
  if(is.null(response_json)) {
    start_pos <- regexpr("\\{", content)[1]
    end_positions <- gregexpr("\\}", content)[[1]]
    if (start_pos != -1 && end_positions[1] != -1) {
      end_pos <- tail(end_positions, 1)
      candidate_json <- substr(content, start_pos, end_pos)
      
      response_json <- tryCatch(
        jsonlite::fromJSON(candidate_json),
        error = function(e) NULL
      )
    }
  }
  if (is.null(response_json) && ALLOW_MANUAL_REPAIR) {
    response_json <- manual_repair(content)
  }
  
  if (!is.null(response_json) &&
      ##is.list(response_json) &&
      !is.null(response_json$code) &&
      is.character(response_json$code) &&
      length(response_json$code) == 1) {
    
    code <- response_json$code
    code <- gsub("\\\\n", "\n", code)
    format_valid <- TRUE
    
  } else {
    code <- NULL
    format_valid <- FALSE
  }
  
  # Classify syntax before execution
  syntax_valid <- validate_syntax(code)
  
  if (is.null(code) || syntax_valid == "no_code") {
    return(list(
      model = model_name,
      task = task$id,
      level = task$level,
      format_valid = format_valid,
      syntax_valid = syntax_valid,
      functionality = NA,
      attempts = 1,
      output_passed = NA,
      output_reason = "format_inconclusive",
      status = "inconclusive"
    ))
  }
  
  exec <- run_code(code, model_name = model_name, task = task, run_index = 1)
  
  result <- read_result(exec$result_path)
  result <- prepare_result_for_scoring(result)
  # Score functionality
  functionality <- score_functionality(exec, result, model, model_name, task)
  
  # Use retry exec if available
  exec <- if (!is.null(functionality$retry_exec)) functionality$retry_exec else exec
  result <- if (!is.null(functionality$retry_result)) functionality$retry_result else result
  
  if(!isTRUE((result$executed))) {
    score <- list(
      output_passed = FALSE,
      output_reason = result$status
    )
  } else {
    score <- score_result(result, task, exec$run_dir)
  }
  
  list(
    model = model_name,
    task = task$id,
    level = task$level,
    format_valid = format_valid,
    syntax_valid = syntax_valid,
    functionality = functionality$score,
    attempts = functionality$attempts,
    output_passed = score$output_passed,
    output_reason = score$output_reason,
    status = result$status
    #score = score,
    #raw = result
  )
}

# ---- Orchestrator ----

run_evaluation <- function(models, tasks, N_RUNS) {
  results <- list()
  for (model_name in names(models)) {
    model <- models[[model_name]]
    for (task in tasks) {
      for (i in seq_len(N_RUNS)) {
        results <- append(results, list(
          evaluate_once(model, model_name, task)
        ))
      }
    }
  }
  results
}

# ---- Syntax validation ----
validate_syntax <- function(code) {
  if (is.null(code) || nchar(trimws(code)) == 0) {
    return("no_code")
  }
  
  tmp <- tempfile(fileext = ".py")
  writeLines(code, tmp)
  
  check <- system2(
    PYTHON_PATH,
    args = c("-m", "py_compile", tmp),
    stdout = TRUE,
    stderr = TRUE
  )
  
  if (length(check) == 0) "valid_syntax" else "invalid_syntax"
}

# ---- Clean up ----
cleanup_workspace <- function() {
  choice <- readline(prompt = "Clear workspace? 'y' for yes, 'n' or anything else for no.")
  if (dir.exists(WORK_DIR) && (tolower(trimws(choice)) == "y" || !interactive())) {
    unlink(WORK_DIR, recursive = TRUE)
    print("Workspace cleaned up.")
  }
}

# ---- Entry point ----
ALLOW_MANUAL_REPAIR <- init_manual_repair()
cleanup_workspace()
tasks <- get_tasks(TASK_DIR)
tasks <- add_scoring_prompts(tasks)
results <- run_evaluation(models, tasks, N_RUNS)
saveRDS(results, "results.rds")
writeLines(jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE), "results.json")
df <- do.call(rbind, lapply(results, as.data.frame))
print(df)
