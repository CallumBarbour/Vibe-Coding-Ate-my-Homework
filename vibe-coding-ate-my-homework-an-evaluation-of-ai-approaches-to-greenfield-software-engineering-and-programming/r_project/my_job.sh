#!/bin/bash
#SBATCH --job-name=my-gpu-job
#SBATCH --output=my-gpu-job.out
#SBATCH --error=my-gpu-job.err
#SBATCH --time=02:00:00
#SBATCH --partition=k2-gpu-a100mig
#SBATCH --gres gpu:3g.40gb:1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4

source env_setup.sh

echo "=== Quota before Ollama setup ==="
quota -s
du -sh "$OLLAMA_MODELS" 2>/dev/null
echo

ollama serve > ollama_server.log 2>&1 &
OLLAMA_PID=$!

sleep 10

echo "Ensuring required Ollama models are available..."
ollama pull qwen2.5:14b
ollama pull gemma3:12b
ollama pull phi4:14b
ollama pull mistral-nemo:12b
ollama pull qwen2.5-coder:14b
ollama pull llama3.2-vision:11b
echo "Available models for this run:"
ollama list

echo "=== Quota after Ollama setup ==="
quota -s
du -sh "$OLLAMA_MODELS" 2>/dev/null
echo

printf 'n\ny\n' | Rscript greenfieldEvaluationSuite.R
R_EXIT=$?
kill "$OLLAMA_PID"
exit "$R_EXIT"
