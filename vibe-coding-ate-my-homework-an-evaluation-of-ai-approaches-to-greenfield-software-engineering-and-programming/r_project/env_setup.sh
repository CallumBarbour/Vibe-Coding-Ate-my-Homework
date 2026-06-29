#!/bin/bash

case ":$PATH:" in
  *":/mnt/scratch2/users/40290129/local/R/4.5.3/bin:"*) ;;
  *) export PATH=/mnt/scratch2/users/40290129/local/R/4.5.3/bin:$PATH ;;
esac

case ":$PATH:" in
  *":/opt/apps/ollama/0.17.7/bin:"*) ;;
  *) export PATH=/opt/apps/ollama/0.17.7/bin:$PATH ;;
esac

case ":$PATH:" in
  *":/mnt/scratch2/users/40290129/pyenv/bin:"*) ;;
  *) export PATH=/mnt/scratch2/users/40290129/pyenv/bin:$PATH ;;
esac

export R_LIBS_USER=/mnt/scratch2/users/40290129/local/Rlibs
export OLLAMA_MODELS=/mnt/scratch2/users/40290129/ollama_models
export OLLAMA_HOST=127.0.0.1:11434
export MPLBACKEND=Agg
export OLLAMA_MAX_LOADED_MODELS=1
export OLLAMA_KEEP_ALIVE=5m
export OLLAMA_NUM_PARALLEL=1
export OLLAMA_CONTEXT_LENGTH=8192
