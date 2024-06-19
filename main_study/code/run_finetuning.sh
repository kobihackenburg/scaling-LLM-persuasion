#!/bin/bash

#SBATCH --account vjgo8416-llm-tune
#SBATCH --qos turing
#SBATCH --time 0-20:00:0
#SBATCH --nodes 1
#SBATCH --gpus 3
#SBATCH --cpus-per-gpu 36
#SBATCH --job-name falcon-40b
#SBATCH --constraint=a100_80

module purge; module load baskerville
module load bask-apps/live 
module load Python/3.10.8-GCCcore-12.2.0

source kobi-venv/bin/activate

export HUGGINGFACE_HUB_TOKEN='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'

# Check NVIDIA GPU status
echo "Checking NVIDIA GPU setup:"
nvidia-smi
echo "--------------------------"

export TRANSFORMERS_CACHE=/bask/projects/v/vjgo8416-llm-tune/DATASTORE/models

python main_study/01_instructionTune.py \
    --model_or_models=tiiuae/falcon-40b