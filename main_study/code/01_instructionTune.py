import os
# os.environ["CUDA_VISIBLE_DEVICES"] = "0,1,2"

import json
import fire
import torch
import tempfile
import pandas as pd
from trl import SFTTrainer
from peft import LoraConfig
from datasets import Dataset
from transformers import TrainingArguments


BASE_REPOSITORY = 'persuasion-scaling-laws'
os.environ["WANDB_DISABLED"] = "True"

# Determine the directory of the script
script_directory = os.path.dirname(os.path.abspath(__file__))

# Use script_directory to construct the path to the data files
train_data_path = os.path.join(script_directory, "GPT-4_Alpaca_train.jsonl")
eval_data_path = os.path.join(script_directory, "GPT-4_Alpaca_eval.jsonl")

def load_jsonl(file_path):
    data = []
    with open(file_path, 'r') as file:
        for line in file:
            data.append(json.loads(line))
    return data

def create_prompt(row):
    return ("Below is an instruction that describes a task. "
            "Write a response that appropriately completes the request.\n\n"
            "### Instruction:\n{instruction}\n\n### Response:\n{output}").format_map(row)

def train_model(model_id, train_dataset, eval_dataset):
    MODEL_NAME = model_id.split('/')[-1]

    model_kwargs = {
        "device_map": "auto",
        "trust_remote_code": False, #set to true for non-falcon models? 
        "torch_dtype": torch.bfloat16,
        "use_cache": False,
    }

    if "Llama" in MODEL_NAME:
        target_modules = ["q_proj", "k_proj", "v_proj", "o_proj", "gate_proj", "down_proj", "up_proj"]
    elif "pythia" in MODEL_NAME:
        target_modules = ["query_key_value", "dense","dense_h_to_4h","dense_4h_to_h"]  
    elif "yi" in MODEL_NAME:
        target_modules = ["q_proj", "k_proj", "v_proj", "o_proj", "gate_proj", "down_proj", "up_proj"]  
    elif "Qwen" in MODEL_NAME:
        target_modules = ["q_proj", "k_proj", "v_proj", "o_proj", "gate_proj", "down_proj", "up_proj"]  
    elif "falcon" in MODEL_NAME:
        target_modules = ["query_key_value", "dense","dense_h_to_4h","dense_4h_to_h"]
    else:
        target_modules = ["q_proj", "k_proj", "v_proj", "o_proj", "gate_proj", "down_proj", "up_proj"]


    peft_config = LoraConfig(
        r=64,
        lora_alpha=16,
        lora_dropout=0.1,
        bias="none",
        task_type="CAUSAL_LM",
        target_modules=target_modules
    )

    batch_size = 16
    gradient_accumulation_steps = 2
    num_train_epochs = 3
    total_num_steps = num_train_epochs * 11_210 // (batch_size * gradient_accumulation_steps)

    custom_temp_dir = "/bask/projects/v/vjgo8416-llm-tune/DATASTORE"
    output_dir = tempfile.mkdtemp(dir=custom_temp_dir)
    # output_dir = tempfile.mkdtemp()
    training_args = TrainingArguments(
        output_dir=output_dir,
        per_device_train_batch_size=batch_size,
        per_device_eval_batch_size=batch_size//2,
        bf16=True,
        learning_rate=2e-4,
        lr_scheduler_type="cosine",
        warmup_ratio=0.1,
        max_steps=total_num_steps,
        gradient_accumulation_steps=gradient_accumulation_steps,
        gradient_checkpointing=True,
        gradient_checkpointing_kwargs={"use_reentrant": False},
        evaluation_strategy="steps",
        eval_steps=total_num_steps // num_train_epochs,
        logging_strategy="no",
        logging_steps=1,
        save_strategy="no",
        save_steps=total_num_steps // num_train_epochs,
    )

    trainer = SFTTrainer(
        model=model_id,
        model_init_kwargs=model_kwargs,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        packing=True,
        max_seq_length=1024,
        args=training_args,
        formatting_func=create_prompt,
        peft_config=peft_config,
    )

    trainer.train()

    # EXPORT MODEL
    os.environ["HF_API_TOKEN"] = "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"
    model = trainer.model

    # Push model to Hugging Face hub
    model.push_to_hub(
        f"{BASE_REPOSITORY}/{MODEL_NAME}",
        use_auth_token=os.environ["HF_API_TOKEN"],
        private=True,
    )
    
    # Clear cache
    # os.system("rm -rf /home/grte3673/.cache/*")
    os.system("rm -rf /bask/projects/v/vjgo8416-llm-tune/DATASTORE/models/*")
    
    # Free GPU memory
    del model
    torch.cuda.empty_cache()

def main(model_or_models):
    train_dataset = load_jsonl(train_data_path)
    eval_dataset = load_jsonl(eval_data_path) 

    train_dataset = Dataset.from_pandas(pd.DataFrame(train_dataset))
    eval_dataset = Dataset.from_pandas(pd.DataFrame(eval_dataset))

    if isinstance(model_or_models, list):
        for model_id in model_or_models:
            train_model(model_id, train_dataset, eval_dataset)
    else:
        train_model(model_or_models, train_dataset, eval_dataset)

if __name__ == "__main__":
    fire.Fire(main)
