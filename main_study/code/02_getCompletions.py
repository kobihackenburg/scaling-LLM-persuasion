import os
import pandas as pd
import torch
from peft import PeftModel    
from transformers import AutoModelForCausalLM, AutoTokenizer
import fire

def process_models(input_csv='main_study/prompts.csv'):
    models_and_adapters = {
        # "EleutherAI/pythia-70m": "persuasion-scaling-laws/pythia-70m",
        # "EleutherAI/pythia-160m": "persuasion-scaling-laws/pythia-160m",
        # "EleutherAI/pythia-410m": "persuasion-scaling-laws/pythia-410m",
        # "EleutherAI/pythia-1b": "persuasion-scaling-laws/pythia-1b",
        # "EleutherAI/pythia-1.4b": "persuasion-scaling-laws/pythia-1.4b",
        # "EleutherAI/pythia-2.8b": "persuasion-scaling-laws/pythia-2.8b",
        # "EleutherAI/pythia-6.9b": "persuasion-scaling-laws/pythia-6.9b",
        # "EleutherAI/pythia-12b": "persuasion-scaling-laws/pythia-12b",
        # "Qwen/Qwen1.5-0.5B": "persuasion-scaling-laws/Qwen1.5-0.5B",
        # "Qwen/Qwen1.5-1.8B": "persuasion-scaling-laws/Qwen1.5-1.8B",
        # "Qwen/Qwen1.5-4B": "persuasion-scaling-laws/Qwen1.5-4B",
        # "Qwen/Qwen1.5-7B": "persuasion-scaling-laws/Qwen1.5-7B",
        # "Qwen/Qwen1.5-14B": "persuasion-scaling-laws/Qwen1.5-14B",
        # "Qwen/Qwen1.5-72B": "persuasion-scaling-laws/Qwen1.5-72B",
        # "01-ai/Yi-6B": "persuasion-scaling-laws/Yi-6B",
        # "01-ai/Yi-9B": "persuasion-scaling-laws/Yi-9B",
        # "01-ai/Yi-34B": "persuasion-scaling-laws/Yi-34B",
        # "meta-llama/Llama-2-7b-hf": "persuasion-scaling-laws/Llama-2-7b-hf",
        # "meta-llama/Llama-2-13b-hf": "persuasion-scaling-laws/Llama-2-13b-hf",
        # "meta-llama/Llama-2-70b-hf": "persuasion-scaling-laws/Llama-2-70b-hf",
        # "tiiuae/falcon-7b": "persuasion-scaling-laws/falcon-7b",
        "tiiuae/falcon-40b": "persuasion-scaling-laws/falcon-40b"
    }

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    df = pd.read_csv(input_csv)
    df['prompt_with_chat_template'] = ''  
    df['response'] = ''
    df['temperature'] = 1  
    df['top_k'] = 20  
    df['top_p'] = 0.9  

    all_dfs = []

    for model_name, adapters_name in models_and_adapters.items():
        m = AutoModelForCausalLM.from_pretrained(model_name, torch_dtype=torch.bfloat16, device_map = "auto") #device_map={"": 0}
        m = PeftModel.from_pretrained(m, adapters_name).merge_and_unload()
        tok = AutoTokenizer.from_pretrained(model_name)
        tok.bos_token_id = 1

        # m = m.to(device)
        adapter_short_name = adapters_name.split("/")[-1]
        df['model'] = adapter_short_name

        for index, row in df.iterrows():
            prompt = row['prompt_full_text']
            prompt_with_chat_template = "Below is an instruction that describes a task. Write a response that appropriately completes the request.\n\n ### Instruction:\n" + prompt + "\n\n### Response:\n"
            inputs = tok(prompt_with_chat_template, return_tensors="pt").to(device)
            outputs = m.generate(**inputs, do_sample=True, num_beams=1, max_new_tokens=600, temperature=row['temperature'], top_k=row['top_k'], top_p=row['top_p'])
            response_text = tok.batch_decode(outputs, skip_special_tokens=True)[0]
            response_only = response_text.split("### Response:\n")[-1].strip()
            df.at[index, 'response'] = response_only
            df.at[index, 'prompt_with_chat_template'] = prompt_with_chat_template
        
        df['response_word_count'] = df['response'].apply(lambda x: len(x.split()))
        df.to_csv(f'main_study/completions/{adapter_short_name}_responses.csv', index=False, encoding='utf-8')
        all_dfs.append(df.copy())
        # os.system("rm -rf ~/.cache/huggingface/hub/*")
        os.system("rm -rf /bask/projects/v/vjgo8416-llm-tune/DATASTORE/models/*")
        
    # final_df = pd.concat(all_dfs)
    # final_df['response_id'] = range(1, len(final_df) + 1)
    # final_df.to_csv('main_study/completions/all_responses.csv', index=False, encoding='utf-8')

if __name__ == '__main__':
    fire.Fire(process_models)
