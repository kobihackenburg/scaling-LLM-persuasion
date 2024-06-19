
library(tidyverse)
library(metafor)
library(broom)
library(estimatr)
library(ggrepel)
library(cowplot)
library(tidybayes)
set.seed(42)
options(scipen = 999)

# Read in data ----
#setwd("/Users/kobihackenburg/Documents/Papers/scaling laws + persuasion/project github/scaling-laws-pp/main_study")
df_data <- read.csv("final_data_with_metrics.csv")

df_data <- 
  df_data %>%
  mutate(model = case_when(
    model == "pythia-70m" ~ "Pythia-70M",
    model == "pythia-160m" ~ "Pythia-160M",
    model == "pythia-410m" ~ "Pythia-410M",
    model == "pythia-1b" ~ "Pythia-1B",
    model == "pythia-1.4b" ~ "Pythia-1.4B",
    model == "pythia-2.8b" ~ "Pythia-2.8B",
    model == "pythia-6.9b" ~ "Pythia-6.9B",
    model == "pythia-12b" ~ "Pythia-12B",
    model == "Llama-2-7b-hf" ~ "Llama2-7B",
    model == "Llama-2-13b-hf" ~ "Llama2-13B",
    model == "Llama-2-70b-hf" ~ "Llama2-70B",
    model == "falcon-7b" ~ "Falcon-7B",
    model == "falcon-40b" ~ "Falcon-40B",
    model == "claude-3-opus-20240229" ~ "Claude-3-Opus",
    model == "gpt-4-0125-preview" ~ "GPT-4-Turbo",
    TRUE ~ model
  ))

df_data <-
  df_data %>% 
  mutate(issue = str_to_title(str_replace_all(issue, "_", " ")))

# df_data <- 
#   df_data %>%
#   mutate(issue = case_when(
#     issue == "veteran healthcare" ~ "Veterans' Healthcare",
#     issue == "solitary_confinement" ~ "Solitary Confinement",
#     issue == "worker_pensions" ~ "Worker Pensions",
#     issue == "foreign_aid" ~ "Foreign Aid",
#     issue == "medicaid" ~ "Medicaid",
#     issue == "electoral_college" ~ "Electoral College",
#     issue == "felons_voting" ~ "Felons' Voting Rights",
#     issue == "border_restrictions" ~ "Border Restrictions",
#     issue == "affirmative_action" ~ "Affirmative Action",
#     issue == "assisted suicide" ~ "Assisted Suicide",
#     TRUE ~ issue
#   ))

# Inspect it etc.
df_data %>% names

df_data %>% 
  count(condition) %>% 
  mutate(prop = n/sum(n))

df_data %>% 
  count(model) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop))

df_data %>% count(treatment_message_id)

# Get data for modelling
df_for_model <-
  df_data %>% 
  #filter(condition != "human") %>% 
  select(dv_response_mean, message_id, condition, model, model_family, issue, parameters,
         treatment_message_word_count, political_party, political_ideology, political_knowledge,
         moral_nonmoral_ratio, flesch, emotion_proportion, type_token_ratio, starts_with("gpt_"), 
         valence_correct, task_completion, authorship, pretraining_tokens) %>% 
  rename_with(~str_remove_all(.x, "political_"), starts_with("political")) %>% 
  mutate(message_id = ifelse(condition == "control", 0, message_id)) %>% 
  mutate(pretraining_tokens = case_when(model %in% c("GPT-4-Turbo", "Claude-3-Opus") ~ 4,
                                        T ~ pretraining_tokens))

saveRDS(df_for_model, "output/processed_data/prepared_data.rds")
