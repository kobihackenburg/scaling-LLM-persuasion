
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
df_for_model <- readRDS("output/processed_data/prepared_data.rds")
df_estimates <- readRDS("output/processed_data/df_estimates.rds")

# Make variable for is this AI ----
df_for_model %>% count(authorship)

df_for_model <-
  df_for_model %>% 
  mutate(yes_AI = case_when(authorship == "An AI language model" ~ 1,
                            authorship %in% c(df_for_model$authorship %>% unique %>% .[2:7]) ~ 0))

df_for_model %>% count(authorship)
df_for_model %>% count(yes_AI)

# Estimate message-level means ----

# For each issue fit linear regression to estimate individual message means
list_issues <- df_for_model %>% pull(issue) %>% unique

df_estimates_ai <-
  map(list_issues,
      function(.x) {
        
        # Fit model for issue .x
        reg_model <-
          lm_robust(
            formula = yes_AI ~ 0 + factor(message_id) + party + ideology + knowledge, 
            data = df_for_model %>% filter(issue == .x)
          )
        
        # Get and return estimates
        tidy(reg_model) %>% 
          filter(str_detect(term, "message_id")) %>% # Retain only message ID terms in estimates
          mutate(issue = .x)
        
      }) %>% 
  bind_rows()

# Join estimates with those from primary analyses 
df_estimates_ai <-
  df_estimates_ai %>% 
  rename(message_id = term) %>% 
  mutate(message_id = as.numeric(str_remove(message_id, "factor\\s*\\([^\\)]+\\)"))) %>% 
  left_join(df_estimates %>% select(model, issue, message_id, parameters),
            by = c("issue", "message_id"))

# Fit model with parameter count ----
out_fit <-
  map(1,
     function(.x){
      
      df_temp <-
        df_estimates_ai %>% 
        filter(model != "human") %>% 
        mutate(parameters = case_when(is.na(parameters) ~ 300, T ~ parameters)) %>% 
        mutate(param_count = parameters*1e9) %>%  # Get parameter count on full scale
        mutate(log_param_count = log(param_count)) %>% # Log it
        mutate(log_param_count_c = as.numeric(scale(log_param_count, center = T, scale = F))) # Center it
      
      rma.mv(
        yi = estimate,
        V = std.error^2,
        mods = ~ 1 + poly(log_param_count_c, degree = 1, raw = TRUE),
        random = list(~1 + poly(log_param_count_c, degree = 1, raw = TRUE) | issue,
                      ~1 | message_id,
                      ~1 | model),
        control = list(rel.tol = 1e-8),
        sparse = T,
        data = df_temp
      ) 
      
    }) %>% 
  .[[1]]

# Predicted values ----

x_values <- 
  seq(from = min(out_fit$data$log_param_count_c), 
      to   = max(out_fit$data$log_param_count_c), 
      length.out = 100)

mean_log_param_count <- mean(out_fit$data$log_param_count)

out_fit_predict <-
  predict(out_fit, newmods = unname(poly(x_values, degree = 1, raw = TRUE))) %>% 
  as.data.frame() %>% 
  mutate(estimate = pred,
         param_count_in_b = exp(x_values + mean_log_param_count) / 1e9)

# Raw model means ----
out_raw_mod_means <-
  rma.uni(
    yi = estimate,
    sei = std.error,
    mods = ~ 0 + model,
    control = list(rel.tol = 1e-8),
    data = df_estimates_ai
  ) 

# Plot ----

out_raw_mod_means <-
  out_raw_mod_means %>% 
  tidy() %>% 
  mutate(term = str_remove_all(term, "model")) %>% 
  rename(model = term) %>% 
  left_join(df_estimates %>% select(model, parameters) %>% distinct(.keep_all = T), 
            by = "model") %>% 
  mutate(lwr = estimate - 1.96*std.error,
         upr = estimate + 1.96*std.error)

g <- 
  out_raw_mod_means %>% 
  filter(model != "human") %>% 
  mutate(parameters = case_when(is.na(parameters) ~ 300, T ~ parameters)) %>% 
  rename(param_count_in_b = parameters) %>% 
  ggplot(aes(x = param_count_in_b, y = estimate)) +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  scale_x_continuous(trans = "log10") +
  ggrepel::geom_text_repel(aes(label = model)) +
  geom_line(data = out_fit_predict) +
  geom_ribbon(data = out_fit_predict, aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = out_raw_mod_means %>% filter(model == "human") %>% pull(estimate),
             color = "red") +
  annotate(geom = "rect", xmin = 1/1e9, xmax = 1000, alpha = 0.1, fill = "red",
           ymin = out_raw_mod_means %>% filter(model == "human") %>% pull(lwr),
           ymax = out_raw_mod_means %>% filter(model == "human") %>% pull(upr)) +
  annotate(geom = "text", x = 0.08, y = 0.15, label = "Human", color = "red") +
  coord_cartesian(xlim = c(0.08, 300)) +
  labs(x = "Parameter count in billions",
       y = "Estimated probability subject said the message was authored by AI (95% CI)")

ggsave(plot = g, filename = "output/plots/is_this_ai.pdf", height = 8, width = 12)
