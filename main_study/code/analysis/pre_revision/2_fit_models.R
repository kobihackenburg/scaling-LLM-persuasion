
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

# Estimate message-level effects ----

# For each issue fit linear regression to estimate individual message effects
list_issues <- df_for_model %>% pull(issue) %>% unique

out_regs <-
  map(list_issues,
      function(.x) {
        
        # Fit model for issue .x
        reg_model <-
          lm_robust(
            formula = dv_response_mean ~ 1 + factor(message_id) + party + ideology + knowledge, 
            data = df_for_model %>% filter(issue == .x)
          )
        
        # Get covariance matrix
        vcov_matrix <- reg_model$vcov
        
        # Retain only message ID terms in covariance matrix
        vcov_matrix <- vcov_matrix[str_detect(rownames(vcov_matrix), "message_id"),
                                   str_detect(colnames(vcov_matrix), "message_id")]
        
        # Get estimates
        model_estimates <- 
          tidy(reg_model) %>% 
          filter(str_detect(term, "message_id")) %>% # Retain only message ID terms in estimates
          mutate(issue = .x)
        
        # Check SEs match
        stopifnot(identical(
          vcov_matrix %>% diag %>% sqrt %>% unname %>% round(8), 
          model_estimates$std.error %>% round(8)
        ))
        
        # Check symmetric
        stopifnot(isSymmetric.matrix(vcov_matrix))
        
        # Return
        list("estimates" = model_estimates,
             "vcov" = vcov_matrix)
        
      })

names(out_regs) <- list_issues

# Bind estimates
df_estimates <-
  map(out_regs, ~.x$estimates) %>% 
  bind_rows()

# Make block diagonal vcov matrix
vcov_block <-
  map(out_regs, ~.x$vcov) %>% 
  bdiag() %>% 
  as.matrix()

dim(vcov_block)

# Wrangle message-level estimates ----

# Add message-level features
df_estimates <-
  df_estimates %>% 
  rename(message_id = term) %>% 
  mutate(message_id = as.numeric(str_remove(message_id, "factor\\s*\\([^\\)]+\\)"))) %>% 
  left_join(df_for_model %>% 
              #filter(condition != "human") %>% 
              filter(message_id != 0) %>%
              distinct(message_id, .keep_all = T) %>% 
              select(message_id, model, model_family, issue, treatment_message_word_count, parameters,
                     moral_nonmoral_ratio, flesch, emotion_proportion, type_token_ratio, starts_with("gpt_"), 
                     valence_correct, task_completion, authorship, pretraining_tokens),
            by = c("message_id", "issue")) %>% 
  # Factor task completion
  mutate(task_completion_fac = factor(task_completion))

# Create model categories variable
model_bins <- 
  list("Human" = "human",
       "Small (<=7B)" = df_for_model %>% filter(parameters <= 7) %>% pull(model) %>% unique,
       "Medium (9-40B)" = df_for_model %>% filter(parameters > 7 & parameters <= 40) %>% pull(model) %>% unique,
       "Large (69-72B)" = df_for_model %>% filter(parameters > 40 & parameters <= 72) %>% pull(model) %>% unique,
       "Frontier" = df_for_model %>% filter(str_detect(model, "Claude-3-Opus|GPT-4")) %>% pull(model) %>% unique)

model_names <- df_estimates %>% pull(model) %>% unique

# Attach
df_estimates <-
  df_estimates %>% 
  mutate(model_bins = map(
    df_estimates$model,
    function(model_y) { model_bins[map(model_bins, ~ model_y %in% .x) %>% unlist] %>% names }) %>% unlist) %>% 
  mutate(model_bins = factor(model_bins, levels = c("Frontier", 
                                                    "Large (69-72B)", 
                                                    "Medium (9-40B)", 
                                                    "Small (<=7B)", 
                                                    "Human")),
         model_inds = factor(model, levels = c(model_names[str_detect(model_names, "Claude-3-Opus")],
                                               model_names[str_detect(model_names, "Claude-3-Opus", negate = T)])))

# Make model-level means of mediators
mediator_col_names <- c("moral_nonmoral_ratio", "flesch", "emotion_proportion", "type_token_ratio", 
                        "task_completion",  "pretraining_tokens", "treatment_message_word_count")

df_mod_lvl_means <-
  df_estimates %>% 
  reframe(across(all_of(mediator_col_names), ~ mean(.x), .names = "{col}_mod_lvl"), .by = model)
  
df_estimates <-
  df_estimates %>% 
  left_join(df_mod_lvl_means, by = "model")


saveRDS(df_estimates, "output/processed_data/df_estimates.rds")

# Compute raw model-level ATEs ----

stopifnot(identical(vcov_block %>% diag %>% sqrt %>% round(8), df_estimates$std.error %>% round(8))) # Check SEs match
stopifnot(isSymmetric.matrix(vcov_block)) # Check symmetric

out_raw_mod_ATEs <-
  rma.mv(
    yi = estimate,
    V = vcov_block,
    mods = ~ 0 + model,
    control = list(rel.tol = 1e-8),
    sparse = T,
    data = df_estimates
  ) %>% 
  tidy() %>% 
  mutate(lwr = estimate - 1.96*std.error,
         upr = estimate + 1.96*std.error) %>% 
  mutate(term = str_remove(term, "model")) %>% 
  rename(model = term) %>% 
  left_join(df_estimates %>% select(model, model_family, parameters) %>% distinct(.keep_all = T), by = "model")

saveRDS(out_raw_mod_ATEs, "output/processed_data/raw_model_ATEs.rds")

# Compute contrast tests ----
out_contrasts <-
  rma.mv(
    yi = estimate,
    V = vcov_block,
    mods = ~ 1 + factor(model_inds),
    control = list(rel.tol = 1e-8),
    sparse = T,
    data = df_estimates
  ) %>% 
  tidy()

# Wrangle
out_contrasts <-
  out_contrasts %>% 
  mutate(term = str_remove(term, "factor\\s*\\([^\\)]+\\)")) %>% 
  mutate(estimate_scaled_vs_intercept = case_when(
    term != "intercept" ~ estimate + out_contrasts %>% filter(term == "intercept") %>% pull(estimate),
    T ~ NA_real_
  ))

saveRDS(out_contrasts, "output/processed_data/contrasts_vs_claude.rds")

# Fit primary models with parameter count covariate ----

# First define model specifications

# Get specifications for potential mediators
list_mediators <- names(df_estimates)[str_detect(names(df_estimates), "mod_lvl")] %>% map_chr(~paste0(.x, "_z"))
spec_mediators <-
  map(list_mediators,
    function(.x) {
      
      tribble(
        ~specification,     ~fixef,                       ~ranef,
        .x,                 paste0("~1+", .x),            list("~1|issue", 
                                                               "~1|message_id",
                                                               "~1|model")
      )
      
    }) %>% 
  bind_rows()

# Create all specifications
specifications <- 
  tribble( 
  
  ~specification,     ~fixef,                                                            ~ranef,
  
  "linear",           "~1+poly(log_param_count_c, degree=1, raw=TRUE)",                  list("~1+poly(log_param_count_c, degree=1, raw=TRUE)|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model"),
  "family_fixed_fx",  "~1+poly(log_param_count_c, degree=1, raw=TRUE)+model_family",     list("~1+poly(log_param_count_c, degree=1, raw=TRUE)|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model"),
  "quadratic",        "~1+poly(log_param_count_c, degree=2, raw=TRUE)",                  list("~1+poly(log_param_count_c, degree=2, raw=TRUE)|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model"),
  "cubic",            "~1+poly(log_param_count_c, degree=3, raw=TRUE)",                  list("~1+poly(log_param_count_c, degree=3, raw=TRUE)|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model"),
  "mediators_joint",  paste0("~1+", paste0(list_mediators, collapse = "+")),             list("~1|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model"),
  "task_comp2_alone", "~1+poly(task_completion_mod_lvl, degree=2, raw=TRUE)",            list("~1+poly(task_completion_mod_lvl, degree=2, raw=TRUE)|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model"),
  "task_comp3_alone", "~1+poly(task_completion_mod_lvl, degree=3, raw=TRUE)",            list("~1+poly(task_completion_mod_lvl, degree=3, raw=TRUE)|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model"),
  "primary_adjust2", paste0("~1+poly(log_param_count_c, degree=1, raw=TRUE)", 
                            "+poly(task_completion_mod_lvl, degree=2, raw=TRUE)"),       list("~1+poly(log_param_count_c, degree=1, raw=TRUE)|issue",
                                                                                              "~1|message_id",
                                                                                              "~1|model")
) %>% 
  bind_rows(spec_mediators)

# Define n parameters for frontier models (in billions)
upper_b <- 1000
n_p_frontier <- seq(from = 300, to = upper_b, by = 100)

# Define combinations for modelling
df_combos <- crossing(specifications, "frontier_params_in_b" = n_p_frontier)

# Drop redundant rows
df_combos <-
  df_combos %>% 
  mutate(retain = case_when(
    specification %in% c(df_combos$specification[df_combos$specification != "linear"] %>% unique) & frontier_params_in_b == 300 ~ 1,
    specification == "linear" ~ 1,
    T ~ 0
    )) %>% 
  filter(retain == 1)

# Now fit the models!
out_models <- list()

for (i in 1:nrow(df_combos)) {
  
  id_spec   <- df_combos[[i,"specification"]]
  id_fixef  <- df_combos[[i,"fixef"]]
  id_ranef  <- df_combos[[i,"ranef"]]
  id_params <- df_combos[[i,"frontier_params_in_b"]]
  
  # Drop human estimates
  drop_humans       <- df_estimates$model != "human"
  df_estimates_temp <- df_estimates[drop_humans,]
  vcov_block_temp   <- vcov_block[drop_humans, drop_humans]
  
  stopifnot(identical(vcov_block_temp %>% diag %>% sqrt %>% round(8), df_estimates_temp$std.error %>% round(8))) # Check SEs match
  stopifnot(isSymmetric.matrix(vcov_block_temp)) # Check symmetric
  
  # Wrangle parameter count variable
  df_estimates_temp <-
    df_estimates_temp %>% 
    # Parameter count variable
    mutate(parameters = ifelse(is.na(parameters), id_params, parameters)) %>% # Impute parameter count for frontier models
    mutate(param_count = parameters*1e9) %>%  # Get parameter count on full scale
    mutate(log_param_count = log(param_count)) %>% # Log it
    mutate(log_param_count_c = as.numeric(scale(log_param_count, center = T, scale = F))) %>% # Center it
    # Z-score mediators
    mutate(across(str_remove_all(list_mediators, "_z"), ~ as.numeric(scale(.x)), .names = "{col}_z"))
  
  # Fit
  out_models[[i]] <-
    rma.mv(
      yi = estimate,
      V = vcov_block_temp,
      mods = as.formula(id_fixef),
      random = map(id_ranef[[1]], ~as.formula(.x)),
      struct = "GEN",
      control = list(
        rel.tol = 1e-8
        # optimizer = "optimParallel", 
        # ncpus = parallel::detectCores()
        ),
      sparse = T,
      data = df_estimates_temp
    )
  
  print(paste0("Model ", i, " of ", nrow(df_combos), " fitted: ", id_spec, " (frontier params = ", id_params, "B)"))
  
}

# Quick-look model results
map(out_models, ~tidy(.x)) %>% bind_rows()

df_combos$model_fit <- out_models

# Record n degree for polynomial
df_combos <-
  df_combos %>% 
  mutate(n_degree = as.numeric(
    case_when(str_detect(fixef, "poly", negate = T) ~ NA_character_, 
              T ~ gsub(".*degree=(\\d+).*", "\\1", fixef)))
  )

# Write to file
saveRDS(df_combos, "output/processed_data/metafor_fits.rds")

# Issue-level analysis ----

# Estimate prediction interval across issues (for linear 300B-frontier model only)
model_x <- df_combos %>% filter(specification == "linear", frontier_params_in_b == 300) %>% pull(model_fit) %>% .[[1]]

# Get mus and taus
mu_int    <- model_x$b[1]
tau_int   <- model_x$tau2[1] %>% sqrt
mu_x      <- model_x$b[2]
tau_x     <- model_x$tau2[2] %>% sqrt
rho_int_x <- model_x$rho

# Store mus and taus
mus  <- c(mu_int, mu_x)
taus <- c(tau_int, tau_x)

# Make covariance matrix
rho <- diag(1, length(mus), length(mus))
rho[lower.tri(rho) == T] <- rho_int_x
rho[upper.tri(rho)] <- t(rho)[upper.tri(rho)]
stopifnot(isSymmetric.matrix(rho))
sigma <- diag(taus) %*% rho %*% diag(taus) # Covariance matrix sigma

# How many issues?
n_issues <- 10000

# Sample true parameter values
true_params <- 
  MASS::mvrnorm(n = n_issues, mus, sigma) %>% 
  data.frame() %>% 
  set_names("intercept", "slope") %>% 
  mutate(issue = row_number())

# Define x values
x_values <- 
  seq(from = min(model_x$data$log_param_count_c), 
      to   = max(model_x$data$log_param_count_c), 
      length.out = 100)

# Compute predicted values
mean_log_param_count <- mean(model_x$data$log_param_count)
df_pi_out <-
  crossing(true_params, x_values) %>% 
  mutate(pred = intercept + slope*x_values,
         param_count_in_b = exp(x_values + mean_log_param_count) / 1e9)

# Get average and 95% quantiles across issues
df_pi_out_summary <-
  df_pi_out %>% 
  group_by(param_count_in_b) %>% 
  tidybayes::mean_qi(pred) %>% 
  ungroup()

saveRDS(df_pi_out_summary, "output/processed_data/issue_prediction_interval.rds")

# Fit models and get predicted values for each issue
res_out_issues <-
  map(list_issues,
      function(.x) {
        
        # Wrangle parameter count variable
        df_estimates_issue <- df_estimates %>% filter(issue == .x)
        
        # Drop human estimates
        drop_humans <- df_estimates_issue$model != "human"
        
        df_estimates_issue_temp <- df_estimates_issue[drop_humans,]
        
        # Get issue-level covariance matrix
        vcov_issue <- out_regs[[.x]]$vcov
        vcov_issue_temp <- vcov_issue[drop_humans, drop_humans]
        
        df_estimates_issue_temp <-
          df_estimates_issue_temp %>% 
          # Impute param count for frontier models
          mutate(parameters = ifelse(is.na(parameters), 300, parameters)) %>% 
          # Get param count on full scale and log it
          mutate(param_count = parameters*1e9) %>% 
          mutate(log_param_count = log(param_count)) %>% 
          # Center it
          mutate(log_param_count_c = as.numeric(scale(log_param_count, center = T, scale = F)))
        
        stopifnot(identical(vcov_issue_temp %>% diag %>% sqrt %>% unname %>% round(8), 
                            df_estimates_issue_temp$std.error %>% round(8))) # Check SEs match
        stopifnot(isSymmetric.matrix(vcov_issue_temp)) # Check symmetric
        
        # Fit
        mod_out_issue <-
          rma.mv(
            yi = estimate,
            V = vcov_issue_temp,
            # Fixed effects:
            mods = ~1+poly(log_param_count_c, degree=1, raw=TRUE),
            # Random effects:
            random = list(~1|message_id,
                          ~1|model),
            # Other arguments
            control = list(rel.tol = 1e-8),
            data = df_estimates_issue_temp
          )
        
        mean_log_param_count <- mean(mod_out_issue$data$log_param_count)
        
        pred_values_issue <-
          predict(mod_out_issue, newmods = unname(poly(x_values, degree = 1, raw = TRUE))) %>% 
          as.data.frame() %>% 
          mutate(estimate = pred,
                 param_count_in_b = exp(x_values + mean_log_param_count) / 1e9)
        
        list("mod_out" = mod_out_issue,
             "pred" = pred_values_issue)
        
      })

df_res_issues <-
  tibble(issue = list_issues,
         model_fit = map(res_out_issues, ~.x$mod_out),
         pred_values = map(res_out_issues, ~.x$pred))

# Save
saveRDS(df_res_issues, "output/processed_data/metafor_fits_by_issue.rds")



