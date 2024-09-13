
library(extrafont)
library(tidyverse)
library(metafor)
library(broom)
library(estimatr)
library(ggrepel)
library(cowplot)
library(patchwork)
library(tidybayes)
set.seed(42)
options(scipen = 999)

#font_import()
#loadfonts(device = "all")
#fonts()

run_get_cis <- FALSE # Set TRUE if you wanna run the get CIs for primary model (takes ~2h)
#setwd("/Users/kobihackenburg/Documents/Papers/scaling laws + persuasion/project github/scaling-laws-pp/main_study/data/refactor")

# Read in data
df_combos         <- readRDS("output/processed_data/metafor_fits.rds")
df_estimates      <- readRDS("output/processed_data/df_estimates.rds")
out_raw_mod_ATEs  <- readRDS("output/processed_data/raw_model_ATEs.rds")
out_contrasts     <- readRDS("output/processed_data/contrasts_vs_claude.rds")
df_for_model      <- readRDS("output/processed_data/prepared_data.rds")
df_res_issues     <- readRDS("output/processed_data/metafor_fits_by_issue.rds")
df_pi_out_summary <- readRDS("output/processed_data/issue_prediction_interval.rds")

# Compute predicted values over parameter count range
pred_out <-
  map(1:nrow(df_combos),
      function(.x) {
        
        id_model    <- df_combos[[.x,"model_fit"]][[1]]
        id_n_degree <- df_combos[[.x,"n_degree"]]
        id_spec     <- df_combos[[.x,"specification"]]
        id_params   <- df_combos[[.x,"frontier_params_in_b"]]
        
        if(id_spec %in% c("linear", "quadratic", "cubic")) {
          
          x_values <- 
            seq(from = min(id_model$data$log_param_count_c), 
                to   = max(id_model$data$log_param_count_c), 
                length.out = 100)
          
          mean_log_param_count <- mean(id_model$data$log_param_count)
          
          predict(id_model, newmods = unname(poly(x_values, degree = id_n_degree, raw = TRUE))) %>% 
            as.data.frame() %>% 
            mutate(estimate = pred,
                   param_count_in_b = exp(x_values + mean_log_param_count) / 1e9,
                   specification = id_spec,
                   frontier_params_in_b = id_params)
          
        } else if(id_spec == "task_comp2_alone") {
          
          x_values <- 
            seq(from = min(id_model$data$task_completion_mod_lvl), 
                to   = max(id_model$data$task_completion_mod_lvl), 
                length.out = 100)
          
          predict(id_model, newmods = unname(poly(x_values, degree = id_n_degree, raw = TRUE))) %>% 
            as.data.frame() %>% 
            mutate(estimate = pred,
                   task_completion_mod_lvl = x_values,
                   specification = id_spec,
                   frontier_params_in_b = id_params)
          
        } else { }
        
      }) %>% 
  bind_rows()

df_combos <-
  df_combos %>% 
  left_join(pred_out %>% nest(.by = c(specification, frontier_params_in_b)) %>% rename(pred_values = data),
            by = c("specification", "frontier_params_in_b"))


# Figure 1 (main figure) ----

# Assign color map:
model_family_colors <- c(
  "Pythia" = "#304f5f",  
  "Qwen1.5" = "#568F84",   
  "Yi" = "#dcad04",    
  "Llama2" = "#A55087",  
  "Falcon" = "#876A4F",
  "Frontier" = "#cc3224"  
)

model_family_shapes <- c(
  "Pythia" = 16,  
  "Qwen1.5" = 16,   
  "Yi" = 16,    
  "Llama2" = 16,  
  "Falcon" = 16,
  "Frontier" = 16  
)

# Fix model_family labels in raw model estimates
out_raw_mod_ATEs <- 
  out_raw_mod_ATEs %>%
  mutate(model_family = case_when(
    model_family == "pythia" ~ "Pythia",
    model_family == "Qwen1.5" ~ "Qwen1.5",
    model_family == "Yi" ~ "Yi",
    model_family == "Llama" ~ "Llama2",
    model_family == "falcon" ~ "Falcon",
    model_family == "gpt" ~ "Frontier",
    model_family == "claude" ~ "Frontier",
    TRUE ~ model_family
  ))

x_scale <- c("log", "linear")

out_plots <-
  map(1:nrow(df_combos),
      function(.x) {
        
        pred_values <- df_combos[[.x,"pred_values"]][[1]]
        id_model    <- df_combos[[.x,"model_fit"]][[1]]
        id_spec     <- df_combos[[.x,"specification"]]
        
        if(id_spec %in% c("linear", "quadratic", "cubic")) {
          
          # Add paramater count variable to raw estimates
          out_raw_mod_ATEs_temp <-
            out_raw_mod_ATEs %>% 
            mutate(param_count_in_b = case_when(
              model != "human" & is.na(parameters) ~ max(id_model$data$param_count / 1e9),
              T ~ parameters))
          
          # raw_model_estimates_temp <-
          #   raw_model_estimates %>% 
          #   left_join(df_estimates %>% select(model, parameters) %>% distinct(.keep_all = T), by = "model") %>% 
          #   mutate(param_count_in_b = case_when(type == "LLM" & is.na(parameters) ~ max(id_model$data$param_count / 1e9),
          #                                       T ~ parameters))
          
          # Make plots
          out_plots <-
            map(x_scale,
                function(.y) {
                  
                  if(.y == "linear") xmin_value <- -Inf else xmin_value <- 1/1e9
                  
                  g <- 
                    pred_values %>% 
                    ggplot(aes(x = param_count_in_b, y = estimate)) +
                    theme_bw() +
                    theme(text = element_text(family = "Times New Roman")) +
                    geom_line(alpha = 1, color = "#232323", show.legend = FALSE, size = .75) +
                    geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), fill = "#232323", alpha = 0.06) +
                    geom_point(aes(x = param_count_in_b, y = estimate, color = model_family), 
                               data = out_raw_mod_ATEs_temp %>% filter(model != "human"), 
                               alpha = 1, size = 4) +
                    # geom_point(aes(x = param_count_in_b, y = estimate, color = model_family), 
                    #            data = raw_model_estimates_temp %>% filter(model != "human"), 
                    #            alpha = 1, size = 2) +
                    ggrepel::geom_text_repel(aes(label = model, color = model_family),
                                             data = out_raw_mod_ATEs_temp %>% filter(model != "human"),
                                             box.padding = 0.5, size = 5, 
                                             family = "Times New Roman", 
                                             min.segment.length = .5, direction = "both") +
                    geom_errorbar(aes(ymin = lwr, ymax = upr, color = model_family), 
                                  data = out_raw_mod_ATEs_temp %>% filter(model != "human"),
                                  alpha = 0.55, width = 0, size = .5) +
                    scale_color_manual(values = model_family_colors) +
                    # scale_shape_manual(values = model_family_shapes) + # Example shapes
                    theme(panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          plot.title = element_text(hjust = 0.5, vjust = -1.5, size = 24),
                          legend.position = "none",
                          legend.box.background = element_rect(),
                          axis.line = element_line(color = "black", size = 0.25),  
                          panel.border = element_blank(),
                          text = element_text(
                            #family = "Times New Roman", 
                            size = 22),
                          axis.text = element_text(size = 22)) +
                    geom_hline(yintercept = 0, linetype = "dashed", color = "#404040", alpha = 0.85) +
                    labs(x = paste0("\nModel Parameters (Billions)"),
                         y = "Estimated Persuasive Impact in Percentage Points (95% CI)\n",
                         title = paste0(str_to_sentence(.y), " Scale"),
                         color = NULL) +
                    geom_hline(yintercept = out_raw_mod_ATEs_temp %>% filter(model == "human") %>% pull(estimate),
                               color = "#404040", alpha = 1, linetype = "dashed") +
                    annotate(geom = "rect", xmin = xmin_value, xmax = 1e14, alpha = 0.06, fill = "#404040",
                             ymin = out_raw_mod_ATEs_temp %>% filter(model == "human") %>% pull(lwr),
                             ymax = out_raw_mod_ATEs_temp %>% filter(model == "human") %>% pull(upr)) +
                    coord_cartesian(xlim = c(min(pred_values$param_count_in_b), max(pred_values$param_count_in_b)))
                  
                  
                  if(.y == "log") { 
                    g <- 
                      g + 
                      scale_x_continuous(trans = "log10", breaks = c(0.1, 1, 10, 100, max(out_raw_mod_ATEs_temp$param_count_in_b, na.rm = T))) +
                      annotate(geom = "text", 
                               x = min(pred_values$param_count_in_b), 
                               y = out_raw_mod_ATEs_temp %>% filter(model == "human") %>% pull(estimate) + 0.5,
                               label = "Human", color = "#404040", 
                               family = "Times New Roman", 
                               size = 6, fontface = "bold")
                  }
                  
                  g
                  
                })
          
          #Join log linear panels
          g <-
            plot_grid(out_plots[[1]] + theme(legend.position = "none"),
                      out_plots[[2]] + labs(y = "\n") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.85, 0.25)),
                      labels = c("A", "B"), label_size = 18, label_fontfamily = "Times New Roman")
          
        } else { }
        
      })

# Store in df combos
df_combos$plot <- out_plots

# Save main plot
ggsave(plot = df_combos %>% filter(specification == "linear", frontier_params_in_b == 300) %>% pull(plot) %>% .[[1]], 
       filename = "output/plots/log_linear_plot.pdf", height = 9, width = 18)

# Figure 2 (contrasts vs. claude) ----

# Assign color map:
model_bin_colors <- c(
  "Frontier" = "#cc3224",  
  "Human" = "#A55087",   
  "Large (69-72B)" = "#dcad04",    
  "Medium (9-40B)" = "#568F84",  
  "Small (<=7B)" = "#304f5f"
)


g <- 
  out_contrasts %>% 
  filter(term != "intercept") %>% 
  rename(model = term) %>% 
  left_join(df_estimates %>% select(model, model_bins) %>% distinct(.keep_all = T),
            by = "model") %>% 
  ggplot(aes(x = estimate_scaled_vs_intercept, y = reorder(model, estimate_scaled_vs_intercept), color = model_bins)) +
  scale_color_manual(values = model_bin_colors) +
  theme_bw() +
  theme_bw(base_family = "Times New Roman") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate_scaled_vs_intercept - 1.96*std.error, xmax = estimate_scaled_vs_intercept + 1.96*std.error),
                 height = 0, alpha = .8) +
  geom_vline(xintercept = out_contrasts %>% filter(term == "intercept") %>% pull(estimate),
             linetype = "dashed", linewidth = .75, color = "#cc3224") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(x = "\nEstimated Persuasive Impact in Percentage Points (95% CI)",
       y = "",
       color = NULL, fontface = "bold") +
  theme(legend.position = c(0.1, 0.8),
        legend.box.background = element_rect(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = .25),
        axis.title.x = element_text(size = 16, face = "bold"),  
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 14)) +
  geom_text(aes(label = model, x = estimate_scaled_vs_intercept - 1.96*std.error, y = reorder(model, estimate_scaled_vs_intercept)),
            hjust = 0, vjust = -.4, 
            family = "Times New Roman", 
            show.legend = FALSE) +
  annotate(geom = "text",
           label = "Reference:\nClaude-3-Opus",
           x = out_contrasts %>% filter(term == "intercept") %>% pull(estimate) + 0.5,
           y = 4, hjust = 0, fontface = "bold", color = "#cc3224", alpha = 0.8, 
           family = "Times New Roman", 
           size = 4)

# Save the updated plot
ggsave(plot = g, filename = "output/plots/contrasts_vs_claude.pdf", height = 7, width = 10)


# Figure 3 (message/model features) ----

#Re-name columns in df_estimates
 
# df_estimates <- df_estimates %>%
#   rename(
#     "Moral Language" = moral_nonmoral_ratio_mod_lvl,
#     "Emotional Language" = emotion_proportion_mod_lvl,
#     "Readability" = flesch_mod_lvl,
#     "Pre-training Tokens" = pretraining_tokens_mod_lvl,
#     "Task Completion" = task_completion_mod_lvl,
#     "Message Length" = treatment_message_word_count_mod_lvl,
#     "Type-Token Ratio" = type_token_ratio_mod_lvl
#   )


# > Forest plot of mediator estimates ----
n_vars <- 7
critval <- qnorm(.05/n_vars, lower.tail = F)

# label_mapping <- c(
#   "Moral Language" = "moral_nonmoral_ratio_mod_lvl",
#   "Emotional Language" = "emotion_proportion_mod_lvl",
#   "Readability" = "flesch_mod_lvl",
#   "Pre-training Tokens" = "pretraining_tokens_mod_lvl",
#   "Task Completion" = "task_completion_mod_lvl",
#   "Message Length" = "treatment_message_word_count_mod_lvl",
#   "Type-Token Ratio" = "type_token_ratio_mod_lvl"
# )

g1 <-
  df_combos %>% 
  filter(specification == "mediators_joint") %>% 
  pull(model_fit) %>% 
  .[[1]] %>% 
  tidy() %>% 
  filter(term != "intercept") %>% 
  mutate(p_sig = ifelse(statistic < critval, "p<.0X", "p>.0X")) %>% 
  ggplot(aes(x = estimate, y = reorder(term, estimate), color = p_sig)) +
  theme_bw(base_family = "Times New Roman") + # Set the base font family for the plot
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - critval*std.error, xmax = estimate + critval*std.error), 
                 height = 0, size = .75, alpha = .75) +
  geom_vline(xintercept = 0, alpha = .75, linetype = "dashed") +
  labs(x = "Estimate (95% CI, Bonferroni-corrected)\n",
       y = NULL,
       title = "Predicting message persuasiveness\nfrom message and model features") +
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        legend.position = "none",
        legend.box.background = element_rect(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        text = element_text(family = "Times New Roman")) + # Apply Times New Roman to all text
  scale_color_manual(values = c("#304f5f", "#cc3224")) +
  geom_text(aes(label = term), nudge_y = 0.4, nudge_x = 0.0, family = "Times New Roman") # Ensure labels also use Times New Roman


# > Association between task completion and model size ----

df_completion_vs_size <-
  df_estimates %>% 
  distinct(model, .keep_all = T) %>% 
  select(model, parameters, task_completion_mod_lvl) %>% 
  mutate(parameters = case_when(model %in% c("GPT-4-Turbo", "Claude-3-Opus") ~ 300,
                                T ~ parameters)) %>% 
  filter(model != "human")

g2 <- df_completion_vs_size %>%
  ggplot(aes(x = task_completion_mod_lvl, y = parameters)) +
  theme_bw() +
  geom_point(size = 3, alpha = 1, color = "#304f5f") +
  scale_y_continuous(trans = "log10") +
  ggrepel::geom_text_repel(aes(label = model), family = "Times New Roman", color = "#304f5f") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
              se = TRUE, color = "#cc3224", alpha = 0.1, fill = "#cc3224") +
  labs(x = "Mean Task Completion Score",
       y = "\nParameters (Billions), Log Scale",
       title = "Task completion score is non-linearly\nassociated with language model size") +
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(size = 0.25)) 
  
# > Association between task completion and persuasiveness ----

g3 <- df_completion_vs_size %>%
  left_join(out_raw_mod_ATEs %>% select(model, estimate, lwr, upr), 
            by = "model") %>% 
  ggplot(aes(x = task_completion_mod_lvl, y = estimate)) +
  theme_bw() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0, alpha = 0.3) +
  geom_point(size = 3, alpha = 1, color = "#304f5f") +
  ggrepel::geom_text_repel(aes(label = model),
                           family = "Times New Roman",
                           color = "#304f5f") + 
  geom_line(data = df_combos %>%
              filter(specification == "task_comp2_alone") %>% 
              pull(pred_values) %>% 
              .[[1]], color = "#cc3224", size = 1) +
  geom_ribbon(data = df_combos %>%
                filter(specification == "task_comp2_alone") %>% 
                pull(pred_values) %>% 
                .[[1]],
              aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1, fill = "#cc3224") +
  labs(x = "Mean Task Completion Score",
       y = "\nEstimated persuasive impact (95% CI)",
       title = "Task completion score is non-linearly\nassociated with language model persuasiveness") +
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.25)) + # Thinner axis lines
  geom_hline(yintercept = 0, linetype = "dashed")

# > Parameter count adjusted coefficient ----

g4 <- bind_rows(
  df_combos %>% 
    filter(specification == "linear", frontier_params_in_b == 300) %>% 
    pull(model_fit) %>% 
    .[[1]] %>% 
    tidy() %>% 
    filter(str_detect(term, "param_count")) %>% 
    mutate(type = "Unadjusted"),
  df_combos %>% 
    filter(specification == "primary_adjust2") %>% 
    pull(model_fit) %>% 
    .[[1]] %>% 
    tidy() %>% 
    filter(str_detect(term, "param_count")) %>% 
    mutate(type = "Adjusted")
) %>% 
  mutate(term = "") %>% 
  ggplot(aes(x = term, y = estimate, color = fct_rev(type))) +
  theme_bw() +
  geom_point(position = position_dodge(.5), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0, position = position_dodge(.5), size = 1, alpha = .75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = type), position = position_dodge(1.1), fontface = "bold", family = "Times New Roman") +
  labs(x = "Coefficient on log(parameter count)\n",
       y = "\nEstimate (95% CI)",
       title = "Adjusting for task completion score renders\nmodel size a non-significant predictor of persuasion") +
  scale_color_manual(values = c("#cc3224", "#304f5f")) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(), # Remove all axis lines
        axis.line.y = element_line(size = 0.25),
        axis.text.x = element_blank(), # Remove x-axis text (ticks)
        axis.ticks.x = element_blank(),
        legend.position = "none") # Remove x-axis ticks

# Join plots
g_joint <- g1 + g3 + g2 + g4 + plot_annotation(tag_levels = "A")

ggsave(plot = g_joint, filename = "output/plots/feature_mediators.pdf", height = 10, width = 12)


#print mean of df
mean_parameters <- mean(df_completion_vs_size$parameters, na.rm = TRUE)

# Print the mean
print(mean_parameters)

# Issue-level plot ----

# Assign color map:
issue_colors <- c(
  "Veterans' Healthcare" = "#304f5f",  
  "Solitary Confinement" = "#568F84",   
  "Worker Pensions" = "#dcad04",    
  "Foreign Aid" = "#A55087",  
  "Medicaid" = "#304f5f",
  "Electoral College" = "#A55087",
  "Felons' Voting Rights" = "#568F84",  
  "Border Restrictions" = "#876A4F",   
  "Affirmative Action" = "#dcad04",    
  "Assisted Suicide" = "#304f5f"
)

#fix typos in df_res_issue
df_res_issues <- df_res_issues %>%
  mutate(issue = case_when(
    issue == "Veteran Healthcare" ~ "Veterans' Healthcare",
    issue == "Felon Voting Rights" ~ "Felons' Voting Rights",
    TRUE ~ issue
  ))

# Plot
out_plot_pi <-
  map(x_scale,
      function(.x) {
        
        last_points <- 
          df_res_issues %>%
          unnest(cols = pred_values) %>%
          group_by(issue) %>%
          summarize(max_param = max(param_count_in_b, na.rm = TRUE),
                    last_pred = last(pred[param_count_in_b == max_param])) %>%
          ungroup()
        
        g <-
          df_res_issues %>% 
          unnest(cols = pred_values) %>% 
          # mutate(issue = str_replace_all(issue, "_", " ")) %>% 
          ggplot(aes(x = param_count_in_b, y = pred, color = issue)) +
          scale_color_manual(values = issue_colors) +
          theme_bw() +
          theme_bw(base_family = "Times New Roman") +
          geom_line(linewidth = .5) +
          # Add average line
          geom_line(data = df_pi_out_summary,
                    aes(x = param_count_in_b, y = pred), 
                    inherit.aes = F, linewidth = 2, linetype = "twodash", alpha = 1, color = "#cc3224") +
          # Add prediction interval
          geom_ribbon(data = df_pi_out_summary,
                      aes(x = param_count_in_b, ymin = .lower, ymax = .upper), 
                      inherit.aes = F, fill = "#232323", alpha = 0.06) +
          theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                element_text(
                family = "Times New Roman", 
                size = 22),
                axis.title = element_text(
                  family = "Times New Roman",
                  size = 22),
                axis.line = element_line(color = "black", size = 0.25),  
                panel.border = element_blank(),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
                legend.position = "none", 
                plot.margin = unit(c(1, 6, 1, 1), "lines"),
                axis.text = element_text(size = 22)) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(x = "\nModel Parameters (Billions)",
               y = "Estimated Persuasive Impact in Percentage Points (95% CI)\n",
               title = paste0(str_to_sentence(.x), " scale"),
               color = "Issue")
        
        if (.x != "log") { # Only add labels when .x is not "log"
          g <- g + geom_text(data = last_points, aes(x = max_param, y = last_pred, label = issue), 
                             family = "Times New Roman", 
                             fontface = "bold", color = issue_colors[last_points$issue], hjust = -0.1, size = 5) + 
            coord_cartesian(clip = "off") + 
            theme(plot.margin = unit(c(1, 8.5, 1, 1), "lines"))
        }
        
        if(.x == "log") {
          g <- 
            g + 
            scale_x_continuous(trans = "log10", breaks = c(0.1, 1, 10, 100, max(df_pi_out_summary$param_count_in_b, na.rm = T)))
        }
        
        g
        
      })

# Join plots and save
g <-
  plot_grid(out_plot_pi[[1]] + 
              annotate(geom = "text", 
                       label = "", 
                       x = 1, y = -3, fontface = "bold", hjust = 0),
            out_plot_pi[[2]] + 
              labs(y = "\n") + 
              theme(axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    legend.position = "none"),
            labels = c("A","B"), label_size = 18, label_fontfamily = "Times New Roman")

ggsave(plot = g, filename = "output/plots/issue_plot.pdf", height = 8, width = 18)



# Get tables ----

# > Primary model results ----

# Tidy fixed effects for all models
df_combos <-
  df_combos %>% 
  mutate(fixef_tidied = map(df_combos$model_fit, ~.x %>% tidy))

# Get random effects for primary model 
# Note we currently only get these for the linear 300B-frontier model
model_x <- 
  df_combos %>% 
  filter(specification == "linear", frontier_params_in_b == 300) %>% 
  pull(model_fit) %>% .[[1]]

# Get confidence intervals
# Note: this took ~2 hours on a 2022 M1 Macbook Pro
# So I've written it out for now
if(run_get_cis) {
  
  model_x_cis <- confint(model_x)
  df_model_x_cis <- 
    model_x_cis %>% 
    as.data.frame %>% 
    data.frame %>% 
    rownames_to_column %>% 
    filter(str_detect(rowname, "2.", negate = T))
  
  df_rfx <-
    bind_rows(
      data.frame(
        coef = "intrcpt",
        param = "tau",
        group = c(model_x$s.names %>% unname %>% .[1],
                  model_x$s.names %>% unname %>% .[2]), 
        estimate = c(model_x$sigma2 %>% sqrt %>% .[1], 
                     model_x$sigma2 %>% sqrt %>% .[2])
      ),
      data.frame(
        coef = model_x$g.names %>% unname %>% .[1:2],
        param = "tau",
        group = model_x$g.names %>% unname %>% .[3],
        estimate = c(model_x$tau2 %>% sqrt %>% .[1],
                     model_x$tau2 %>% sqrt %>% .[2])
      ),
      data.frame(
        coef = "[intrcpt, log(parameter count)]",
        param = "rho",
        group = "issue",
        estimate = model_x$rho
      )
    ) %>% 
    mutate(coef = case_when(str_detect(coef, "poly") ~ "log(parameter count)", T ~ coef)) %>% 
    select(group, coef, param, estimate) %>% 
    add_column(.before = "group", effect = "random")
  
  # Add CIs
  df_rfx <- df_rfx %>% bind_cols(df_model_x_cis %>% select(contains("ci")))
  
  tidied_model_x <- tidy(model_x)
  
  # Combine random and fixed effects into table
  out_table <-
    bind_rows(
      data.frame(
        effect = "fixed",
        group = NA,
        coef = c("intrcpt", "log(parameter count)"),
        param = c("mu", "mu"),
        estimate = c(tidied_model_x[[1,"estimate"]],
                     tidied_model_x[[2,"estimate"]]),
        se = c(tidied_model_x[[1,"std.error"]],
               tidied_model_x[[2,"std.error"]]),
        p = c(tidied_model_x[[1,"p.value"]],
              tidied_model_x[[2,"p.value"]])
      ) %>% 
        mutate(ci.lb = estimate - 1.96*se,
               ci.ub = estimate + 1.96*se),
      df_rfx
    ) %>% 
    select(-se)
  
  # Tidy numbers and write to file
  
  out_table <-
    out_table %>% 
    mutate(p = case_when(p < .001 ~ "<.001", T ~ as.character(round(p, 3)))) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
    mutate(`95% CI` = paste0("[", ci.lb, ", ", ci.ub, "]")) %>% 
    select(-ci.lb, -ci.ub)
  
  write.csv(out_table, "output/tables/table_primary_model.csv", row.names = F)
  
} else { print("Note: re-running the primary model table is set to <<FALSE>>") }

# > Sensitivity analysis for # frontier model parameters ----

# Make table
robust_frontier_table <-
  pmap(df_combos %>% filter(specification == "linear") %>% select(frontier_params_in_b, model_fit),
       ~tidy(..2) %>% mutate("Frontier parameters (in B)" = ..1)) %>% 
  bind_rows() %>% 
  filter(term != "intercept") %>% 
  mutate(term = "log(parameter count)") %>% 
  select(`Frontier parameters (in B)`, term, estimate, std.error, p.value) %>%
  mutate(p.value = case_when(p.value < .001 ~ "<.001", T ~ as.character(round(p.value, 3)))) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2)))

write.csv(robust_frontier_table, "output/tables/table_robust_frontier.csv", row.names = F)

# > Other models ----

df_combos$specification %>% unique

list_models <- c("cubic", "quadratic", "family_fixed_fx", "drop_pythia70m",
                 "mediators_joint", "primary_adjust2", "task_comp2_alone", "primary_adjust3")

map(list_models,
    function(.x) {
      
      out_table <-
        df_combos %>% 
        filter(specification == .x) %>% 
        pull(fixef_tidied) %>% 
        .[[1]]
      
      if(.x == "mediators_joint") {
        out_table <-
          out_table %>% 
          mutate(`95% CI adjusted` = paste0("[", 
                                            format(round(estimate - critval*std.error, 2), nsmall = 2), ", ", 
                                            format(round(estimate + critval*std.error, 2), nsmall = 2), 
                                            "]"))
      }
      
      out_table <-
        out_table %>% 
        #add_column(specification = .x, .before = "term") %>% 
        mutate(p.value = case_when(p.value < .001 ~ "<.001", T ~ format(round(p.value, 3), nsmall = 2))) %>% 
        mutate(across(where(is.numeric), ~ format(round(.x, 2), nsmall = 2))) %>% 
        select(-type)
      
      write.csv(out_table, 
                paste0("output/tables/table_", .x, ".csv"), 
                row.names = F)
      
    })


