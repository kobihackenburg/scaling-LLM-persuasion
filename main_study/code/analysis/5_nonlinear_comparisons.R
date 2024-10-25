
library(tidyverse)
library(metafor)
library(broom)
library(estimatr)
library(ggrepel)
library(cowplot)
library(patchwork)
library(nlme)
set.seed(42)

# Data ----
df_for_model <- readRDS("output/processed_data/prepared_data.rds")

# Wrangle
df_for_model2 <-
  df_for_model %>% 
  filter(condition == "AI") %>% 
  mutate(parameters = ifelse(is.na(parameters), 300, parameters))


# Specifications ----
specifications <- 
  tribble( 
    
    ~specification,      ~model_function,                               ~start_values,
    "log-linear",        "a + b * log(parameters)",                     c(a_start = 1, b_start = 1),
    "power law",         "a * parameters^b",                            c(a_start = 50, b_start = 0.5),
    "saturating growth", "a * (1 - exp(-b * parameters))",              c(a_start = 50, b_start = 1),
    "logistic",          "a / (1 + exp(-b * (parameters - c)))",        c(a_start = 50, b_start = 1, c_start = 1),
    "log-logistic",      "a / (1 + exp(-b * (log(parameters) - c)))",   c(a_start = 50, b_start = 1, c_start = 1)
                                                                                            
  )


# Iterate and fit models ----
newdata <- 
  data.frame(parameters = seq(from = min(df_for_model2$parameters),
                              to = max(df_for_model2$parameters),
                              length.out = 5000))

list_models <- df_for_model2 %>% pull(model) %>% unique # For CV

list_out <- list()
for (i in 1:nrow(specifications)) {
        
        id_spec     <- specifications[[i,"specification"]]
        id_function <- specifications[[i,"model_function"]]
        id_start    <- specifications[[i,"start_values"]][[1]]
        
        model_formula <- as.formula(paste0("dv_response_mean ~ ", id_function))
        if(id_spec == "log-log") {
          model_formula <- as.formula(paste0("log(dv_response_mean + 1) ~ ", id_function))
        }
        
        # Fit model!
        if(str_detect(id_spec, "logistic")) {
          out_fit <-
            nls(
              model_formula,   
              data = df_for_model2,
              start = c(a = id_start[["a_start"]], 
                        b = id_start[["b_start"]],
                        c = id_start[["c_start"]]),
            )
        }
        
        if(str_detect(id_spec, "logistic", negate = T)) {
          out_fit <-
            nls(
              model_formula,   
              data = df_for_model2,
              start = c(a = id_start[["a_start"]], b = id_start[["b_start"]])
            )
        }
        
        # > Predicted values ----
        df_pred <-
          predict(out_fit, level = 0, newdata = newdata) %>% 
          as.data.frame() %>% 
          bind_cols(newdata) %>% 
          mutate(model = id_spec)
        
        names(df_pred)[1] <- "pred_value"  
        
        
        # > Cross-validation (leave-one-model-out) ----
        
        cv_error <-
          map_dbl(list_models,
                  function(holdout_model) {
                    
                    #holdout_model <- list_models[24] # COMMENT THIS OUT!
            
                    df_train <- df_for_model2 %>% filter(!(model %in% holdout_model))
                    df_test  <- df_for_model2 %>% filter(model %in% holdout_model)
                    
                    if(str_detect(id_spec, "logistic")) {
                      out_train <-
                        nls(
                          model_formula,   
                          data = df_train,
                          start = c(a = id_start[["a_start"]], 
                                    b = id_start[["b_start"]],
                                    c = id_start[["c_start"]]),
                        )
                    }
                    
                    if(str_detect(id_spec, "logistic", negate = T)) {
                      out_train <-
                        nls(
                          model_formula,   
                          data = df_train,
                          start = c(a = id_start[["a_start"]], b = id_start[["b_start"]])
                        )
                    }
                    
                    # Predict on heldout data
                    out_train_pred <- predict(out_train, level = 0, newdata = df_test)
                    
                    # Compute prediction error for model
                    pred_error <- abs(mean(df_test$dv_response_mean) - mean(out_train_pred))
                    
                    pred_error # Return
              
            }) %>% mean() # Compute mean prediction error across model folds
        
        # Return
        list_out[[i]] <- 
          list("model_fit" = out_fit,
               "pred_values" = df_pred,
               "cv_error" = cv_error)
        
        
        
        
    }

specifications$model_fit   <- map(list_out, ~.x$model_fit)
specifications$pred_values <- map(list_out, ~.x$pred_values)
specifications$cv_error    <- map_dbl(list_out, ~.x$cv_error)


# Write AIC/BIC/CV-error ----
out_aic_bic <-
  map(1:nrow(specifications),
      function(.x) {
        
        list("AIC" = AIC(specifications$model_fit[[.x]]),
             "BIC" = BIC(specifications$model_fit[[.x]]))
      
    })

# Write table to file
data.frame("Model" = specifications$specification,
           "AIC" = map_dbl(out_aic_bic, ~.x$AIC),
           "BIC" = map_dbl(out_aic_bic, ~.x$BIC),
           "CV_error" = specifications$cv_error) %>%
  arrange(AIC) %>%
  mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)) %>%
  write_csv("output/tables/nonlinear_comparisons.csv")

# Write coefficient estimates ----
map(1:nrow(specifications),
    function(.x) {

      out <- specifications$model_fit[[.x]] %>% tidy
      out %>% 
        mutate(Model = specifications$specification[[.x]]) %>%
        select(Model, everything()) %>%
        mutate(`p.value` = case_when(
          `p.value` <.001 ~ "<.001",
          T ~ format(round(`p.value`, 3), nsmall = 3)
        )) %>%
        mutate_if(is.numeric, ~format(round(., 2), nsmall = 2))

    }) %>%
  bind_rows() %>%
  write_csv("output/tables/nonlinear_results.csv")

# Plot ----

# Get raw mean outcome for each model
model_mean_y <-
  df_for_model2 %>% 
  reframe(pred_value = mean(dv_response_mean),
          param_in_b = first(parameters),
          .by = "model")

# Mean attitude in control group
control_mean <-
  df_for_model %>% 
  filter(condition == "control") %>% 
  pull(dv_response_mean) %>% 
  mean()

# Define custom colors, line types, and line sizes
custom_colors <- c("log-linear" = "#304f5f", 
                   "power law" = "#568F84", 
                   "saturating growth" = "#A55087", 
                   "logistic" = "#dcad04", 
                   "log-logistic" = "#cc3224")
custom_linetypes <- c("log-linear" = "solid",
                      "power law" = "solid",
                      "saturating growth" = "solid",
                      "logistic" = "solid",
                      "log-logistic" = "solid")
custom_sizes <- c("log-linear" = 1, 
                  "power law" = 1, 
                  "saturating growth" = 1, 
                  "logistic" = .75, 
                  "log-logistic" = 2)

# Define the order of models
model_order <- c("power law", "saturating growth", "logistic", "log-linear", "log-logistic")

# Update the plotting function
out_plots <-
  map(c("Linear scale", "Log scale"),
      function(.x) {
        
        g <-
          specifications %>% 
          unnest(cols = pred_values) %>% 
          rename(param_in_b = parameters) %>% 
          mutate(model = factor(model, levels = model_order)) %>% 
          ggplot(aes(x = param_in_b, y = pred_value, color = model, linetype = model)) +
          theme_bw(base_family = "Times New Roman") +
          theme(text = element_text(family = "Times New Roman")) +
          geom_hline(yintercept = control_mean, linetype = "dashed", color = "#404040", alpha = 0.85) +
          geom_point(data = model_mean_y,
                     aes(x = param_in_b, y = pred_value),
                     color = "#304f5f",
                     inherit.aes = F,
                     alpha = .5, size = 3, stroke = 0) +
          geom_text_repel(data = model_mean_y, 
                          aes(x = param_in_b, y = pred_value, label = model), 
                          inherit.aes = F,
                          color = "#304f5f",
                          alpha = .5,
                          family = "Times New Roman") +
          geom_line(aes(size = model), alpha = 1) +
          scale_color_manual(values = custom_colors) +
          scale_linetype_manual(values = custom_linetypes) +
          scale_size_manual(values = custom_sizes) +
          labs(x = "Model Parameters (Billions)", 
               y = "Policy Attitude (0-100 Scale)",
               color = "Model",
               linetype = "Model",
               size = "Model",
               title = .x) +
          coord_cartesian(ylim = c(45, 60)) +
          theme(axis.ticks.x = element_line(size = 0.25),
                axis.ticks.y = element_line(size = 0.25),
                axis.line = element_line(color = "black", size = 0.25),
                plot.title = element_text(hjust = 0.5, size = 16),
                panel.grid = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.title.x = element_text(size = 16, face = "bold"),
                axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
                legend.text = element_text(size = 12),
                axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14))
        
        if(.x == "Log scale") {
          g <- 
            g + 
            scale_x_log10() +
            annotate("text", label = "Control",
                     x = 100, 
                     y = control_mean + 0.25,
                     hjust = 1, fontface = "bold", family = "Times New Roman") +
            theme(legend.position = "none")  
        } else {
          g <- 
            g + 
            theme(legend.position = c(0.95, 0.05),  
                  legend.justification = c(1, 0),   
                  legend.box.background = element_rect(),
                  legend.title = element_blank(),   
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank()) + 
            labs(y = "")
        }
        
        g
        
      })

# Update the final plot
g <- plot_grid(out_plots[[2]], out_plots[[1]], labels = "AUTO")
ggsave(plot = g, filename = "output/plots/nonlinear_comparisons.pdf",
       height = 7, width = 14, device = cairo_pdf)



# Extrapolation exercise ----

extrapolate_specs <- c("log-logistic", "power law", "log-linear", "logistic", "saturating growth")
extrapolate_to_values <- c(3000, 30000) # In billions

df_combos <- expand_grid(extrapolate_specs, extrapolate_to_values)

plot_extrapolation <-
  map(1:nrow(df_combos),
    function(i) {
      
      extrap_spec  <- df_combos[[i,"extrapolate_specs"]]
      extrap_value <- df_combos[[i,"extrapolate_to_values"]]
      
      x <- specifications %>% filter(specification == extrap_spec) %>% pull(model_fit) %>% .[[1]]
      
      newdata2 <-
        data.frame(parameters = c(
          seq(from = min(df_for_model2$parameters),
              to = max(df_for_model2$parameters),
              length.out = 100),
          seq(from = max(df_for_model2$parameters) + 1,
              to = extrap_value,
              length.out = 500)))
      
      df_pred2 <-
        predict(x, level = 0, newdata = newdata2) %>% 
        as.data.frame() %>% 
        bind_cols(newdata2) %>% 
        set_names("pred_value", "param_in_b")
      
      df_pred2 <-
        df_pred2 %>% 
        mutate(control_mean = control_mean) %>% 
        mutate(implied_tx = pred_value - control_mean)
      
      max_tx <- df_pred2 %>% slice_max(param_in_b)
      obs_tx <- df_pred2 %>% filter(param_in_b == 300)
      
      obs_color <- "blue"
      max_color <- "red"
      hum_color <- "green4"
      
      human_mean <-
        df_for_model %>% 
        filter(condition == "human") %>% 
        pull(dv_response_mean) %>% 
        mean()
      
      g2 <-
        df_pred2 %>% 
        ggplot(aes(x = param_in_b/1000, y = pred_value)) +
        theme_bw() +
        geom_line() +
        geom_point(data = model_mean_y, 
                   aes(x = param_in_b/1000, y = pred_value), 
                   inherit.aes = F,
                   alpha = 0.3,
                   size = 3) +
        labs(x = "Parameters (in Trillions)",
             y = "Policy attitude (0-100 scale)",
             title = paste0("Extrapolation of ", extrap_spec, " function")) +
        geom_text_repel(data = model_mean_y %>% mutate(model  = ifelse(param_in_b < 300, NA, model)), 
                        aes(x = param_in_b/1000, y = pred_value, label = model), 
                        inherit.aes = F) +
        scale_y_continuous(breaks = 46:64) +
        theme(panel.grid.minor.y = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        
        # Annotate implied treatment effects
        
        # Full extrapolation:
        annotate("segment", 
                 x    = max_tx$param_in_b/1000, 
                 xend = max_tx$param_in_b/1000, 
                 y    = max_tx$control_mean + 0.1, 
                 yend = max_tx$pred_value - 0.1,
                 arrow = arrow(ends = "both", type = "closed", length = unit(0.02, "npc")),
                 color = max_color) +
        annotate("text",
                 x = max_tx$param_in_b/1000*0.99,
                 y = 55,
                 hjust = 1,
                 label = paste0("Parameters: ", max_tx$param_in_b/1000, "T (", max_tx$param_in_b, "B)\n",
                                "Implied treatment effect: ", max_tx$implied_tx %>% round(2), "pp"),
                 color = max_color) +
        
        # Empirically observed max
        annotate("segment", 
                 x    = 300/1000, 
                 xend = 300/1000, 
                 y    = obs_tx$control_mean + 0.1, 
                 yend = obs_tx$pred_value - 0.1,
                 arrow = arrow(ends = "both", type = "closed", length = unit(0.02, "npc")),
                 color = obs_color) +
        annotate("text",
                 x = obs_tx$param_in_b/1000*1.05,
                 y = 52,
                 hjust = 0,
                 label = paste0("Parameters: 0.3T (300B)\nImplied treatment effect: ", 
                                obs_tx$implied_tx %>% round(2), "pp"),
                 color = obs_color) +
        
        # Add human mean
        geom_hline(yintercept = human_mean, linewidth = 1, alpha = 0.5,
                   color = hum_color) +
        annotate("text",
                 x = (extrap_value/1000)/2,
                 y = human_mean + 0.25,
                 hjust = 0.5,
                 label = "Mean attitude in human-message group",
                 color = hum_color) +
        
        # Control group mean
        geom_hline(yintercept = control_mean, linewidth = 1, alpha = 0.5) +
        annotate("text", 
                 label = "Mean attitude in control group",
                 x = (extrap_value/1000)/2, 
                 y = control_mean - 0.25,
                 hjust = 0.5)
      
      ggsave(plot = g2, 
             filename = paste0("output/plots/extrapolation_", 
                               str_replace_all(extrap_spec, c("-" = "_", " " = "_")), "_", 
                               extrap_value/1000, "T.pdf"),
             height = 7, width = 9)
      
    })


