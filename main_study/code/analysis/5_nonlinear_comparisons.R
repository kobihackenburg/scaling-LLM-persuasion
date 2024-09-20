
library(tidyverse)
library(metafor)
library(broom)
library(estimatr)
library(ggrepel)
library(cowplot)
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
    
    ~specification,      ~model_function,                           ~start_values,
    "logarithmic",       "a + b * log(parameters)",                 c(a_start = 1, b_start = 1),
    "power law",         "a * parameters^b",                        c(a_start = 50, b_start = 0.5),
    "exponential decay", "a * (1 - exp(-b * parameters))",          c(a_start = 1, b_start = 0.5),
    "logistic",          "L / (1 + exp(-k * (parameters - X0)))",   c(L_start = 50, k_start = 1, X0_start = 1),
    "log-logistic",      "L / (1 + exp(-k * (log_params - X0)))",   c(L_start = 50, k_start = 1, X0_start = 1)
                                                                                            
  )


# Iterate and fit models ----
list_out <- list()
for (i in 1:nrow(specifications)) {
        
        id_spec     <- specifications[[i,"specification"]]
        id_function <- specifications[[i,"model_function"]]
        id_start    <- specifications[[i,"start_values"]][[1]]
        
        model_formula <- as.formula(paste0("dv_response_mean ~ ", id_function))

        # Adjust data and make newdata for predicting values
        if(id_spec == "log-logistic") {
          
          df_for_model2 <- df_for_model2 %>% mutate(log_params = log(parameters))
          
          newdata <- 
            data.frame(log_params = seq(from = min(df_for_model2$log_params),
                                        to = max(df_for_model2$log_params),
                                        length.out = 100))
        } else {
          
          newdata <- 
            data.frame(parameters = seq(from = min(df_for_model2$parameters),
                                        to = max(df_for_model2$parameters),
                                        length.out = 100))
          
        }
        
        # Fit model!
        if(str_detect(id_spec, "logistic", negate = T)) {
          
          # out_fit <-
          #   nlme(
          #     model_formula,   
          #     data = df_for_model2,
          #     fixed = a + b ~ 1,               
          #     random = a + b ~ 1 | issue ,   
          #     start = c(a = id_start[["a_start"]], b = id_start[["b_start"]]),     
          #     control = nlmeControl(maxIter = 100, tolerance = 1e-6)
          #   )
          
          out_fit <-
            nls(
              model_formula,   
              data = df_for_model2,
              start = c(a = id_start[["a_start"]], b = id_start[["b_start"]])
            )
          
        } else {
          
          # out_fit <-
          #   nlme(
          #     model_formula,   
          #     data = df_for_model2,
          #     fixed = L + k + X0 ~ 1,                  
          #     random = L + X0 ~ 1 | issue,   
          #     start = c(L = id_start[["L_start"]], 
          #               k = id_start[["k_start"]], 
          #               X0 = id_start[["X0_start"]]),     
          #     control = nlmeControl(maxIter = 100, tolerance = 1e-6)
          #   )
          
          out_fit <-
            nls(
              model_formula,   
              data = df_for_model2,
              start = c(L = id_start[["L_start"]], 
                        k = id_start[["k_start"]],
                        X0 = id_start[["X0_start"]]),
            )
          
        }
        
        df_pred <-
          predict(out_fit, level = 0, newdata = newdata) %>% 
          as.data.frame() %>% 
          bind_cols(newdata) %>% 
          mutate(model = id_spec)
        
        names(df_pred)[1] <- "pred_value"  
        
        if(id_spec == "log-logistic") { 
          df_pred <- df_pred %>% mutate(log_params = exp(log_params)) %>% rename(parameters = log_params) 
        }
          
        list_out[[i]] <- 
          list("model_fit" = out_fit,
               "pred_values" = df_pred)
        
    }

specifications$model_fit   <- map(list_out, ~.x$model_fit)
specifications$pred_values <- map(list_out, ~.x$pred_values)

# Get AIC/BIC ----
out_aic_bic <-
  map(1:nrow(specifications),
      function(.x) {
        
        list("AIC" = AIC(specifications$model_fit[[.x]]),
             "BIC" = BIC(specifications$model_fit[[.x]]))
      
    })

# Write table to file
data.frame("Model" = specifications$specification,
           "AIC" = map_dbl(out_aic_bic, ~.x$AIC),
           "BIC" = map_dbl(out_aic_bic, ~.x$BIC)) %>%
  arrange(AIC) %>%
  mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)) %>%
  write_csv("output/tables/nonlinear_comparisons.csv")


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

# Plot predicted values and overlay raw means
out_plots <-
  map(c("Linear scale", "Log scale"),
      function(.x) {
      
      g <-
        specifications %>% 
        unnest(cols = pred_values) %>% 
        rename(param_in_b = parameters) %>% 
        ggplot(aes(x = param_in_b, y = pred_value, color = model)) +
        geom_line(size = 1.5, alpha = 0.8) +
        labs(x = "Model parameters (billions)", 
             y = "Policy attitude (0-100 scale)",
             color = "Model",
             title = .x) +
        theme_bw() +
        coord_cartesian(ylim = c(45, 60)) +
        geom_hline(yintercept = control_mean, linewidth = 1, alpha = 0.5) +
        geom_point(data = model_mean_y, 
                   aes(x = param_in_b, y = pred_value), 
                   inherit.aes = F,
                   alpha = 0.5,
                   size = 3) +
        geom_text_repel(data = model_mean_y, 
                        aes(x = param_in_b, y = pred_value, label = model), 
                        inherit.aes = F) +
        theme(legend.position = c(0.85, 0.5),
              legend.box.background = element_rect(),
              plot.title = element_text(hjust = 0.5))
      
      if(.x == "Log scale") {
        g <- 
          g + 
          scale_x_log10() +
          annotate("text", label = "Mean attitude in control group",
                   x = 100, 
                   y = control_mean - 0.25,
                   hjust = 1, fontface = "bold") +
          annotate("text", label = "Points show mean attitude in LM groups",
                   x = log(50, 10), 
                   y = 60,
                   fontface = "bold")
      } else {
        g <- 
          g + 
          theme(legend.position = "none",
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank()) + 
          labs(y = "")
      }
      
      g
      
    })

g <- plot_grid(out_plots[[2]], out_plots[[1]], labels = "AUTO")

ggsave(plot = g, filename = "output/plots/nonlinear_comparisons.pdf",
       height = 7, width = 14)



# x <- 
#   anova(specifications$model_fit[[1]],
#         specifications$model_fit[[4]]) 

# Write table to file
# data.frame("Model" = specifications$specification,
#            "AIC" = x$AIC,
#            "BIC" = x$BIC,
#            "LL" = x$logLik) %>% 
#   arrange(AIC) %>% 
#   mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)) %>% 
#   write_csv("output/tables/nonlinear_comparisons.csv")

# map(1:nrow(specifications),
#     function(.x) {
#       
#       out <- specifications$model_fit[[.x]] %>% summary
#       out$tTable %>% 
#         as.data.frame %>% 
#         rownames_to_column() %>% 
#         mutate(Model = specifications$specification[[.x]]) %>% 
#         rename(Parameter = rowname) %>% 
#         select(Model, everything()) %>% 
#         mutate(`p-value` = case_when(
#           `p-value` <.001 ~ "<.001",
#           T ~ format(round(`p-value`, 3), nsmall = 3)
#         )) %>% 
#         mutate_if(is.numeric, ~format(round(., 2), nsmall = 2))
#       
#     }) %>% 
#   bind_rows() %>% 
#   write_csv("output/tables/nonlinear_results.csv")


