
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
    "Logarithmic",       "a + b * log(parameters)",                 c(a_start = 1, b_start = 1),
    "Power law",         "a * parameters^b",                        c(a_start = 50, b_start = 0.5),
    "Exponential decay", "a * (1 - exp(-b * parameters))",          c(a_start = 1, b_start = 0.5),
    "Michaelis-Menten",  "(a * parameters) / (1 + b * parameters)", c(a_start = 50, b_start = 50)
                                                                                            
  )
# see here for mm: https://analyticsartist.wordpress.com/2015/03/08/advertising-diminishing-returns-saturation/

# Iterate and fit models ----
newdata <- data.frame(parameters = seq(from = min(df_for_model2$parameters),
                                       to = max(df_for_model2$parameters),
                                       length.out = 100))

list_out <- list()
for (i in 1:nrow(specifications)) {
        
        id_spec     <- specifications[[i,"specification"]]
        id_function <- specifications[[i,"model_function"]]
        id_start    <- specifications[[i,"start_values"]][[1]]
        
        model_formula <- as.formula(paste0("dv_response_mean ~ ", id_function))
        
        # Fit model!
        out_fit <-
          nlme(
            model_formula,   
            data = df_for_model2,
            fixed = a + b ~ 1,               
            random = a + b ~ 1 | issue,       
            start = c(a = id_start[["a_start"]], b = id_start[["b_start"]]),     
            control = nlmeControl(maxIter = 100, tolerance = 1e-6)
        )
        
        df_pred <-
          predict(out_fit, level = 0, newdata = newdata) %>% 
          as.data.frame() %>% 
          bind_cols(newdata) %>% 
          mutate(model = id_spec)
        
        names(df_pred)[1] <- "pred_value"  
          
        list_out[[i]] <- 
          list("model_fit" = out_fit,
               "pred_values" = df_pred)
        
    }

specifications$model_fit   <- map(list_out, ~.x$model_fit)
specifications$pred_values <- map(list_out, ~.x$pred_values)

x <- 
  anova(specifications$model_fit[[1]],
        specifications$model_fit[[2]],
        specifications$model_fit[[3]],
        specifications$model_fit[[4]]) 

# Write table to file
data.frame("Model" = specifications$specification,
           "AIC" = x$AIC,
           "BIC" = x$BIC,
           "LL" = x$logLik) %>% 
  arrange(AIC) %>% 
  mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)) %>% 
  write_csv("output/tables/nonlinear_comparisons.csv")

map(1:nrow(specifications),
    function(.x) {
      
      out <- specifications$model_fit[[.x]] %>% summary
      out$tTable %>% 
        as.data.frame %>% 
        rownames_to_column() %>% 
        mutate(Model = specifications$specification[[.x]]) %>% 
        rename(Parameter = rowname) %>% 
        select(Model, everything()) %>% 
        mutate(`p-value` = case_when(
          `p-value` <.001 ~ "<.001",
          T ~ format(round(`p-value`, 3), nsmall = 3)
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
g <-
  specifications %>% 
  unnest(cols = pred_values) %>% 
  rename(param_in_b = parameters) %>% 
  ggplot(aes(x = param_in_b, y = pred_value, color = model)) +
  geom_line(size = 1.5, alpha = 0.8) +
  labs(x = "Model parameters (billions)", 
       y = "Policy attitude (0-100 scale)",
       color = "Model") +
  theme_bw() +
  coord_cartesian(ylim = c(45, 60)) +
  geom_hline(yintercept = control_mean, linewidth = 1, alpha = 0.5) +
  annotate("text", label = "Mean attitude in control group",
           x = 200, 
           y = control_mean + 0.25) +
  annotate("text", label = "Points show mean attitude in LM groups",
           x = 50, 
           y = 60) +
  geom_point(data = model_mean_y, 
             aes(x = param_in_b, y = pred_value), 
             inherit.aes = F,
             alpha = 0.5,
             size = 3) +
  geom_text_repel(data = model_mean_y, 
                  aes(x = param_in_b, y = pred_value, label = model), 
                  inherit.aes = F) +
  theme(legend.position = c(0.8, 0.5),
        legend.box.background = element_rect())

ggsave(plot = g, filename = "output/plots/nonlinear_comparisons.pdf",
       height = 8, width = 8)

