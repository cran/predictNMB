## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(predictNMB)
library(parallel)
library(ggplot2)
library(flextable)
set.seed(42)

## -----------------------------------------------------------------------------
get_nmb_sampler_training <- get_nmb_sampler(
  wtp = 28033,
  qalys_lost = function() rnorm(n = 1, mean = 0.0036, sd = 0.0005),
  high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 20, sd = 3),
  high_risk_group_treatment_effect = function() rbeta(n = 1, shape1 = 40, shape2 = 60),
  use_expected_values = TRUE
)

get_nmb_sampler_evaluation <- get_nmb_sampler(
  wtp = 28033,
  qalys_lost = function() rnorm(n = 1, mean = 0.0036, sd = 0.0005),
  high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 20, sd = 3),
  high_risk_group_treatment_effect = function() rbeta(n = 1, shape1 = 40, shape2 = 60)
)

## ---- echo=FALSE--------------------------------------------------------------
sim_screen_obj <- readRDS("fixtures/predictNMB-sim_screen_obj.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  cl <- makeCluster(2)
#  sim_screen_obj <- screen_simulation_inputs(
#    n_sims = 500,
#    n_valid = 10000,
#    sim_auc = seq(0.7, 0.95, 0.05),
#    event_rate = c(0.1, 0.2),
#    fx_nmb_training = get_nmb_sampler_training,
#    fx_nmb_evaluation = get_nmb_sampler_evaluation,
#    cutpoint_methods = c("all", "none", "youden", "value_optimising"),
#    cl = cl
#  )
#  stopCluster(cl)

## -----------------------------------------------------------------------------
autoplot(sim_screen_obj, x_axis_var = "sim_auc")

## -----------------------------------------------------------------------------
autoplot(sim_screen_obj, x_axis_var = "event_rate", dodge_width = 0.002)

## -----------------------------------------------------------------------------
autoplot(sim_screen_obj, x_axis_var = "sim_auc", constants = list(event_rate = 0.1))
autoplot(sim_screen_obj, x_axis_var = "sim_auc", constants = list(event_rate = 0.2))

## ---- message=FALSE-----------------------------------------------------------
autoplot(sim_screen_obj, what = "nmb")
autoplot(sim_screen_obj, what = "inb", inb_ref_col = "all")
autoplot(sim_screen_obj, what = "cutpoints")

## ---- message=FALSE-----------------------------------------------------------
autoplot(sim_screen_obj)
autoplot(sim_screen_obj, plot_range = FALSE)
autoplot(sim_screen_obj, plot_conf_level = FALSE)
autoplot(sim_screen_obj, plot_conf_level = FALSE, plot_range = FALSE)
autoplot(sim_screen_obj, plot_conf_level = FALSE, plot_range = FALSE, plot_line = FALSE)

## ---- message=FALSE-----------------------------------------------------------
autoplot(sim_screen_obj)
autoplot(sim_screen_obj, dodge_width = 0.01)

## ---- message=FALSE-----------------------------------------------------------
autoplot(sim_screen_obj)
autoplot(
  sim_screen_obj,
  rename_vector = c("Treat All" = "all", 
                    "Treat None" = "none",
                    "Youden Index" = "youden", 
                    "Value Optimisation" = "value_optimising")
)

## ---- message=FALSE-----------------------------------------------------------
autoplot(sim_screen_obj)
autoplot(sim_screen_obj, methods_order = c("all", "none"))
autoplot(
  sim_screen_obj,
  # Assign new names to the two methods of interest
  rename_vector = c("Treat All" = "all", "Treat None" = "none"), 
  
  # Call the methods by their new names
  methods_order = c("Treat All", "Treat None") 
)

## ---- message=FALSE-----------------------------------------------------------
autoplot(sim_screen_obj)
autoplot(sim_screen_obj, plot_alpha = 0.2)
autoplot(sim_screen_obj, plot_alpha = 1)

## ---- include=FALSE-----------------------------------------------------------
do_nmb_sim_obj <- sim_screen_obj$simulations[[1]]

## ---- eval=FALSE--------------------------------------------------------------
#  do_nmb_sim_obj <- do_nmb_sim(
#    n_sims = 500,
#    n_valid = 10000,
#    sim_auc = 0.7,
#    event_rate = 0.1,
#    fx_nmb_training = get_nmb_sampler_training,
#    fx_nmb_evaluation = get_nmb_sampler_evaluation,
#    cutpoint_methods = c("all", "none", "youden", "value_optimising")
#  )

## -----------------------------------------------------------------------------
autoplot(do_nmb_sim_obj) + theme_sim()

## -----------------------------------------------------------------------------
autoplot(do_nmb_sim_obj, what = "nmb") + theme_sim()
autoplot(
  do_nmb_sim_obj,
  what = "inb",
  inb_ref_col = "all",
  rename_vector = c(
    "Value-Optimising" = "value_optimising",
    "Treat-None" = "none",
    "Youden Index" = "youden"
  )
) + theme_sim()
autoplot(
  do_nmb_sim_obj,
  what = "cutpoints",
  methods_order = c("all", "none", "youden", "value optimising")
) + theme_sim()

## -----------------------------------------------------------------------------
autoplot(
  do_nmb_sim_obj,
  fill_cols = c("red", "blue"),
  median_line_col = "yellow",
  median_line_alpha = 1,
  median_line_size = 0.9
) + theme_sim()

## ---- fig.height=3, fig.width=6-----------------------------------------------
autoplot(
  do_nmb_sim_obj,
  n_bins = 15,
  rename_vector = c(
    "Value- Optimising" = "value_optimising",
    "Treat- None" = "none",
    "Treat- All" = "all",
    "Youden Index" = "youden"
  ),
  label_wrap_width = 5,
  conf.level = 0.8
) + theme_sim()

## -----------------------------------------------------------------------------
ce_plot(do_nmb_sim_obj, ref_col = "none")

## -----------------------------------------------------------------------------
attr(do_nmb_sim_obj$meta_data$fx_nmb_evaluation, "wtp")

## -----------------------------------------------------------------------------
ce_plot(do_nmb_sim_obj, ref_col = "none", wtp = 100000)

## -----------------------------------------------------------------------------
ce_plot(do_nmb_sim_obj, ref_col = "none", show_wtp = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  summary(sim_screen_obj)

## ---- echo=FALSE--------------------------------------------------------------
summary(sim_screen_obj) %>% flextable()

## ---- eval=FALSE--------------------------------------------------------------
#  summary(do_nmb_sim_obj)

## ---- echo=FALSE--------------------------------------------------------------
summary(do_nmb_sim_obj) %>% flextable()

## ---- eval=FALSE--------------------------------------------------------------
#  summary(
#    do_nmb_sim_obj,
#    agg_functions = list(
#      "mean" = function(x) round(mean(x), digits=2),
#      "min" = min,
#      "max" = max
#    )
#  )

## ---- echo=FALSE--------------------------------------------------------------
summary(
  do_nmb_sim_obj,
  agg_functions = list(
    "mean" = function(x) round(mean(x), digits=2),
    "min" = min, 
    "max" = max
  )
) %>%
  flextable()

## ---- eval=FALSE--------------------------------------------------------------
#  summary(
#    do_nmb_sim_obj,
#    what = "inb",
#    inb_ref_col = "all",
#    rename_vector = c(
#      "Value-Optimising" = "value_optimising",
#      "Treat-None" = "none",
#      "Youden Index" = "youden"
#    )
#  )

## ---- echo=FALSE--------------------------------------------------------------
summary(
  do_nmb_sim_obj,
  what = "inb",
  inb_ref_col = "all",
  rename_vector = c(
    "Value-Optimising" = "value_optimising",
    "Treat-None" = "none",
    "Youden Index" = "youden"
  )
) %>%
  flextable()

## ---- eval=FALSE--------------------------------------------------------------
#  summary(sim_screen_obj)

## ---- echo=FALSE--------------------------------------------------------------
summary(sim_screen_obj) %>% flextable()

## ---- eval=FALSE--------------------------------------------------------------
#  summary(sim_screen_obj, show_full_inputs = TRUE)

## ---- echo=FALSE--------------------------------------------------------------
summary(sim_screen_obj, show_full_inputs = TRUE) %>%
  flextable() %>%
  merge_v(j = 1:9) %>% 
  theme_box()

