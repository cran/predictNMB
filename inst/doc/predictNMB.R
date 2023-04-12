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
set.seed(42)

## -----------------------------------------------------------------------------
get_nmb_sampler <- function() {
  c(
    "TP" = rnorm(n = 1, mean = -80, sd = 5),
    "TN" = 0,
    "FP" = -20,
    "FN" = rnorm(n = 1, mean = -100, sd = 10)
  )
}

get_nmb_sampler()
get_nmb_sampler()
get_nmb_sampler()

## -----------------------------------------------------------------------------
get_nmb_sampler_training <- function() {
  c(
    "TP" = -80,
    "TN" = 0,
    "FP" = -20,
    "FN" = -100
  )
}
get_nmb_sampler_training()

## ---- echo=FALSE--------------------------------------------------------------
nmb_simulation <- readRDS("fixtures/predictNMB-nmb_simulation.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  nmb_simulation <- do_nmb_sim(
#    sample_size = 1000,
#    n_sims = 500,
#    n_valid = 10000,
#    sim_auc = 0.7,
#    event_rate = 0.1,
#    fx_nmb_training = get_nmb_sampler_training,
#    fx_nmb_evaluation = get_nmb_sampler,
#    show_progress = TRUE
#  )

## -----------------------------------------------------------------------------
nmb_simulation

## -----------------------------------------------------------------------------
hist(
  nmb_simulation$df_result$all, 
  main = "Simulation results - treat all", 
  xlab = "Net monetary benefit (NMB)"
)

summary(nmb_simulation$df_result$all)

## -----------------------------------------------------------------------------
autoplot(nmb_simulation) + theme_sim()

## -----------------------------------------------------------------------------
get_inbuilt_cutpoint_methods()

autoplot(nmb_simulation, methods = c("all", "none", "youden")) + theme_sim()

## -----------------------------------------------------------------------------
autoplot(nmb_simulation, what = "cutpoints") + theme_sim()

## -----------------------------------------------------------------------------
autoplot(nmb_simulation, what = "inb", inb_ref_col = "all") + theme_sim()

## -----------------------------------------------------------------------------
head(nmb_simulation$df_result)

## -----------------------------------------------------------------------------
head(nmb_simulation$df_thresholds)

## ---- eval=FALSE--------------------------------------------------------------
#  cl <- makeCluster(2)

## ---- echo=FALSE--------------------------------------------------------------
sim_screen_obj <- readRDS("fixtures/predictNMB-sim_screen_obj.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  sim_screen_obj <- screen_simulation_inputs(
#    n_sims = 500,
#    n_valid = 10000,
#    sim_auc = seq(0.7, 0.95, 0.05),
#    event_rate = c(0.1, 0.2),
#    fx_nmb_training = get_nmb_sampler_training,
#    fx_nmb_evaluation = get_nmb_sampler,
#    cutpoint_methods = c("all", "none", "youden", "value_optimising"),
#    cl = cl
#  )
#  stopCluster(cl)

## -----------------------------------------------------------------------------
autoplot(sim_screen_obj, x_axis_var = "sim_auc", constants = c(event_rate = 0.2))
autoplot(sim_screen_obj, x_axis_var = "sim_auc", constants = c(event_rate = 0.1))

