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

## -----------------------------------------------------------------------------
fx_nmb <- get_nmb_sampler(
  outcome_cost = 9324,
  wtp = 28033,
  qalys_lost = 0.23,
  high_risk_group_treatment_effect = 0.58,
  high_risk_group_treatment_cost = 161
)


fx_nmb()

## ---- echo=FALSE--------------------------------------------------------------
nmb_simulation <- readRDS("fixtures/detailed_example-nmb_simulation.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  nmb_simulation <- do_nmb_sim(
#    # Evaluating a theoretical cohort of 1,000 patients
#    sample_size = 1000,
#  
#    # The larger the number of simulations, the longer it takes to run, but the
#    # more reliable the results
#    n_sims = 500,
#  
#    # Number of times the NMB is evaluated under each cutpoint
#    n_valid = 10000,
#  
#    # The AUC of our proposed model
#    sim_auc = 0.82,
#  
#    # The incidence of pressure ulcers at our hypothetical hospital
#    event_rate = 0.1,
#  
#    # As a first pass, we will just use our confusion matrix vector above for
#    # training and evaluation
#    fx_nmb_training = fx_nmb,
#    fx_nmb_evaluation = fx_nmb
#  )

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  saveRDS(nmb_simulation, "fixtures/detailed_example-nmb_simulation.rds")

## -----------------------------------------------------------------------------
nmb_simulation

# Get the median incremental NMB for each threshold selection method
summary(nmb_simulation) 

# Demonstrates the range of selected cutpoints under each method
autoplot(nmb_simulation, what = "cutpoints") + theme_sim()

## -----------------------------------------------------------------------------
# Compares the incremental benefit of each alternate strategy to our 
# current practice (treat all)
autoplot(nmb_simulation, what = "inb", inb_ref_col = "all") + theme_sim()

## -----------------------------------------------------------------------------

fx_nmb_sampler <- get_nmb_sampler(
  outcome_cost = function() rnorm(n = 1, mean = 9324, sd = 814),
  wtp = 28033,
  qalys_lost = function() (rbeta(n = 1, shape1 = 25.41, shape2 = 4.52) - rbeta(n = 1, shape1 = 67.34, shape2 = 45.14)),
  high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
  high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49)
)


fx_nmb_sampler()
fx_nmb_sampler()
fx_nmb_sampler()

## ---- echo=FALSE--------------------------------------------------------------
sim_pup_screen <- readRDS("fixtures/detailed_example-sim_pup_screen.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  cl <- makeCluster(2)
#  sim_pup_screen <- screen_simulation_inputs(
#    n_sims = 500,
#    n_valid = 10000,
#    sim_auc = seq(0.72, 0.92, 0.05),
#    event_rate = c(0.05, 0.1, 0.15),
#    cutpoint_methods = c("all", "none", "value_optimising", "youden"),
#    fx_nmb_training = fx_nmb,
#    fx_nmb_evaluation = fx_nmb_sampler,
#    cl = cl
#  )
#  stopCluster(cl)

## -----------------------------------------------------------------------------
summary(sim_pup_screen)

## -----------------------------------------------------------------------------
autoplot(
  sim_pup_screen, 
  x_axis_var = "sim_auc",
  constants = c(event_rate = 0.05), 
  dodge_width = 0.01
)

autoplot(
  sim_pup_screen, 
  x_axis_var = "sim_auc",
  constants = c(event_rate = 0.10),
  dodge_width = 0.01
)

autoplot(
  sim_pup_screen, 
  x_axis_var = "sim_auc",
  constants = c(event_rate = 0.15),
  dodge_width = 0.01
)

autoplot(
  sim_pup_screen, 
  x_axis_var = "event_rate", 
  dodge_width = 0.0075
)

