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

fx_nmb <- function() {
  cost_of_pi <- 9324
  eff_pup <- 0.58
  cost_pup <- 161
  c(
    # True positive = Correctly predicted event savings - intervention cost
    "TP" = -cost_of_pi * eff_pup - cost_pup, 
    
    # False positive: Cost of (unnecessary) treatment from incorrectly 
    # predicted positive
    "FP" = -cost_pup, 
    
    # True negative: No cost of treatment or event from correctly 
    # predicted negative
    "TN" = 0, 
    
    # False negative: Full cost of event from incorrectly predicted negative
    "FN" = -cost_of_pi 
  )
}

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
fx_nmb_sampler <- function() {
  cost_of_pi <- rnorm(n = 1, mean = 9234, sd = 814)
  eff_pup <- exp(rnorm(n = 1, mean = log(0.58), sd = 0.43))
  cost_pup <- rnorm(n = 1, mean = 161, sd = 49)
  c(
    "TP" = -cost_of_pi * eff_pup - cost_pup,
    "FP" = -cost_pup,
    "TN" = 0,
    "FN" = -cost_of_pi
  )
}

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

