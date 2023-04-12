## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
foo1 <- function() {
  c(
    "TP" = -3,
    "FP" = -1,
    "TN" = 0,
    "FN" = -4
  )
}
foo1()

## -----------------------------------------------------------------------------
foo2 <- function() {
  intervention_cost <- rgamma(n = 1, shape = 1)
  intervention_effectiveness <- rbeta(n = 1, shape1 = 10, shape2 = 10)
  fall_cost <- rgamma(n = 1, shape = 4)

  c(
    "TP" = -intervention_cost - fall_cost * (1 - intervention_effectiveness),
    "FP" = -intervention_cost,
    "TN" = 0,
    "FN" = -fall_cost
  )
}

foo2()
foo2()
foo2()

## -----------------------------------------------------------------------------
foo3 <- function() {
  # intervention for high risk (hr) group
  hr_intervention_cost <- rgamma(n = 1, shape = 1)
  hr_intervention_effectiveness <- rbeta(n = 1, shape1 = 10, shape2 = 10)

  # intervention for low risk (lr) group
  lr_intervention_cost <- rgamma(n = 1, shape = 0.5 * 10, rate = 1 * 10)
  lr_intervention_effectiveness <- rbeta(n = 1, shape1 = 10, shape2 = 30)


  fall_cost <- rgamma(n = 1, shape = 4)

  c(
    "TP" = -hr_intervention_cost - fall_cost * 
      (1 - hr_intervention_effectiveness),
    "FP" = -hr_intervention_cost,
    "TN" = -lr_intervention_cost,
    "FN" = -lr_intervention_cost - fall_cost * 
      (1 - lr_intervention_effectiveness)
  )
}

foo3()
foo3()
foo3()

## -----------------------------------------------------------------------------
library(predictNMB)

foo1_remake <-
  get_nmb_sampler(
    outcome_cost = 4,
    high_risk_group_treatment_cost = 1,
    high_risk_group_treatment_effect = 0.5
  )

foo1_remake()
foo1()

## ---- warning=FALSE-----------------------------------------------------------
library(tidyr)
library(ggplot2)
plot_nmb_dist <- function(f, n = 10000) {
  data <- do.call("rbind", lapply(1:n, function(x) f()))

  data_long <- pivot_longer(
    as.data.frame(data),
    cols = everything(),
    names_to = "classification",
    values_to = "NMB"
  )

  ggplot(data_long, aes(NMB)) +
    geom_histogram() +
    facet_wrap(~classification) +
    theme_bw() +
    labs(y = "", x = "Net Monetary Benefit ($)")
}

## ---- fig.show='hold'---------------------------------------------------------
foo2_remake <-
  get_nmb_sampler(
    # our blank function, followed by a sampler
    outcome_cost = function() rgamma(n = 1, shape = 4), 
    high_risk_group_treatment_cost = function() rgamma(n = 1, shape = 1),
    high_risk_group_treatment_effect = function() rbeta(n = 1, 
                                                        shape1 = 10, 
                                                        shape2 = 10)
  )

plot_nmb_dist(foo2_remake) + ggtitle("foo2_remake()")
plot_nmb_dist(foo2) + ggtitle("foo2()")

## ---- fig.show='hold'---------------------------------------------------------
foo3_remake <-
  get_nmb_sampler(
    outcome_cost = function() rgamma(n = 1, shape = 4),
    high_risk_group_treatment_cost = function() rgamma(n = 1, shape = 1),
    high_risk_group_treatment_effect = function() rbeta(n = 1, 
                                                        shape1 = 10, 
                                                        shape2 = 10),
    low_risk_group_treatment_cost = function() rgamma(n = 1, 
                                                      shape = 0.5 * 10, 
                                                      rate = 1 * 10),
    low_risk_group_treatment_effect = function() rbeta(n = 1, 
                                                       shape1 = 10, 
                                                       shape2 = 30)
  )

plot_nmb_dist(foo3_remake) + ggtitle("foo3_remake()")
plot_nmb_dist(foo3) + ggtitle("foo3()")

## -----------------------------------------------------------------------------
foo4 <-
  get_nmb_sampler(
    wtp = 8,
    qalys_lost = 0.5,
    high_risk_group_treatment_cost = 1,
    high_risk_group_treatment_effect = 0.5
  )

foo4()
foo1()

## ---- echo=FALSE--------------------------------------------------------------
simulation_res1 <- readRDS("fixtures/nmb-functions-simulation_res1.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  simulation_res1 <- do_nmb_sim(
#    sample_size = 200, n_sims = 500, n_valid = 10000, sim_auc = 0.7,
#    event_rate = 0.1, fx_nmb_training = foo1, fx_nmb_evaluation = foo1,
#    cutpoint_methods = c("all", "none", "youden", "value_optimising")
#  )

## -----------------------------------------------------------------------------
autoplot(simulation_res1) + theme_sim()

## -----------------------------------------------------------------------------
foo2_remake <-
  get_nmb_sampler(
    outcome_cost = function() rgamma(n = 1, shape = 4),
    high_risk_group_treatment_cost = function() rgamma(n = 1, shape = 1),
    high_risk_group_treatment_effect = function() rbeta(n = 1, 
                                                        shape1 = 10, 
                                                        shape2 = 10)
  )

foo2_remake_training <-
  get_nmb_sampler(
    outcome_cost = function() rgamma(n = 1, shape = 4),
    high_risk_group_treatment_cost = function() rgamma(n = 1, shape = 1),
    high_risk_group_treatment_effect = function() rbeta(n = 1,
                                                        shape1 = 10, 
                                                        shape2 = 10),
    use_expected_values = TRUE
  )

foo2_remake_training()
foo2_remake_training()
foo2_remake_training()

## ---- echo=FALSE--------------------------------------------------------------
simulation_res2 <- readRDS("fixtures/nmb-functions-simulation_res2.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  simulation_res2 <- do_nmb_sim(
#    sample_size = 200, n_sims = 500, n_valid = 10000, sim_auc = 0.7,
#    event_rate = 0.1, fx_nmb_training = foo2_remake_training,
#    fx_nmb_evaluation = foo2_remake,
#    cutpoint_methods = c("all", "none", "youden", "value_optimising")
#  )

## -----------------------------------------------------------------------------
autoplot(simulation_res2) + theme_sim()

