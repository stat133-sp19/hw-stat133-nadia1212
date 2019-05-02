## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(binomial)
library(ggplot2)
source("../R/functions.R")

## ------------------------------------------------------------------------
bin_choose(n = 5, k = 2)

## ------------------------------------------------------------------------
# probability of getting 2 successes in 5 trials
# (assuming prob of success = 0.5)
bin_probability(success = 2, trials = 5, prob = .5)

## ------------------------------------------------------------------------
# binomial probability distribution
bin_distribution(trials = 5, prob = 0.5)

#plot the distribution
plot(bin_distribution(trials = 5, prob = 0.5))

## ------------------------------------------------------------------------
# binomial probability distribution
bin_cumulative(trials = 5, prob = 0.5)

#plot the distribution
plot(bin_cumulative(trials = 5, prob = 0.5))

## ------------------------------------------------------------------------
bin1 <- bin_variable(trials = 10, p = 0.3)
bin1
bin_sum <- summary(bin1)
bin_sum

## ------------------------------------------------------------------------
#summary statistics binomial variable with 10 trials and .3 probability of success per trial
bin_mean(trials = 10, prob = .3)
bin_variance(trials = 10, prob = .3)
bin_mode(trials = 10, prob = .3)
bin_skewness(trials = 10, prob = .3)
bin_kurtosis(trials = 10, prob = .3)

