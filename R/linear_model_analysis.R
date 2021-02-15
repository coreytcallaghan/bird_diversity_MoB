# This script is used to read in the individual
# BCR analyses and assess the patterns shown in the data
# Still a bit preliminary for now

# packages
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)
library(brms)
library(rstan)
library(tidybayes)

options(mc.cores=15)
rstan_options(auto_write = TRUE)

# read in data
# let's do grid size of 0.1 for now
setwd("Intermediate_results/no_bootstrapping/grid_size_0.1/")
dat <- list.files(pattern = ".RDS") %>%
  map_dfr(readRDS) %>%
  mutate(BCR_CODE=as.factor(as.integer(BCR_CODE)))
setwd("../../..")

# Write a function
run_model_function <- function(scale_name, index_name){
  
  temp_dat <- dat %>%
    dplyr::filter(value<1000) %>%
    dplyr::filter(index==index_name) %>%
    dplyr::filter(scale==scale_name)
  
  linear_mod <- brm(value ~ ghm + (1+ghm|BCR_CODE),
                 data = temp_dat, 
                 family = gaussian(), 
                 cores = 12, 
                 chains=4,
                 seed = 40,
                 iter = 4000, 
                 warmup = 1000, 
                 thin = 10,
                 control = list(adapt_delta = 0.92, max_treedepth = 12),
                 file=paste0("Intermediate_results/brms_mods/linear_mods/0.1_", index_name, "_", scale_name))
  
}

run_model_function("alpha", "S")
run_model_function("alpha", "S_n")
run_model_function("alpha", "S_PIE")
run_model_function("gamma", "S")
run_model_function("gamma", "S_n")
run_model_function("gamma", "S_PIE")

# read in data
# let's do grid size of 0.1 for now
setwd("Intermediate_results/no_bootstrapping/grid_size_0.5/")
dat_0.5 <- list.files(pattern = ".RDS") %>%
  map_dfr(readRDS) %>%
  mutate(BCR_CODE=as.factor(as.integer(BCR_CODE)))
setwd("../../..")

# Write a function
run_model_function_0.5 <- function(scale_name, index_name){
  
  temp_dat <- dat_0.5 %>%
    dplyr::filter(value<1000) %>%
    dplyr::filter(index==index_name) %>%
    dplyr::filter(scale==scale_name)
  
  linear_mod <- brm(value ~ ghm + (1+ghm|BCR_CODE),
                    data = temp_dat, 
                    family = gaussian(), 
                    cores = 12, 
                    chains=4,
                    seed = 40,
                    iter = 4000, 
                    warmup = 1000, 
                    thin = 10,
                    control = list(adapt_delta = 0.92, max_treedepth = 12),
                    file=paste0("Intermediate_results/brms_mods/linear_mods/0.5_", index_name, "_", scale_name))
  
}

run_model_function_0.5("alpha", "S")
run_model_function_0.5("alpha", "S_n")
run_model_function_0.5("alpha", "S_PIE")
run_model_function_0.5("gamma", "S")
run_model_function_0.5("gamma", "S_n")
run_model_function_0.5("gamma", "S_PIE")

