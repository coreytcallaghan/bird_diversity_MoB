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

# read in data
# let's do grid size of 0.1 for now
setwd("Intermediate_results/no_bootstrapping/grid_size_0.1/")
dat <- list.files(pattern = ".RDS") %>%
  map_dfr(readRDS) %>%
  mutate(BCR_CODE=as.factor(as.integer(BCR_CODE)))
setwd("../../..")

# Let's look at S, S_n, and S_Pie for starters
dat %>%
  dplyr::filter(index %in% c("S_n", "S_PIE", "S")) %>%
  ggplot(., aes(x=ghm, y=value, group=BCR_CODE, color=scale))+
  geom_smooth(method="lm", aes(color=scale), se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale~index, scales="free_y")

plot_dat_S <- dat %>%
  dplyr::filter(index %in% c("S_n", "S_PIE", "S")) %>%
  dplyr::filter(value<1000)

ggplot()+
  geom_smooth(data=plot_dat_S, 
              method="gam", 
              formula = y ~ s(x, k=4),
              aes(x=ghm, y=value, color=scale))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~index, scales="free_y")+
  scale_color_brewer(palette = "Set1")+
  ggtitle("Grid size=0.1")

S_alpha_dat <- dat %>%
  dplyr::filter(value<1000) %>%
  dplyr::filter(index=="S") %>%
  dplyr::filter(scale=="alpha")

mod <- mgcv::gam(value ~ s(ghm, k=4), data=S_alpha_dat)
summary(mod)
plot(mod, page=1)

# but add random effect for BCR
# could check distribution as Poisson too
mod <- mgcv::gam(value ~ s(ghm, k=4) + s(BCR_CODE, bs="re"), 
                 method="REML",
                 data=S_alpha_dat)
summary(mod)
plot(mod, page=1)

# see if we can get a brms model to fit
# with nonlinear response in ghm and a random
# effect for BCR code
# for starters, will do it for 5 BCRs
# to see if we can get it how we want
# then run it for all BCRs
options(mc.cores=15)
rstan_options(auto_write = TRUE)


# try a bayesian model
S_alpha_mod <- brm(bf(value ~ s(ghm, k=5, bs="cr") + (1|BCR_CODE)),
          data = S_alpha_dat, 
          family = gaussian(), 
          cores = 15, 
          seed = 40,
          iter = 4000, 
          warmup = 1000, 
          thin = 10,
          control = list(adapt_delta = 0.99))

plot(S_alpha_mod)

msms_S_alpha_mod <- conditional_smooths(S_alpha_mod)

plot(msms_S_alpha_mod)

pp_check(m2_S_alpha_mod)

S_BCRs <- ggplot()+
  geom_smooth(data=plot_dat_S %>%
                dplyr::filter(index=="S_n"), 
            method="gam", 
            formula = y ~ s(x, k=4),
            se=FALSE,
            aes(x=ghm, y=value, color=scale),
            size=0.6)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~BCR_CODE, scales="free_y")+
  scale_color_brewer(palette = "Set1")

S_BCRs

