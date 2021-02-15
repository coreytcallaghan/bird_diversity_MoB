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

mod <- mgcv::gam(value ~ s(ghm, k=5), data=S_alpha_dat)
summary(mod)
plot(mod, page=1)

# but add random effect for BCR
# could check distribution as Poisson too
mod <- mgcv::gam(value ~ s(ghm, k=4) + s(BCR_CODE, bs="re"), 
                 method="REML",
                 data=S_alpha_dat,
                 family=poisson())
summary(mod)
plot(mod, page=1)


mod <- mgcv::gam(value ~ s(ghm, k=4) + s(ghm, k=4, by=BCR_CODE), 
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

ex_dat <- S_alpha_dat %>% 
  dplyr::filter(complete.cases(value)) %>%
  group_by(BCR_CODE) %>%
  sample_n(225) %>%
  droplevels()
  
# try a bayesian model
testing <- brm(value ~ ghm + (1+ghm|BCR_CODE),
                   data = ex_dat, 
                   family = gaussian(), 
                   cores = 15, 
                   chains=2,
                   seed = 40,
                   iter = 2000, 
                   warmup = 500, 
                   thin = 10,
                   control = list(adapt_delta = 0.8, max_treedepth = 10))

plot(testing)

pp_check(testing)

fe_only <- crossing(ghm = seq(min(ex_dat$ghm, na.rm=TRUE),
                              max(ex_dat$ghm, na.rm=TRUE), 
                              length.out=100)) %>%
  tidybayes::add_fitted_draws(testing,
                              re_formula = NA,
                              scale = "response", 
                              n = 100) 

fe_only_mean <- fe_only %>% 
  group_by(ghm) %>%
  summarize(.value = mean(.value))

ggplot(fe_only,
       aes(x = ghm, y = .value,
           group = .draw)) +
  geom_line(alpha = 0.1) +
  geom_line(data = fe_only_mean, color = "red", lwd = 2, group = 1)+
  theme_bw()+
  xlab("Global Human Modification")+
  ylab("Value")+
  theme(axis.text=element_text(color="black"))

ggplot(fe_only,
       aes(x = ghm, y = .value)) +
  tidybayes::stat_interval(alpha = 0.5) +
  geom_line(data = fe_only_mean, 
            color = "red", lwd = 2)+
  theme_bw()+
  xlab("Global Human Modification")+
  ylab("Value")+
  theme(axis.text=element_text(color="black"))

re_model_only <- crossing(ghm = seq(min(ex_dat$ghm, na.rm=TRUE),
                                    max(ex_dat$ghm, na.rm=TRUE), 
                                    length.out=100),
                          BCR_CODE = unique(ex_dat$BCR_CODE)) %>%
  tidybayes::add_fitted_draws(testing,
                              scale = "response", 
                              n = 100)

re_model_summary <- re_model_only %>%
  group_by(BCR_CODE, ghm) %>%
  summarize(.value = mean(.value))

ggplot(re_model_summary,
       aes(x = ghm, y = .value)) +
  geom_line(aes( group = BCR_CODE), alpha = 0.8) +
  geom_line(data = fe_only_mean, color = "red", lwd = 2)

ggplot(re_model_only,
       aes(x = ghm, y = .value)) +
  facet_wrap(~BCR_CODE) +
  tidybayes::stat_interval() 

S_alpha_mod <- brm(value ~ s(ghm, k=5, bs="cr") + 
                     s(ghm, k=5, bs="fs", by=BCR_CODE),
          data = ex_dat, 
          family = gaussian(), 
          cores = 15, 
          chains=2,
          seed = 40,
          iter = 2000, 
          warmup = 500, 
          thin = 10,
          control = list(adapt_delta = 0.8, max_treedepth = 10),
          file="Intermediate_results/brms_mods/S_alpha_mod")

S_alpha_mod_test1 <- brm(value ~ s(ghm, k=5, bs="cr") + (1|BCR_CODE),
                   data = ex_dat, 
                   family = gaussian(), 
                   cores = 15, 
                   chains=2,
                   seed = 40,
                   iter = 2000, 
                   warmup = 500, 
                   thin = 10,
                   control = list(adapt_delta = 0.8, max_treedepth = 10))

S_alpha_mod_test2 <- brm(value ~ s(ghm, k=5, bs="cr"),
                         data = ex_dat, 
                         family = gaussian(), 
                         cores = 15, 
                         chains=2,
                         seed = 40,
                         iter = 2000, 
                         warmup = 500, 
                         thin = 10,
                         control = list(adapt_delta = 0.8, max_treedepth = 10))

saveRDS(S_alpha_mod, "Intermediate_results/brms_mods/S_alpha_mod.RDS")

plot(S_alpha_mod)

msms_S_alpha_mod <- conditional_smooths(S_alpha_mod)

plot(msms_S_alpha_mod)

pp_check(S_alpha_mod)

fe_only <- crossing(ghm = seq(min(S_alpha_dat$ghm, na.rm=TRUE),
                            max(S_alpha_dat$ghm, na.rm=TRUE), 
                            length.out=100),
                  BCR_CODE = unique(ex_dat$BCR_CODE)) %>%
  tidybayes::add_fitted_draws(S_alpha_mod,
                   re_formula = NA,
                   scale = "response", 
                   n = 100) %>%
  group_by(ghm, .draw) %>%
  summarize(.value=mean(.value))

fe_only_mean <- fe_only %>% 
  group_by(ghm) %>%
  summarize(.value = mean(.value))

ggplot(fe_only,
       aes(x = ghm, y = .value,
           group = .draw)) +
  geom_line(alpha = 0.1) +
  geom_line(data = fe_only_mean, color = "red", lwd = 2, group = 1)+
  theme_bw()+
  xlab("Global Human Modification")+
  ylab("Value")+
  theme(axis.text=element_text(color="black"))

ggplot(fe_only,
       aes(x = ghm, y = .value)) +
  tidybayes::stat_interval(alpha = 0.5) +
  geom_line(data = fe_only_mean, 
            color = "red", lwd = 2)+
  theme_bw()+
  xlab("Global Human Modification")+
  ylab("Value")+
  theme(axis.text=element_text(color="black"))

re_model_only <- crossing(ghm = seq(min(ex_dat$ghm, na.rm=TRUE),
                                    max(ex_dat$ghm, na.rm=TRUE), 
                                    length.out=100),
                          BCR_CODE = unique(ex_dat$BCR_CODE)) %>%
  tidybayes::add_fitted_draws(S_alpha_mod,
                              scale = "response", 
                              allow_new_levels=TRUE,
                              n = 100)

re_model_summary <- re_model_only %>%
  group_by(BCR_CODE, ghm) %>%
  summarize(.value = mean(.value))

ggplot(re_model_summary,
       aes(x = ghm, y = .value)) +
  geom_line(aes( group = BCR_CODE), alpha = 0.8) +
  geom_line(data = fe_only_mean, color = "red", lwd = 2)

ggplot(re_model_only,
       aes(x = ghm, y = .value)) +
  facet_wrap(~BCR_CODE) +
  tidybayes::stat_interval() 



fe_only_test1 <- crossing(ghm = seq(min(ex_dat$ghm, na.rm=TRUE),
                              max(ex_dat$ghm, na.rm=TRUE), 
                              length.out=100)) %>%
  tidybayes::add_fitted_draws(S_alpha_mod_test1,
                              re_formula = NA,
                              scale = "response", 
                              n = 100)

fe_only_mean_1 <- fe_only_test1 %>% 
  group_by(ghm) %>%
  summarize(.value = mean(.value))

fe_only_test2 <- crossing(ghm = seq(min(ex_dat$ghm, na.rm=TRUE),
                                    max(ex_dat$ghm, na.rm=TRUE), 
                                    length.out=100)) %>%
  tidybayes::add_fitted_draws(S_alpha_mod_test2,
                              re_formula = NA,
                              scale = "response", 
                              n = 100)

fe_only_mean_2 <- fe_only_test2 %>% 
  group_by(ghm) %>%
  summarize(.value = mean(.value))



ggplot() +
  geom_line(data=fe_only_test1,
            aes(x = ghm, y = .value,
                group = .draw), color="orange", alpha = 0.1) +
  geom_line(data=fe_only_test2,
            aes(x = ghm, y = .value,
                group = .draw), color="blue", alpha = 0.1) +
  geom_line(data = fe_only_mean_1, aes(x=ghm, y=.value), color = "red", lwd = 2, group = 1)+
  geom_line(data = fe_only_mean_2, aes(x=ghm, y=.value), color = "green", lwd = 2, group = 1)+
  theme_bw()+
  xlab("Global Human Modification")+
  ylab("Value")+
  theme(axis.text=element_text(color="black"))





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

