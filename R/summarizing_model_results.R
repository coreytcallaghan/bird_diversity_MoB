# This script is used to read in each brms model
# that was fit and saved out
# and then summarize the posterior draws for the
# main effects and the random effects
# and then combine these all together
# for the different scales
# and biodiversity values
# then make summaries of these

# packages
library(tidybayes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(brms)

# read in data
# let's do grid size of 0.1 for now
setwd("Intermediate_results/no_bootstrapping/grid_size_0.1/")
dat <- list.files(pattern = ".RDS") %>%
  map_dfr(readRDS) %>%
  mutate(BCR_CODE=as.factor(as.integer(BCR_CODE)))
setwd("../../..")

# Make some prelim plots of the 'raw' data
# do this for two BCRs just as examples
dat %>%
  dplyr::filter(BCR_CODE==23) %>%
  dplyr::filter(index %in% c("S_n", "S_PIE", "S")) %>%
  ggplot(., aes(x=ghm, y=value, group=BCR_CODE, color=scale))+
  geom_point(aes(color=scale))+
  geom_smooth(method="lm", color="black", lwd=3, se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale~index, scales="free_y")+
  scale_color_brewer(palette="Set1")+
  ggtitle("BCR 23")

dat %>%
  dplyr::filter(BCR_CODE==5) %>%
  dplyr::filter(index %in% c("S_n", "S_PIE", "S")) %>%
  ggplot(., aes(x=ghm, y=value, group=BCR_CODE, color=scale))+
  geom_point(aes(color=scale))+
  geom_smooth(method="lm", color="black", lwd=3, se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale~index, scales="free_y")+
  scale_color_brewer(palette="Set1")+
  ggtitle("BCR 5")

dat %>%
  dplyr::filter(BCR_CODE==12) %>%
  dplyr::filter(index %in% c("S_n", "S_PIE", "S")) %>%
  ggplot(., aes(x=ghm, y=value, group=BCR_CODE, color=scale))+
  geom_point(aes(color=scale))+
  geom_smooth(method="lm", color="black", lwd=3, se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale~index, scales="free_y")+
  scale_color_brewer(palette="Set1")+
  ggtitle("BCR 12")

# summarize_data_function
summarize_modelling <- function(grid_size, scale_name, index_name){
  
  mod <- readRDS(paste0("Intermediate_results/brms_mods/linear_mods/", 
                        grid_size, "_", index_name, "_", scale_name, ".rds"))
  
  fe_only <- tibble(ghm = seq(0, 1, length.out=100)) %>%
    tidybayes::add_fitted_draws(mod,
                                re_formula = NA,
                                scale = "response", 
                                n = 1000) %>%
    dplyr::mutate(index=index_name) %>%
    dplyr::mutate(scale=scale_name) %>%
    dplyr::mutate(level="fixed_effects")
  
  re_only <- crossing(ghm = seq(0, 1, length.out=100),
                            BCR_CODE = unique(dat$BCR_CODE)) %>%
    tidybayes::add_fitted_draws(mod,
                                scale = "response", 
                                n = 1000) %>%
    dplyr::mutate(index=index_name) %>%
    dplyr::mutate(scale=scale_name) %>%
    dplyr::mutate(level="random_effects")
  
  means <- fixef(mod)
  blups <- brms::ranef(mod)
  
  intercepts <- means[1,1] + as.numeric(blups$BCR_CODE[ , 1, 1])
  slopes <-   means[2,1] + + as.numeric(blups$BCR_CODE[ , 1, 2])
  
  # errors
  intercepts_se <- sqrt(means[1,2]^2 + as.numeric(blups$BCR_CODE[ , 2, 1])^2)
  slopes_se <-   sqrt(means[2,2]^2 + as.numeric(blups$BCR_CODE[ , 2, 2])^2)
  
  n_spp <- length(unique(dat$BCR_CODE))
  blups_df <- tibble(BCR_CODE = rep(attr(blups$BCR_CODE, "dimnames")[[1]], 2),
                     type = rep(c("intercept","slope"), each = n_spp),
                     estimate = c(intercepts, slopes), 
                     se =  c(intercepts_se, slopes_se))
  
  results <- bind_rows(fe_only, re_only) %>%
    left_join(., blups_df, by="BCR_CODE")
  
  return(results)
  
}

results_0.1_linear <- bind_rows(summarize_modelling("0.1", "alpha", "S"),
                 summarize_modelling("0.1", "alpha", "S_n"),
                 summarize_modelling("0.1", "alpha", "S_PIE"),
                 summarize_modelling("0.1", "gamma", "S"),
                 summarize_modelling("0.1", "gamma", "S_n"),
                 summarize_modelling("0.1", "gamma", "S_PIE"))

# make a plot of the fixed effects for alpha and gamma scale
fe_only <- results_0.1_linear %>%
  dplyr::filter(level=="fixed_effects") %>%
  dplyr::filter(.draw %in% c(1:1000)) %>%
  unite(line_id, .draw, index, scale, sep="_", remove=FALSE)

fe_only_mean <- results_0.1_linear %>% 
  dplyr::filter(level=="fixed_effects") %>%
  group_by(ghm, scale, index) %>%
  summarize(.value = mean(.value))

ggplot() +
  facet_wrap(~index, scales="free")+
  geom_line(data=fe_only,
            aes(x=ghm, y=.value, group=line_id, color=scale), alpha=0.1)+
  geom_line(data = fe_only_mean, 
            aes(x=ghm, y=.value, group=scale),
            color="gray90",
            linetype="dashed",
            lwd=1)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  xlab("Global Human Modification")+
  ylab("Value")+
  scale_color_brewer(palette = "Set1")+
  theme(axis.text=element_text(color="black"))
  
ggplot() +
  facet_wrap(scale~index, scales="free")+
  geom_line(data=fe_only,
            aes(x=ghm, y=.value, group=line_id, color=scale), alpha=0.1)+
  geom_line(data = fe_only_mean, 
            aes(x=ghm, y=.value, group=scale),
            color="gray90",
            linetype="dashed",
            lwd=1)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  xlab("Global Human Modification")+
  ylab("Value")+
  scale_color_brewer(palette = "Set1")+
  theme(axis.text=element_text(color="black"))

re_model_only <- results_0.1_linear %>%
  dplyr::filter(level=="random_effects")

re_model_summary <- re_model_only %>%
  group_by(BCR_CODE, ghm, scale, index) %>%
  summarize(.value = mean(.value))

ggplot()+
  geom_line(data=re_model_summary, 
            aes(x=ghm, y=.value, group = BCR_CODE), alpha = 0.8) +
  facet_wrap(index~scale, scales="free_y")+
  geom_line(data = fe_only_mean, aes(x=ghm, y=.value), color = "red", lwd = 2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  xlab("Global Human Modification")+
  ylab("Value")+
  scale_color_brewer(palette = "Set1")+
  theme(axis.text=element_text(color="black"))

# try making a map of the slope of the relationship to see
# if this shows anything
# will do this just for S_alpha
# as a proof of concept
bcrs <- st_read("Data/Spatial data/bcr_terrestrial_shape/BCR_Terrestrial_master.shp") %>%
  dplyr::filter(COUNTRY=="USA") %>%
  dplyr::filter(!PROVINCE_S %in% c("ALASKA", "HAWAIIAN ISLANDS")) %>%
  mutate(BCR_CODE=as.character(as.integer(BCR)))

bcr_S_alpha_slopes <- results_0.1_linear %>%
  ungroup() %>%
  dplyr::filter(level=="random_effects") %>%
  dplyr::filter(index=="S") %>%
  dplyr::filter(scale=="alpha") %>%
  dplyr::select(BCR_CODE, type, estimate) %>%
  dplyr::filter(type=="slope") %>%
  distinct()
  
bcr_slope_S_alpha_plot_dat <- bcrs %>%
  left_join(., bcr_S_alpha_slopes)

ggplot()+
  geom_sf(data=bcr_slope_S_alpha_plot_dat, aes(fill=estimate))+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  scale_fill_gradient2()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("S_alpha slope")

#################################################
#################################################
################### Repeat the above
# but for the nonlinear models
# summarize_data_function
summarize_modelling_nl <- function(grid_size, scale_name, index_name){
  
  mod <- readRDS(paste0("Intermediate_results/brms_mods/nonlinear_mods/", 
                        grid_size, "_", index_name, "_", scale_name, ".rds"))
  
  fe_only <- tibble(ghm = seq(0, 1, length.out=100)) %>%
    tidybayes::add_fitted_draws(mod,
                                re_formula = NA,
                                scale = "response", 
                                n = 1000) %>%
    dplyr::mutate(index=index_name) %>%
    dplyr::mutate(scale=scale_name) %>%
    dplyr::mutate(level="fixed_effects")
  
  re_only <- crossing(ghm = seq(0, 1, length.out=100),
                      BCR_CODE = unique(dat$BCR_CODE)) %>%
    tidybayes::add_fitted_draws(mod,
                                scale = "response", 
                                n = 1000) %>%
    dplyr::mutate(index=index_name) %>%
    dplyr::mutate(scale=scale_name) %>%
    dplyr::mutate(level="random_effects")
  
  results <- bind_rows(fe_only, re_only)
  
  return(results)
  
}

results_0.1_nonlinear <- bind_rows(summarize_modelling_nl("0.1", "alpha", "S"),
                                summarize_modelling_nl("0.1", "alpha", "S_n"),
                                summarize_modelling_nl("0.1", "alpha", "S_PIE"),
                                summarize_modelling_nl("0.1", "gamma", "S"),
                                summarize_modelling_nl("0.1", "gamma", "S_n"),
                                summarize_modelling_nl("0.1", "gamma", "S_PIE"))

fe_only <- results_0.1_nonlinear %>%
  dplyr::filter(level=="fixed_effects") %>%
  dplyr::filter(.draw %in% c(1:1000)) %>%
  unite(line_id, .draw, index, scale, sep="_", remove=FALSE)

fe_only_mean <- results_0.1_nonlinear %>% 
  dplyr::filter(level=="fixed_effects") %>%
  group_by(ghm, scale, index) %>%
  summarize(.value = mean(.value))

ggplot() +
  facet_wrap(~index, scales="free")+
  geom_line(data=fe_only,
            aes(x=ghm, y=.value, group=line_id, color=scale), alpha=0.1)+
  geom_line(data = fe_only_mean, 
            aes(x=ghm, y=.value, group=scale),
            color="gray90",
            linetype="dashed",
            lwd=1)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  xlab("Global Human Modification")+
  ylab("Value")+
  scale_color_brewer(palette = "Set1")+
  theme(axis.text=element_text(color="black"))

ggplot() +
  facet_wrap(scale~index, scales="free")+
  geom_line(data=fe_only,
            aes(x=ghm, y=.value, group=line_id, color=scale), alpha=0.1)+
  geom_line(data = fe_only_mean, 
            aes(x=ghm, y=.value, group=scale),
            color="gray90",
            linetype="dashed",
            lwd=1)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  xlab("Global Human Modification")+
  ylab("Value")+
  scale_color_brewer(palette = "Set1")+
  theme(axis.text=element_text(color="black"))
