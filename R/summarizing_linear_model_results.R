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

# read in data
# let's do grid size of 0.1 for now
setwd("Intermediate_results/no_bootstrapping/grid_size_0.1/")
dat <- list.files(pattern = ".RDS") %>%
  map_dfr(readRDS) %>%
  mutate(BCR_CODE=as.factor(as.integer(BCR_CODE)))
setwd("../../..")

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
  
  results <- bind_rows(fe_only, re_only)
  
  return(results)
  
}

results_0.1 <- bind_rows(summarize_modelling("0.1", "alpha", "S"),
                 summarize_modelling("0.1", "alpha", "S_n"),
                 summarize_modelling("0.1", "alpha", "S_PIE"),
                 summarize_modelling("0.1", "gamma", "S"),
                 summarize_modelling("0.1", "gamma", "S_n"),
                 summarize_modelling("0.1", "gamma", "S_PIE"))

# make a plot of the fixed effects for alpha and gamma scale
fe_only <- results_0.1 %>%
  dplyr::filter(level=="fixed_effects") %>%
  dplyr::filter(.draw %in% c(1:1000)) %>%
  unite(line_id, .draw, index, scale, sep="_", remove=FALSE)

fe_only_mean <- results_0.1 %>% 
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
  