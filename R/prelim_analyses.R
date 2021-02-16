# some prelim analyses for fun!
# objective is to start out with a given BCR
# and if we can get it working for once BCR
# we can then either scale up
# to a global analysis
# and/or work through each BCR individually, 
# with the concept that BCRs represent a geographic species pool

library(ggplot2)
library(mgcv)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(sf)
library(concaveman)
library(mobr)

# read in eBird data and
# only read in breeding season eBird data for the time being
# join with modification scores
# for each eBird checklist
ebird_data <- readRDS("Data/ebird_data_raw_May.RDS") %>% 
  bind_rows(readRDS("Data/ebird_data_raw_Jun.RDS")) %>%
  bind_rows(readRDS("Data/ebird_data_raw_Jul.RDS")) %>%
  bind_rows(readRDS("Data/ebird_data_raw_Aug.RDS")) %>%
  left_join(., read_csv("Data/checklists_mod_scores/ebird_samples_mod_scores.csv") %>%
              dplyr::select(first, SAMPLIN) %>%
              rename(ghm=first) %>%
              rename(SAMPLING_EVENT_IDENTIFIER=SAMPLIN), by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., read_csv("Data/Clements-Checklist-v2019-August-2019.csv") %>%
  dplyr::filter(category=="species") %>%
  dplyr::select(category, `English name`, `scientific name`, order, family) %>%
  rename(COMMON_NAME=`English name`,
         SCIENTIFIC_NAME=`scientific name`)) %>%
  dplyr::filter(!family %in% c("Strigidae (Owls)", "Tytonidae (Barn-Owls)",
                               "Stercorariidae (Skuas and Jaegers)", "Alcidae (Auks, Murres, and Puffins)",
                               "Sulidae (Boobies and Gannets)", "Procellariidae (Shearwaters and Petrels)",
                               "Hydrobatidae (Northern Storm-Petrels)", "Oceanitidae (Southern Storm-Petrels)"))


# total number of checklists in the data
length(unique(ebird_data$SAMPLING_EVENT_IDENTIFIER))

# total number of species in the data
length(unique(ebird_data$COMMON_NAME))

# let's just select one smaller area
# where we don't have to think abbout a lot of
# large scale biases for now, so it is easy to test some models
# and get some data together
# because I'm US at heart, I'll choose the BCR I grew up in: BCR 13
# I'll also just trim down to the 'breeding season' which will ignore a lot of inherent noise 
# mixed in with the intra-annual changes present in bird populaton data
filtered_dat <- ebird_data %>%
  dplyr::filter(BCR_CODE==13)

# now how much data are we looking at?
# total number of checklists in the data
length(unique(filtered_dat$SAMPLING_EVENT_IDENTIFIER))

# total number of species in the data
length(unique(filtered_dat$COMMON_NAME))

samples_sf <- filtered_dat %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, LONGITUDE, LATITUDE) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=3857)

# make a quick convex hull of points to create grids over
# with a small buffer
con_hull <- concaveman(samples_sf, concavity=8) %>%
  st_buffer(0.4)

# quick check the data match up
ggplot()+
  geom_sf(data=samples_sf)+
  geom_sf(data=con_hull, fill="orange", alpha=0.4)+
  theme_bw()

# create hexagonal grid over con hull
# and then subset to only grids that intersect with the con_hull (i.e., points)
initial <- con_hull
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

# can mess with grid size a bit more down the road
grids <- con_hull %>%
  st_make_grid(cellsize=.5, square=FALSE)

# To sf
grid <- st_sf(index = 1:length(lengths(grids)), grids) # Add index

# We identify the grids that belongs to a entity by assessing the centroid
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge)) %>%
  mutate(grid_id=1:nrow(.))

ggplot()+
  geom_sf(data=grid_new, fill="transparent")+
  geom_sf(data=samples_sf)+
  theme_bw()

# assign each eBird sample to a grid
a <- samples_sf %>%
  st_within(grid_new) %>%
  as.data.frame() %>%
  left_join(., grid_new %>%
              mutate(col.id=1:nrow(.))) %>%
  right_join(., samples_sf %>%
              mutate(row.id=1:nrow(.))) %>%
  dplyr::select(grid_id, SAMPLING_EVENT_IDENTIFIER)

sum(is.na(a$grid_id))

dat_with_grids <- filtered_dat %>%
  left_join(., a, by="SAMPLING_EVENT_IDENTIFIER")


#####################################################
############## SOME PLAY STUFF FROM COREY ###########
#####################################################
# get richness values for each checklist/sample
# and total abundance as well
# can easily add other things like phylo diversity
# and/or shannon diversity and/or functional diversity etc.
# just need to pull a few extra things together
community_dat <- dat_with_grids %>%
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME,
           LATITUDE, LONGITUDE, grid_id) %>%
  summarize(OBSERVATION_COUNT=sum(OBSERVATION_COUNT),
            ghm=mean(ghm)) %>%
  ungroup()

metric_dat <- community_dat %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarize(SPECIES_RICHNESS=length(unique(COMMON_NAME)),
            ABUNDANCE=sum(OBSERVATION_COUNT),
            GHM=mean(ghm)) %>%
  dplyr::filter(complete.cases(GHM))

plot(density(metric_dat$SPECIES_RICHNESS))
plot(density(log10(metric_dat$ABUNDANCE)))

# raw plot of community level vs
# modification score
ggplot(metric_dat, aes(x=GHM, y=SPECIES_RICHNESS))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed Species Richness")+
  geom_smooth(method="gam")


ggplot(metric_dat, aes(x=GHM, y=ABUNDANCE))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed Abundance(log10)")+
  scale_y_log10()+
  geom_smooth(method="gam")


# MoB data restructure --------------------------------------------------

comm <- pivot_wider(community_dat, id_cols = 'SAMPLING_EVENT_IDENTIFIER',
                    names_from = 'SCIENTIFIC_NAME', 
                    values_from = 'OBSERVATION_COUNT', 
                    values_fill = 0)

env <- community_dat %>%
       group_by(SAMPLING_EVENT_IDENTIFIER) %>%
       summarize(lat = mean(LATITUDE),
                 long = mean(LONGITUDE),
                 ghm = mean(ghm),
                 grid_id = mean(grid_id))

all.equal(comm$SAMPLING_EVENT_IDENTIFIER, env$SAMPLING_EVENT_IDENTIFIER)

comm <- as.data.frame(comm)
row.names(comm) <- comm$SAMPLING_EVENT_IDENTIFIER
comm <- comm[ , -1]
comm[1:5, 1:5]

bird_mob <- make_mob_in(comm, env, coord_names = c('long', 'lat'),
                        latlong = TRUE)
bird_mob
## multi-metric MoB analysis --------------------------------------------------

stats <- get_mob_stats(bird_mob, group_var = 'grid_id', n_perm = 1)

alphas <- stats$samples_stats
alphas$ghm <- env$ghm[match(alphas$group, env$grid_id)]

gammas <- stats$groups_stats  
gammas$ghm <- env$ghm[match(gammas$group, env$grid_id)]

# fit linear models
lm_alpha = alphas %>% group_by(index, effort) %>%
    do(mod = lm(value ~ ghm, data = .))
lm_gamma = gammas %>% group_by(index, effort) %>%
    do(mod = lm(value ~ ghm, data = .))

# get model coefs

mod_coef_alpha = broom::tidy(lm_alpha, mod)
mod_coef_gamma = broom::tidy(lm_gamma, mod)

# get model summary
mod_sum_alpha = broom::glance(lm_alpha, mod)
mod_sum_gamma = broom::glance(lm_gamma, mod)

mob_met = rbind(data.frame(scale = 'alpha', alphas),
                data.frame(scale = 'gamma', gammas))
mob_met$index = factor(mob_met$index, 
                       levels = levels(mob_met$index)[c(2:1, 3:7)])

N1 = mob_met %>%
     subset(index %in% 'N') %>%
     ggplot(aes(x = ghm, y = value)) + 
         geom_point(aes(color = scale)) +
         geom_smooth(aes(color = scale), method = 'lm', se = F) +
         labs(x = "GHM", y = "Total abundance (N)")
ggsave("./Figures/ghm_vs_N.pdf", plot = N1, width = 15, height = 12, 
       units = "cm")

p1 = mob_met %>% 
  subset(abs(value) < 1000) %>%
  subset(index %in% c('S', 'S_n', 'S_PIE')) %>% 
  ggplot(aes(x = ghm, y = value, col = scale)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(. ~ index, scales = "free")


p2 = mob_met %>% 
    subset(abs(value) < 1000) %>%
    subset(index %in% c('beta_S', 'beta_S_n', 'beta_S_PIE')) %>% 
    ggplot(aes(x = ghm, y = value)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(. ~ index, scales = "free")

g = egg::ggarrange(p1, p2)
ggsave("./Figures/ENS.pdf", plot = g,  width = 20, height = 15,
       units = "cm")

## multi-scale MoB analysis --------------------------------------------------
ct <- with(bird_mob$env, tapply(SAMPLING_EVENT_IDENTIFIER, list(grid_id),
                          function(x) length(unique(x))))
# drop all grids with less than 10 plots
bird_mob$env$ct <- ct[match(bird_mob$env$grid_id, names(ct))]

nrow(bird_mob$comm)

ct_min = 50

bird_mob_sub <- subset(bird_mob, ct >= ct_min) 

nrow(bird_mob_sub$comm)

# now take a single random grab of first 10 checklists in each grid cell
# need to create an index vector 
uni_grids <- sort(unique(bird_mob_sub$env$grid_id))
row_indices <- NULL
for (i in seq_along(uni_grids)) {
    row_indices <- c(row_indices, 
                     sample(which(bird_mob_sub$env$grid_id == uni_grids[i]),
                            size = ct_min))
}

bird_mob_sub <- subset(bird_mob_sub, row_indices, type = 'integer', drop_levels = TRUE)

dim(bird_mob_sub$comm)

# map once more to see what still is in the analysis
ggplot()+
  geom_sf(data=grid_new, fill="transparent")+
  geom_sf(data=samples_sf)+
  geom_point(data=bird_mob_sub$spat, aes(x=long, y = lat, col = 'red')) +
  theme_bw()
##

# now analyze multiscale pattern 
deltas <- get_delta_stats(bird_mob_sub, env_var = "ghm", group_var = "grid_id",
                          type = 'continuous', log_scale = TRUE, n_perm = 19)

#save(deltas, file = './Results/deltas_prelim_50ct.Rdata')
#load('./Results/deltas_prelim_50ct.Rdata')

pdf("./Figures/multi_scale_prelim_50ct.pdf")                        
plot(deltas, eff_disp_smooth = TRUE)
plot(deltas, scale_by = 'indiv')
dev.off()

