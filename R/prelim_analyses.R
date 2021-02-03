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
              rename(SAMPLING_EVENT_IDENTIFIER=SAMPLIN), by="SAMPLING_EVENT_IDENTIFIER")


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
  st_buffer(0.1)

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
  st_make_grid(cellsize=.1, square=FALSE)

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
community_dat <- filtered_dat %>%
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarize(OBSERVATION_COUNT=sum(OBSERVATION_COUNT),
            ghm=mean(ghm)) %>%
  ungroup() %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarize(SPECIES_RICHNESS=length(unique(COMMON_NAME)),
            ABUNDANCE=sum(OBSERVATION_COUNT),
            GHM=mean(ghm)) %>%
  dplyr::filter(complete.cases(GHM))


# raw plot of community level vs
# modification score
ggplot(community_dat, aes(x=GHM, y=SPECIES_RICHNESS))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed Species Richness")+
  geom_smooth(method="gam")


ggplot(community_dat, aes(x=GHM, y=ABUNDANCE))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed Abundance(log10)")+
  scale_y_log10()+
  geom_smooth(method="gam")








