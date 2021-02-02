# some prelim analyses for fun!

library(ggplot2)
library(mgcv)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# read in eBird data and
# join with modification scores
# for each eBird checklist
ebird_data <- readRDS("Data/ebird_data_raw.RDS") %>%
  left_join(., read_csv("Data/checklists_mod_scores/ebird_samples_mod_scores.csv") %>%
              dplyr::select(first, SAMPLIN) %>%
              rename(ghm=first) %>%
              rename(SAMPLING_EVENT_IDENTIFIER=SAMPLIN), by="SAMPLING_EVENT_IDENTIFIER") %>%
  mutate(OBSERVATION_COUNT=as.numeric(as.character(OBSERVATION_COUNT)))


# total number of checklists in the data
length(unique(ebird_data$SAMPLING_EVENT_IDENTIFIER))

# total number of species in the data
length(unique(ebird_data$COMMON_NAME))

# let's just select one smaller area
# where we don't have to think aobut a lot of
# global scale biases for now, so it is easy to test some models
# and get some data together
# because I'm US at heart, I'll choose the BCR I grew up in: BCR 13
# I'll also just trim down to the 'breeding season' which will ignore a lot of inherent noise 
# mixed in with the intra-annual changes present in bird populaton data
filtered_dat <- ebird_data %>%
  dplyr::filter(BCR_CODE==13) %>%
  mutate(MONTH=month(OBSERVATION_DATE, label=TRUE, abbr=TRUE)) %>%
  dplyr::filter(MONTH %in% c("May", "Jun", "Jul", "Aug"))

# now how much data are we looking at?
# total number of checklists in the data
length(unique(filtered_dat$SAMPLING_EVENT_IDENTIFIER))

# total number of species in the data
length(unique(filtered_dat$COMMON_NAME))

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


#####################################
#####################################
#####################################
###### SPECIES LEVEL ################
#####################################

# Now will try to look at the same figure, but for some species
# probably well-sampled species or something
# write a quick function to
# get the data into species format (zero-filled)

species_dat_function <- function(species_name){
  
  sp_dat <- filtered_dat %>%
    dplyr::filter(COMMON_NAME==species_name)
  
  other_lists <- filtered_dat %>%
    dplyr::filter(!SAMPLING_EVENT_IDENTIFIER %in% sp_dat$SAMPLING_EVENT_IDENTIFIER) %>%
    mutate(COMMON_NAME=species_name) %>%
    mutate(SCIENTIFIC_NAME=unique(sp_dat$SCIENTIFIC_NAME)) %>%
    mutate(OBSERVATION_COUNT=0) %>%
    distinct()
  
  final_dat <- sp_dat %>%
    bind_rows(other_lists) %>%
    dplyr::filter(complete.cases(.))
  
  return(final_dat)
}

# get the data for a random sample of the top species
species_list <- filtered_dat %>%
  group_by(COMMON_NAME) %>%
  summarize(N=n()) %>%
  dplyr::filter(N>=100)

# now apply the data aggregation function to the species
# randomly sampled above
species_level_dat <- bind_rows(lapply(species_list$COMMON_NAME, species_dat_function)) %>%
  mutate(presence_absence=ifelse(OBSERVATION_COUNT==0, 0, 1))

# now we have a dataframe where each species has an
# equal number of observations, where absences are filled in as zero
# can double check this real quick
# this should return "TRUE"
species_level_dat %>%
  group_by(COMMON_NAME) %>%
  summarize(N=n()) %>%
  .$N %>%
  unique(.) %>%
  length() == 1

# lets make a raw data plot
# showing each species' response to
# ghm

ggplot(species_level_dat, aes(x=ghm, y=OBSERVATION_COUNT, color=COMMON_NAME, group=COMMON_NAME))+
  geom_smooth(method = "gam", se=FALSE, formula = y ~ s(x, bs = "cs"),
              method.args = list(family = "poisson"))+
  #geom_smooth(method="gam", se=FALSE)+
  #scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed Abundance")+
  guides(color=FALSE)

# looks like some pretty obvious differences among species at a quick look
# but this is super simple and only uses a gam in order to show all the data at the same time
# this could be done much better statistically
# and some species with massive abundances are driving this pattern

# will look at this one more way, using facet_wrap as well 
# to see the individual species responses a bit better
ggplot(species_level_dat, aes(x=ghm, y=OBSERVATION_COUNT, group=COMMON_NAME))+
  geom_point()+
  geom_smooth(method = "gam", se=FALSE, formula = y ~ s(x, bs = "cs"),
              method.args = list(family = "poisson"))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed Abundance")+
  guides(color=FALSE)+
  facet_wrap(~COMMON_NAME, ncol=5, scales="free_y")


# Pick a few species to play around with and run models on
# species that we "know" the response for
increasing <- species_level_dat %>%
  dplyr::filter(COMMON_NAME=="House Sparrow")

decreasing <- species_level_dat %>%
  dplyr::filter(COMMON_NAME=="American Redstart")

middle <- species_level_dat %>%
  dplyr::filter(COMMON_NAME=="American Goldfinch")

test_dat <- bind_rows(increasing, decreasing, middle)

ggplot(test_dat, aes(x=ghm, y=OBSERVATION_COUNT, group=COMMON_NAME))+
  geom_point()+
  geom_smooth(method = "gam", se=FALSE, formula = y ~ s(x, bs = "cs"),
              method.args = list(family = "poisson"))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed Abundance")+
  guides(color=FALSE)+
  facet_wrap(~COMMON_NAME, scales="free_y")

mod_increasing <- mgcv::gam(OBSERVATION_COUNT ~ s(ghm, bs="cs", k=5), family="poisson", data=increasing)
summary(mod_increasing)
plot(mod_increasing)

mod_decreasing <- mgcv::gam(OBSERVATION_COUNT ~ s(ghm, bs="cs", k=5), family="poisson", data=decreasing)
summary(mod_decreasing)
plot(mod_decreasing)

mod_middle <- mgcv::gam(OBSERVATION_COUNT ~ s(ghm, bs="cs", k=5), family="poisson", data=middle)
summary(mod_middle)
plot(mod_middle)



# can model each species' response with a GAM
# and get predicted data out of the model
# hacky for now
species_level_model <- function(species_name){
  
  message(paste0("Modelling ", species_name))
  
  sp_dat <- species_level_dat %>%
    dplyr::filter(COMMON_NAME==species_name)
  
  mod_abund <- mgcv::gam(OBSERVATION_COUNT ~ s(ghm, bs="cs", k=5), family="poisson", data=sp_dat)
  
  dat <- data.frame(ghm=plot(mod_abund)[[1]]$x,
                    fit=plot(mod_abund)[[1]]$fit,
                    se=plot(mod_abund)[[1]]$se) %>%
    mutate(fit_scaled=scales::rescale(fit)) %>%
    mutate(edf=summary(mod_abund)$edf) %>%
    mutate(COMMON_NAME=species_name)
  
  return(dat)
  
}

species_list <- filtered_dat %>%
  group_by(COMMON_NAME) %>%
  summarize(N=n()) %>%
  dplyr::filter(N>=100)

modelled_response <- bind_rows(lapply(species_list$COMMON_NAME, species_level_model))





#######################################
#######################################
######## "ECOSYSTEM" LEVEL ############
#######################################

# very quickly just join some body mass data
# with some missing species
# with occurrence along a modification gradient
# and plot the raw data
# without any summarizing or anything.
# not sure how we would want to summarize the total biomass along the gradient per se.

# body size data
bird_mass <- read_csv("birds/Data/body_size_data/cleaned_body_size_data.csv")

# join with the eBird data
# and create an 'ecosystem' level
idk <- filtered_dat %>%
  left_join(., bird_mass) %>%
  dplyr::filter(complete.cases(adult_body_mass_g))

ggplot(idk, aes(x=ghm, y=adult_body_mass_g))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification Index")+
  ylab("Observed body mass(log10)")+
  scale_y_log10()+
  geom_smooth(method="gam")





