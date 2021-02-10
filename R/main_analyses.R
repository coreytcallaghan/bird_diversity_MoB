# This script is the main analysis
# where for each BCR, we split
# the data and perform the analysis
# and save the BCR results out individually
# to read in and mess with later
# It is all wrapped up in one big function
# that we can split up later for better programming if need be

# packages
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
# only read in breeding season eBird data 
# join with modification scores
# for each eBird checklist
# and filter out some taxonomic groups not of interest
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

# function to run the analysis
# split by BCR
# BCR is used as it helps to account
# for the 'macro-ecological species pool' essentially
perform_analysis_function <- function(bcr_name, grid_size){
  
  message(paste0("Analyzing BCR ", bcr_name))
  
  # filter data for a BCR
  filtered_dat <- ebird_data %>%
    dplyr::filter(BCR_CODE==bcr_name)
  
  # convert points to an sf object
  samples_sf <- filtered_dat %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, LONGITUDE, LATITUDE) %>%
    distinct() %>%
    st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=3857)
  
  # make a quick convex hull of points to create grids over
  # with a small buffer
  con_hull <- concaveman(samples_sf, concavity=8) %>%
    st_buffer(0.5)
  
  # create hexagonal grid over con hull
  # and then subset to only grids that intersect with the con_hull (i.e., points)
  initial <- con_hull
  initial$index_target <- 1:nrow(initial)
  target <- st_geometry(initial)
  
  # can mess with grid size a bit more down the road
  grids <- con_hull %>%
    st_make_grid(cellsize=grid_size, square=FALSE)
  
  # To sf
  grid <- st_sf(index = 1:length(lengths(grids)), grids) # Add index
  
  # We identify the grids that belongs to a entity by assessing the centroid
  cent_grid <- st_centroid(grid)
  cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
  grid_new <- inner_join(grid, st_drop_geometry(cent_merge)) %>%
    mutate(grid_id=1:nrow(.))
  
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
  
  # structure data for MoB approach
  community_dat <- dat_with_grids %>%
    group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME,
             LATITUDE, LONGITUDE, grid_id) %>%
    summarize(OBSERVATION_COUNT=sum(OBSERVATION_COUNT),
              ghm=mean(ghm)) %>%
    ungroup()
  
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
  
  ## multi-metric MoB analysis 
  stats <- get_mob_stats(bird_mob, group_var = 'grid_id', n_perm = 1)
  
  alphas <- stats$samples_stats
  alphas$ghm <- env$ghm[match(alphas$group, env$grid_id)]
  
  gammas <- stats$groups_stats  
  gammas$ghm <- env$ghm[match(gammas$group, env$grid_id)]
  
  mob_met <- rbind(data.frame(scale = 'alpha', alphas),
                  data.frame(scale = 'gamma', gammas))
  mob_met$index <- factor(mob_met$index, 
                         levels = levels(mob_met$index)[c(2:1, 3:7)])
  
  # prepare final data summary to write out
  summary_data <- mob_met %>%
    mutate(BCR_CODE=bcr_name) %>%
    mutate(total_number_checklists=length(unique(community_dat$SAMPLING_EVENT_IDENTIFIER))) %>%
    mutate(total_number_species=length(unique(community_dat$COMMON_NAME))) %>%
    mutate(total_number_observations=nrow(community_dat)) %>%
    mutate(number_of_grids=length(unique(community_dat$grid_id)))
  
  saveRDS(summary_data, paste0("Intermediate_results/no_bootstrapping/grid_size_", grid_size, "/BCR_", bcr_name, ".RDS"))
}

# want to do this 32 times - once for each BCR
length(unique(ebird_data$BCR_CODE))

# apply the function over BCRs
lapply(unique(ebird_data$BCR_CODE), function(x){perform_analysis_function(x, 0.5)})

