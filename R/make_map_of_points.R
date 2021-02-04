# make map of observations

library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(readr)

# read in eBird data and
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

points_sf <- ebird_data %>%
  dplyr::select(LATITUDE, LONGITUDE, SAMPLING_EVENT_IDENTIFIER, ghm) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

us <- ne_countries(scale="medium", returnclass="sf", country = "United States of America")

ggplot()+
  geom_sf(data=us)+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  geom_sf(data=points_sf, aes(color=ghm), size=0.4)+
  scale_color_viridis_c(option="inferno")+
  xlim(130, 60)+
  ylim(25, 50)+
  theme(legend.position="bottom")

ggsave("Figures/map_of_points_in_USA.png", height=4.3, width=5.4, units="in")

