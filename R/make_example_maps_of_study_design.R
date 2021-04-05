# This is an R script to visualize
# the increasing grid size
# and the number of observations per grid
# choosing just one BCR as an example
# to probably put in the supporting information
# so people can follow what I'm doing
# it basically follows the "apply_MoB_across_BCRs.R" script
# but only for a single BCR

# packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(sf)
library(concaveman)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

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
                               "Hydrobatidae (Northern Storm-Petrels)", "Oceanitidae (Southern Storm-Petrels)")) %>%
  dplyr::filter(complete.cases(BCR_CODE))

bcr_name=30

# filter data for a BCR
filtered_dat <- ebird_data %>%
  dplyr::filter(BCR_CODE==bcr_name)

# convert points to an sf object
samples_sf <- filtered_dat %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, LONGITUDE, LATITUDE) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)

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
grids_0.1 <- con_hull %>%
  st_make_grid(cellsize=0.1, square=FALSE)
grids_0.5 <- con_hull %>%
  st_make_grid(cellsize=0.5, square=FALSE)
grids_1 <- con_hull %>%
  st_make_grid(cellsize=1, square=FALSE)

# To sf
grid_0.1 <- st_sf(index = 1:length(lengths(grids_0.1)), grids_0.1) # Add index
grid_0.5 <- st_sf(index = 1:length(lengths(grids_0.5)), grids_0.5)
grid_1 <- st_sf(index = 1:length(lengths(grids_1)), grids_1)

# We identify the grids that belongs to a entity by assessing the centroid
cent_grid_0.1 <- st_centroid(grid_0.1)
cent_merge_0.1 <- st_join(cent_grid_0.1, initial["index_target"], left = F)
grid_new_0.1 <- inner_join(grid_0.1, st_drop_geometry(cent_merge_0.1)) %>%
  mutate(grid_id=1:nrow(.))

cent_grid_0.5 <- st_centroid(grid_0.5)
cent_merge_0.5 <- st_join(cent_grid_0.5, initial["index_target"], left = F)
grid_new_0.5 <- inner_join(grid_0.5, st_drop_geometry(cent_merge_0.5)) %>%
  mutate(grid_id=1:nrow(.))

cent_grid_1 <- st_centroid(grid_1)
cent_merge_1 <- st_join(cent_grid_1, initial["index_target"], left = F)
grid_new_1 <- inner_join(grid_1, st_drop_geometry(cent_merge_1)) %>%
  mutate(grid_id=1:nrow(.))

# assign each eBird sample to a grid
a_0.1 <- samples_sf %>%
  st_within(grid_new_0.1) %>%
  as.data.frame() %>%
  left_join(., grid_new_0.1 %>%
              mutate(col.id=1:nrow(.))) %>%
  right_join(., samples_sf %>%
               mutate(row.id=1:nrow(.))) %>%
  dplyr::select(grid_id, SAMPLING_EVENT_IDENTIFIER)

sum(is.na(a_0.1$grid_id))

a_0.5 <- samples_sf %>%
  st_within(grid_new_0.5) %>%
  as.data.frame() %>%
  left_join(., grid_new_0.5 %>%
              mutate(col.id=1:nrow(.))) %>%
  right_join(., samples_sf %>%
               mutate(row.id=1:nrow(.))) %>%
  dplyr::select(grid_id, SAMPLING_EVENT_IDENTIFIER)

sum(is.na(a_0.5$grid_id))

a_1 <- samples_sf %>%
  st_within(grid_new_1) %>%
  as.data.frame() %>%
  left_join(., grid_new_1 %>%
              mutate(col.id=1:nrow(.))) %>%
  right_join(., samples_sf %>%
               mutate(row.id=1:nrow(.))) %>%
  dplyr::select(grid_id, SAMPLING_EVENT_IDENTIFIER)

sum(is.na(a_1$grid_id))

dat_with_grids_0.1 <- filtered_dat %>%
  left_join(., a_0.1, by="SAMPLING_EVENT_IDENTIFIER")

dat_with_grids_0.5 <- filtered_dat %>%
  left_join(., a_0.5, by="SAMPLING_EVENT_IDENTIFIER")

dat_with_grids_1 <- filtered_dat %>%
  left_join(., a_1, by="SAMPLING_EVENT_IDENTIFIER")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

us <- ne_countries(scale="medium", returnclass="sf", country = "United States of America") %>%
  st_transform(crs=st_crs(grid_new_1))

bcrs <- st_read("Data/Spatial data/bcr_terrestrial_shape/BCR_Terrestrial_master.shp")

cutoff_grid_1 <- dat_with_grids_1 %>%
  group_by(grid_id) %>%
  summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  mutate(cutoff_10=ifelse(number_checklists>=10, "Yes", "No")) %>%
  mutate(cutoff_30=ifelse(number_checklists>=30, "Yes", "No")) %>%
  mutate(cutoff_50=ifelse(number_checklists>=50, "Yes", "No"))

grid_1_plot <- grid_new_1 %>%
  left_join(., cutoff_grid_1) %>%
  dplyr::filter(complete.cases(number_checklists))

cutoff_10_grid_1 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_1_plot, aes(fill=cutoff_10), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_10_grid_1

cutoff_30_grid_1 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_1_plot, aes(fill=cutoff_30), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_30_grid_1

cutoff_50_grid_1 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_1_plot, aes(fill=cutoff_50), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_50_grid_1

# repeat for 0.5 grid size
cutoff_grid_0.5 <- dat_with_grids_0.5 %>%
  group_by(grid_id) %>%
  summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  mutate(cutoff_10=ifelse(number_checklists>=10, "Yes", "No")) %>%
  mutate(cutoff_30=ifelse(number_checklists>=30, "Yes", "No")) %>%
  mutate(cutoff_50=ifelse(number_checklists>=50, "Yes", "No"))

grid_0.5_plot <- grid_new_0.5 %>%
  left_join(., cutoff_grid_0.5) %>%
  dplyr::filter(complete.cases(number_checklists))

cutoff_10_grid_0.5 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_0.5_plot, aes(fill=cutoff_10), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_10_grid_0.5

cutoff_30_grid_0.5 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_0.5_plot, aes(fill=cutoff_30), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_30_grid_0.5

cutoff_50_grid_0.5 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_0.5_plot, aes(fill=cutoff_50), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_50_grid_0.5


# repeat for 0.1 grid size
cutoff_grid_0.1 <- dat_with_grids_0.1 %>%
  group_by(grid_id) %>%
  summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  mutate(cutoff_10=ifelse(number_checklists>=10, "Yes", "No")) %>%
  mutate(cutoff_30=ifelse(number_checklists>=30, "Yes", "No")) %>%
  mutate(cutoff_50=ifelse(number_checklists>=50, "Yes", "No"))

grid_0.1_plot <- grid_new_0.1 %>%
  left_join(., cutoff_grid_0.1) %>%
  dplyr::filter(complete.cases(number_checklists))

cutoff_10_grid_0.1 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_0.1_plot, aes(fill=cutoff_10), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_10_grid_0.1

cutoff_30_grid_0.1 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_0.1_plot, aes(fill=cutoff_30), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_30_grid_0.1

cutoff_50_grid_0.1 <- ggplot()+
  geom_sf(data=bcrs, color="black", fill="gray80")+
  geom_sf(data=grid_0.1_plot, aes(fill=cutoff_50), alpha=0.9)+
  scale_fill_brewer(palette="Set1")+
  coord_sf()+
  theme_bw()+
  xlim(85, 65)+
  ylim(35, 50)+
  theme(panel.grid.major=element_blank())+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

cutoff_50_grid_0.1

# put all the plots together into a patchwork plot
# to show the sampling design
cutoff_10_grid_0.1 + ggtitle("Grid size=small") + 
  cutoff_10_grid_0.5 + ggtitle("Grid size=medium") + 
  cutoff_10_grid_1 + ggtitle("Grid size=large") +
  cutoff_30_grid_0.1 + cutoff_30_grid_0.5 + cutoff_30_grid_1 +
  cutoff_50_grid_0.1 + cutoff_50_grid_0.5 + cutoff_50_grid_1 + plot_layout(ncol=3)

ggsave("Figures/talk_figures/study_design_example_BCR_30.png", height=7.5, width=9, units="in")

