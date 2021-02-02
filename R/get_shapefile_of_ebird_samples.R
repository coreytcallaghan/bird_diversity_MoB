# A script to get a shapefile for GEE ingestion

library(dplyr)
library(sf)

dat <- readRDS("birds/Data/ebird_data_raw.RDS")

# unique points/localities

shape <- dat %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, LATITUDE, LONGITUDE) %>%
  distinct() %>%
  st_as_sf(., coords=c("LONGITUDE", "LATITUDE"), crs=4326)

class(shape)


st_write(shape, "birds/Data/shapefile_of_ebird_samples/ebird_samples.shp")



