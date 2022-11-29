### This script generates a mapping between countries, territories, and GLOBIOM designations. 
### It also generates EEZ shapefiles for each.

library(sf)
library(tidyverse)
library(here)

GLOBIOM_regions <- read.csv(here("Inputs/Regions_GLOBIOM37_.csv"),fileEncoding="UTF-8-BOM")
ISO_regions <- read.csv(here("Inputs/Regions_GLOBIOM37.csv"))
regions <- read.csv(here("Inputs/GLOBIOM_region_agg.csv"),fileEncoding="UTF-8-BOM") %>% 
  separate(correspondance.to.GLOBIOM.economic.markets, c("Country1", "Country2","Country3","Country4", "Country5","Country6","Country7"), ",") %>% 
  suppressWarnings() %>% 
  pivot_longer(cols = paste0("Country",1:7), values_to = "Region") %>% 
  dplyr::select(Region, Regional.aggregation) %>% 
  mutate(Region = trimws(Region)) %>% 
  filter(!is.na(Region)) %>% 
  left_join(GLOBIOM_regions, by = c("Region" = "REGION")) %>% 
  dplyr::select(GLOBIOM_COUNTRY, Region, Regional.aggregation) %>% 
  filter(!is.na(GLOBIOM_COUNTRY)) %>% 
  left_join(ISO_regions, by = "GLOBIOM_COUNTRY") %>% 
  rename_with(~c("GLOBIOM_Country", "Region", "Regional_Agg", "ISO3", "Country_Name")) 

eez <- st_read(here("Constraints/World_EEZ_v11_20191118_gpkg/eez_v11.gpkg"), quiet = TRUE) %>% 
  right_join(regions, by = c( "ISO_TER1"="ISO3")) 

eez_clean <- as.data.frame(eez) %>% dplyr::select(-geom)
  
eez$TERRITORY1 <- recode(eez$TERRITORY1,"Falkland / Malvinas Islands" = "Falkland Islands")

