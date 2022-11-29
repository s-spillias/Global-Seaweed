### Trim SDMs by 'native' distribution

## Long Running ~ 2+ hours

library(tidyverse)
library(terra)
library(rgdal)
library(rgeos)
library(sf)
library(here)
library(beyonce)
library(patchwork)
library(dismo)

source(here("1_regions.R"))

figure_folder <- here("Manuscript_Latex/YSSP_Report_Latex/figures") %>% str_remove("LUC_Project_min/")

eez_vec <- eez %>% filter(!is.na(TERRITORY1)) %>% vect()

eez_rast <- rast(here("Inputs/eez_rast.tif"))

ocean_area <- 35700 ### in Mha

all_138 = FALSE

occurrence_all <- read.csv(here("Inputs/occurrence_use.csv")) 
occurrence_all_eez <- occurrence_all %>% 
  as_tibble %>%
  terra::vect(geom = c("x", "y"),
              crs = "+proj=longlat +datum=WGS84 +no_defs"
  ) %>% 
  terra::intersect(eez_vec)

sp_data <- readRDS(here("Inputs/util_data.RDS")) %>% 
  pull(Taxa) %>% 
  unique()

threshed <- rast(list.files(path = here("Outputs/max_threshed/"), pattern = ".tif", full.names = TRUE)) 

sp_all <- names(threshed)

sp_avail <- if(all_138){sp_all}else{intersect(sp_data, sp_all)}

threshed <- threshed %>% 
  subset(sp_avail) ##### Select only seaweed species with profile data

### Constrain by EEZ

thresh_eez <- threshed %>% mask(eez_rast, filename = here("Outputs/thresh_eez.tif"), overwrite = TRUE)

### Constrain by Native Distribution
max_threshed_nat_files <- list.files(path = here("Outputs/max_threshed_nat/"), pattern = ".tif", full.names = TRUE)

if(max_threshed_nat_files %>% length != 0){
thresh_nat <- rast(list.files(path = here("Outputs/max_threshed_nat/"), pattern = ".tif", full.names = TRUE)) %>% 
  subset(sp_avail)

} else { ## Takes ~ 2 Hours for 34 species
  
nat_area <- vector()

sp_map <- sp_avail %>% c("All_Sp")

for (i in 15:length(sp_map)){

  sp_name = if(sp_map[i] != "All_Sp"){sp_map[i]}else{sp_avail}
  
  thr <- threshed[[sp_name]]
  message(sp_name)
  
  terr = occurrence_all_eez %>% as_tibble() %>% 
    filter(species %in% sp_name) %>% 
    pull(TERRITORY1) %>% 
    unique
  
  extent <- eez %>% 
    filter(TERRITORY1 %in% terr) %>% 
    as("Spatial") %>% 
    vect()
  
  nat <- thr %>% mask(extent)
  writeRaster(nat,filename = here(paste0("Outputs/max_threshed_nat/",sp_map[i],".tif")), overwrite = TRUE)
  nat_area[i] <- expanse(nat, unit = "ha")
  message(nat_area[i])
}
thresh_nat_files <- list.files(path = here("Outputs/max_threshed_nat/"), pattern = ".tif", full.names = TRUE) 
 thresh_nat <- rast(thresh_nat_files[!str_detect(thresh_nat_files,"All_Sp")])
}

### Constrain by SocioEconomic Layers

cons_ras <- rast(here("Outputs/cons_ras.tif")) %>% setNames("Constraints")

min_constraint = 0.5

thresh_cons <- cons_ras %>% mask(thresh_nat)

names(thresh_cons) <- sp_avail

writeRaster(thresh_cons, filename = here("Outputs/thresh_cons.tif"), overwrite = TRUE)

assign(paste0("thresh_cons_",min_constraint), thresh_cons %>% app(fun = function(x){ifelse(x < min_constraint,NA,x)}
                                          ,              filename = here("Outputs",paste0("thresh_cons_",min_constraint,".tif")), overwrite = TRUE
                                          )
       )

#### thresh_cons can now be used as input in the HPC