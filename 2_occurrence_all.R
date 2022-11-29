## Consolidate all occurrence data from various sources and prune bad records

library(tidyverse)
library(rgdal)
library(sf)
library(robis)
#library(rgbif)
library(here)
#library(raster)
library(terra)
library(tidyterra)


new_old_names <- c("Laminaria saccharina", 
                   "Laminaria ochotensis",
                   "Neopyropia tenera",
                   "Neopyropia yezoensis",
                   "Saccharina japonica",
                   "Laminaria japonica",
                   "Porphyra yezoensis",
                   "Porphyra tenera",
                   "Pyropia tenera",
                   "Enteromorpha prolifera",
                   "Ulva prolifera",
                   "Eucheuma spinosum",
                   "Eucheuma denticulatum",
                   "Ulva prolifera",
                   "Enteromorpha clathrata",
                   "Ulva clathrata") 

country_use <- suppressWarnings(read.csv(here("Inputs/white_wilson_sp.csv")) %>% 
                                  filter(Use != "") %>% 
                                  separate('Country', paste("Countries", 1:15, sep="_"), sep=", ", extra="drop") %>% 
                                  pivot_longer(cols = paste("Countries", 1:15, sep="_"), values_to = "Country") %>% 
                                  dplyr::select(-name) %>% 
                                  separate('Use', paste("Uses", 1:6, sep="_"), sep=", ", extra="drop") %>% 
                                  pivot_longer(cols = paste("Uses", 1:6, sep="_"), values_to = "Uses") %>% 
                                  filter(!is.na(Uses),
                                         !is.na(Country)) %>% 
                                  dplyr::select(Taxa, Uses, Country) %>% 
                                  mutate(Uses = case_when(Uses == "F" ~ "Food",
                                                          Uses == "A" ~ "Agar",
                                                          Uses == "C" ~ "Carageenan",
                                                          Uses == "Al" ~ "Alginate",
                                                          Uses == "M" ~ "Medicine",
                                                          Uses == "RoK" ~ "Roe on Kelp",
                                                          Uses == "Ag" ~ "Agricultural",
                                                          Uses == "P" ~ "Paper",
                                                          Uses == "Co" ~ "Unknown")) %>% 
                                  relocate(Uses, .after = Taxa) %>% 
                                  mutate(Taxa = str_replace_all(Taxa, c(" spp\\." = " sp\\.",
                                                                        " sp\\." = ""))))

sp_country <- country_use %>% dplyr::select(Taxa, Country) %>% distinct() %>% relocate(Country) %>% 
  mutate(Taxa = paste0(word(Taxa,1)," ", ifelse(is.na(word(Taxa,2)),"",word(Taxa,2))))
sp_search <- sp_country %>% pull(Taxa) %>% c(new_old_names) %>% unique()
sp_use <- country_use %>% dplyr::select(Taxa, Uses) %>%  distinct() 

### Import Occurrence DATA

## Macroalgae Herbarium

occ_MH <- list.files(here("Inputs/obs_data/MH"), pattern = "occurrences", full.names = TRUE) %>% 
  lapply(function(x) {read.csv(x) %>% 
      dplyr::select(decimalLongitude,decimalLatitude,scientificName, basisOfRecord) %>% 
      rename(species = scientificName) %>% 
      filter(!str_detect(decimalLongitude,"http")) %>% 
      filter(!str_detect(decimalLatitude,"http")) %>% 
      mutate(decimalLongitude = as.numeric(decimalLongitude),
             decimalLatitude = as.numeric(decimalLatitude))
  }
  ) %>% 
  bind_rows()


## Global Biodiversity Information Facility
occ_gbif <- list.files(here("Inputs/obs_data/GBIF"), pattern = "220831081235567", full.names = TRUE) %>% 
  lapply(function(x) {
    read.delim(x, quote = "", 
               row.names = NULL, 
               stringsAsFactors = FALSE) %>% 
      dplyr::select(decimalLongitude,decimalLatitude, genus, species, basisOfRecord, issue)
  }
  ) %>% 
  bind_rows() %>% 
  mutate(species = ifelse(species == "", genus, species)) %>% 
  filter(across(.cols = c(decimalLatitude, decimalLongitude, genus, species), 
                .fns = function(x) {!is.na(x)})) %>% 
  filter(across(.cols = c(decimalLatitude, decimalLongitude, genus, species), 
                .fns = function(x) {x != ""}))  %>% 
  filter(!str_detect(issue, "PRESUMED"), !str_detect(issue, "ROUNDED")) %>% 
  dplyr::select(-c(genus, issue))

#### Atlas of Living Australia
occ_ala <- list.files(here("Inputs/obs_data/aus-living-atlas"), pattern = "records", full.names = TRUE) %>% 
  lapply(function(x) { 
    read.csv(x) %>% 
      as.data.frame() %>%  mutate(species = ifelse(species == "", genus, species)) %>% 
      filter(across(.cols = c(decimalLatitude, decimalLongitude, genus, species), 
                    .fns = function(x) {!is.na(x)})) %>% 
      filter(across(.cols = c(decimalLatitude, decimalLongitude, genus, species), 
                    .fns = function(x) {x != ""}))  %>% 
      filter(species %in% unique(sp_country$Taxa)) %>% 
      dplyr::select(decimalLongitude,decimalLatitude, species, basisOfRecord)
  }
  ) %>% 
  bind_rows()


if (!(list.files(here("Inputs/obs_data/ROBIS")) %>% str_detect("robis.RDS") %>% sum())) {
  occ_robis <- robis::occurrence(scientificname = sp_search)  %>% 
    dplyr::select(decimalLongitude,decimalLatitude,scientificName, basisOfRecord) %>% 
    rename(species = scientificName) %>% 
    mutate(decimalLongitude = as.numeric(decimalLongitude),
           decimalLatitude = as.numeric(decimalLatitude))
  saveRDS(occ_robis, here("Inputs/obs_data/ROBIS/robis.RDS")) 
} else { message("Sourcing Local ROBIS Directory")
  occ_robis <- readRDS(here("Inputs/obs_data/ROBIS/robis.RDS"))
}


taxonomy <- read.csv(here("Inputs/sw_taxonomy.csv")) %>% 
  mutate(Species = word(species,2))


occ_lit_review <- read.csv(here("Inputs/SW_Farming_Locations.csv")) %>% 
  separate_rows(Seaweed.Species, sep = "&") %>% 
  separate_rows(Seaweed.Species, sep = ",") %>% 
  mutate(Species = Seaweed.Species %>% trimws() %>% word(2),
         basisOfRecord = "OBSERVATION") %>% 
  left_join(taxonomy %>% filter(species %in% (readRDS(here("Inputs/util_data.RDS")) %>% pull(Taxa) %>% unique)), by = "Species") %>% 
  dplyr::select(species, decimalLongitude,decimalLatitude, basisOfRecord) %>% 
  distinct() %>% 
  filter(!is.na(species)) 

occurrence_all_raw <-  bind_rows(
  occ_ala,
  occ_gbif,
  occ_MH,
  occ_robis,
  occ_lit_review
) %>% 
  rename(x = decimalLongitude,
         y = decimalLatitude,
         record_type = basisOfRecord) %>% 
  mutate(species = case_when(
    str_detect(species,"Pyropia tenera")~"Neopyropia tenera",
    str_detect(species,"Porphyra tenera")~"Neopyropia tenera" ,
    str_detect(species,"Pyropia yezoensis")~"Neopyropia yezoensis" ,
    str_detect(species,"Porphyra yezoensis")~"Neopyropia yezoensis" ,
    str_detect(species,"Laminaria japonica")~"Saccharina japonica"  ,
    str_detect(species,"Laminaria ochotensis")~"Saccharina japonica",
    str_detect(species,"Laminaria saccharina")~"Saccharina latissima",
    str_detect(species,"Eucheuma spinosum")~"Eucheuma denticulatum",
    str_detect(species,"Enteromorpha prolifera")~"Ulva prolifera",
    str_detect(species,"Enteromorpha clathrata")~"Ulva clathrata",
    TRUE ~ as.character(species)
  )) %>% 
  filter(species %in% c(sp_search)) %>% 
  distinct()

write.csv(occurrence_all_raw,here("Inputs/occurrence_all_raw.csv"))
#occurrence_all_raw <- read.csv(here("Inputs/occurrence_all_raw.csv")) %>% dplyr::select(-X)

### Filter by EEZ 

eez_vec <- st_read(here("Constraints/World_EEZ_v11_20191118_gpkg/eez_v11.gpkg"), quiet = TRUE) %>% 
  vect()

points <- occurrence_all_raw %>%
  as_tibble %>%
  terra::vect(geom = c("x", "y"),
              crs = "+proj=longlat +datum=WGS84 +no_defs"
  )

in_eez <- terra::intersect(points, eez_vec)

occurrence_all <- terra::crds(in_eez) %>% bind_cols(terra::values(in_eez)) %>%
  dplyr::select(-c(X_1,Y_1)) %>%
  dplyr::select(x,y,species, record_type)


## Save
message("Saving Occurrence Data")

write.csv(occurrence_all,here("Inputs/occurrence_all.csv"), row.names = FALSE)

