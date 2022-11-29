## This script cleans FAO data

library(tidyverse)
library(readxl)
library(sf)
library(here)
source(here("1_regions.R"))


limits <- readRDS(here("Inputs/main_data.RDS")) %>% 
  dplyr::select(Country, UN_Code, Total_Production) %>% 
  rename("Limit" = "Total_Production") %>% 
  distinct()

FAO_Data <- read.csv(here("Inputs/FAO_DATA/TS_FI_AQUACULTURE.csv") )
country <-  read.csv(here("Inputs/FAO_DATA/CL_FI_COUNTRY_GROUPS.csv"))
species <-  read.csv(here("Inputs/FAO_DATA/CL_FI_SPECIES_GROUPS.csv"))



FAO_Clean <- FAO_Data %>% left_join(country, by = c("COUNTRY" = "UN_Code")) %>% 
  left_join(species, by = c("SPECIES" = "X3Alpha_Code")) %>% 
  rename("UN_Code" = "COUNTRY") %>% 
  dplyr::select(YEAR,SPECIES,ENVIRONMENT, QUANTITY, QUANTITY_UNIT, QUANTITY_SYMBOL,VALUE, VALUE_SYMBOL, Name_En.x, Official_Name_En, UN_Code, Continent_Group, EcoClass_Group, GeoRegion_Group, Scientific_Name, Family, Major_Group) %>% 
  filter(ENVIRONMENT == 3) %>% 
  mutate(Genus = word(Scientific_Name,1)) %>% 
  rename(Country = Name_En.x,
         Year = YEAR,
         Continent = Continent_Group,
         Region = GeoRegion_Group) %>% 
  mutate(Metric = QUANTITY) %>%  ### Choose HERE: VALUE, QUANTITY, PRICE
  group_by(Year, Scientific_Name, Genus, Major_Group, Country, UN_Code, Region, Continent) %>% 
  distinct() %>% 
  summarize(Metric = sum(Metric, na.rm = TRUE)) %>% 
  left_join(eez_clean, by = c("UN_Code"="UN_SOV1")) %>% 
  mutate(UN_Code = case_when(UN_Code == 836 ~ 834,
                             UN_Code == 184 ~ 554,
                             TRUE ~ UN_Code)) %>% 
  left_join(limits, by = c("UN_Code" = ifelse(is.na("UN_Code"),"UN_TER1","UN_Code"))) %>% 
  rename("Country" = "Country.y",
         "Region" = "Region.y") %>% 
  dplyr::select(Year, Scientific_Name, Genus, Major_Group, Country, Country_Name, Limit, GLOBIOM_Country, Region, Regional_Agg, Continent, Metric, Limit) %>% 
  mutate(Scientific_Name = case_when(
         Scientific_Name =="Pyropia tenera"~"Neopyropia tenera",
         Scientific_Name =="Porphyra tenera"~"Neopyropia tenera" ,
         Scientific_Name =="Pyropia yezoensis"~"Neopyropia yezoensis" ,
         Scientific_Name =="Porphyra yezoensis"~"Neopyropia yezoensis" ,
         Scientific_Name =="Laminaria japonica"~"Saccharina japonica"  ,
         Scientific_Name =="Laminaria ochotensis"~"Saccharina japonica",
         Scientific_Name =="Laminaria saccharina"~"Saccharina latissima",
         TRUE ~ as.character(Scientific_Name)
  )) %>% 
  distinct() 
rm(FAO_Data);rm(country);rm(species)
