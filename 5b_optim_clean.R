### Import and Clean Data from Spatial Prioritization

library(here)
library(tidyverse)
library(beyonce)
library(terra)
source(here("1_regions.R"))

 globiom <- readRDS(here("Inputs/globiom.RDS"))
# globiom_ag <- readRDS(here("Inputs/globiom.RDS"))

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

dm_assumption = ""
native = ifelse(exists("native"),native, TRUE)
native_file = ifelse(native,"native","nonnative")

files_RDS <- list.files(here(paste0("Outputs/Regional_Production",dm_assumption,"/",native_file)),pattern = ".RDS", full.names = TRUE) %>% 
  c(list.files(here(paste0("Outputs/Regional_Production",dm_assumption,"/",native_file,"/combos")),pattern = ".RDS", full.names = TRUE) )

# files_RDS_ <- list.files(here(paste0("Outputs/Regional_Production",dm_assumption,"/",native_file)) %>% str_replace("LUC_Project_min","LUC_Project"),pattern = ".RDS", full.names = TRUE) %>% 
#   c(list.files(here(paste0("Outputs/Regional_Production",dm_assumption,"/",native_file,"/combos")),pattern = ".RDS", full.names = TRUE) )
# 
#   
  

files_ras <- list.files(here(paste0("Outputs/Regional_Production",dm_assumption,"/",native_file)),pattern = ".tif", full.names = TRUE) %>% str_remove(".aux.xml") %>% unique()

#files_RDS <- c(files_RDS[files_RDS %>% str_detect("_m")], files_RDS[files_RDS %>% str_detect("COMB_")])

sw_supply <- lapply(files_RDS, readRDS) %>% 
  lapply(function(x) {bind_rows(x) %>% 
      ungroup() %>%  
      mutate_if(is_all_numeric, as.numeric )}) %>% 
   bind_rows() %>%
  mutate(Demand_Met = Supply>Demand)
  # filter(Units == "MJ")
  
scen_order <- (globiom$SW_Scen %>% unique)[-1]

#sw_supply <- lapply(files_RDS, function(x) readRDS(x)) %>% lapply(function(x) bind_rows(x) %>% mutate_at(c(1,3,7:48), as.numeric )) %>% bind_rows() 

reg_order <- sw_supply %>% 
  group_by(Region) %>% 
  summarise(Demand_Met = sum(Demand_Met,na.rm = TRUE)) %>% 
  arrange(Demand_Met) %>% 
  pull(Region)

sw_supply <- sw_supply %>% mutate( Region = factor(Region, levels = reg_order),
                     SW_Scen = factor(SW_Scen, levels = scen_order),
                     Demand = as.double(Demand)) 
  
saveRDS(sw_supply,here(paste0("Inputs/sw_supply.RDS")))





# 
# region_names <- c("WORLD",(eez_clean %>% arrange(Regional_Agg) %>% 
#                              mutate(Region = str_remove_all(Region, "Reg"),
#                                     Region = str_replace(Region, "_"," "),
#                                     Region = str_replace(Region, "Af"," Africa"),
#                                     Region = str_replace(Region, "Africar","Africa"),
#                                     Region = gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", Region),
#                                     Region = case_when(
#                                       Region == "RSAM" ~ "Rest of South America",
#                                       Region == "RCAM" ~ "Rest of Central America",
#                                       Region == "RSAS" ~ "Rest of South Asia",
#                                       Region == "ROWE" ~ "Rest of Western Europe",
#                                       Region == "RCEU" ~ "Rest of Central Europe",
#                                       Region == "RSEA PAC" ~ "Rest of South East Asia - P",
#                                       Region == "RSEA OPA" ~ "Rest of South East Asia - O",
#                                       TRUE ~ Region
#                                     )
#                                     #    Region = str_replace(Region, "Afr"," Africa"),
#                              ) %>% 
#                              pull(Region) %>% unique())) %>% rev 




# 

# 
# # beyonce_palette(33)[c(1,3,4,5)],
# beyonce_palette(72)[c(1,2,3,6)])
# sw_brks = globiom$SW_Scen %>% unique %>% re