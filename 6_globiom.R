## Import and reshape GLOBIOM outputs.

library(gdxrrw)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
#library(globiomvis)
library(tidyverse)

igdx("C:/GAMS/36") ## Needed to import GDX file
options(scipen = 9999)
library(beyonce)
library(here)
globiom_path <- here("Inputs/")
source(here("Inputs/1_regions.R"))
source(here("Inputs/6a_var_id.R"))


globiom_filename <- "output_w_affor_176_merged"
globiom_file <- file.path(globiom_path,globiom_filename)


region_names <- rgdx.set(file.path(globiom_path,"a6_r1"), "ANYREGIONS")

region_ag <- rgdx.set(globiom_file, "REGION_AG_MAP") %>% 
  setNames(c("file","Region","REGION_AG")) %>% 
  mutate(REGION_AG = as.character(REGION_AG)) %>% 
  filter(REGION_AG != "World")

#land_comp <- rgdx.param(file.path(globiom_path,"a6_r1"),"Land_Compare3")
# globiom_path <- here("Inputs/gdx_62") #"C:/Users/Scott/Documents/_PhD/GLOBIOM_Prerelease_Model-master"
# globiom_run <- file.path(globiom_path,"Model/0_executebatch.gms")
# gams(globiom_run)

# globiom_file <- list.files(globiom_path, full.names = TRUE)
# 
# merged = !(length(globiom_file) > 1)
# 
# globiom_file = if (merged == FALSE) {globiom_file[1]} else {globiom_file}

region_ag <- rgdx.set(globiom_file, "REGION_AG_MAP") %>% 
  setNames(c("file","REGION_AG","REGION")) %>% 
  mutate(REGION_AG = as.character(REGION_AG)) %>% 
  filter(REGION_AG != "World")


sw_order <- c(rgdx.set(globiom_file, "IEA_SCEN") %>% filter(j!="scenBASE") %>% 
              #  {if (merged) {
                pull(.,2) %>% #} else {pull(.,1)}} %>% 
                unique %>% 
                as.character,"SW_Base")

affr_compare <-  rgdx.param(globiom_file, "FOREST_EMISSION_COMPARE") %>% 
  #droplevels() %>% 
  setNames(c("source","REGION", "ITEM_AG","MacroScen","BioenScen","SW_Scen","YEAR","OUTPUT_AG")) %>% 
  left_join(region_ag[,2:3] %>% distinct(), by = "REGION") %>% 
  mutate(OUTPUT_AG = OUTPUT_AG/(10*1000)) %>%  ## CHange units from 1000 tCO2/10yr to MT-CO2EQ_YEAR
  mutate(VAR_UNIT =  "MT-CO2EQ_YEAR",
         VAR_DESC = "Sequestration from Afforestation [MT-CO2EQ_YEAR]",
         VAR_ID = "EMIS",
         YEAR = as.integer(as.character(YEAR)) ,
         ITEM_AG = "AFFR") %>% 
  group_by(source,REGION,ITEM_AG,MacroScen,BioenScen,SW_Scen, YEAR, REGION_AG,VAR_UNIT,VAR_DESC,VAR_ID) %>% 
  summarise(OUTPUT_AG = sum(OUTPUT_AG)) %>% 
  mutate(SW_Scen = ifelse(SW_Scen == "scenBASE","SW_Base",as.character(SW_Scen)))


globiom <-
  rgdx.param(globiom_file, "OUTPUT") %>%
  setNames(c("file","VAR_ID", "VAR_UNIT", "REGION_AG", "ITEM_AG", "MacroScen", "BioenScen", "IEA_Scen","YEAR", "OUTPUT_AG")) %>% 
  dplyr::select(-file) %>% 
    mutate(YEAR = as.integer(as.character(YEAR)),
         ITEM_AG = toupper(as.character(ITEM_AG)),
         VAR_ID = toupper(VAR_ID),
         VAR_UNIT = toupper(VAR_UNIT),
         IEA_Scen = toupper(IEA_Scen),
         REGION_AG = as.character(REGION_AG),
         source = "globiom") %>%
  droplevels %>% 
   pivot_wider(names_from = IEA_Scen, values_from = OUTPUT_AG) %>% 
   left_join(var_id, by = "VAR_ID") %>% 
   rename("SW_Base" = "SCENBASE") %>% 
   pivot_longer(cols = starts_with("SW_"),names_to = "SW_Scen", values_to = "OUTPUT_AG") %>% 
   mutate(VAR_UNIT = str_replace_all(VAR_UNIT,"/","_"),
          VAR_UNIT = str_replace_all(VAR_UNIT," ","-"),
          IEA_Scen = toupper(SW_Scen)) %>% 
  bind_rows(
   affr_compare %>% mutate(REGION_AG = REGION) %>% dplyr::select(-REGION)
  ) %>% 
   mutate(REGION_LEVEL = case_when(REGION_AG %in% region_ag$REGION_AG ~ 3,
                             REGION_AG %in% region_ag$REGION ~ 2,
                             TRUE ~ 1)) %>% 
   dplyr::select(-IEA_Scen) %>% 
  mutate(SW_Scen = factor(SW_Scen, levels = sw_order))  
 
saveRDS(globiom,here("Inputs/globiom.RDS"))

globiom_ag <-# {if (!merged) {
#  lapply(list.files(here("Inputs/gdx_62"), full.names = TRUE),function(x) rgdx.param(x, "OUTPUT_AG")) %>% bind_rows %>% 
 #   setNames(c("VAR_ID", "VAR_UNIT", "REGION_AG", "ITEM_AG", "MacroScen", "BioenScen", "IEA_Scen","YEAR", "OUTPUT_AG")) 
#} else { 
  rgdx.param(globiom_file, "OUTPUT_AG") %>%
    setNames(c("file","VAR_ID", "VAR_UNIT", "REGION_AG", "ITEM_AG", "MacroScen", "BioenScen", "IEA_Scen","YEAR", "OUTPUT_AG")) %>% 
    dplyr::select(-file) %>% 
#}} %>%   
mutate(YEAR = as.integer(as.character(YEAR)),
         ITEM_AG = toupper(as.character(ITEM_AG)),
         VAR_ID = toupper(VAR_ID),
         VAR_UNIT = toupper(VAR_UNIT),
         IEA_Scen = toupper(IEA_Scen),
         REGION_AG = as.character(REGION_AG),
         source = "globiom") %>%
  droplevels %>% 
  pivot_wider(names_from = IEA_Scen, values_from = OUTPUT_AG) %>% 
  left_join(var_id, by = "VAR_ID") %>% 
  rename("SW_Base" = "SCENBASE") %>% 
  pivot_longer(cols = starts_with("SW_"),names_to = "SW_Scen", values_to = "OUTPUT_AG") %>% 
  mutate(VAR_UNIT = str_replace_all(VAR_UNIT,"/","_"),
         VAR_UNIT = str_replace_all(VAR_UNIT," ","-"),
         IEA_Scen = toupper(SW_Scen)) %>% 
  bind_rows(
   affr_compare
  ) %>% 
  mutate(REGION_LEVEL = case_when(REGION_AG %in% region_ag$REGION_AG ~ 3,
                                  REGION_AG %in% region_ag$ANYREGION ~ 2,
                                  TRUE ~ 1)) %>% 
  dplyr::select(-IEA_Scen,-source) %>% 
  mutate(SW_Scen = factor(SW_Scen, levels = sw_order))

saveRDS(globiom_ag,here("Inputs/globiom_ag.RDS"))

scen_map <- globiom %>% dplyr::select(REGION_AG,SW_Scen) %>% distinct()
saveRDS(scen_map,here("Inputs/scen_map.RDS"))

# dm_content <-  rgdx.param(globiom_file, "DM_CONTENT") %>% 
#   dplyr::select(-1) %>% 
#   setNames(c("ITEM_AG","DM_CONTENT")) %>% 
#   distinct()

 nutr_profile_ter <- readRDS(here("Inputs/nutr_profile_ter.RDS")) %>% 
  rownames_to_column(var = "ITEM_AG")

 food_compare <-# {if (!merged) {
  # lapply(list.files(here("Inputs/gdx_62"), full.names = TRUE),function(x) rgdx.param(x, "DDATA_COMPARE")) %>% bind_rows %>% 
   #  setNames(c("REGION_AG","ITEM_AG","VAR_ID","MacroScen", "BioenScen", "IEA_Scen","YEAR", "OUTPUT_AG")) 
#} else { 
   rgdx.param(globiom_file, "DDATA_COMPARE") %>%
     setNames(c("file","REGION_AG","ITEM_AG","VAR_ID","MacroScen", "BioenScen", "IEA_Scen","YEAR", "OUTPUT_AG")) %>% 
     dplyr::select(-file) %>% 
 #}} %>%   
 mutate(#YEAR = as.integer(as.character(YEAR)),
          ITEM_AG = toupper(as.character(ITEM_AG)),
          VAR_ID = toupper(VAR_ID)) %>% 
   mutate(REGION_LEVEL = case_when(REGION_AG %in% region_ag$REGION_AG ~ 3,
                                   REGION_AG %in% region_ag$ANYREGION ~ 2,
                                   TRUE ~ 1)) %>% 
   filter(VAR_ID == "QUANTITY") %>% 
   mutate(VAR_ID = "FOOD") %>% 
   pivot_wider(names_from = IEA_Scen, values_from = OUTPUT_AG) %>% 
   left_join(nutr_profile_ter, by = "ITEM_AG") %>% 
  # left_join(dm_content, by = "ITEM_AG") %>% 
   mutate(across(.cols = starts_with("SW_"), .fns = list(
     DM_Shortfall = function(x) {(x-scenBASE)*#DM_CONTENT*
         1000000}), # 1000 converts '1000 tonnes' to tonnes, and 1000 more converts tonnes to kg
     .names = "{.fn}.{.col}" )) %>% 
   dplyr::select(!starts_with("SW_")) %>% 
   pivot_longer(cols = starts_with("DM_Shortfall"), values_to = "DM_Shortfall", names_to = "SW_Scen") %>%
   mutate(SW_Scen = str_remove(SW_Scen, "DM_Shortfall.")) %>% 
   mutate(Protein_Shortfall = DM_Shortfall*Protein,
          Energy_Shortfall = DM_Shortfall*Energy) %>% 
   group_by(REGION_AG,SW_Scen,YEAR,VAR_ID) %>% 
   summarise(DM_Shortfall = sum(DM_Shortfall, na.rm = TRUE),
             Protein_Shortfall = sum(Protein_Shortfall, na.rm =TRUE),
             Energy_Shortfall = sum(Energy_Shortfall, na.rm = TRUE)) 
 
feed_compare <- #{if (!merged) {
 # lapply(list.files(here("Inputs/gdx_62"), full.names = TRUE),function(x) rgdx.param(x, "FEED_SUB_COMPARE")) %>% bind_rows %>% 
  #  setNames(c("REGION_AG" , "ITEM_AG","LIVE_SYSTEM" ,  "ANIMAL" , "AllMacroScen" ,    "ALLBioenScen"   ,  "IEA_Scen" ,     "YEAR"  ,   
  #             "OUTPUT_AG")) 
#} else { 
  rgdx.param(globiom_file, "FEED_SUB_COMPARE") %>%
    setNames(c("file","REGION_AG" , "ITEM_AG","LIVE_SYSTEM" ,  "ANIMAL" , "AllMacroScen" ,    "ALLBioenScen"   ,  "IEA_Scen" ,     "YEAR"  ,   
               "OUTPUT_AG")) %>% 
    dplyr::select(-file) %>% 
#}} %>% 
mutate(ITEM_AG = toupper(ITEM_AG),
         VAR_ID = "FEED") %>% 
 # filter(ALLITEM %in% nutr_profile_ter$ITEM_AG) %>% 
   pivot_wider(names_from = IEA_Scen, values_from = OUTPUT_AG) %>% 
  left_join(nutr_profile_ter, by = "ITEM_AG") %>% 
  #left_join(dm_content, by = "ITEM_AG") %>% 
  mutate(across(.cols = starts_with("SW_"), .fns = list(
    DM_Shortfall = function(x) {(x)*#DM_CONTENT*
        -1000000}), # 1000 converts '1000 tonnes' to tonnes, and 1000 more converts tonnes to kg
    .names = "{.fn}.{.col}" )) %>% 
  dplyr::select(!starts_with("SW_")) %>% 
  pivot_longer(cols = starts_with("DM_Shortfall"), values_to = "DM_Shortfall", names_to = "SW_Scen") %>%
  mutate(SW_Scen = str_remove(SW_Scen, "DM_Shortfall.")) %>% 
  mutate(Protein_Shortfall = DM_Shortfall*Protein,
         Energy_Shortfall = DM_Shortfall*Energy) %>% 
  left_join(eez_clean,by = c("REGION_AG" = "GLOBIOM_Country")) %>% 
  group_by(Region,SW_Scen,YEAR,VAR_ID) %>% 
  summarise(DM_Shortfall = sum(DM_Shortfall, na.rm = TRUE),
            Protein_Shortfall = sum(Protein_Shortfall, na.rm =TRUE),
            Energy_Shortfall = sum(Energy_Shortfall, na.rm = TRUE)) %>% 
  rename("REGION_AG" = "Region") 

fuel_conv <- read.csv(here("Inputs/fuel_process.csv")) %>% 
  rename("REGION_AG" = 1) %>% 
  pivot_wider(names_from = PROCESS, values_from = VALUE) %>% 
  mutate(ITEM = toupper(ITEM)) %>% 
  filter(ITEM %in% c("FAME", "C_ETHANOL")) %>% 
  discard(~all(is.na(.x))) %>% 
  pivot_longer(cols = contains("to"), names_to = "ITEM_AG", values_to = "CONVERSION_to_MJ_1000t") %>% 
  filter(!is.na(CONVERSION_to_MJ_1000t)) %>% 
  mutate(ITEM_AG = str_remove(ITEM_AG,"ToFAME") %>% str_remove("ToEthol") %>% toupper,
         CONVERSION_to_MJ_1000t = CONVERSION_to_MJ_1000t*1000000) 
   #### fuel_conv should be in MJ per 1000 tonnes

fuel_compare <-  #{if (!merged) {
 # lapply(list.files(here("Inputs/gdx_62"), full.names = TRUE),function(x) rgdx.param(x, "ABS_FUEL_REQ_COMPARE")) %>% bind_rows %>% 
  #  setNames(c("REGION_AG","ITEM_AG","MacroScen", "BioenScen", "IEA_Scen","YEAR", "OUTPUT_AG")) 
#} else { 
  rgdx.param(globiom_file, "ABS_FUEL_REQ_COMPARE") %>%
    setNames(c("file","REGION_AG","ITEM_AG","MacroScen", "BioenScen", "IEA_Scen","YEAR", "OUTPUT_AG")) %>% 
    dplyr::select(-file) %>% 
#}} %>% 
  mutate(ITEM_AG = toupper(ITEM_AG),
         VAR_ID = "FUEL") %>% 
  # filter(ALLITEM %in% nutr_profile_ter$ITEM_AG) %>% 
  pivot_wider(names_from = IEA_Scen, values_from = OUTPUT_AG) %>% 
 # left_join(nutr_profile_ter, by = "ITEM_AG") %>% 
#  left_join(dm_content, by = "ITEM_AG") %>% 
  mutate(across(.cols = starts_with("SW_"), .fns = list(
    Shortfall = function(x) {(x-scenBASE)}),
    .names = "{.fn}.{.col}" )) %>% 
  dplyr::select(!starts_with("SW_")) %>% 
  pivot_longer(cols = starts_with("Shortfall"), values_to = "Shortfall", names_to = "SW_Scen") %>%
  mutate(SW_Scen = str_remove(SW_Scen, "Shortfall.")) %>% 
  group_by(REGION_AG,SW_Scen,YEAR,VAR_ID) %>% 
  left_join(fuel_conv, by = c("REGION_AG", "ITEM_AG")) %>% 
  mutate(Shortfall = Shortfall*CONVERSION_to_MJ_1000t) %>% 
  summarise(Fuel_Energy_Shortfall = sum(Shortfall, na.rm = TRUE)) 

shortfalls <- bind_rows(food_compare,feed_compare,fuel_compare)
saveRDS(shortfalls,here("Inputs/shortfalls.RDS"))
 

