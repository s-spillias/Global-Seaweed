# Optimization


library(here)

gc()
if(exists("last_run")){if (last_run != spatial_extent) {rm(util_rast)}}
sf::sf_use_s2(FALSE)
if(!exists("initial")) {
  message(paste0("Initializing: ", spatial_extent," | ", sw_scen," | ", level))
  
  
  per_cell = 0.5 ## proportion of cell given over to production
 # Weights of constraint priority
  bio = 1
  ship = 1
  depth = 1
  MPA = 1
  ports = 1
  wave = 1
  year = 2050
  native = ifelse(!exists("native"), TRUE, native) ### 2030,2
  
  iter <- 1 #100*(1 - minimum_suitability))#ifelse(place != "WORLD",100*(1 - minimum_suitability),1)
  initial = TRUE #ifelse(place != "WORLD",TRUE,FALSE)
  final = FALSE
  ### If previous iteration doesn't exist -> Load Data
  if(!exists("GLOBIOM_regions")) {source(here("1_regions.R"))}
  use = str_split(sw_scen,"_")[[1]][[2]]
  
  #Load Substitution Data
  replacements <- readRDS(here("Inputs/replacements.RDS"))
  
  
  broad_use <- case_when(use == "FOOD" ~ "FOOD",
                         use %in% c("FEED","ASPA") ~ "FEED",
                         use %in% c("ETOL","DIES","FUEL") ~ "FUEL")

  units_name <- case_when(broad_use == "FOOD" ~ "kcal_Energy",
                          broad_use == "FEED" ~ "kcal_Energy",
                          broad_use == "FUEL" ~ "MJ_Fuel_Energy")
  # }
  if(broad_use == "FUEL") {fuel_type = case_when(use == "ETOL" ~ "Ethanol",
                                                 use == "DIES" ~ "Biooil",
                                                 use == "FUEL" ~ c("Ethanol","Biooil"))}
  sub_param <- (str_split(sub("_",";",units_name),";") %>% unlist)[2]
  output = paste0(sub_param,"_",level)
  measures <-  case_when(broad_use == "FOOD" ~ c("Taxa","FOOD","Energy_per_ha","Protein_per_ha"),
                         broad_use == "FEED" ~ c("Taxa","FEED","Energy_per_ha","Protein_per_ha"),
                         broad_use == "FUEL" ~ c("Taxa","FUEL","Fuel_Type","Fuel_Energy_per_ha"),
                         TRUE ~ "")
  util_data <- readRDS(here("Inputs/util_data.RDS")) %>% 
    mutate(FOOD = ifelse(FOOD == 1, "FOOD",""),
           FEED = ifelse(FEED == 1, "FEED",""),
           FUEL = ifelse(FUEL == 1, "FUEL","")) %>% 
    mutate(Production_mean = (Production_min + Production_max)/2,
      Production_per_ha = per_cell*(if(prod_uniform){20}else{!!as.symbol(paste0("Production_",level))}), 
           Energy = case_when(level == "min" ~ !!as.symbol(paste0("Energy","_min")),  
                              level == "max" ~ !!as.symbol(paste0("Energy","_max")),  
                              level == "mean"  ~ (!!as.symbol(paste0("Energy","_min")) + !!as.symbol(paste0("Energy","_max")))/2),
           Protein = case_when(level == "min" ~ !!as.symbol(paste0("Protein","_min")),  
                               level == "max" ~ !!as.symbol(paste0("Protein","_max")),  
                               level == "mean"  ~ (!!as.symbol(paste0("Protein","_min")) + !!as.symbol(paste0("Protein","_max")))/2),
           Energy_per_ha = 1000*Energy*Production_per_ha,
           Protein_per_ha = 1000*Protein*Production_per_ha,
           Fuel_Energy_per_ha = 1000*Fuel_Energy*Production_per_ha) %>% # 1000 converts tonnes to kg,
    {if (broad_use == "FUEL") mutate(.,Fuel_Energy_per_ha = ifelse(Fuel_Type %in% fuel_type, Fuel_Energy_per_ha,NA)) else .} %>% 
    dplyr::select(all_of(measures)) %>% 
    distinct() %>% 
    arrange(!!as.symbol(paste0(sub_param, "_per_ha")))
  
  
  

    
    message("Loading species data...")
  
   # if ((minimum_suitability == list.files(here("Outputs"), pattern = "thresh_cons") %>% str_remove("thresh_cons") %>% str_remove(".tif") %>% str_remove("_") %>% as.numeric()) %>% sum(na.rm = TRUE) == 1) {
      thresh_cons <- rast(here("Outputs",paste0("thresh_cons_05.tif"))) 
      #} else {
      #  thresh_cons <- rast(here("Outputs/thresh_cons.tif")) %>% app(fun = function(x){ifelse(x < minimum_suitability,NA,x)},
    #                                                                 filename = here("Outputs",paste0("thresh_cons_",minimum_suitability,".tif")), overwrite = TRUE)
     # }
    
    
    sp_avail <- util_data %>% filter(Taxa %in% names(thresh_cons)) %>%  pull(Taxa) %>% unique() # captures order of productivity, from most to least 
    
    thresh_cons <- thresh_cons %>% 
      subset(sp_avail)
    
    sp_use <- util_data %>% filter(!!as.symbol(broad_use)!="") %>% {if (broad_use == "FUEL") filter(.,Fuel_Type %in% fuel_type, !is.na(Fuel_Energy_per_ha)) else .} %>% pull(Taxa) %>% unique
    
    thresh_cons <- thresh_cons %>% subset(sp_avail) ## Reorder
    
    
    ####### Cropping to Region     
    message(paste0("Cropping to the extent of ",spatial_extent,"..."))
    
    
    spatial_extent_type <- case_when(spatial_extent %in% eez$Regional_Agg ~ "Regional_Agg",
                                     spatial_extent %in% eez$Region ~ "Region",
                                     spatial_extent %in% eez$GLOBIOM_Country ~ "GLOBIOM_Country",
                                     spatial_extent %in% eez$Country_Name ~ "Country_Name",
                                     spatial_extent %in% eez$ISO3 ~ "ISO3",
                                     spatial_extent %in% eez$TERRITORY1 ~ "TERRITORY1",
                                     spatial_extent == "WORLD" ~ "WORLD")
    
    ## Weights Vector
    #w = c(ports,bio,wave, ship, MPA,depth) 
    
    ## Crop to extent 
    
    ## If REGION or Country
    
    territory.shp <- eez %>% filter(!is.na(TERRITORY1)) %>% 
      {if(spatial_extent_type != "WORLD"){filter(.,!!as.symbol(spatial_extent_type) == spatial_extent)} else {.}} %>% 
      {. ->> regional_agg} %>% 
      as("Spatial") %>% vect()
    
    
 util_rast <-  thresh_cons %>% crop(ext(territory.shp)) %>% 
   mask(territory.shp) %>% 
  {if(place != "WORLD" & sum(ext(.)[1:2] %>% unname() == c(-180,180))==2){
    terra::rotate(.) %>% trim()
    } else {.}} %>% 
   {if (exists("used_area")) {
     message("Eliminating Used Area...")
     mask(.,used_area, inverse = TRUE)
          } else {.}} %>%
   round(2) 
    
    

  #####
  
  region <- if(spatial_extent_type != "WORLD") {regional_agg %>% pull(Region) %>% unique()
  } else {eez$Region %>% unique}
  last_run = spatial_extent

  
  replacements_trim <- replacements %>% 
    filter(Source == "Terrestrial") %>% 
    mutate(Nearest_Neighbor = lapply(Nearest_Neighbor, function(df) {df %>% as.data.frame() %>% filter(NN %in% sp_avail) %>% slice_head(n = 1) %>% 
        rename_with(~c("Best_Substitute", "Energy.sub", "Protein.sub", "Fat.sub"))}),
        combos = lapply(combos, function(df) {if (is.null(df)) {NA} else {df %>% as.data.frame() %>% filter(Item_1 %in% sp_avail)} })) %>% 
    unnest(everything())# %>% 
  #  dplyr::select(-combos)
  
  
  ##Set  DEMAND FROM SCENARIO
  demand <- readRDS(here("Inputs/shortfalls.RDS")) %>% ##NEED TO CHECK FUEL UNITS
    mutate(SW_Scen = toupper(SW_Scen)) %>% 
    filter(VAR_ID == broad_use,
           SW_Scen == sw_scen,
           YEAR == year,
           REGION_AG %in% region) %>%
    {if (broad_use == "FUEL") {pull(.,Fuel_Energy_Shortfall)} else {pull(.,!!as.symbol(paste0(sub_param,"_Shortfall")))} } %>% ### 1e9 converts PJ to MJ
    `*`(-1) %>% ##### THIS IS WHERE TO DECIDE HOW TO MEET DEMAND
    sum(na.rm = TRUE)
  if (demand <=0) {demand = 0}
  
  message(paste0("Under ",sw_scen, ", demand for ",use," in ",spatial_extent, " in ", year," is ",formatC(demand, format = "e", digits = 2)," ",units_name))
  
  meta <- c(year,sw_scen,spatial_extent,per_cell,level, units_name, demand, "hectares") %>% 
    setNames(c("Year", "SW_Scen","Region","Per_Cell","Assumption Level", "Units","Demand","sp_unit" )) %>% 
    as.list() %>% 
    as_tibble()
  
  
}   else  { 
 # message("Iterating...")
  iter <- iter + 1 
  initial = FALSE
  
}



### End Load Data 
#################################################################################

## Identify Locations and Iterate
if(initial == TRUE) {
  message("Selecting suitable locations...")
  }
if(final){
  util_data <- util_data %>% mutate(across(where(is.numeric), .fns = function(x) x*overshoot_correction))
}

#### PRIORITIZATION #######################


sel <- (util_rast == ((100-iter)/100) ) %>% subst(0,NA) 


if (initial) {sel_com <- (util_rast[[1]] > 1) %>% subst(0,NA) 
message("Initializing sel_com")}

if(initial == TRUE | place == "WORLD") {message("Choosing optimal species distributions...")} 
if (place == "WORLD" | sum(!is.na(minmax(sel))) > 0 & 
    nrow(util_data %>% {if (broad_use == "FUEL") filter(.,Fuel_Type %in% fuel_type, !is.na(Fuel_Energy_per_ha)) else .} %>% 
         filter(Taxa %in% names(sel)[unname(is.na(minmax(sel))[1,])])) > 0) {

  ## Select only appropriate species
  sp_map <- names(which(!is.na(minmax(sel))[1,], useNames = TRUE))
  
  sp_avail_trim <- sp_avail %>% 
    {if (use == "ASPA") intersect(.,c("Asparagopsis taxiformis", "Asparagopsis armata")) else .} %>% 
    intersect(sp_use) %>% 

    intersect(sp_map)  
  

  
  sel <- app(sel, fun = function(x) {
        ifelse((names(x) %in% sp_avail_trim)==0,NA,x*1:nlyr(sel))
    } ) %>% setNames(names(sel))
  
  
  sel_com_new <- app(sel, fun = "max", na.rm = TRUE)
  
  sel_com <- sel_com %>% cover(sel_com_new, values = NA) 

} 

for(i in 1:nlyr(sel)) {

  sel[[i]] <-  (sel_com == i) 

  names(sel[[i]]) <- names(util_rast)[[i]]
} 
#message("Classifying zeroes...")

sel <- classify(sel, cbind(0,NA), right = TRUE)


#### END PRIORITIZATION #######################
message("Calculating Area....")

area <- vector()
for (i in 1:nlyr(sel)){
  if (place == "WORLD") {message(i)}
  area[i] <- expanse(sel[[i]], unit = "ha") %>% 
    `*`(per_cell) }

area <- suppressMessages(area %>% 
                           bind_cols(Taxa = names(sel)) %>% 
                           rename("hectares" = "...1") %>% 
                           rownames_to_column("remove") %>% 
                           dplyr::select(hectares,Taxa))

eez_size <-  regional_agg %>% dplyr::select(-geom) %>% as_tibble() %>% 
  summarize(EEZ_Area = 100*sum(AREA_KM2)) %>% ## convert km2 to hectares
  pull(EEZ_Area)  

suitable_area <- util_rast %>% app("sum",na.rm = TRUE) %>% expanse(unit = "ha")

supply_df <- area %>%  
  left_join(util_data %>% {if (broad_use == "FUEL") filter(.,Fuel_Type %in% fuel_type) else .}, by = "Taxa") %>% 
  suppressMessages() %>% 
  {if(broad_use == "FOOD" | broad_use == "FEED") {
    mutate(.,Protein_Production = Protein_per_ha*hectares,
           Energy_Production = Energy_per_ha*hectares,
           EEZ_Proportion = hectares/eez_size) %>% 
      dplyr::select(Taxa, Protein_Production, Energy_Production, hectares, EEZ_Proportion)
  } else {
    mutate(.,Fuel_Energy_Production = Fuel_Energy_per_ha*hectares,
           EEZ_Proportion = hectares/eez_size) %>% 
      dplyr::select(Taxa, Fuel_Energy_Production, hectares, EEZ_Proportion) %>% 
      filter(!is.na(hectares)) %>% 
      group_by(Taxa) %>% 
      summarise(across(everything(),function(x) sum(x,na.rm=TRUE)))
  } 
    
  }

supply = supply_df %>% pull(!!as.symbol(paste0(sub_param,"_Production"))) %>% sum(na.rm = TRUE) 


## Plot

################################################################# 
if(print) {
  library(beyonce)
  p = ggplot() +
    geom_sf(data = territory.shp %>% st_as_sf(), size = .1, color = "grey75", fill = "grey75") +
    geom_tile(data = as.data.frame(sel_com, xy = TRUE), aes(x = x, y = y, fill = as.factor(!!as.symbol(names(as.data.frame(sel_com, xy = TRUE))[3])))) +
    coord_sf() +
 
    xlim(ext(sel_com)[1:2]) +
    ylim(ext(sel_com)[3:4]) +
    theme_classic() +
    scale_fill_manual(labels= names(sel),
                      na.value = 'black',
                      breaks = 1:nlyr(sel),
                      values = c(c(beyonce_palette(10)),c(beyonce_palette(48)),c(beyonce_palette(29)),c(beyonce_palette(13)),c(beyonce_palette(22)),c(beyonce_palette(40)))) +
    theme(legend.position="right") +
    labs(fill = "Taxa") +
    ggtitle(paste0("Iter == ",iter))
}
hectares = supply_df %>% pull(hectares) %>% sum(na.rm = TRUE) 
eez_prop = supply_df %>% pull(EEZ_Proportion) %>% sum(na.rm = TRUE) 
if(initial){    
  max_supply = supply
  max_hectares = hectares
  max_eez_prop = eez_prop
}

### Reshape for output 
supply_df <- meta  %>% bind_cols("Supply" = supply) %>% 
  c(
    supply_df %>% arrange(Taxa) %>% tidyr::pivot_longer(-1) %>%
      tidyr::pivot_wider(names_from = 1, values_from = value) %>% filter(name == "hectares") %>% 
      slice(1) %>%
      unlist(use.names = TRUE)
  ) %>%
  as_tibble()

  if(supply<demand){
    
    if(iter < 100*(1-minimum_suitability)) {
      if (iter %% 1 == 0) {
        message(paste0("Iteration = ",iter,"; Production = ",formatC(supply, format = "e", digits = 2), " ",units_name,"; ", 100*round(supply/demand,2), " %"))
        if(print){p %>% print();Sys.sleep(3)}
      }
      
      source(optim_script) 
    } else {
      rm(initial)
      Demand_Met = FALSE
      message(paste0("Supply is ",formatC(supply, format = "e", digits = 2),"; there is not enough viable space to meet the chosen demand...choose something lower"))
      levels(sel_com) <- c("index",names(sel))
      supply_df <- supply_df %>% bind_cols("Demand_Met" = Demand_Met,
                                           "Native_Restriction" = native,
                                           "Hectares" = hectares, 
                                           "Suitable_Area" = suitable_area,
                                           "Suitable_Area_Used" = hectares/suitable_area,
                                           "Max_Supply" = max_supply, 
                                           "Max_EEZ_prop" = max_eez_prop, 
                                           "Max_Hectares" = max_hectares)

      if (exists("used_area")) {message("Adding to Used Area")
        used_area <<- sel_com %>% cover(used_area)
      } else {
        if(is_all == "_ALL"){message("Initializing Used Area")
        used_area <<- sel_com
        }
      }
      new_scen = TRUE
    } 
    
  } else { ## Supply is Greater than Demand

      
      s_thresh = (100-iter)/100
      Demand_Met = TRUE
      message(paste0(formatC(supply, format = "e", digits = 2)," ", units_name," could be produced in ", place," at a suitability threshold of ", s_thresh, ".   
    This level of production would require ",round(hectares,2), " hectares, which is ",round(100*hectares/suitable_area,2),"% of the available suitable area, and ",round(eez_prop,4),"% of the EEZ."))
      if(print) { print(p) }
      rm(initial)
      levels(sel_com) <- c("index",names(sel))
      supply_df <- supply_df %>% bind_cols("Demand_Met" = Demand_Met,
                                           "Native_Restriction" = native, 
                                           "Hectares" = hectares, 
                                           "Suitable_Area" = suitable_area,
                                           "Suitable_Area_Used" = hectares/suitable_area,
                                           "Max_Supply" = max_supply,
                                           "Max_EEZ_prop" = max_eez_prop, 
                                           "Max_Hectares" = max_hectares)
       if (exists("used_area")) { message("Adding to Used Area")
        used_area <<- sel_com %>% cover(used_area)
      } else {
        if(is_all == "_ALL"){ message("Initializing Used Area")
        used_area <<- sel_com
        }
      }
     new_scen = TRUE
   
  }


