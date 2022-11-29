# Diversity Table S3 
source(here("1_regions.R"))
thresh_cons <- rast(here("Outputs/thresh_cons_05.tif"))
scen <- c("Food", "Feed", "Fuel" ,"All",  "Aspa")



#######
###     Area and Diversity by Country
to_map <- c("thresh_cons",scen)



for(j in 2:(length(to_map))){
  scen_do <- to_map[j]
  if(j==1){
    maps <- thresh_cons
    direc <- "Outputs/Potential_By_Region"
  } else {
    
    sw = to_map[j]
    # ass = index[1] %>% pull %>% as.character
    # sw = index[2] %>% pull %>% as.character
      
      if(str_detect("ASPA",toupper(sw)) ){message(sw)
        maps <- files_ras[files_ras %>% str_detect("WORLD")][4] %>% rast() %>% subset(c(2,1,3))
      } else {
        if(str_detect("FOOD",toupper(sw))){message(sw)
          maps <- files_ras[files_ras %>% str_detect("WORLD")][-4] %>% 
            lapply(function(x) rast(x)[[1]]) %>% rast()
        } else {
          if(str_detect("FEED",toupper(sw))){message(sw)
            maps <- files_ras[files_ras %>% str_detect("WORLD")][-4] %>% 
              lapply(function(x) rast(x)[[9]]) %>% rast()
          } else {message(sw)
            maps <- files_ras[files_ras %>% str_detect("WORLD")][-4] %>% 
              lapply(function(x) rast(x)[[17]]) %>% rast()
          }
        }
      }    
      
      
    direc <- paste0("Outputs/Potential_By_Region/",scen_do)
  }
  
  country_vec <- vector()
  diversity_vec <- vector()
  country_vec_mean <- vector()
  country_vec_min <- vector()
  country_vec_max <- vector()
  
  for (i in 1:length(eez %>% pull(Region) %>% unique)){
    agg_level = "Region"
    place <- (eez %>% pull(Region) %>% unique)[i]
    message(place)
    terr <- eez %>% filter(Region == place) %>% vect
    thresh_cons_terr <- maps %>% crop(ext(terr)) %>% 
      mask(terr, filename = here(direc,paste0(place,".tif")), overwrite = TRUE)
    if(j==1){
      country_vec[i] <- thresh_cons_terr %>% app("sum", na.rm = TRUE) %>% expanse(unit = "ha") 
    } else {
      country_vec_mean[i] <- thresh_cons_terr[[1]] %>% app("sum", na.rm = TRUE) %>% expanse(unit = "ha")
      country_vec_min[i] <- thresh_cons_terr[[2]] %>% app("sum", na.rm = TRUE) %>% expanse(unit = "ha")
      country_vec_max[i] <- thresh_cons_terr[[3]] %>% app("sum", na.rm = TRUE) %>% expanse(unit = "ha")
      country_vec <- bind_cols(country_vec_mean,country_vec_min,country_vec_max) %>% setNames(c("Mean","Min","Max"))
    }
    diversity_vec[i] <- minmax(thresh_cons_terr)[1,][-36] %>% 
      is.na() %>% 
      `!` %>% 
      sum(na.rm = TRUE)
  }
  
  #country_vec/1000000 %>% setNames(eez$Region %>% unique)
  if(j==1){
    country_potential <- bind_cols(eez$Region %>% unique, country_vec/1000000) %>% 
      bind_cols(diversity_vec) %>% 
      setNames(c("Region", "Suitable Area", "Species Richness")) %>% 
      arrange(-`Suitable Area`) %>% 
      mutate(Region = factor(Region, levels = .$Region)) %>% 
      left_join(by = "Region", eez_clean %>% group_by(Region) %>% summarise(AREA_KM2 = sum(AREA_KM2, na.rm = TRUE))) %>% 
      mutate(eez_area = 100*AREA_KM2/1000000) %>% ## CONVERT TO Mha
      dplyr::select(-AREA_KM2) %>% 
      setNames(c("Region","Suitable Area","Species Richness","EEZ Area")) %>% 
      pub_region() %>% 
      mutate(`Species Richness` = round(`Species Richness`,0),
             `EEZ Percent` = 100*round(`Suitable Area`/`EEZ Area`,2)) %>% 
      relocate(`Species Richness`, .after = `EEZ Percent`)
  } else{
    country_potential <- bind_cols(eez$Region %>% unique, country_vec/1000000) %>% 
      bind_cols(diversity_vec) %>% 
      setNames(c("Region", "Mean","Min","Max", "Species Richness")) %>% 
      arrange(-`Mean`) %>% 
      mutate(Region = factor(Region, levels = .$Region)) %>% 
      left_join(by = "Region", eez_clean %>% group_by(Region) %>% summarise(AREA_KM2 = sum(AREA_KM2, na.rm = TRUE))) %>% 
      mutate(eez_area = 100*AREA_KM2/1000000) %>% ## CONVERT TO Mha
      dplyr::select(-AREA_KM2) %>% 
      setNames(c("Region","Mean","Min","Max","Species Richness","EEZ Area")) %>% 
      pub_region() #%>% 
      # mutate(`Species Richness` = round(`Species Richness`,0),
      #        `EEZ Percent` = 100*round(`Suitable Area`/`EEZ Area`,2)) %>% 
      # relocate(`Species Richness`, .after = `EEZ Percent`) 
  }
  
  saveRDS(country_potential,here(paste0("Outputs/region_potential_",scen_do %>% toupper(),".RDS")))
}

readRDS(here(paste0("Outputs/region_potential_",scen_do %>% toupper(),".RDS"))) %>%
  write.csv(here("Outputs/region_potential.csv"),row.names = FALSE)
