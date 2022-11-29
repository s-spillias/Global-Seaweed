### Set-Up Parameters to run Spatial Prioritization
## Currently set-up for minimal run in Japan on for Food-10 & Feed-10 scenarios.

library(here)
library(tidyverse)
library(terra)
library(rgdal)

# Run Settings

native = TRUE
native_name = ifelse(native, "native","nonnative" ) # use native maps constrained by native distribution

optim_script = here("5_optim_add.R")

reg <- readRDS(here("Inputs/eez_ordered.RDS")) %>% pull(Region) %>% c("WORLD","WORLD","WORLD") %>% rev


### Scenarios to Run: If 'is_all' is TRUE, will run all combinations of these scenarios and combine

if(Sys.getenv("PBS_ARRAY_INDEX")=="") { # Run Locally 
  
  index = 16
  
} else { # Run on HPC
  
  index = as.double(Sys.getenv("PBS_ARRAY_INDEX")) 
}
  
  if(index > 100){ # Run Asparagopsis Scenario Separately
    aspa = "aspa"
    is_all = ""
    index = index - 100
  }else{ # Run Food, Feed, Fuel scenarios together
    aspa = ""
    is_all = "_ALL"


}

place <- reg[index]

if(aspa == "aspa"){
  to_run = "SW_ASPA_005_COMB"
}else{
to_run = c("SW_FOOD_10"
           ,"SW_FEED_10"
           ,"SW_FUEL_50"
)}


## Seaweed Production Assumptions
if(place == "WORLD" & aspa != "aspa"){ # Run world assumption scenarios in parallel as different HPC runs due to long run-time
  assumption_levels <- c("mean","min"
                         ,"max")[index]
}else{ # run all other scenarios consecutively
assumption_levels <- c("mean","min"
                       ,"max"
)
}


if(is_all == "_ALL"){ ### Build permutations
source(here("Inputs/permutations.R"))
to_run_permn <- matrix(to_run[permutations(length(to_run))],ncol=length(to_run))  %>% 
  as_tibble() %>% 
  merge(assumption_levels) %>% 
  setNames(c(paste0("Scen",1:length(to_run)),"Ass_Level"))
} else {
  to_run_permn = expand.grid(to_run,assumption_levels) %>% 
    setNames(c("Scen","Ass_Level"))
}

 scen_map <- readRDS(here("Inputs/scen_map.RDS")) %>% 
   filter(SW_Scen %in% to_run)

scen <- scen_map$SW_Scen %>% unique()


result_all <- list()
result_scen <- list()
rasters <- list()
#########################################
## Run optimization for all permutations
##########################################
for( j in 1:nrow(to_run_permn)){
to_run <- to_run_permn[j,] %>% as_tibble() %>% mutate(across(everything(), as.character))

run_name <- paste(to_run[1:ncol(to_run)], collapse = "_")

message(to_run)

out <- list()
r_out <- list()

for (z in to_run[1:(length(to_run)-1)]){
  print = FALSE
  sw_scen = z
  level = to_run[length(to_run)] %>% pull
  spatial_extent = place
  prod_uniform = FALSE
  minimum_suitability = 0.5
  # target_area = 0.01
  source(optim_script)
  out[[z]] <- supply_df %>% mutate(Demand = as.numeric(Demand)*case_when(Units == "kcal_Energy" ~ 0.004184,
                                                             Units == "MJ_Fuel_Energy" ~ 1,
                                                             TRUE ~ 1),
                                   Supply = Supply*case_when(Units == "kcal_Energy" ~ 0.004184,
                                                             Units == "MJ_Fuel_Energy" ~ 1,
                                                             TRUE ~ 1),
                                   Max_Supply = Max_Supply*case_when(Units == "kcal_Energy" ~ 0.004184,
                                                             Units == "MJ_Fuel_Energy" ~ 1,
                                                             TRUE ~ 1),
                                   Units = "MJ")
  r_out[[z]] <- sel_com 
  
if(is_all != "_ALL"){rm(sel_com)}

}

if(is_all == "_ALL"){
#if (j %in% c(1,3,5)) {rasters[[(j+1)/2]] <- r_out %>% rast() %>% setNames(paste0(to_run[1:(length(to_run)-1)],"-",1:(length(to_run)-1)))}
rasters[[j]] <- r_out %>% rast() %>% c(cover((r_out %>% rast())[[1]],(r_out %>% rast())[[2]]) %>% cover((r_out %>% rast())[[3]]) %>%  
                                         setNames(run_name))#paste0(to_run[1:(length(to_run)-1)],"-",1:(length(to_run)-1)))
} else {
rasters[[j]] <- r_out %>% rast()
}

message("Removing Used Area")

#######################################
### End Optimization
#########################################

result_scen[[j]] <-  out %>% lapply(function(x) bind_rows(x)) %>% bind_rows() %>%
  group_by(Year, Region, Per_Cell, `Assumption Level`, Units, sp_unit) %>% 
  mutate(across(.cols = contains(" "), .fns = as.numeric ))

 suitable_area = result_scen[[j]] %>% pull(Suitable_Area) %>% max

 if( is_all == "_ALL") { 
  result_all[[j]] <-  result_scen[[j]]  %>% 
    summarise(across(.cols = where(is.numeric), .fns = sum)) %>% 
     mutate(Suitable_Area = suitable_area,
            Suitable_Area_Used = 100*hectares/Suitable_Area,
            SW_Scen = "SW_ALL_HIGH")# %>% 
   # filter(!is.na(Demand_Met))
      }

rm(list=setdiff(ls(), c("out","native",
                        "native_name",
                        "to_run","run_name",
                        "r_out","place",
                        "to_run_permn","optim_script",
                        "result_scen","result_all","rasters","is_all",
                        "index",
                        "aspa")))
}

out_df <- result_scen %>% lapply(function(x) bind_rows(x)) %>% bind_rows() %>% 
  {if(is_all == "_ALL"){
    bind_rows(.,
    result_all %>% 
       lapply('[',1,) %>% bind_rows )
    }else{.}}
  


## SAVING 

if(Sys.getenv("PBS_ARRAY_INDEX")!="") {
  message("saving...")
  figure_folder = here("Outputs") 
  
 run_name = ifelse(is_all=="_ALL","_ALL",run_name)
 # if(place != "WORLD"){run_name = ""} else {run_name = to_run %>% paste(collapse="_")}
  
  #save_folder <- paste0("Outputs/Regional_Production/",native_name,"/")
saveRDS(out_df,
       paste0(figure_folder,"/",place,run_name,index,aspa,".RDS")
        )


rasters %>% rast() %>% 
  writeRaster(filename = paste0(figure_folder,"/",place,run_name,index,aspa,".tif"), overwrite = TRUE)
}
## Note NA outputs indicate not enough suitable sea area to meet the scenario demand.
