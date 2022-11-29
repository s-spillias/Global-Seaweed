## This script builds SDMs and generates maps of mariculture potential. 
## Should be be run on HPC

#library(tidyverse)
#library(biomod2)
library(terra)
library(raster)
#library(MIAmaxent)
library(parallel)
library(rgdal)
library(stringr)
library(here)
library(dismo)
library(tidyverse)
library(arm)
library(rJava)
library(caret)
library(precrec)
#source(here("runLR.R"))
#source(here("testAUC.R"))

#assignInNamespace(".runLR",runLR,ns = "MIAmaxent")
#assignInNamespace("testAUC",testAUC,ns = "MIAmaxent")
options(java.parameters = "-Xmx8000m")
#source(here("Inputs/4a_functions.R"))
source(here("Inputs/maxent_param.R"))

#print(MIAmaxent:::.runLR)
## Import Oceanographic Rasters
oracle_stack <- stack(list.files(path = here("Inputs/oracle_stack"), pattern = ".tif",  all.files = TRUE, full.names = TRUE)) %>% 
  subset(subset = paste0(c("BO2_nitratemean_ss",
                                          #"BO2_phosphatemean_ss",
                                          #"BO_parmean",
                                          "BO_parmax",
                                          "BO2_tempmean_ss",
                                          #"BO2_tempmin_ss",
                                          #"BO2_tempmax_ss",
                                          "BO2_temprange_ss",
                                          "BO_ph",
                                          #"BO_damin",
                                          #"BO_damax",
                                          "BO_damean",
                                          "BO2_curvelmax_ss",
                                          "BO2_curvelmean_ss", 
                                          "BO2_salinitymean_ss"),"_lonlat"))
names(oracle_stack) <- str_remove(names(oracle_stack), "_lonlat")

## Grab Index number, for species
if(Sys.getenv("PBS_ARRAY_INDEX")=="") {
  index = 24
} else { 
index = as.double(Sys.getenv("PBS_ARRAY_INDEX")) 
}

### Import occurrence data
sp_avail <- read.csv(here("Inputs/occurrence_use.csv"))  %>% 
  group_by(species) %>% 
  mutate(n = n()) %>% 
  filter(n >=30) %>%  #### Remove species with fewer than 25 observations.
  ungroup() %>% 
  {occurrence_all <<- .} %>% 
  pull(species) %>%
  unique() %>% 
  sort()

message(sp_avail[index])

hit_rate <- vector()
sdm_cut <- list()
maxmod <- list()

for (counter in 1:5){
set.seed(counter)
  message("Iteration:")
message(counter)
source(here("Inputs/stratified.R"))
train_whole <- stratified(occurrence_all, group = "species", size = 0.7) 

train <- train_whole %>% 
  filter(species == sp_avail[index]) %>% 
  dplyr::select(x,y) %>% mutate(occ = 1)

test <- anti_join(occurrence_all, train_whole, by = NULL) %>% 
  filter(species == sp_avail[index])  %>% 
  dplyr::select(x,y) %>% mutate(occ = 1)

presences <- rbind(train,test) %>% as.data.frame()


### Sample Random Background Points
message("Background Points...")
bground <- randomPoints(oracle_stack[[1]], 10000, p = presences, 
                        ext=NULL, extf=1.1, excludep=TRUE, prob=FALSE, 
             cellnumbers=FALSE, tryf=3, warn=2, lonlatCorrection=TRUE) %>% 
  as_tibble() %>% 
  mutate(occ = 0)

### Combine Presence and Background Points
train_bground <- bind_rows(train, bground )

message("Extracting Environmental Data...")
## Extract environmental data at each occurrence (interpolate four closest cells)
covars <- terra::extract(oracle_stack,
                         train_bground %>% dplyr::select(-occ),
                         method = "bilinear", 
                        # buffer = 10000,
                       #  fun = "mean",
                       #  na.rm = TRUE, 
                        list = FALSE
                        ) %>%
  as_tibble() #%>% 
 # dplyr::select(-ID)


message("Tuning...")
nfolds <- ifelse(sum(train$occ) < 10, 2, 5)

#set.seed(32639)
# tune maxent parameters   !!!!!!!!!! LONG RUNNING !!!!!!!!!!
param_optim <- maxent_param(data = train_bground,
                            k = nfolds,
                            filepath = here(paste0("Outputs/maxent_data/",sp_avail[index])))

message("Fitting...")
# fit a maxent model with the tuned parameters
maxmod[[counter]] <- dismo::maxent(x = covars,
                        p = train_bground$occ,
                        removeDuplicates = TRUE,
                        #path = here(paste0("Outputs/maxent_files",index)),
                       args = param_optim
                        )

message("Predicting...")

max_pred <- dismo::predict(maxmod[[counter]], 
                           oracle_stack, 
                           filename = here(paste0("Outputs/maxent_data/",sp_avail[index],".tif")), 
                           overwrite = TRUE)

message("Threshing...")

eval <- evaluate(p = test %>% 
                   dplyr::select(x,y),
                 a = bground %>% 
                   dplyr::select(x,y),
                 model = maxmod[[counter]], 
                 x = oracle_stack) 


thresh <- threshold(eval)
sdm_cut[[counter]] <- overlay(max_pred, 
                              fun = function(x){
                                ifelse(x <= thresh$spec_sens,NA,1
)})



hits <- terra::extract(sdm_cut[[counter]], presences %>% dplyr::select(-occ)) 
#print(hits)
hit_rate[counter] <- sum(hits, na.rm = TRUE)/length(hits)
message(hit_rate[counter])
}

best_counter <- which(hit_rate == max(hit_rate, na.rm = TRUE))[1]
                       
message("Writing threshed raster...")

saveRDS(lst(hit_rate,maxmod[[best_counter]]),here("Outputs/maxent_data",paste0(sp_avail[index],"_max.RDS")))

writeRaster(sdm_cut[[best_counter]],here(paste0("Outputs/max_threshed/",sp_avail[index])),
            format = "GTiff",
            overwrite = TRUE)

###

