## Import Species Substitution and Production Data

nutr <-  read.csv(file = here(paste0("Inputs/nutr.csv")))
fuel <-  read.csv(file = here(paste0("Inputs/fuel.csv")))
prod <-  read.csv(file = here(paste0("Inputs/prod.csv")))
use  <-  read.csv(file = here(paste0("Inputs/use.csv")))
watr <-  read.csv(file = here(paste0("Inputs/watr.csv")))

sp_avail <- readRDS(here("Inputs/sp_avail.RDS"))

util_data <- nutr %>% full_join(fuel, by = "Taxa") %>% 
  full_join(prod, by = "Taxa") %>% 
  full_join(use, by = "Taxa") %>% 
  full_join(watr, by = "Taxa") %>% 
  as_tibble() 

names(util_data) <- names(util_data) %>% str_replace("\\.","_")
  
saveRDS(util_data, here("Inputs/util_data.RDS"))

