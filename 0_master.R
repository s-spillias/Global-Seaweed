#### Master File

library(here)
# require(devtools)
# figure_folder = here("figures") 
# devtools::install_github("dill/beyonce") ## Custom Color Palettes
# library(beyonce)

#Load Data ~ 10 minutes
source(here("1_regions.R"))
source(here("2_occurrence_all.R"))
source(here("3_sp_data.R"))
source(here("3a_replacements.R"))

rm(list=ls())
 
 #Build SDMs
 source(here("4b_SDMs.R")) # ~ 90 minutes
 
 rm(list=ls())
 
 #Load and Process Constraint Maps # ~ 60 minutes (to run fully) ~ 5 minutes (to load intermediate files as set up)
 source(here("4c_constraints.R"))
 source(here("4d_SDM_trim.R"))
 rm(list=ls())
 
 
 # Load and Clean GLOBIOM result dataset from GAMS # a few minutes
 source(here("6_globiom.R"))
 
 # Spatial Prioritization # Takes ~ 45 minutes
 source(here("5_optim_run.R"))
 source(here("5b_optim_clean.R"))
 
 # Generate Figures and Outputs -> Best to Run Chunks Individually
 #source(here("7_final.RMD"))
