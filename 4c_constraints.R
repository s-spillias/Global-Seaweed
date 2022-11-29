## Import and clean constraint layers.

clean_mask <- rast(here("Inputs/oracle_stack/BO_parmax_lonlat.tif")) %>%
  app(fun = function(x) {ifelse(x>0,1,NA)})

###  waves
url_grid <-
  "https://data-cbr.csiro.au/thredds/dodsC/catch_all/CMAR_CAWCR-Wave_archive/CAWCR_Wave_Hindcast_aggregate/gridded/ww3.glob_24m.202105.nc" # note: netcdf4 does not work on windows R

url_time <-
  "https://data-cbr.csiro.au/thredds/catalog/catch_all/CMAR_CAWCR-Wave_archive/CAWCR_Wave_Hindcast_aggregate/gridded/catalog.html?dataset=allDatasetScan/CMAR_CAWCR-Wave_archive/CAWCR_Wave_Hindcast_aggregate/gridded/ww3.glob_24m.202105.nc"

download.file(url_time, "waves.nc", method = "auto",
              quiet = FALSE, mode="wb", cacheOK = TRUE)

#waves.nc <- nc_open(here("Inputs/Waves/Hs_glob_FIO_FIO-ESM-2-0_ssp126_r1i1p1f1_mon_201501-210012.nc"))
waves <- rast(here("Inputs/Waves/adaptor.mars.internal-1624429308.1907678-19845-29-ff25d27b-b8c7-4763-b5c1-5e9d610f1d65.nc"), varname = "swh")
e <- c(-0.125,359.875/2,0,90.125)
waves <- crop(waves,e)
#extent(waves) <- extent(clean_mask)
extent(waves) <- c(0,360,-90,90)
waves <- terra::rotate(waves)
waves_mean <- app(waves, fun = mean, na.rm = TRUE)
waves_mean <- resample(waves_mean, clean_mask)
names(waves_mean) <- "mean_mean_wave_height"
writeRaster(waves_mean,here("Inputs/oracle_stack/waves_mean.tif"), overwrite = TRUE)

wind <- rast(here("Inputs/Waves/adaptor.mars.internal-1624429308.1907678-19845-29-ff25d27b-b8c7-4763-b5c1-5e9d610f1d65.nc"), varname = "u10")
#e <- c(-0.125,359.875/2,0,90.125)
#wind <- crop(wind,e)
#extent(wind) <- extent(clean_mask)
extent(wind) <- c(0,360,-90,90)
wind <- terra::rotate(wind)
wind_mean <- app(wind, fun = mean, na.rm = TRUE)
wind_mean <- resample(wind_mean, clean_mask)

names(wind_max) <- "max_mean_wind"
writeRaster(wind_mean,here("Inputs/oracle_stack/wind_mean.tif"), overwrite = TRUE)

#extent(waves) <- extent(clean_mask)

### BIODIVERSITY (DEPRECATED ATM)

# 
# biodiversity <- raster(here("Constraints/GeoTIFFs_dryad/GeoTIFFs_dryad/Fig4A_global_priorities.tif")) %>% 
#   raster::projectRaster(crs = crs(clean_mask), res = res(clean_mask), method = "bilinear", over = TRUE) %>% 
#   calc(fun = function(x) {ifelse(ifelse(is.na(x),0,x)>0.01,x,NA)}) %>% 
#   rast() %>% 
#   terra::resample(clean_mask, method = "bilinear",
#                   filename = here("Inputs/biodiversity.tif"),
#                   overwrite = TRUE)
# #  terra::writeRaster(here("Inputs/biodiversity.tif"), overwrite = TRUE)

#### PORT DISTANCE

port_dist <- rast(here("Constraints/distance-from-port-v1.tiff")) %>% 
  terra::resample(clean_mask, method = "bilinear",
                  filename = here("Inputs/distance-from-port.tif"),
                  overwrite = TRUE)

### SHIPPING

shipping <- rast(here("Constraints/shipdensity_global.tif")) %>% 
  resample(clean_mask)
shipping[shipping>minmax(shipping)[2]] <- NA

 shipping <- log(shipping - shipping %>% focal(w = 31, na.rm = TRUE, fun = "sum"))
shipping[shipping>18] <- NA 
shipping <- log(shipping)
shipping[is.na(shipping)] <- 0

shipping %>% writeRaster(filename = here("Inputs/shipping.tif"), overwrite = TRUE)

## EEZ RASTER
source(here("Inputs/1_regions.R"))

threshed <- rast(list.files(path = here("Outputs/max_threshed/"), pattern = ".tif", full.names = TRUE))

eez_rast <- eez %>% vect %>% rasterize(threshed, filename = here("Inputs/eez_rast.tif"), overwrite = TRUE)


## Sea Ice

ice_S <- rast(list.files(path = here("Inputs/sea_ice"), pattern = "S_", full.names = TRUE)) %>% 
  clamp(upper = 1, value = FALSE) %>% 
  app(fun = function(x) sum(x,na.rm = TRUE)) %>% 
  project(clean_mask)

ice_N <- rast(list.files(path = here("Inputs/sea_ice"), pattern = "N_", full.names = TRUE)) %>% 
  clamp(upper = 1, value = FALSE) %>% 
  app(fun = function(x) sum(x,na.rm = TRUE)) %>% 
  project(clean_mask)

ice_constraint <- merge(ice_S,ice_N)  %>% 
  terra::resample(clean_mask, method = "bilinear") %>% 
  stretch(minv = 0, maxv = 1) %>% 
  subst(NA,0) %>% 
  `-`(1) %>% `*`(-1) %>% 
  writeRaster(filename = here("Inputs/ice_constraint.tif"),
              overwrite = TRUE)
  

##### Combine Constraint Layers

waves_mean <- rast(here("Inputs/Wind_Waves/waves_mean.tif")) %>% `*`(-1) %>% stretch(minv = 0, maxv = 1)
#biodiversity <- rast(here("Inputs/biodiversity.tif")) 
shipping <- rast(here("Inputs/shipping.tif"))  %>% `*`(-1) %>% stretch(minv = 0, maxv = 1)
MPAs <- rast(here("Inputs/MPA_mask.tif")) %>% subst(NA, 0) %>% stretch(minv = 0, maxv = 1)
depth_200 <- rast(here("Inputs/depth_200.tif")) %>% stretch(minv = 0, maxv = 1) 
port_dist <- rast(here("Inputs/distance-from-port.tif")) %>% `*`(-1) %>% stretch(minv = 0, maxv = 1)
#fishing <- rast("Inputs/fishing.tif")  %>% `*`(-1) %>% stretch(minv = 0, maxv = 1)
ice_constraint <- rast(here("Inputs/ice_constraint.tif")) 

constraints <- c(depth_200,
                 port_dist, 
                 shipping,
                 waves_mean,
                 ice_constraint,
                 # fishing,
                 MPAs) %>% setNames(c("Depth", 
                                      "Port_Distance",
                                      "Shipping",
                                      "Wave_Energy",
                                      "Ice",
                                      # "Fishing",
                                      "MPAs")) %>% 
  writeRaster(filename = here("Inputs/constraints_normalized.tif"), overwrite = TRUE)

cons_ras <- app(constraints, fun = "prod", filename = here("Outputs/cons_ras.tif"), overwrite = TRUE) 

