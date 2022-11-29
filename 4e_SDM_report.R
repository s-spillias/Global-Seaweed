### SDM REPORTING and Supplementary Material


### Load Layers

threshed <- rast(list.files(path = here("Outputs/max_threshed/"), pattern = ".tif", full.names = TRUE)) 

thresh_eez <- rast( here("Outputs/thresh_eez.tif"))

thresh_nat_files <- list.files(path = here("Outputs/max_threshed_nat/"), pattern = ".tif", full.names = TRUE) 

thresh_nat <- rast(thresh_nat_files[!str_detect(thresh_nat_files,"All_Sp")])
 
thresh_cons <- rast(here("Outputs/thresh_cons_05.tif"))

occurrence_all <- read.csv(here("Inputs/occurrence_use.csv"), row.names = NULL) 

sp_avail = names(threshed)
#### Calculate new summary layer
gc()

### Calculate Area of Fully Constrained Maps
cons_area_file <- list.files(path = here("Outputs"), pattern = "cons_area.RDS")
if(length(cons_area_file != 0)){
  cons_area <- readRDS(here("Outputs/cons_area.RDS"))
} else {
  cons_area_ <- vector()
  for (i in 1:nlyr(thresh_cons)){
    message(paste0(i,names(thresh_cons)[i]))
    cons_area_[i] <- expanse(thresh_cons[[i]], unit = "ha")
  }
  cons_area <- bind_cols(names(thresh_cons), cons_area_) %>% setNames(c("Species", "cons_area")) 
  saveRDS(cons_area, here("Outputs/cons_area.RDS"))
}


### Table of Occurrence Counts
occ_count <- occurrence_all %>% as_tibble() %>% 
  filter(species %in% sp_avail) %>% 
  group_by(species) %>% 
  summarise(Count = n())# %>% 
# add_row(species = "All_Sp", Count = sum(.$Count)) 

#global <- tibble("Global", thresh_comb %>% expanse(unit = "ha"), thresh_comb_eez %>% expanse(unit = "ha"), threshed_comb_nat, viable, sum(occ_count$Count))

max_models <- list.files(here("Outputs/maxent_data"), pattern = "_max.RDS", full.name = TRUE) %>% 
  {sp_models <<- .} %>% 
  lapply(readRDS)

sp_models <- str_remove(sp_models,here("Outputs//")) %>% 
  str_remove("_max.RDS")

hit_rates_all <- max_models %>% sapply("[[",1)

hit_rate_sd <- sp_models %>% bind_cols(hit_rates_all %>% apply(function(x) sd(x), MARGIN = 2) %>% unlist) %>% 
  setNames(c("Species", "Prediction Accuracy Standard Deviation")) %>% 
  filter(Species %in% sp_avail)

max_models_flat <- max_models %>% sapply("[[",2)

AUCs <- lapply(max_models_flat, function(x) x@results %>% 
                 as.data.frame %>% 
                 rownames_to_column() %>% 
                 filter(str_detect(rowname,"Training.AUC") )) %>% 
  setNames(sp_models) %>% 
  bind_rows(.id = "Species") %>% 
  pivot_wider(id_cols = Species, names_from = rowname, values_from = V1) %>% 
  mutate(Species = str_remove(Species, "maxent_data/")) %>% 
  filter(Species %in% sp_avail) %>% 
  mutate(Training.AUC = round(Training.AUC,2))

#write.csv(AUCs, "C:/Users/s4493740/Dropbox/_PhD/Ch4_5_IIASA_Project/Manuscripts/1_Potential/Images/AUCs.csv")


### Percentage of Occurrence Captured By Threshed Maps
hit_rate <- vector()
for (i in 1:length(sp_avail)){
  sp = sp_avail[i]
  occ <- occurrence_all %>% as_tibble() %>% 
    filter(species == sp) %>% 
    dplyr::select(x,y)
  hits <- terra::extract(threshed[[sp_avail[i]]], occ) 
  hit_rate[i] <- sum(hits[,2], na.rm = TRUE)/nrow(hits)}
# hit_rate[length(sp_avail) + 1] <- NA ## For All Species Slot

hit_rate <- sp_avail %>% 
  bind_cols(hit_rate) %>% 
  setNames(c("Species", "hit_rate"))

#### Build Table of Areas

df_area_plot <- sp_avail %>% as.vector %>% 
  bind_cols(lapply(sp_avail, function(x) {expanse(threshed[[x]], unit = "ha")}) %>% 
              unlist() %>% 
              as.vector()) %>% 
  setNames(c("Species","Total Potential (Mha)") )%>% 
  bind_cols(lapply(sp_avail, function(x) {expanse(thresh_eez[[x]], unit = "ha")}) %>% 
              unlist() %>% 
              as.vector()) %>% 
  bind_cols(lapply(sp_avail, function(x) {expanse(thresh_nat[[x]], unit = "ha")}) %>% 
              unlist() %>% 
              as.vector()) %>% 
  left_join(cons_area %>% filter(Species != "All_Sp"), by = "Species") %>% 
  mutate(across(where(is.numeric), function(x) {x/1000000})) %>% 
  left_join(occ_count, by = c("Species" = "species")) %>% 
  left_join(AUCs, by = "Species") %>% 
  left_join(hit_rate, by = "Species") %>% 
  setNames(c("Species",
             "Total Potential (Mha)", 
             "Potential in EEZs (Mha)", 
             "Native Potential (Mha)", 
             "Constrained Potential (Mha)", 
             "Number of Occurrence Records",
             "AUC Value", 
             "Occurrence Prediction Rate")
  ) %>% 
  arrange(-`Total Potential (Mha)`) %>%
  mutate(Species = factor(Species, levels = .$Species)) %>% 
  {df_area<<-.} %>% 
  # filter(Species %in% sp_data)  %>% 
  # mutate(across(where(is.numeric), function(x) {log(x)})) %>% 
  mutate(ID = row_number() -1,
         angle =  90 - 360 * (ID-0.5)/nrow(.),
         hjust =  ifelse(angle < -90,0,1),
         angle  =  ifelse(angle < -90, angle+180, angle))


saveRDS(df_area, here("Outputs/df_area.RDS"))
write.csv(df_area, file.path(figure_folder,"df_area.csv"),row.names = FALSE)

#df_area <- readRDS(here("Outputs/df_area.RDS"))

##################################
### Calculate Extent of "All" Layers
sum_layers <- c(app(threshed, fun = "sum", na.rm = TRUE),
                app(thresh_eez, fun = "sum", na.rm = TRUE),
                app(thresh_nat, fun = "sum", na.rm = TRUE),
                app(thresh_cons, fun = "sum", na.rm = TRUE)
                
)
names(sum_layers) <- c("Viable", "EEZ", "Native", "Suitable")

writeRaster(sum_layers, filename = here("Outputs/sum_layers.tif"), overwrite = TRUE)

#sum_layers <- rast(here("Outputs/sum_layers.tif"))
### Plot Global Maps  

sum_layers <- rast(here("Outputs/sum_layers.tif"))


sel_com <- (sum_layers[[c(1,4)]] > 0) %>% 
  app("sum", na.rm = TRUE)

#source(here("Inputs/4f_plot_global.R"))
# 
# if(names(threshed) %>% str_detect("All_Sp") %>% sum() != 1){
#   threshed[["All_Sp"]]  <- app(threshed, fun = "sum", na.rm = TRUE, 
#                                filename = here("Outputs/max_threshed/All_Sp.tif"),
#                                overwrite = TRUE)
# } 
# 
# if(names(thresh_cons) %>% str_detect("All_Sp") %>% sum() != 1){
#   thresh_cons[["All_Sp"]]  <- app(thresh_cons, fun = "sum", na.rm = TRUE, 
#                                   filename = here("Outputs/All_Sp.tif"),
#                                   overwrite = TRUE)
# } 


AUC_plot <- ggplot(df_area) +
  geom_histogram(aes(x = `AUC Value`), binwidth = 0.05, center = 0.975) +
  xlab("AUC Value") +
  ylab("Number of Models") +
  theme_classic()

hit_plot <-  ggplot(df_area) +
  geom_histogram(aes(x = `Occurrence Prediction Rate`), binwidth = 0.05, center = 0.975) +
  xlab("% Occurrences Predicted") +
  ylab("Number of Models") +
  theme_classic()

ggsave(hit_plot + AUC_plot, 
       filename = file.path(figure_folder,"validation.png"),
       width = 8, height =6, dpi = 450,
       device = "png")

ggsave(hit_plot + AUC_plot, 
       filename = file.path(figure_folder,"validation.eps"),
       width = 8, height =6, dpi = 450,
       device = "eps")

#######

# Generate Plots

# 
# ### Radial Plot of Species Potential
# 
# ggplot(df_area_plot) +
#   geom_hline(yintercept = seq(from = 2000, to = 10000, by = 2000), col = "grey85") +
# 
#   geom_col(aes(x = Species, y = `Total Potential (Mha)`), fill = beyonce_palette(64)[c(10)]) +
#   geom_col(aes(x = Species, y = `Potential in EEZs (Mha)`), fill = beyonce_palette(64)[c(8)]) +
#   geom_col(aes(x = Species, y = `Native Potential (Mha)`), fill = beyonce_palette(64)[c(6)]) +
#   geom_col(aes(x = Species, y = `Constrained Potential (Mha)`), fill = beyonce_palette(64)[c(4)]) +
#   ylim(-2*(df_area_plot$`Total Potential (Mha)` %>% max),df_area_plot$`Total Potential (Mha)` %>% max) +
#   annotate(geom = "label", 
#            x = nrow(df_area_plot)/2 + 0.5, 
#            y = (1:5)*2000,
#            label = (1:5)*2000, 
#            col = "grey85",
#            size = 3,
#            fill = "white",
#            label.size = NA,
#            label.padding = unit(0.05, "lines")) +
#        #    ) +
#   coord_polar() +
#   geom_text(#data=country_potential,
#     aes(x=ID,
#         y=-100,
#         label=Species,
#         hjust=hjust,
#         vjust = 0.3),
#     color="black",
#     family = "serif",
#     fontface="plain",
#     alpha=1,
#     size=2.0,
#     angle= df_area_plot$angle,
#     inherit.aes = FALSE ) +
#   theme(axis.text=element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.y=element_blank(),
#         # = element_line(colour = "black", linetype = "solid", size=2),
#         panel.background = element_blank(),
#         legend.position = c(0.5,0.1),
#         legend.direction = "horizontal") +
#   scale_fill_gradient(low = "black", #beyonce_palette(64)[c(10)],
#                       high = beyonce_palette(32)[c(2)],
#                       #mid = "black",#beyonce_palette(64)[c(6)],
#                       # midpoint = 0,
#                       # na.value = "white",
#                       guide = guide_colorbar(reverse =FALSE),
#                       # name = str_wrap("Potential to Meet Demand", width = 10)
#   ) +
#   ggsave(filename =  file.path(figure_folder,"radial_sp.png"), 
#          device = "png", width = 8, height =6, dpi = 450)



#### Area of Constraint Raster
expanse(cons_ras, unit = "ha")/1e6



# %>% 
#   mutate(ID = row_number(), ### This block allows radial plotting
#          angle =  90 - 360 * (ID-0.5)/nrow(.),
#          hjust =  ifelse(angle < -90,0,1),
#          angle  =  ifelse(angle < -90, angle+180, angle)) # %>% 


#### Country Occurrences

count_points <- vector()
for (i in 1:length(eez$SOVEREIGN1 %>% unique)){
  message((eez$SOVEREIGN1 %>% unique)[i])
  terr <- eez %>% filter(SOVEREIGN1 == (eez$SOVEREIGN1 %>% unique)[i]) %>% vect %>% rasterize(threshed)
  occ <- occurrence_all %>% filter(species %in% sp_name) %>% dplyr::select(x,y) %>%
    as.matrix() %>% 
    terra::vect(type = "points",
                #geom = c("x", "y"),
                atts = NULL,
                crs = "+proj=longlat +datum=WGS84 +no_defs"#,
                #crs = 4326
    )
  points <- terra::extract(x = terr, y = occ) 
  count_points[i] <- points$V2 %>% sum(na.rm = TRUE)
}
count_points <- count_points %>% setNames(eez$SOVEREIGN1 %>% unique)
saveRDS(count_points,here("Outputs/count_points.RDS"))

# 
# 
# ## Radial Plot of Country Potential
# 
#   ggplot(country_potential) + 
#   #  geom_point(aes(y = `Constrained Area (Mha)`, x = `Number of Species` )) +
#   geom_col(aes(x = Country, y = `Constrained Area (Mha)`, fill = `Number of Species`),
#            #width = country_potential$`Number of Species`/max(country_potential$`Number of Species`)
#            ) +
#   coord_polar() +
#   ylim(-200,105) +
#   geom_text(#data=country_potential,
#             aes(x=ID,
#                 y=-5,
#                 label=Country,
#                 hjust=hjust,
#                 vjust = 0.3),
#             color="black",
#             family = "serif",
#             fontface="plain",
#             alpha=1,
#             size=2.0,
#             angle= country_potential$angle,
#             inherit.aes = FALSE ) +
#   theme(axis.text=element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.y=element_blank(),
#         # = element_line(colour = "black", linetype = "solid", size=2),
#         panel.background = element_blank(),
#         legend.position = c(0.5,0.1),
#         legend.direction = "horizontal") +
#   scale_fill_gradient(low = "black", #beyonce_palette(64)[c(10)],
#                        high = beyonce_palette(32)[c(2)],
#                        #mid = "black",#beyonce_palette(64)[c(6)],
#                        # midpoint = 0,
#                        # na.value = "white",
#                        guide = guide_colorbar(reverse =FALSE),
#                        # name = str_wrap("Potential to Meet Demand", width = 10)
#     ) +
#     ggsave(filename =  file.path(figure_folder,"radial.png"), 
#            device = "png", width = 8, height =6, dpi = 450)

