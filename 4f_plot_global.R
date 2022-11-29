#### Plot Global and Regional 

### Need sel_com to have been created from GLOBIOM

library(beyonce)
library(patchwork)
library(grid)
library(cowplot)


# country_map <- vect(here("ne_10m_land.shp")) %>% 
#   rasterize(rast(here("Inputs/eez_rast.tif")))
#  #mutate(ISO_A3_EH = ifelse(ISO_A3_EH == -99,SOV_A3,ISO_A3_EH)) 

# sel_com %>% lapply(function(x) {
#   rast(here("Inputs/distance-from-port.tif")) %>% crop(.) %>% mask(x) %>% 
#     app(function(y) mean(y,na.rm = TRUE))
# })

level_names <- c("Minimum","Mean","Maximum")


sel_com_sp <- lapply(sel_com, function(x) as.polygons(x#, readpoly=F
                 ) %>% st_as_sf()) %>% bind_rows %>% 
  {if(prio){
      mutate(.,Assumption = case_when(!is.na(lyr.1) ~ "Minimum",
                                !is.na(lyr.2) ~ "Mean",
                                !is.na(lyr.3) ~ "Maximum") %>% factor(levels = level_names)) %>% 
      dplyr::select(-contains("lyr"))
    }else{
      mutate(.,Assumption = case_when(`sum` == 1 ~ "Suitable",
                                    `sum` == 2 ~ "Feasible")%>% factor(levels = c("Suitable","Feasible"))) %>% 
        dplyr::select(-`sum`)
             }} 



(world <- tm_shape(wrld, 
                  projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
  tm_fill("grey85") +
  tm_shape(sel_com_sp,
          # raster.downsample = FALSE,
           projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ,raster.warp = FALSE)+
  tm_fill("Assumption",#col = file_name,
    style = "cat",
    palette = pal,
    title = if(prio){"Productivity Assumption"}else{""},
    labels = if(prio) {c("Minimum","Mean","Maximum")}else{c("Feasible","Suitable")},
    legend.is.portrait = FALSE) +
  tm_layout(
   legend.position = c("right", "bottom"),
   frame = FALSE,
              legend.text.size = 1.3,
              #legend.outside.position = "top",
              main.title = "A",
              main.title.size = 2,
           # main.title = names(to_map)[i], 
            main.title.position = "left"
           ) ) 

file_location <- if(prio){here(paste0("Outputs/region_potential_",sw %>% as_tibble() %>% setNames("SW_Scen") %>%  pub_scen %>% pull %>% toupper(),".RDS"))
}else{here(paste0("Outputs/region_potential_THRESH_CONS.RDS"))}

product_bar <- readRDS(file_location) %>% 
  {if(!prio){mutate(.,proportion = `Suitable Area`/sum(`Suitable Area`)) %>% filter(proportion > 0.01) %>% 
      arrange(desc(`Suitable Area`))
    }else{
      mutate(.,proportion = Max/sum(Max)) %>% filter(.,proportion > 0.01) %>% 
      arrange(desc(`Min`)) 
    } 
    } %>% 
    pub_region() %>% 
  mutate(Region = str_replace(Region, " - ","-")) %>% 
  mutate(Region = as.character(Region) %>% str_replace("South East","SE")) %>% 
  mutate(Region = factor(Region, levels = .$Region)) %>% 
  {if(!prio){
    ggplot(.) +
    geom_col(aes(x = Region, y = `Suitable Area`), fill = pal[2])
  } else {
    ggplot(.) +
    geom_col(aes(x = Region, y = Min), fill = pal[1]) +
      geom_col(aes(x = Region, y = Mean), fill = pal[2]) +
      geom_col(aes(x = Region, y = Max), fill = pal[3])
  }} +
#  coord_flip() +
  ylab("Suitable Area (Mha)") +
  theme_classic() +
  ggtitle("B") +
  scale_y_continuous(expand = c(0.01,0)) +
  theme(axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        title = element_text(size = 19,# hjust = -1
                             ),
        plot.title.position = "plot",
    axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1.0,
                                   size = 14))


if(prio){
  
  
  (port_dist_plot <- lapply(1:nlyr(sel_com),function(x){
    rast(here("Inputs/distance-from-port.tif")) %>% crop(sel_com[[x]]) %>%
      mask(sel_com[[x]]) %>% 
      terra::as.matrix() %>% 
      as_tibble() %>% 
      setNames(level_names[x]) #%>% 
    # filter(!is.na(!!as.symbol(level_names[x])))
  }) %>%  bind_cols() %>%  filter(if_any(everything(), ~ !is.na(.))) %>% 
      pivot_longer(everything(),names_to = "Assumption",
                   values_to = 'value') %>% ggplot( aes(x=value, fill= pal[3]#Assumption
                   )) +
      geom_density(alpha=1) +
      theme_classic() +
      scale_fill_manual(values = pal[2]) +
      output_legend +
      theme(legend.position = "none") +
      
      xlab("Distance to Nearest Port (km)"))
  
  
  
  (depth_plot <- lapply(1:nlyr(sel_com),function(x){
    rast(here("Inputs/depth_200.tif")) %>% crop(sel_com[[x]]) %>%
      mask(sel_com[[x]]) %>% 
      terra::as.matrix() %>% 
      as_tibble() %>% 
      setNames(level_names[x])
  }) %>%  bind_cols() %>%  filter(if_any(everything(), ~ !is.na(.))) %>% 
      pivot_longer(everything(),names_to = "Assumption",
                   values_to = 'value') %>% ggplot( aes(x=value, fill=pal[1]#Assumption
                   )) +
      geom_density(alpha=1,adjust =3) +
      theme_classic() +
      scale_fill_manual(values = pal[2]) +
      output_legend +
      theme(legend.position = "none",
            plot.title = element_text(size = 23),
            plot.title.position = "plot" ) +
      ggtitle("C") +
      xlab("Depth (m)"))
  
  (add_ons <- depth_plot + port_dist_plot + plot_layout(ncol = 1, guides = "collect") )
  
}

# yscale = 1.3
pdf(paste0(figure_folder, "/map_global_",ind,".pdf"), 
    width = 10, height = 12#, units = "in", res = 450
    )
grid.newpage()
print(world,  vp = viewport(0.5, 0.7, width = 1, height = 0.7))
if(prio){
print(product_bar , vp = viewport(0.351, 0.25, width = .7, height = 0.4))
print(add_ons , vp = viewport(0.85, 0.25, width = .3, height = 0.4))
} else {
  print(product_bar , vp = viewport(0.51, 0.25, width = 1, height = 0.4))
}

dev.off()
Sys.sleep(0.1)


png(paste0(figure_folder, "/map_global_",ind,".png"),
    width = 10, height = 12, units = "in", res = 450
)
grid.newpage()
print(world,  vp = viewport(0.5, 0.7, width = 1, height = 0.7))
if(prio){
  print(product_bar , vp = viewport(0.351, 0.25, width = .7, height = 0.4))
  print(add_ons , vp = viewport(0.85, 0.25, width = .3, height = 0.4))
} else {
  print(product_bar , vp = viewport(0.51, 0.25, width = 1, height = 0.4))
}
dev.off()










# grid.arrange(tmap_grob(world),product_bar, nrow = 2,
#             # heights = c(0.6,0.4),
#              widths = c(0.8,0.6))
# # 
# indo_ext <- files_ras[str_detect(files_ras,"IndonesiaReg")] %>%
#   rast() %>% st_bbox
# euro_ext <- files_ras[str_detect(files_ras,"EU_North")] %>%
#   rast() %>% st_bbox
# braz_ext <- files_ras[str_detect(files_ras,"BrazilReg")] %>%
#   rast() %>% st_bbox
# usa_ext <- files_ras[str_detect(files_ras,"USAReg")] %>%
#   rast() %>% st_bbox
# rcam_ext <- files_ras[str_detect(files_ras,"RCAM")] %>%
#   rast() %>% st_bbox
# rcam_ext["ymin"] <- rcam_ext["ymin"] - 5
# rcam_ext["ymax"] <- rcam_ext["ymax"] - 15
# #euro_ext["ymin"] <- euro_ext["ymin"] + 2
# indo_ext["ymin"] <- indo_ext["ymin"] - 3
# 
# 
# sa_ext <- terra::merge(files_ras[str_detect(files_ras,"RSAM")] %>%
#                rast(),
#              files_ras[str_detect(files_ras,"BrazilReg")] %>%
#                rast()) %>% st_bbox
# 
# 
# #tmap_options(max.raster = c(plot = 1e8, view = 1e6))
# indo <- tm_shape(wrld, 
#                  bbox = indo_ext,
#                 # raster.downsample = FALSE,
#          projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
#   tm_fill("grey85") +
#   tm_shape(sel_com_sp,
#            bbox = indo_ext,
#            projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ,raster.warp = FALSE)+
#   tm_fill("sum",
#     style = "cat",
#     palette = pal) +
#   tm_layout(legend.show = FALSE,
#             title = "E", 
#             # main.title.position = "left"
#   ) 
# # indo %>% tmap_save(here(paste0(figure_folder,"/","indo.png")),
# #                    height = 16,width = 24,dpi = 450)
# 
# sa <- tm_shape(wrld, 
#                  bbox = sa_ext,
#                  # raster.downsample = FALSE,
#                  projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
#   tm_fill("grey85") +
#   tm_shape(sel_com_sp,
#            bbox = sa_ext,
#            projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ,raster.warp = FALSE)+
#   tm_fill("sum",
#           style = "cat",
#           palette = pal) +
#   tm_layout(legend.show = FALSE,
#             title = "D", 
#             # main.title.position = "left"
#   ) 
# 
# rcam <- tm_shape(wrld, 
#                bbox = rcam_ext,
#                # raster.downsample = FALSE,
#                projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
#   tm_fill("grey85") +
#   tm_shape(sel_com_sp,
#            bbox = rcam_ext,
#            projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ,raster.warp = FALSE)+
#   tm_fill("sum",
#           style = "cat",
#           palette = pal) +
#   tm_layout(legend.show = FALSE,
#             title = "D", 
#             # main.title.position = "left"
#   ) 
# 
# usa <- tm_shape(wrld, 
#                  bbox = usa_ext,
#                  # raster.downsample = FALSE,
#                  projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
#   tm_fill("grey85") +
#   tm_shape(sel_com_sp,
#            bbox = usa_ext,
#            projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ,raster.warp = FALSE)+
#   tm_fill("sum",
#           style = "cat",
#           palette = pal) +
#   tm_layout(legend.show = FALSE,
#             title = "B", 
#             # main.title.position = "left"
#   ) 
# 
# euro <- tm_shape(wrld, 
#                  bbox = euro_ext,
#                  # raster.downsample = FALSE,
#                  projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
#   tm_fill("grey85") +
#   tm_shape(sel_com_sp,
#            bbox = euro_ext,
#            projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ,raster.warp = FALSE)+
#   tm_fill("sum",
#           style = "cat",
#           palette = pal) +
#   tm_layout(legend.show = FALSE,
#             title = "C", 
#             # main.title.position = "left"
#   ) 



# design = '
# CC
# CC
# AB
# DE
# '
# 
# map_pot <- wrap_plots(C = world %>% tmap_grob,
#                     B = euro %>% tmap_grob,
#                     E = indo %>% tmap_grob,
#                     A = rcam %>% tmap_grob,
#                     D = braz %>% tmap_grob,

# 
# indo <- ggplot() +
#   geom_tile(data = as.data.frame(sel_com, xy = TRUE), aes(x = x, y = y, fill = as.factor(!!as.symbol(names(as.data.frame(sel_com, xy = TRUE))[3])))) +
#   geom_tile(data = as.data.frame(country_map, xy = TRUE), #size = .1,
#             aes(x = x, y = y), color = "grey85", fill = "grey85") +
#   coord_sf() +
#   xlim(100,150) +
#   ylim(-20,20) +
#   # xlim(0,20) +
#   # ylim(30,50) +
#   theme_classic() +
#   scale_fill_manual(values = pal,
#                     # breaks = unique(sel_com),
#                     #name = NULL,
#                     na.value = "white",
#                     guide = FALSE
#                     #na.translate = FALSE,
#                     # labels = "Viable for Seaweed Farming"
#   ) +
#   # scale_color_gradient2(low = rev(beyonce_palette(33,56, "continuous"))[22],
#   #                       mid = rev(beyonce_palette(33,56, "continuous"))[56],
#   #                       high = beyonce_palette(34)[6],
#   #                  # breaks = unique(sel_com),
#   #                   #name = NULL,
#   #                   na.value = "white",
#   #                   #na.translate = FALSE,
#   #                   #labels = "Viable for Seaweed Farming",
#   #                 guide = "colourbar"   
#   #                   )+
#   theme(     legend.position = "bottom",
#              legend.key.size = unit(0.5, 'cm'), #change legend key size
#              #legend.key.height = unit(1, 'cm'), #change legend key height
#              #legend.key.width = unit(1, 'cm'), #change legend key width
#              legend.title = element_text(size=8), #change legend title font size
#              legend.text = element_text(size=7)
#   ) +
#   labs(col = "Seaweed Farming Viability") +
#   xlab(NULL) + ylab(NULL)
# 
# braz <- ggplot() +
#   geom_tile(data = as.data.frame(sel_com, xy = TRUE), aes(x = x, y = y, fill = as.factor(!!as.symbol(names(as.data.frame(sel_com, xy = TRUE))[3])))) +
#   geom_tile(data = as.data.frame(country_map, xy = TRUE), #size = .1,
#             aes(x = x, y = y), color = "grey85", fill = "grey85") +
#   coord_sf() +
#   #scale_fill_viridis_c(na.value = 'black') +
#   xlim(-12,30) +
#   ylim(30,70) +
#   # xlim(0,20) +
#   # ylim(30,50) +
#   theme_classic() +
#   scale_fill_manual(values = pal,
#                     # breaks = unique(sel_com),
#                     #name = NULL,
#                     na.value = "white",
#                     guide = FALSE
#                     #na.translate = FALSE,
#                     # labels = "Viable for Seaweed Farming"
#   ) +
#   # scale_color_gradient2(low = rev(beyonce_palette(33,56, "continuous"))[22],
#   #                       mid = rev(beyonce_palette(33,56, "continuous"))[56],
#   #                       high = beyonce_palette(34)[6],
#   #                  # breaks = unique(sel_com),
#   #                   #name = NULL,
#   #                   na.value = "white",
#   #                   #na.translate = FALSE,
#   #                   #labels = "Viable for Seaweed Farming",
#   #                 guide = "colourbar"   
#   #                   )+
#   theme(     legend.position = "bottom",
#              legend.key.size = unit(0.5, 'cm'), #change legend key size
#              #legend.key.height = unit(1, 'cm'), #change legend key height
#              #legend.key.width = unit(1, 'cm'), #change legend key width
#              legend.title = element_text(size=8), #change legend title font size
#              legend.text = element_text(size=7)
#   ) +
#   labs(col = "Seaweed Farming Viability") +
#   xlab(NULL) + ylab(NULL)
# 
# ggsave(world + (braz + indo + plot_layout(ncol = 2)) + plot_layout(nrow = 2,heights = c(2.3,2), guides = "collect") & theme(legend.position = "top"),
#        filename =  paste0(figure_folder, "map_global_supply.png"), 
#        device = "png", width = 6, height = 6, dpi = 450)
