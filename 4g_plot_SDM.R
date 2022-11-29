library(terra)
library(tidyverse)
library(tmap)
library(here)
library("rnaturalearth")
library("rnaturalearthdata")
library(beyonce)

tmap_options(max.raster = c(plot = 1e6, view = 1e6))

wrld <- ne_countries(scale = "medium", returnclass = "sf")

figure_folder <- here("Manuscript_Latex/YSSP_Report_Latex/figures") %>% str_remove("LUC_Project_min/")

max_threshed <- here("Outputs/max_threshed") %>% 
  list.files(full.names = TRUE) %>% 
  rast() 

constraints <- here("Inputs/constraints_normalized.tif") %>% rast() 

constraints_final <- rast(here("Outputs/cons_ras.tif")) %>% setNames("Constraints")

to_map <- lst(max_threshed,constraints, constraints_final) #%>% subst(0,NA)

maps <- list()
for (j in 1:length(names(to_map))){
  maps[[j]] <- list()
  for(i in 1:length(names(to_map[[j]]))){
    
    type <- names(to_map)[[j]]
    nm <- names(to_map[[j]][[i]]) %>% str_replace("_"," ")
    
    message(type," : ",nm)
    
    maps[[j]][[i]] <- tm_shape(to_map[[j]][[i]],
                               projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m", 
                               raster.warp = FALSE, 
                               raster.downsample = TRUE) +
      tm_raster(palette = 
                  if(type == "max_threshed")
                    {
                    beyonce_palette(8)[4]
        }else{
          c("white",
                beyonce_palette(7)[3])},
                style = "cont") +
      tm_shape(wrld,
               projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") +
      tm_fill("grey85") +
      tm_layout(legend.show = FALSE,
                legend.text.size = 0.1,
                main.title = nm,
                main.title.position = "left")
  }
}

## Plot SDMs

maps[[1]][[35]] <- "NULL"
maps[[1]][[36]] <- "NULL"

for (i in 3:(length(maps[[1]])/6)
     ) {
  
  if(i != (length(maps[[1]])/6)){
    message("Map : ",i)
    map <- tmap_arrange(maps[[1]][[(6*i)-5]],
                        maps[[1]][[(6*i)-4]],
                        maps[[1]][[(6*i)-3]],
                        maps[[1]][[(6*i)-2]],
                        maps[[1]][[(6*i)-1]],
                        maps[[1]][[(6*i)]],
                        ncol = 2)
  }else{
    map <- tmap_arrange(maps[[1]][[(6*i)-5]],
                        maps[[1]][[(6*i)-4]],
                        maps[[1]][[(6*i)-3]],
                        maps[[1]][[(6*i)-2]],
                        ncol = 2)
  }
  
  
 # tmap_save(map,filename =  paste0(figure_folder, "/maps_",i,".png"),height = ifelse(i == 6,16/3,8),width = 8, dpi = 300)
  tmap_save(map,filename =  paste0(figure_folder, "/maps_",i,".eps"),height = ifelse(i == 6,16/3,8),width = 8, dpi = 300)
  
}

## Plot Constraints

message("Map : Constraints")
i=1
map_c <- tmap_arrange(maps[[2]][[(6*i)-5]],
                    maps[[2]][[(6*i)-4]],
                    maps[[2]][[(6*i)-3]],
                    maps[[2]][[(6*i)-2]],
                    maps[[2]][[(6*i)-1]],
                    maps[[2]][[(6*i)]],
                    ncol = 2)

tmap_save(map_c,filename =  paste0(figure_folder, "/maps_constraints.png"),height = ifelse(i == 6,16/3,8),width = 8, dpi = 300)
#tmap_save(map_c,filename =  paste0(figure_folder, "/maps_constraints.pdf"),height = ifelse(i == 6,16/3,4),width = 4, dpi = 300,units = "in")
for(z in 1:length(map_c)){
  tmap_save(map_c[[z]],filename =  paste0(figure_folder, "/maps_constraints",z,".pdf"),height = ifelse(i == 6,16/3,2),width = 3, dpi = 300,units = "in")
}

## Plot Overall Constraint Layer

map_co <- maps[[3]][[1]]

tmap_save(map_co,filename =  paste0(figure_folder, "/maps_constraints_overall.png"),height = ifelse(i == 6,16/3,8),width = 8, dpi = 300)
tmap_save(map_co,filename =  paste0(figure_folder, "/maps_constraints_overall.eps"),height = ifelse(i == 6,16/3,8),width = 8, dpi = 300)




for (i in 1:6) {
  
  caption <- (paste0(word(names(max_threshed),1) %>% substring(1,1),".",word(names(max_threshed),2)))[(6*i-5):(6*i)] %>% 
    paste(collapse=", ")
  
  message("\\begin{figure}[!htb]")
  message("\\centering")
  message("\\includegraphics[width=1\\textwidth]{maps_",i,".png}")
  message("\\caption[Global Potential for ",caption,"]{,",caption,",}")
  message("\\label{fig:maps_",i,"}")
  message("\\end{figure}")
  
}

