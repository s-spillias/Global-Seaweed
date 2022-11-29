
#install.packages(c("RANN","factoextra","ggrepel","data.table"))
library(cluster)
library(factoextra)
library(ggrepel)
library(reshape2)
library(RANN)
library(terra)
library(data.table)
library(here)
source(here("1a_FAO_Clean.R"))

market_balance_SSP2 <- read.csv(here("Inputs/Market_Balance_GLOBIOM_proj_SSP2_2050 (002).csv"), fileEncoding="UTF-8-BOM")

Total_Supply <- market_balance_SSP2 %>% 
  filter(Var == "CONS",
         Unit != "1000 t") %>% 
  mutate(Amount_Tonnes = Val*1000,
         Use = "Human_Food",
         Regional_Agg = Reg) %>% 
  dplyr::select(Regional_Agg, Year, Item, Amount_Tonnes, Use)

feed <- list()
for (i in 1:3) {
  feed[[i]] <- read.csv(here(paste0("Inputs/Feed_AgMIP3_2000_2050_split3_",i,".csv")), fileEncoding="UTF-8-BOM")
}
names(feed) <- c("Ruminants_Meat","Ruminants_Dairy","Mono-Gastrics")
feed <- melt(feed)
names(feed) <- c("Reg","Item", "X","SSP", "X2", "Year", "Amount_1000_Tonnes", "Use")
feed <- feed %>% 
  mutate(Year = as.numeric(str_remove(Year,"X")),
         Amount_Tonnes = Amount_1000_Tonnes*1000,
         Item = toupper(Item)) %>% 
  left_join(eez, by = c("Reg" = "Region")) %>% 
  group_by(Regional_Agg, Year, Item, Use) %>% 
  summarise(Amount_Tonnes = sum(Amount_Tonnes, na.rm =TRUE))


SSP2 <- bind_rows(feed,Total_Supply)
saveRDS(SSP2,"Inputs/feed_food_SSP2.RDS")


demand_SSP2 <- read.csv(here("Inputs/Food_availability_GLOBIOM_proj_SSP2_2050.csv"),fileEncoding="UTF-8-BOM") %>% 
  mutate(Item = case_when(Item == "SUGR_C" ~ "SUGC",
                          Item == "BEAN" ~ "BEAD",
                          Item == "COTO" ~ "COTT",
                          Item == "PLSN" ~ "CHKP",
                          Item == "PLMO" ~ "OPAL",
                          TRUE ~ Item
                          )
        , Val = case_when((Item == "PTMEAT") & (Unit == "kg/cap/yr") ~ Val*.55,
                          (Item == "BVMEAT") & (Unit == "kg/cap/yr") ~ Val*.65,
                          (Item == "BGMEAT") & (Unit == "kg/cap/yr") ~ Val*.65,
                          (Item == "SGMEAT") & (Unit == "kg/cap/yr") ~ Val*.65,
                          (Item == "PGMEAT") & (Unit == "kg/cap/yr") ~ Val*.65,
                          TRUE ~ Val)
         )


### nutritional profile
nutr_profile_ter <- demand_SSP2 %>% 
  pivot_wider(names_from = "Unit", values_from = Val) %>% 
  rename(Energy = "kcal/cap/d",
         Protein = "gprot/cap/d",
         Fat = "gfat/cap/d", 
         Weight = "kg/cap/yr") %>% 
  dplyr::select(Reg, Item, Year,Energy, Protein, Fat, Weight) %>% 
  filter(Weight != 0, !is.na(Fat), !is.na(Protein)) %>% 
  mutate(Energy = 365.25*Energy/Weight,
         Protein = 365.25*Protein/Weight,
         Fat = 365.25*Fat/Weight,
         Source = "Terrestrial") %>% 
  group_by(Item, Source) %>% 
  summarise(Energy = mean(Energy, na.rm = TRUE),
            Protein = mean(Protein, na.rm = TRUE),
            Fat = mean(Fat, na.rm  = TRUE)) %>% 
  filter(Item != "TOT") %>% 
  column_to_rownames(var = "Item") %>% 
  relocate(Source, .after = last_col()) 
saveRDS(nutr_profile_ter,here("Inputs/nutr_profile_ter.RDS"))

nutr_profile_sw <- readRDS("Inputs/util_data.RDS") %>% 
  mutate(#Item = paste0(substring(Taxa, 1, 1),". ",word(Taxa,2)),
         Protein = 10*(Protein_min + Protein_max)/2,
         Fat = 10*(Lipid_min + Lipid_max)/2,
         Energy = (Energy_min + Energy_max)/2,
         Source = "Seaweed") %>% 
  dplyr::select(Taxa, Energy, Protein, Fat, Source) %>% 
  distinct() %>% 
  filter(!is.na(Energy),!is.na(Protein),!is.na(Fat)) %>% 
  column_to_rownames(var = "Taxa")

#####
nutr_scale <- bind_rows(nutr_profile_ter,
                        nutr_profile_sw %>% rownames_to_column(var = "Taxa") %>%
                        #  mutate(Taxa = paste0(substring(Taxa, 1, 1),". ",word(Taxa,2))) %>%
                          column_to_rownames("Taxa")) %>% 
  mutate(across(-Source,.fns = function(x) {scale(x)} ))

#########
replacements_sw <- nutr_profile_sw  %>% bind_cols(nn2(
  nutr_scale %>% filter(Source == "Terrestrial") %>% dplyr::select(-Source),
  nutr_scale %>% filter(Source == "Seaweed") %>% dplyr::select(-Source),
  k = nrow(nutr_scale %>% filter(Source == "Terrestrial") %>% dplyr::select(-Source)),
  treetype = "bd")) %>%
  rownames_to_column(var = "Item") %>% 
  nest(Nearest_Neighbor = starts_with("nn.idx")) %>%
  mutate(Nearest_Neighbor = lapply(Nearest_Neighbor, function(x) {
    unlist(x) %>% as_tibble %>%
      left_join(nutr_profile_ter %>%
                  rownames_to_column(var = "NN") %>%
                  mutate(index = row_number()) %>%
                  dplyr::select(NN, index, Energy, Protein, Fat), by = c("value"="index") ) %>%
      dplyr::select(-value)}
    )) %>%
  dplyr::select(-nn.dists)


replacements_ter <- nutr_profile_ter %>% bind_cols(
  nn2(
    nutr_scale %>% filter(Source == "Seaweed") %>% dplyr::select(-Source),
    nutr_scale %>% filter(Source == "Terrestrial") %>% dplyr::select(-Source),
    k = nrow(nutr_scale %>% filter(Source == "Seaweed") %>% dplyr::select(-Source)),
    treetype = "bd")
    ) %>%
   rownames_to_column(var = "Item") %>% 
  nest(Nearest_Neighbor = starts_with("nn.idx")) %>%
  mutate(Nearest_Neighbor = lapply(Nearest_Neighbor, function(x) {
    unlist(x) %>% as_tibble %>%
      left_join(nutr_profile_sw %>%
                  rownames_to_column(var = "NN") %>%
                  mutate(index = row_number()) %>%
                  dplyr::select(NN, index, Energy, Protein, Fat), by = c("value"="index") ) %>%
      dplyr::select(-value)})) %>%
  dplyr::select(-nn.dists)


combos <- expand.grid(replacements_sw$Item,replacements_sw$Item) %>% 
  left_join(replacements_sw, by = c("Var1" = "Item")) %>% 
  left_join(replacements_sw, by = c("Var2" = "Item")) %>% 
  dplyr::select(Var1, Energy.x,Protein.x,Var2,Energy.y,Protein.y) %>% 
  mutate(name = paste0(Var1,"_",Var2)) %>% 
  filter(Var1 != Var2) %>% 
  column_to_rownames("name")
combos <- setNames(split(combos,seq(nrow(combos))), rownames(combos))

# repl_list <- list()
# for (i in 1:nrow(replacements_ter)){
#   V = rbind(replacements_ter$Energy[i],replacements_ter$Protein[i])
#   row.names(V) <- c("Energy","Protein")
#   colnames(V) <- replacements_ter$Item[i]
#   M = lapply(combos,function(x) {
#     matrix(nrow = 2, ncol = 2, data = x %>% dplyr::select(Energy.x,Protein.x,Energy.y,Protein.y) %>% t())
#   } )
#   for (j in 1:length(M)){
#     colnames(M[[j]]) <- str_split(names(M),"_")[[j]]
#     rownames(M[[j]]) <- c("Energy","Protein")
#   }
#   
#   Q = lapply(M,function(x) {unlist(
#     rbind(
#       t(solve(x)%*%V) %>% colnames(),
#       t(solve(x)%*%V)
#     ) %>% 
#       as_tibble() %>% rename_with(~c("Item_1","Item_2"))) %>% t %>% as.data.frame})
#   repl_list[[i]] <- Q %>% rbindlist()
#   names(repl_list)[i] <- replacements_ter$Item[i] 
# }
# repl_sw <- reshape2::melt(repl_list, id.vars = c("Item_11","Item_12","Item_21","Item_22")) %>%
#   as.data.frame() %>% 
#   bind_cols(duplicated(t(apply(., 1, sort)))) %>% 
#   rename_with(~c("Item_1","Amount_1","Item_2","Amount_2","Item","Duplicated")) %>% 
#   filter(Amount_1 > 0, Amount_2 > 0) %>% 
#   relocate(Item) %>% 
#   filter(Duplicated == "FALSE") %>% 
#   dplyr::select(-Duplicated) %>% 
#   nest(combos = contains("_"))


combos <- expand.grid(replacements_ter$Item,replacements_ter$Item) %>% 
  left_join(replacements_ter, by = c("Var1" = "Item")) %>% 
  left_join(replacements_ter, by = c("Var2" = "Item")) %>% 
  dplyr::select(Var1, Energy.x,Protein.x,Var2,Energy.y,Protein.y) %>% 
  mutate(name = paste0(Var1,"_",Var2)) %>% 
  filter(Var1 != Var2, !(Protein.y ==0 & Protein.x == 0)) %>% 
  column_to_rownames("name")
combos <- setNames(split(combos,seq(nrow(combos))), rownames(combos))

# repl_list <- list()
# for (i in 1:nrow(replacements_sw)){
#   V = rbind(replacements_sw$Energy[i],replacements_sw$Protein[i])
#   row.names(V) <- c("Energy","Protein")
#   colnames(V) <- replacements_sw$Item[i]
#   M = lapply(combos,function(x) {
#     matrix(nrow = 2, ncol = 2, data = x %>% dplyr::select(Energy.x,Protein.x,Energy.y,Protein.y) %>% t())
#   } )
#   for (j in 1:length(M)){
#     colnames(M[[j]]) <- str_split(names(M),"_")[[j]]
#     rownames(M[[j]]) <- c("Energy","Protein")
#   }
#   
#   Q = lapply(M,function(x) {unlist(
#     rbind(
#       t(solve(x)%*%V) %>% colnames(),
#       t(solve(x)%*%V)
#     ) %>% 
#       as_tibble() %>% rename_with(~c("Item_1","Item_2"))) %>% t %>% as.data.frame})
#   repl_list[[i]] <- Q %>% rbindlist()
#   names(repl_list)[i] <- replacements_sw$Item[i] 
# }
# repl_ter <- reshape2::melt(repl_list, id.vars = c("Item_11","Item_12","Item_21","Item_22")) %>%
#   as.data.frame() %>% 
#   bind_cols(duplicated(t(apply(., 1, sort)))) %>% 
#   rename_with(~c("Item_1","Amount_1","Item_2","Amount_2","Item","Duplicated")) %>% 
#   filter(Amount_1 > 0, Amount_2 > 0) %>% 
#   relocate(Item) %>% 
#   filter(Duplicated == "FALSE") %>% 
#   select(-Duplicated) %>% 
#   nest(combos = contains("_"))

replacements <- bind_rows(replacements_sw,replacements_ter)# %>% 
 # left_join(bind_rows(repl_ter,repl_sw), by = "Item")

saveRDS(replacements,"Inputs/replacements.RDS")


## Cluster based on nutrient composition
#nutr_scale <- nutr_scale 
#fviz_nbclust(nutr_scale, kmeans, method = "wss")
#gap_stat <- clusGap(nutr_scale,
#                     FUN = kmeans,
#                     nstart = 25,
#                     K.max = 10,
#                     B = 50)
# #fviz_gap_stat(gap_stat)
set.seed(42)
km <- kmeans(nutr_scale %>%
               dplyr::select(-Source) %>% 
               as.data.frame(), centers = 5, nstart = 25)
options(ggrepel.max.overlaps = Inf)

to_plot <- c("Neopyropia yezoensis","SOYA","WHEA","POTA","FRUITS+", "Ascophyllum nodosum",
             "Kappaphycus alvarezii", "PTMEAT",
             #"Gracilaria chilensis",
             "Saccharina latissima",
             "OPAL",
             "CHKP",
             "Porphyra umbilicalis",
             "Alaria esculenta",
             "RICE",
             "VEGOIL+",
             "CORN",
             "VEGET+",
             "PGMEAT",
             "SGMEAT",
             "SUNF",
             "Palmaria palmata",
             "Laminaria nigrescens",
             "GNUT",
             "Asparagopsis taxiformis",
             "RAPE",
             "Asparagopsis armata",
             "SRGH",
             "OTMEAT+",
             "Costaria costata"
             )
source(here("Inputs/clus_plot.R"))
clust_plot(km, data = nutr_scale, 
             repel = TRUE,
             show.clust.cent = FALSE,
             labelsize = 8,
           pointsize = 3,
           main = "Clustered Nutritional Profiles") + 
  guides(colour = FALSE, fill = FALSE)+
  theme_classic() +
 # ggtitle("Clustered Nutritional Profiles") +
  scale_color_manual(values = c(beyonce_palette(114)[c(1,6,11)],beyonce_palette(40)[c(6,7)]))+
  scale_fill_manual(values = c(beyonce_palette(114)[c(1,6,11)],beyonce_palette(40)[c(6,7)]))+
  geom_text_repel(data=plot.data  %>% filter(cluster != 1) %>% 
                    mutate(name = ifelse(name %in% to_plot,as.character(name),"")) %>% 
                    mutate(name = ifelse(str_detect(name," "),
                                       paste0(substr(name,1,1),". ",word(name,2)),
                                       as.character(name))),
            aes(x,y,label=name), size = 3,
            force_pull = 0.1,
            max.time = 5) +
  theme(#legend.position = c(0.85, 0.8),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=12)) 
  ggsave(file.path(figure_folder,"cluster.png"),
         height = 5, width = 9)
  ggsave(file.path(figure_folder,"cluster.eps"),
         height = 5, width = 9)
  
saveRDS(plot.data, here("Inputs/cluster_map.RDS"))
nutr_scale["cluster"] <- km$cluster
nutr_scale %>% rownames_to_column("Taxa") %>%  dplyr::select(Taxa,cluster) %>% filter(str_detect(Taxa," ")) %>% saveRDS(here("Inputs/clusters.RDS"))

