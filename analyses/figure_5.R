#' FIGURE 5


## Import Icons ----

paths <- list.files(path = here::here("data", "icons"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("data", "icons"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)


## Prepare Data ----

load(file = here::here("outputs", "res_for_model.RData"))

filenames <- list.files(path = here::here("outputs"), pattern = "_miss.rds$",
                        full.names = FALSE)

files <- list.files(path = here::here("outputs"), pattern = "_miss.rds$",
                    full.names = TRUE)
list_res <- lapply(files, function(x) readRDS(x))

res_for_graph_miss <- na.omit(data.frame(do.call(rbind, lapply(1:length(list_res),
                                                               function(i) {
  res <- data.frame(list_res[[i]])
  res$taxa <- gsub("_miss\\.rds", "", filenames)[i]
  return(res)
}))))


res_for_graph_miss$SP    <- NA 
res_for_graph_miss$trait <- NA 

for (i in 1:nrow(res_for_graph_miss)){ 

  res_for_graph_miss$SP[i]    <- res_for_model[rownames(res_for_model) %in% res_for_graph_miss$taxa[i], ]$S
  res_for_graph_miss$trait[i] <- res_for_model[rownames(res_for_model) %in% res_for_graph_miss$taxa[i], ]$Nb_trait
}


res_for_graph_miss$taxa <- factor(res_for_graph_miss$taxa, 
                                  levels  = unique(res_for_graph_miss$taxa[order(res_for_graph_miss$SP)]), 
                                  ordered = TRUE)

res_for_graph_miss <- res_for_graph_miss[order(res_for_graph_miss$taxa, decreasing = FALSE), ]



p2 <- ggplot(res_for_graph_miss, aes(x = miss_percent * 100, y = AUC,
                                     fill = as.factor(miss_percent * 100))) + 
  geom_boxplot() +
  theme_bw() +
  labs(x = "Traits omission (%)", 
       y = "Quality of species trait space (AUC)", size = 14) +
  harrypotter::scale_fill_hp_d(option = "ronweasley2",direction = -1)+
  facet_wrap(~ taxa, ncol = 6) + 
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title.x     = element_text( size=14, face="bold"),
        axis.title.y     = element_text( size=14, face="bold"),
        legend.position  = "none")



bdd <- "eallonardo_2013"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b1  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 60, xmax = 85, ymin = 0.68, ymax = 0.9, 
                          data = subdata)


bdd <- "chmura_2016"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b2  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)

bdd <- "villeger_2012"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b3  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)

bdd <- "pavoine_2011"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b4  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)

bdd <- "carvalho_2015"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b5  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)

bdd <- "ribera_2001"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b6  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)
bdd <- "charbonnier_2016"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b7  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)
bdd <- "fried_2012"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b8  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)

bdd <- "gibb_2015"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b9  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                          data = subdata)

bdd <- "goncalves_2014"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b10  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "jeliazkov_2013"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b11  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "yates_2014"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b12  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "bartonova_2016"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b13  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "north_sea_traits"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b14  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "pakeman_2011"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b15  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "clearly_2016"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b16  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "krasnov_2015"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b17  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "diaz_2008"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b18  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "nz_insect"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b19  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "thermal_fauna"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b20  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "coral_traits"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b21  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "bacteria"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b22  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "chondrichtyens_traits"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b23  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "phyto_plankton"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b24  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "usda_plant_traits"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b25  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "palm_traits"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b26  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "plant_alps"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b27  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "mammals_trait"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b28  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "fresh_fish_trait"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b29  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)

bdd <- "birds_trait"
subdata <- res_for_graph_miss[res_for_graph_miss$taxa == bdd, ]
b30  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=60, xmax=85, ymin=0.68, ymax=0.9, 
                           data = subdata)


# b7 = annotation_custom2(rasterGrob(all_im$Bartonova2016, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[7],])
# b8 = annotation_custom2(rasterGrob(all_im$NorthSeaTraits, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[8],])
# b9 = annotation_custom2(rasterGrob(all_im$Pakeman2011, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[9],])
# b10 = annotation_custom2(rasterGrob(all_im$Clearly2016, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[10],])
# b11 = annotation_custom2(rasterGrob(all_im$Flua, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[11],])
# b12 = annotation_custom2(rasterGrob(all_im$Diaz2008, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[12],])
# b13 = annotation_custom2(rasterGrob(all_im$InvertebrateNZ, interpolate=TRUE), xmin=65, xmax=85, ymin=0.71, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[13],])
# b14 = annotation_custom2(rasterGrob(all_im$ThermalFauna, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[14],])
# b15 = annotation_custom2(rasterGrob(all_im$coral, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[15],])
# b16 = annotation_custom2(rasterGrob(all_im$bacteria, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[16],])
# b17 = annotation_custom2(rasterGrob(all_im$Chondri, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[17],])
# b18 = annotation_custom2(rasterGrob(all_im$phytoplankton, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[18],])
# b19 = annotation_custom2(rasterGrob(all_im$USDA_plant, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[19],])
# b20 = annotation_custom2(rasterGrob(all_im$PalmTraits, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[20],])
# b21 = annotation_custom2(rasterGrob(all_im$plant_alps, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[21],])
# b22 = annotation_custom2(rasterGrob(all_im$mammalstrait, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[22],])
# b23 = annotation_custom2(rasterGrob(all_im$freshfish, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[23],])
# b24 = annotation_custom2(rasterGrob(all_im$birds, interpolate=TRUE), xmin=60, xmax=85, ymin=0.68, ymax=0.9, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[24],])


grDevices::pdf(file = here::here("figures", "Figure5.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4

print(p2 + b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + 
 b11 + b12 + b13 + b14 + b15 + b16  + b17 + b18 + b19 + b20 + 
 b21 + b22 + b23 + b24 + b25 + b26  + b27 + b28 + b29 + b30)
dev.off()
