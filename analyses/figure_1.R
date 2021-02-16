#' FIGURE 1


## Import Icons ----

paths <- list.files(path = here::here("data", "icons"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("data", "icons"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)


## Prepare Data ----

load(file = here::here("outputs", "res_for_model.RData"))

filenames <- list.files(path = here::here("outputs"), pattern = "_res.rds$",
                        full.names = FALSE)

files <- list.files(path = here::here("outputs"), pattern = "_res.rds$",
                    full.names = TRUE)
list_res <- lapply(files, function(x) readRDS(x))


res_for_graph_dim <- data.frame(do.call(rbind, lapply(1:length(list_res),
                                                      function(i) {
  res <- data.frame(list_res[[i]][[3]])
  res$taxa <- gsub("_res\\.rds", "", filenames)[i]
  return(res)
})))


res_for_graph_dim$SP    <- NA 
res_for_graph_dim$trait <- NA 

for (i in 1:nrow(res_for_graph_dim)){ 
  
  res_for_graph_dim$SP[i]    <- res_for_model[rownames(res_for_model) %in% res_for_graph_dim$taxa[i], ]$S
  res_for_graph_dim$trait[i] <- res_for_model[rownames(res_for_model) %in% res_for_graph_dim$taxa[i], ]$Nb_trait
  res_for_graph_dim$elbow[i] <- res_for_model[rownames(res_for_model) %in% res_for_graph_dim$taxa[i], ]$Nb_dim_AUC_elbow
}



res_for_graph_dim$selec_elbow_graph <- NA
res_for_graph_dim$AUCwhenelbow      <- NA

for (i in 1:nrow(res_for_graph_dim)) { 
  
  if (res_for_graph_dim$dim[i] == res_for_graph_dim$elbow[i]) {
    
    res_for_graph_dim$selec_elbow_graph[i] <- res_for_graph_dim$dim[i]
    res_for_graph_dim$AUCwhenelbow[i]      <- res_for_graph_dim$AUC[i]
  }
}


## Sort plots by number of species or traits ----

res_for_graph_dim$taxa <- factor(res_for_graph_dim$taxa, 
                                 levels = unique(res_for_graph_dim$taxa[order(res_for_graph_dim$SP)]), 
                                 ordered = TRUE)

res_for_graph_dim <- res_for_graph_dim[order(res_for_graph_dim$taxa, decreasing = FALSE), ]


## Plot ----

p <- ggplot(res_for_graph_dim, aes(x = dim, y = AUC, colour = taxa)) + 
  stat_summary(fun = "mean", geom = "line", size = 1, alpha = 0.4) +
  stat_summary(fun = "mean", size = 0.88) +
  
  labs(x = "Number of dimensions(PCoA axes)") + 
  labs(y = "Quality of species trait space (AUC)") +
  facet_wrap(~ taxa,ncol = 6) + theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        panel.background = element_blank(),
        legend.position = "none") +
  harrypotter::scale_colour_hp_d(option = "LunaLovegood") +
  
  geom_segment(data = plyr::ddply(res_for_graph_dim, "taxa", dplyr::summarize, wavg = AUC), 
               aes(x = res_for_graph_dim$selec_elbow_graph, 
                   xend = res_for_graph_dim$selec_elbow_graph,
                   y = 0 , yend = res_for_graph_dim$AUCwhenelbow),
               color = "black", linetype = "dotted", size = 1) +
  
  geom_segment(data = plyr::ddply(res_for_graph_dim, "taxa", dplyr::summarize, wavg = AUC), 
               aes(y = res_for_graph_dim$AUCwhenelbow,
                   yend = res_for_graph_dim$AUCwhenelbow ,
                   x = 0 , xend = res_for_graph_dim$selec_elbow_graph),
               color = "black", linetype = "dotted", size = 1) +
  
  geom_point(data = plyr::ddply(res_for_graph_dim, "taxa", dplyr::summarize, wavg = AUC), 
             aes(y = res_for_graph_dim$AUCwhenelbow,
                 x = res_for_graph_dim$selec_elbow_graph),
             color = "black", size = 4, shape = 19) +
  
  geom_label(data = res_for_graph_dim, 
             aes(label = paste0("Elbow-AUC = ", elbow, "\n", 
                                "#S = ", SP , "\n",
                                "#T = ", trait),
                 y = 0.5, x = 10), size = 2.1, hjust = 0) +
  
  scale_y_continuous(breaks = seq(0.1, 1, 0.2))

bdd <- "eallonardo_2013"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a1  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "ribera_2001"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a2  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "gibb_2015"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a3  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "goncalves_2014"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a4  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "jeliazkov_2013"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a5  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "yates_2014"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a6  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

# a7  = annotation_custom2(rasterGrob(all_im$Bartonova2016, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[7],])
# a8  = annotation_custom2(rasterGrob(all_im$NorthSeaTraits, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[8],])
# a9  = annotation_custom2(rasterGrob(all_im$Pakeman2011, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[9],])
# a10 = annotation_custom2(rasterGrob(all_im$Clearly2016, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[10],])
# a11 = annotation_custom2(rasterGrob(all_im$Flua, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[11],])
# a12 = annotation_custom2(rasterGrob(all_im$Diaz2008, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[12],])
# a13 = annotation_custom2(rasterGrob(all_im$InvertebrateNZ, interpolate=TRUE), xmin=15, xmax=20, ymin=0.01, ymax=0.31, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[13],])
# a14 = annotation_custom2(rasterGrob(all_im$ThermalFauna, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[14],])
# a15 = annotation_custom2(rasterGrob(all_im$coral, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[15],])
# a16 = annotation_custom2(rasterGrob(all_im$bacteria, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[16],])
# a17 = annotation_custom2(rasterGrob(all_im$Chondri, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[17],])
# a18 = annotation_custom2(rasterGrob(all_im$phytoplankton, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[18],])
# a19 = annotation_custom2(rasterGrob(all_im$USDA_plant, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[19],])
# a20 = annotation_custom2(rasterGrob(all_im$PalmTraits, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[20],])
# a21 = annotation_custom2(rasterGrob(all_im$plant_alps, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[21],])
# a22 = annotation_custom2(rasterGrob(all_im$mammalstrait, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[22],])
# a23 = annotation_custom2(rasterGrob(all_im$freshfish, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[23],])
# a24 = annotation_custom2(rasterGrob(all_im$birds, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[24],])

grDevices::pdf(file = here::here("figures", "Figure1.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4

print(p + a1 + a2 + a3 + a4 + a5 + a6) #+ a7 + a8 + a9 + a10 + 
# a11 + a12 + a13 + a14 + a15 + a16  + a17 + a18 + a19 + a20 + 
# a21 + a22 + a23 + a24
dev.off()
