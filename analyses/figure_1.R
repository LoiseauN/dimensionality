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

bdd <- "chmura_2016"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a2  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "villeger_2012"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a3  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "pavoine_2011"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a4  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "carvalho_2015"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a5  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "ribera_2001"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a6  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)
bdd <- "charbonnier_2016"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a7  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)
bdd <- "fried_2012"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a8  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "gibb_2015"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a9  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "goncalves_2014"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a10  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "jeliazkov_2013"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a11  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "yates_2014"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a12  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "bartonova_2016"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a13  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "north_sea_traits"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a14  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "pakeman_2011"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a15  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "clearly_2016"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a16  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "krasnov_2015"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a17  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "diaz_2008"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a18  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "nz_insect"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a19  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                          data = subdata)

bdd <- "thermal_fauna"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a20  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "coral_traits"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a21  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "bacteria"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a22  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "chondrichtyens_traits"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a23  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "phyto_plankton"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a24  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "usda_plant_traits"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a25  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "palm_traits"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a26  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "plant_alps"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a27  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "mammals_trait"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a28  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "fresh_fish_trait"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a29  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

bdd <- "birds_trait"
subdata <- res_for_graph_dim[res_for_graph_dim$taxa == bdd, ]
a30  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin = 15, xmax = 21, ymin = 0.01, ymax = 0.34, 
                           data = subdata)

grDevices::pdf(file = here::here("figures", "Figure1.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4

print(p + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + 
 a11 + a12 + a13 + a14 + a15 + a16  + a17 + a18 + a19 + a20 + 
 a21 + a22 + a23 + a24 + a25 + a26  + a27 + a28 + a29 + a30)
dev.off()
