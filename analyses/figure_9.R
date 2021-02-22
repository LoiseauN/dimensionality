#' FIGURE 6


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

files <- list.files(path = here::here("outputs"), pattern = "_single.rds$",
                    full.names = TRUE)
list_res_single <- lapply(files, function(x) readRDS(x))

files <- list.files(path = here::here("outputs"), pattern = "_pcoa.rds$",
                    full.names = TRUE)
list_res_pcoa  <- lapply(files, function(x) readRDS(x))


res_for_graph_single <- na.omit(data.frame(do.call(rbind, lapply(1:length(list_res_single),
                                                                 function(i) {
  res_single <- data.frame(list_res_single[[i]]$cluster_core)
  
  # cluster_core = 1 === singleton
  res_single[res_single[ , 1] >  0, ] <- 2
  res_single[res_single[ , 1] == 0, ] <- 1
  res_single[res_single[ , 1] >  1, ] <- 0
  
  res_single$taxa <- gsub("_res\\.rds", "", filenames)[i]
  
  res_pcoa <- data.frame(list_res_pcoa[[i]]$vectors)[ , c(1:3)]
  
  res_cluster <- list_res_single[[i]]$cluster_core
  res <- cbind(res_single, res_pcoa, res_cluster)
  colnames(res) <- c("Single","taxa", "Pcoa1", "Pcoa2", "Pcoa3","cluster_ID")
  
  return(res)
}))))


res_for_graph_single$SP    <- NA 
res_for_graph_single$trait <- NA 

for (i in 1:nrow(res_for_graph_single)) { 
  
  res_for_graph_single$SP[i]    <- res_for_model[rownames(res_for_model) %in% res_for_graph_single$taxa[i], ]$S
  res_for_graph_single$trait[i] <- res_for_model[rownames(res_for_model) %in% res_for_graph_single$taxa[i], ]$Nb_trait
}


res_for_graph_single$taxa <- factor(res_for_graph_single$taxa, 
                                    levels  = unique(res_for_graph_single$taxa[order(res_for_graph_single$SP)]), 
                                    ordered = TRUE)
res_for_graph_single <-res_for_graph_single[order(res_for_graph_single$taxa, decreasing = FALSE),]

res_for_graph_single$Pcoa1  <- jitter(as.numeric(as.character(res_for_graph_single$Pcoa1)), factor = 50)
res_for_graph_single$Pcoa2  <- jitter(as.numeric(as.character(res_for_graph_single$Pcoa2)), factor = 50)
res_for_graph_single$Pcoa3  <- as.numeric(as.character(res_for_graph_single$Pcoa3))
res_for_graph_single$Single <- as.factor(as.character(res_for_graph_single$Single))

res_for_graph_single$cluster_ID[res_for_graph_single$cluster_ID != 1] <- 0


# cluster_core = 1 === singleton
p3 <- ggplot(res_for_graph_single, aes(x = Pcoa1, y = Pcoa2, colour = taxa)) + 
  geom_point(aes(alpha = Single, shape = Single), size = 0.7) + #
  scale_shape_manual(values = c(4, 16)) +
  scale_alpha_manual(values = c(0.3, 0.8)) +
  ggalt::geom_encircle(s_shape = 1, expand = 0,size = 3, alpha = 0.7, 
                       show.legend = FALSE) +
  theme_bw() +
  labs(x = "PCoA axis 1") +
  labs(y = "PCoA axis 2") +
  facet_wrap(~ taxa,ncol = 6, scales = "free") +
  harrypotter::scale_colour_hp_d(option = "LunaLovegood", direction = 1) +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        strip.text.y     = element_blank(),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position  = "none",
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.title.x     = element_text(size = 14, face = "bold"),
        axis.title.y     = element_text(size = 14, face = "bold"),
        axis.ticks       = element_blank())

hull  <- NULL
taxas <- unique(res_for_graph_single$taxa)

for (i in 1:length(taxas)) {
  sub <- res_for_graph_single[res_for_graph_single$taxa == taxas[i],]
  sub_hull <- sub[sub$cluster_ID == 1, ] %>%
    dplyr::slice(grDevices::chull(Pcoa1, Pcoa2)) 
  hull <- rbind(hull, sub_hull)
}


bdd <- "eallonardo_2013"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c1  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 0.20, xmax = 0.32, ymin = 0, ymax = 0.12, 
                          data = subdata)
bdd <- "chmura_2016"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c2  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=0.2, xmax=0.28, ymin=0.08, ymax=0.19, 
                          data = subdata)

bdd <- "villeger_2012"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c3  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=0.10, xmax=0.25, ymin=0.05, ymax=0.15, 
                          data = subdata)

bdd <- "pavoine_2011"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c4  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=0.2, xmax=0.3, ymin=0.18, ymax=0.28, 
                          data = subdata)

bdd <- "carvalho_2015"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c5  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin = 0.19, xmax = 0.32, ymin = 0.07, ymax = 0.17, 
                          data = subdata)

bdd <- "ribera_2001"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c6  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=0.21, xmax=0.32, ymin=0.15, ymax=0.3, 
                          data = subdata)
bdd <- "charbonnier_2016"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c7  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=0.2, xmax=0.3, ymin=0.2, ymax=0.3, 
                          data = subdata)
bdd <- "fried_2012"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c8  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=-0.35, xmax=-0.15, ymin=0.15, ymax=0.35, 
                          data = subdata)

bdd <- "gibb_2015"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c9  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=-0.6, xmax=-0.35, ymin=-0.25, ymax=-0.07, 
                          data = subdata)

bdd <- "goncalves_2014"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c10  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.1, xmax=0.3, ymin=-0.4, ymax=-0.25, 
                           data = subdata)

bdd <- "jeliazkov_2013"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c11  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.1, xmax=0.2, ymin=0.17, ymax=0.28, 
                           data = subdata)

bdd <- "yates_2014"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c12  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.22, xmax=0.45, ymin=0.05, ymax=0.2, 
                           data = subdata)

bdd <- "bartonova_2016"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c13  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.35, xmax=0.52, ymin=-0.25, ymax=-0.11, 
                           data = subdata)

bdd <- "north_sea_traits"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c14  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.17, xmax=0.32, ymin=0.1, ymax=0.25, 
                           data = subdata)

bdd <- "pakeman_2011"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c15  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=-0.3, xmax=-0.2, ymin=-0.3, ymax=-0.11, 
                           data = subdata)

bdd <- "clearly_2016"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c16  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.26, xmax=0.55, ymin=0.4, ymax=0.6, 
                           data = subdata)

bdd <- "krasnov_2015"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c17  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.14, xmax=0.25, ymin=-0.33, ymax=-0.1, 
                           data = subdata)

bdd <- "diaz_2008"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c18  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.18, xmax=0.32, ymin=0.07, ymax=0.18, 
                           data = subdata)

bdd <- "nz_insect"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c19  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.1, xmax=0.25, ymin=0.2, ymax=0.3, 
                           data = subdata)

bdd <- "thermal_fauna"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c20  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.35, xmax=0.6, ymin=0.2, ymax=0.5, 
                           data = subdata)

bdd <- "coral_traits"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c21  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.5, xmax=1, ymin=0.3, ymax=0.65, 
                           data = subdata)

bdd <- "bacteria"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c22  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.18, xmax=0.3, ymin=0.15, ymax=0.27, 
                           data = subdata)

bdd <- "chondrichtyens_traits"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c23  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.27, xmax=0.58, ymin=-0.05, ymax=0.2, 
                           data = subdata)

# Add whale sharks
c24 <- annotation_custom2(grid::rasterGrob(all_im$rhincodontypus, interpolate = TRUE), 
                          xmin=0.15, xmax=0.53, ymin=-1.00, ymax=0.08, 
                          data = res_for_graph_single[res_for_graph_single$taxa == unique(res_for_graph_single$taxa)[23], ])

#c25 <- annotation_custom2(my_arrow(), 
#                          xmin = 0.16, xmax = 0.55, ymin = -0.54, ymax = -0.54, 
#                          data = res_for_graph_single[res_for_graph_single$taxa == unique(res_for_graph_single$taxa)[23], ])


bdd <- "phyto_plankton"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c26  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=-0.05, xmax=-0.2, ymin=-0.35, ymax=-0.2, 
                           data = subdata)

bdd <- "usda_plant_traits"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c27  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.15, xmax=0.3, ymin=0.1, ymax=0.25, 
                           data = subdata)

bdd <- "palm_traits"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c28  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.25, xmax=0.5, ymin=0.25, ymax=0.45, 
                           data = subdata)

bdd <- "plant_alps"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c29  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.24, xmax=0.5, ymin=-0.45, ymax=-0.25, 
                           data = subdata)

bdd <- "mammals_trait"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c30  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=-0.3, xmax=-0.15, ymin=-0.3, ymax=-0.15, 
                           data = subdata)

bdd <- "fresh_fish_trait"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c31 <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                          xmin=-0.1, xmax=-0.25, ymin=-0.3, ymax=-0.2, 
                           data = subdata)

bdd <- "birds_trait"
subdata <- res_for_graph_single[res_for_graph_single$taxa == bdd, ]
c32  <- annotation_custom2(grid::rasterGrob(all_im[[bdd]], interpolate = TRUE), 
                           xmin=0.075, xmax=0.15, ymin=-0.13, ymax=-0.09, 
                           data = subdata)

grDevices::pdf(file = here::here("figures", "Figure6.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4

print(p3 + c1 + c2 + c3+    c4 + c5 + c6 +c7 + c8 + c9 + c10 + 
 c11 + c12 + c13 + c14 + c15 + c16 + c17 + c18 + c19 + c20 + 
 c21 + c22 + c23 + c24 + #c25 +
   c26 + c27 + c28 + c29 + c30 + 
 c31 + c32)
dev.off()
