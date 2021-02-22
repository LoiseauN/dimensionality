#' FIGURE 8


## Load Data ----

load(file = here::here("outputs", "res_for_model.RData"))

# Transform Variables ----

logS    <- log10(res_for_model$S)
logNT   <- log10(res_for_model$Nb_trait)
logNS   <- log10(res_for_model$Nb_single)
PropSin <- res_for_model$Nb_single / res_for_model$S
logNC   <- log10(res_for_model$Nb_cluster)
log1C   <- log10(res_for_model$NbS_Cluster1)
PropC1  <- res_for_model$NbS_Cluster1 / res_for_model$S
FR      <- res_for_model$S / res_for_model$Nb_cluster

res_for_model <- cbind(res_for_model, logS, logNT, logNS, logNC, log1C, PropSin,
                       PropC1, FR)


# Variables to Model ----

var_to_mod <- c("Nb_dim_AUC_elbow", "Nb_dim_AUC_0.7", 
                "rowAUClostwhen50percTraitdepleted",
                "rowAUClostwhen20percTraitdepleted", "logNS", "PropSin")



# Raw Relation ----
# Log for Structure ----

a <- ggplot(data = res_for_model, aes(logS, logNC)) +
  geom_point(size = 2, col = "#3D7688") +
  theme_bw() +
  stat_smooth(method = "lm", size = 1, se = TRUE, color = "#3D7688", 
              fill = "#D6EBEC") + 
  labs(y = "Number of clusters (log)" , x = " ") + 
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x  = element_blank()) +
  
  geom_label(data = res_for_model, 
             aes(label = paste0("Slope = ", 
                                round(lm(logNC ~ logS)$coefficients[2], 2)),
                 y = 3.8, x = 1), size = 4, hjust = 0) +
  
  expand_limits(x = c(1, 4), y = c(1, 4)) +
  geom_abline(slope = 1, intercept = 0)

b <- ggplot(data = res_for_model, aes(logS, log1C)) +
  geom_point(size = 2, col = "#3D7688") +
  theme_bw() +
  stat_smooth(method = "lm",  size = 1, se = TRUE, color = "#3D7688",
              fill = "#D6EBEC") + 
  labs(y = "Number of species in cluster #1 (log)" , x = " ") + 
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x  = element_blank()) +
  
  geom_label(data = res_for_model, 
             aes(label = paste0("Slope = ",
                                round(lm(log1C ~ logS)$coefficients[2], 2)),
                 y = 3.8, x = 1), size = 4, hjust = 0)+
  
  expand_limits(x = c(1, 4), y = c(1, 4)) +
  geom_abline(slope = 1, intercept = 0)

c <- ggplot(data = res_for_model, aes(logS, logNS)) +
  geom_point(size = 2, col = "#3D7688") +
  theme_bw() +
  stat_smooth(method = "lm",  size = 1, se = TRUE, color = "#3D7688",
              fill = "#D6EBEC") + 
  labs(y = "Number of uniques (log)", x = "Number of species (log)") + 
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold")) +
  geom_label(data = res_for_model,
             aes(label = paste0("Slope = ", 
                                round(lm(logNS ~ logS)$coefficients[2], 2)),
                 y = 3.8, x = 1), size = 4, hjust = 0) +
  
  expand_limits(x = c(1, 4), y = c(1, 4)) +
  geom_abline(slope = 1, intercept = 0)

myplot1 <- gridExtra::arrangeGrob(a, top = textGrob("(a)", 
                                                    x    = unit(1, "npc"), 
                                                    y    = unit(1, "npc"), 
                                                    just = c("right", "top"),
                                                    gp   = gpar(col = "black", fontsize = 18)))

myplot2 <- gridExtra::arrangeGrob(b, top = textGrob("(b)", 
                                                    x    = unit(1, "npc"), 
                                                    y    = unit(1, "npc"), 
                                                    just = c("right", "top"),
                                                    gp   = gpar(col = "black", fontsize = 18)))

myplot3 <- gridExtra::arrangeGrob(c, top = textGrob("(c)", 
                                                    x    = unit(1, "npc"), 
                                                    y    = unit(1, "npc"), 
                                                    just = c("right", "top"),
                                                    gp   = gpar(col = "black", fontsize = 18)))

grDevices::pdf(file = here::here("figures", "Figure8.pdf"), 
               width = 5, height = 8)

gridExtra::grid.arrange(myplot1, myplot2, myplot3, ncol = 1)

dev.off()



#' ALTERNATIVE FIGURE



d <- ggplot(data = res_for_model, aes(logS, logNT)) +
  geom_point(size = 2, col = "#3D7688") +
  theme_bw() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = TRUE, 
              color = "#3D7688", fill = "#D6EBEC") +
  labs(y="Number of groups", x = "") + 
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x  = element_blank())

e <- ggplot(data = res_for_model, aes(logS, log1C)) +
  geom_point(size = 2, col = "#3D7688") +
  theme_bw() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = TRUE, 
              color = "#3D7688", fill = "#D6EBEC") +
  labs(y = "Number of species in cluster 1", x = "") + 
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x  = element_blank())

f <- ggplot(data = res_for_model, aes(logS, logNS)) +
  geom_point(size = 2, col = "#3D7688") +
  theme_bw() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = TRUE, 
              color = "#3D7688", fill = "#D6EBEC") +
  labs(y = "Number of sigletons", x = "Number of species") + 
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"))

grDevices::pdf(file = here::here("figures", "Figure8bis.pdf"), 
               width = 5, height = 8)

gridExtra::grid.arrange(d, e, f, ncol = 1)

dev.off()
