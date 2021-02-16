#' FIGURE 4



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



## MODEL NUMBER OF GROUPS ----



mod_logNC <- stats::lm(logNC ~ logS + logNT + NA_perc + quanti_perc + mean_cor, 
                data = res_for_model, na.action = "na.omit")

aov_logNC <- data.frame(car::Anova(mod_logNC))
aov_logNC <- data.frame(term = rownames(aov_logNC), aov_logNC)
aov_logNC <- aov_logNC [ , -3]
aov_logNC <- aov_logNC [-6, ]

aov_logNC $term <- c("log(Number of Species)", "log(Number of Traits)", 
                     "Percentage of NA", "% Quantitative Variables", 
                     "Correlation")

colnames(aov_logNC) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_logNC)

var_vis <- names(step$coefficients[-1])



S_plot <- visreg::visreg(mod_logNC, "logS", scale = "response", partial = TRUE,
                         xlab = " ", ylab = "Number of clusters (log)", 
                         gg = TRUE, line = list(col = "#3D7688"), 
                         fill = list(fill = "#D6EBEC", alpha = 0.5), 
                         points = list(size = 2, col = "#3D7688"))  + theme_bw() + 
 scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) + 
 expand_limits(y = c(0, 2.8)) + 
 theme(axis.text.x = element_blank(), axis.title.y = element_text(face = "bold"))


NT_plot <- visreg::visreg(mod_logNC, "logNT", scale = "response", partial = TRUE, 
                          xlab = " ", ylab = " ", gg = TRUE, line = list(col = "gray78"), 
                          fill = list(fill = "gray90", alpha = 0.5), 
                          points = list(size = 2, col = "gray78"))  + theme_bw() + 
 scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) + 
 expand_limits(y = c(0, 2.8)) + 
 theme(axis.text.y = element_blank()) + 
 theme(axis.text.x = element_blank())

NA_plot <- visreg::visreg(mod_logNC, "NA_perc", scale = "response", partial = TRUE, 
                          xlab = " ", ylab = " ", gg = TRUE, line = list(col = "#3D7688"), 
                          fill = list(fill = "#D6EBEC", alpha = 0.5), 
                          points = list(size = 2, col = "#3D7688"))  + theme_bw() + 
 scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) + 
 expand_limits(y = c(0, 2.8)) + 
 theme(axis.text.y = element_blank()) + 
 theme(axis.text.x = element_blank())

Quanti_plot <- visreg::visreg(mod_logNC, "quanti_perc", scale = "response", partial = TRUE, 
                              xlab = " ", ylab = " ", gg = TRUE, line = list(col = "gray78"), 
                              fill = list(fill = "gray90", alpha = 0.5), 
                              points = list(size = 2, col = "gray78"))  + theme_bw() + 
 scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) + 
 expand_limits(y = c(0, 2.8)) + 
 theme(axis.text.y = element_blank()) + 
 theme(axis.text.x = element_blank())

cor_plot <- visreg::visreg(mod_logNC, "mean_cor", scale = "response", partial = TRUE, 
                           xlab = " ", ylab = " ", gg = TRUE, line = list(col = "gray78"), 
                           fill = list(fill = "gray90", alpha = 0.5), 
                           points = list(size = 2, col = "gray78"))  + theme_bw() + 
 scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) + 
 expand_limits(y = c(0, 2.8)) + 
 theme(axis.text.y = element_blank()) + 
 theme(axis.text.x = element_blank())

plot_logNC <- gridExtra::grid.arrange(S_plot, NT_plot, NA_plot, Quanti_plot, cor_plot, ncol = 5)



## MODEL CLUSTER 1 ----



mod_PropC1 <- stats::lm(PropC1 ~ logS + logNT + NA_perc + quanti_perc + mean_cor, 
                        data = res_for_model, na.action = "na.omit")

aov_PropC1 <- data.frame(car::Anova(mod_PropC1))
aov_PropC1 <- data.frame(term = rownames(aov_PropC1), aov_PropC1)
aov_PropC1 <- aov_PropC1 [ , -3]
aov_PropC1 <- aov_PropC1 [-6, ]

aov_PropC1$term <- c("log(Number of Species)", "log(Number of Traits)", 
                     "Percentage of NA", "% Quantitative Variables", 
                     "Correlation")
colnames(aov_PropC1) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_PropC1)

var_vis <- names(step$coefficients[-1])



S_plot <- visreg::visreg(mod_PropC1, "logS", scale = "response", partial = TRUE, 
                         xlab = "", ylab = "% Cluster #1", 
                         gg = TRUE, line = list(col = "#3D7688"), 
                         fill = list(fill = "#D6EBEC", alpha = 0.5), 
                         points = list(size = 2, col = "#3D7688")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 1)) + 
  theme(axis.text.x = element_blank(), axis.title.y = element_text(face = "bold"))

NT_plot <- visreg::visreg(mod_PropC1, "logNT", scale = "response", partial = TRUE, 
                          xlab = " ", ylab = " ", gg = TRUE, line = list(col = "gray78"), 
                          fill = list(fill = "gray90", alpha = 0.5), 
                          points = list(size = 2, col = "gray78")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 1)) + 
  theme(axis.text.y = element_blank()) + 
  theme(axis.text.x = element_blank())

NA_plot <- visreg::visreg(mod_PropC1, "NA_perc", scale = "response", partial = TRUE, 
                          xlab = " ", ylab = " ", gg = TRUE, line = list(col = "#3D7688"), 
                          fill = list(fill = "#D6EBEC", alpha = 0.5), 
                          points = list(size = 2, col = "#3D7688")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 1)) + 
  theme(axis.text.y = element_blank()) + 
  theme(axis.text.x = element_blank())

Quanti_plot <- visreg::visreg(mod_PropC1, "quanti_perc", scale = "response", partial = TRUE, 
                              xlab = " ", ylab = " ", gg = TRUE, line = list(col = "gray78"), 
                              fill = list(fill = "gray90", alpha = 0.5), 
                              points = list(size = 2, col = "gray78")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 1)) + 
  theme(axis.text.y = element_blank()) + 
  theme(axis.text.x = element_blank())


cor_plot <- visreg::visreg(mod_PropC1, "mean_cor", scale = "response", partial = TRUE, 
                           xlab = " ", ylab = " ", gg = TRUE, line = list(col = "gray78"), 
                           fill = list(fill = "gray90", alpha = 0.5), 
                           points = list(size = 2, col = "gray78")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 1)) + 
  theme(axis.text.y = element_blank()) + 
  theme(axis.text.x = element_blank())

plot_PropC1 <- gridExtra::grid.arrange(S_plot, NT_plot, NA_plot, Quanti_plot, cor_plot, ncol = 5)



## MODEL SINGLETON ----



mod_PropSin <- stats::lm(PropSin ~ logS + logNT + NA_perc + quanti_perc + mean_cor, 
                         data = res_for_model, na.action = "na.omit")

aov_PropSin <- data.frame(car::Anova(mod_PropSin))
aov_PropSin <- data.frame(term = rownames(aov_PropSin), aov_PropSin)
aov_PropSin <- aov_PropSin [ , -3]
aov_PropSin <- aov_PropSin [-6, ]

aov_PropSin $term <- c("log(Number of Species)", "log(Number of Traits)", 
                       "Percentage of NA", "% Quantitative Variables", 
                       "Correlation")
colnames(aov_PropSin) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_PropSin)

var_vis <- names(step$coefficients[-1])



S_plot <- visreg::visreg(mod_PropSin, "logS", scale = "response", partial = TRUE, 
                         ylab = "% uniques", xlab = "Number of Species (log)", 
                         gg = TRUE, line = list(col = "#3D7688"), 
                         fill = list(fill = "#D6EBEC", alpha = 0.5), 
                         points = list(size = 2, col = "#3D7688")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 0.8)) + 
  theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

NT_plot <- visreg::visreg(mod_PropSin, "logNT", scale = "response", partial = TRUE, 
                          ylab = " ", xlab = "Number of Traits (log)", 
                          gg = TRUE, line = list(col = "gray78"), 
                          fill = list(fill = "gray90", alpha = 0.5), 
                          points = list(size = 2, col = "gray78")) +
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 0.8)) + 
  theme(axis.text.y = element_blank(), axis.title.x = element_text(face = "bold"))

NA_plot <- visreg::visreg(mod_PropSin, "NA_perc", scale = "response", partial = TRUE, 
                          ylab = " ", xlab = "% of Missing Values", 
                          gg = TRUE, line = list(col = "#3D7688"), 
                          fill = list(fill = "#D6EBEC", alpha = 0.5), 
                          points = list(size = 2, col = "#3D7688")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 0.8)) + 
  theme(axis.text.y = element_blank(), axis.title.x = element_text(face = "bold"))

Quanti_plot <- visreg::visreg(mod_PropSin, "quanti_perc", scale = "response", partial = TRUE, 
                              ylab = " ", xlab = "% Quantitative Variables", 
                              gg = TRUE, line = list(col = "gray78"), 
                              fill = list(fill = "gray90", alpha = 0.5), 
                              points = list(size = 2, col = "gray78")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 0.8)) + 
  theme(axis.text.y = element_blank(), axis.title.x = element_text(face = "bold"))

cor_plot <- visreg::visreg(mod_PropSin, "mean_cor", scale = "response", partial = TRUE, 
                           ylab = " ", xlab = "Mean Correlation", 
                           gg = TRUE, line = list(col = "gray78"), 
                           fill = list(fill = "gray90", alpha = 0.5), 
                           points = list(size = 2, col = "gray78")) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  expand_limits(y = c(-0.05, 0.8)) + 
  theme(axis.text.y = element_blank(), axis.title.x = element_text(face = "bold"))

plot_PropSin <- gridExtra::grid.arrange(S_plot, NT_plot, NA_plot, Quanti_plot, cor_plot, 
                                        ncol = 5)


grDevices::pdf(file = here::here("figures", "Figure4.pdf"), 
               width = 11.7, height = 8.3)

structure_plot <- gridExtra::grid.arrange(plot_logNC, plot_PropC1, plot_PropSin, 
                                          nrow = 3, ncol = 1) 
dev.off()
