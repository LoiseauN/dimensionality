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



## MODEL AUC ELBOW ----



mod_AUC_elbow <- stats::lm(Nb_dim_AUC_elbow ~ logS + logNT + NA_perc + quanti_perc + mean_cor, 
                           data = res_for_model, na.action = "na.fail")

aov_AUC_elbow <- data.frame(car::Anova(mod_AUC_elbow))
aov_AUC_elbow <- data.frame(term = rownames(aov_AUC_elbow), aov_AUC_elbow)
aov_AUC_elbow <- aov_AUC_elbow[ , -3]
aov_AUC_elbow <- aov_AUC_elbow[-6, ]

aov_AUC_elbow$term <- c("log(Number of Species)", "log(Number of Traits)",
                        "Percentage of NA", "% Quantitative Variables",
                        "Correlation")

colnames(aov_AUC_elbow) <- c("Term","Sum.Sq","F-statistic","P.value")

step <- MASS::stepAIC(mod_AUC_elbow)

var_vis <- names(step$coefficients[-1])



S_plot <- visreg::visreg(mod_AUC_elbow, "logS", scale = "response", partial = TRUE,
                         ylab = "Dimensionality AUC Elbow",
                         gg = TRUE, line = list(col = "gray78"),
                         fill = list(fill = "gray90", alpha = 0.5),
                         points = list(size = 2, col = "gray78")) + 
  theme_bw() +
  xlab(" ") +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold")) +
  scale_y_continuous(breaks = seq(1, 9, 2)) +
  expand_limits(y = c(1, 9))

NT_plot <- visreg::visreg(mod_AUC_elbow, "logNT", scale = "response", partial = TRUE,
                          gg = TRUE, line = list(col = "gray78"),
                          fill = list(fill = "gray90", alpha = 0.5),
                          points = list(size = 2, col = "gray78")) +
  theme_bw() + xlab(" ") + ylab(" ") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(1, 9, 2)) +
  expand_limits(y = c(1, 9)) +
  theme(axis.text.y = element_blank())

NA_plot <- visreg::visreg(mod_AUC_elbow, "NA_perc", scale = "response", partial = TRUE,
                          gg = TRUE, line = list(col = "gray78"),
                          fill = list(fill = "gray90", alpha = 0.5),
                          points = list(size = 2, col = "gray78")) +
  theme_bw() + xlab(" ") + ylab(" ") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(1, 9, 2)) +
  expand_limits(y = c(1, 9)) +
  theme(axis.text.y = element_blank())

Quanti_plot <- visreg::visreg(mod_AUC_elbow, "quanti_perc", scale = "response", partial = TRUE,
                              gg = TRUE, line = list(col = "gray78"),
                              fill = list(fill = "gray90", alpha = 0.5),
                              points = list(size = 2, col = "gray78")) +
  theme_bw() + xlab(" ") + ylab(" ") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(1, 9, 2)) +
  expand_limits(y = c(1, 9)) +
  theme(axis.text.y = element_blank())

cor_plot <- visreg::visreg(mod_AUC_elbow, "mean_cor", scale = "response", partial = TRUE,
                           gg = TRUE, line = list(col = "gray78"),
                           fill = list(fill = "gray90", alpha = 0.5),
                           points = list(size = 2, col = "gray78")) +
  theme_bw() + xlab(" ") + ylab(" ") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(1, 9, 2)) +
  expand_limits(y = c(1, 9)) +
  theme(axis.text.y = element_blank())

plot_AUC_elbow <- gridExtra::grid.arrange(S_plot, NT_plot, NA_plot, Quanti_plot,
                                          cor_plot, ncol = 5)



## MODEL AUC 0.7 ----



mod_AUC_0.7 <- stats::lm(Nb_dim_AUC_0.7 ~ logS + logNT + NA_perc + quanti_perc + mean_cor,
                         data = res_for_model, na.action = "na.omit")

aov_AUC_0.7 <- data.frame(car::Anova(mod_AUC_0.7))
aov_AUC_0.7 <- data.frame(term = rownames(aov_AUC_0.7), aov_AUC_0.7)
aov_AUC_0.7 <- aov_AUC_0.7[ , -3]
aov_AUC_0.7 <- aov_AUC_0.7[-6, ]

aov_AUC_0.7$term <- c("log(Number of Species)", "log(Number of Traits)", 
                      "Percentage of NA", "% Quantitative Variables",
                      "Correlation")

colnames(aov_AUC_0.7) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_AUC_0.7)

var_vis <- names(step$coefficients[-1])



S_plot <- visreg::visreg(mod_AUC_0.7, "logS", scale = "response", partial = TRUE, 
                         xlab = " ", ylab = "Dimensionality AUC 0.7", gg = TRUE, 
                         line = list(col = "gray78"), 
                         fill = list(fill = "gray90", alpha = 0.5), 
                         points = list(size = 2, col = "gray78")) + 
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold"), axis.text.x = element_blank()) +
  scale_y_continuous(breaks = seq(1, 18, 3)) +
  expand_limits(y = c(1, 18))

NT_plot <- visreg::visreg(mod_AUC_0.7, "logNT", scale = "response", partial = TRUE, 
                          xlab = " ", gg = TRUE, line = list(col = "#3D7688"), 
                          fill = list(fill = "#D6EBEC", alpha = 0.5), 
                          points = list(size = 2, col = "#3D7688")) + 
  theme_bw() + ylab(" ") +
  theme(axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(1, 18, 3)) +
  expand_limits(y = c(1, 18)) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

NA_plot <- visreg::visreg(mod_AUC_0.7, "NA_perc", scale = "response", partial = TRUE, 
                          xlab = " ", gg = TRUE, line = list(col = "gray78"), 
                          fill = list(fill = "gray90", alpha = 0.5), 
                          points = list(size = 2, col = "gray78")) + 
  theme_bw() + ylab(" ") +
  theme(axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(1, 18, 3)) +
  expand_limits(y = c(1, 18)) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

Quanti_plot <- visreg::visreg(mod_AUC_0.7, "quanti_perc", scale = "response", partial = TRUE, 
                              xlab = " ", gg = TRUE, line = list(col = "gray78"), 
                              fill = list(fill = "gray90", alpha = 0.5), 
                              points = list(size = 2, col = "gray78")) + 
  theme_bw() + ylab(" ") +
  theme(axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(1, 18, 3)) +
  expand_limits(y = c(1, 18)) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

cor_plot <- visreg::visreg(mod_AUC_0.7, "mean_cor", scale = "response", partial = TRUE, 
                           xlab = " ", 
                           gg = TRUE, line = list(col = "#3D7688"), 
                           fill = list(fill = "#D6EBEC", alpha = 0.5), 
                           points = list(size = 2, col = "#3D7688")) + 
  theme_bw() + ylab(" ") +
  scale_y_continuous(breaks = seq(1, 18, 3)) +
  expand_limits(y = c(1, 18)) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

plot_AUC_0.7 <- gridExtra::grid.arrange(S_plot, NT_plot, NA_plot, Quanti_plot, 
                                        cor_plot, ncol = 5)



## MODEL ROW AUC LOST WHEN 50% TRAIT DEPLETED ----



mod_AUClostwhen50percTraitdepleted <- stats::lm(rowAUClostwhen50percTraitdepleted ~ logS + logNT + NA_perc + quanti_perc + mean_cor, 
                                                data = res_for_model, na.action = "na.omit")

aov_AUClostwhen50percTraitdepleted <- data.frame(car::Anova(mod_AUClostwhen50percTraitdepleted))
aov_AUClostwhen50percTraitdepleted <- data.frame(term = rownames(aov_AUClostwhen50percTraitdepleted), aov_AUClostwhen50percTraitdepleted)
aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[ , -3]
aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[-6, ]

aov_AUClostwhen50percTraitdepleted$term <- c("log(Number of Species)", "log(Number of Traits)", 
                                             "Percentage of NA", "% Quantitative Variables", 
                                             "Correlation")

colnames(aov_AUClostwhen50percTraitdepleted) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_AUClostwhen50percTraitdepleted)

var_vis <- names(step$coefficients[-1])



S_plot <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "logS", 
                         scale = "response", partial = TRUE, 
                         xlab = "Number of Species (log)", 
                         ylab = "AUC loss - 50% traits omission", 
                         gg = TRUE, line = list(col = "gray78"), 
                         fill = list(fill = "gray90", alpha = 0.5), 
                         points = list(size = 2, col = "gray78")) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1) ) +
  expand_limits(y = c(0.2, 1)) +
  theme(axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))

NT_plot <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "logNT", 
                          scale = "response", partial = TRUE, 
                          xlab = "Number of Traits (log)", ylab = " ", 
                          gg = TRUE, line = list(col = "#3D7688"), 
                          fill = list(fill = "#D6EBEC", alpha = 0.5), 
                          points = list(size = 2, col = "#3D7688")) + 
  theme_bw() +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1) ) +
  expand_limits(y = c(0.2, 1)) +
  theme(axis.text.y = element_blank(), 
        axis.title.x  = element_text( face = "bold"))

NA_plot <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "NA_perc", 
                          scale = "response", partial = TRUE, 
                          xlab = "% of Missing Values", ylab = " ", 
                          gg = TRUE, line = list(col = "#3D7688"), 
                          fill = list(fill = "#D6EBEC", alpha = 0.5), 
                          points = list(size = 2, col = "#3D7688")) + 
  theme_bw() +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1) ) +
  expand_limits(y = c(0.2, 1)) +
  theme(axis.text.y  = element_blank(), 
        axis.title.x = element_text( face = "bold"))

Quanti_plot <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "quanti_perc", 
                              scale = "response", partial = TRUE, 
                              xlab = "% Quantitative Variables", ylab = " ", 
                              gg = TRUE, line = list(col = "gray78"), 
                              fill = list(fill = "gray90", alpha = 0.5), 
                              points = list(size = 2, col = "gray78")) + 
  theme_bw() +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1) ) +
  expand_limits(y = c(0.2, 1)) +
  theme(axis.text.y  = element_blank(), 
        axis.title.x = element_text( face = "bold"))

cor_plot <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "mean_cor", 
                           scale = "response", partial = TRUE, 
                           xlab = "Mean Correlation", ylab = " ", 
                           gg = TRUE, line = list(col = "#3D7688"), 
                           fill = list(fill = "#D6EBEC", alpha = 0.5), 
                           points = list(size = 2, col = "#3D7688")) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1)) +
  expand_limits(y = c(0.2, 1)) +
  theme(axis.text.y  = element_blank(), 
        axis.title.x = element_text( face = "bold"))

plot_AUClostwhen50percTraitdepleted <- gridExtra::grid.arrange(S_plot, NT_plot, NA_plot, 
                                                               Quanti_plot, cor_plot, 
                                                               ncol = 5)


grDevices::pdf(file = here::here("figures", "Figure4.pdf"), 
               width = 11.7, height = 8.3)

Dimensionality_plot <- gridExtra::grid.arrange(plot_AUC_elbow, plot_AUC_0.7,
                                               plot_AUClostwhen50percTraitdepleted, 
                                               nrow = 3, ncol = 1)
dev.off()



#To make a table summarizing all models for supplementary
#aov_table_df <- rbind(aov_AUC_elbow, aov_AUC_0.7,aov_AUClostwhen50percTraitdepleted)
#aov_table_df <- data.frame(Variables = c(rep("Dimensionality AUC Elbow",5),rep("Dimensionality AUC 0.7",5)
#                                         ,rep("AUC - 50% traits omission",5)),aov_table_df)

#aov_table_df[aov_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
#aov_table_df[aov_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
#aov_table_df[aov_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
#aov_table_df[aov_table_df$Term=="Correlation",]$Term <- "Mean Correlation"
#aov_table_df <- aov_table_df %>% dplyr::mutate_at(vars("Sum.Sq","F.statistic","P.value",), dplyr::funs(round(., 3)))

#for(i in 1:nrow(aov_table_df)){ 
#  if(aov_table_df[i, 5]<0.001 )    { 
#    aov_table_df[i, 5] <- "<0.001"
#    aov_table_df[i, 5] <- kableExtra::cell_spec(aov_table_df[i, 5],  bold = T)
#  } 
  
#  if(aov_table_df[i, 5]<0.05 & aov_table_df[i, 5]>0.001)     {  
#    aov_table_df[i, 5] <- kableExtra::cell_spec(aov_table_df[i, 5],  bold = T)
#  } 
#}

#table_mod_aov<-pixiedust::dust(aov_table_df) %>% 
# kableExtra::kable( booktabs = T, escape = F)%>% 
# kableExtra::kable_styling()%>% 
# kableExtra::collapse_rows()

# table_mod_aov
