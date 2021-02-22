#' FIGURE 3

## Influence of Species_Groups and Ecosystem_type ----

## Prepare Data ----

res_for_model_Species_Groups<-res_for_model
res_for_model_Species_Groups$Species_Groups <- c(NA,"Invertebrate","Vertebrate",
                                                 "Vertebrate","Vertebrate",
                                                 "Plant","Vertebrate",
                                                 "Vertebrate","Invertebrate",
                                                 "Invertebrate","Plant",
                                                 "Vertebrate","Plant",
                                                 "Invertebrate","Invertebrate",
                                                 "Invertebrate","Invertebrate",
                                                 "Vertebrate","Vertebrate",
                                                 "Invertebrate","Plant",
                                                 "Plant","Plant",
                                                 "Plant","Plant",
                                                 "Invertebrate","Invertebrate",
                                                 "Plant","Vertebrate","Invertebrate")

res_for_model_Species_Groups$Ecosystem_type <- c(NA,"Terrestrial","Terrestrial",
                                                 "Aquatic","Terrestrial",
                                                 "Terrestrial","Aquatic",
                                                 "Terrestrial","Aquatic",
                                                 "Terrestrial","Terrestrial",
                                                 "Aquatic","Terrestrial",
                                                 "Terrestrial","Terrestrial",
                                                 "Aquatic","Terrestrial",
                                                 "Terrestrial","Aquatic",
                                                 "Terrestrial","Terrestrial",
                                                 "Terrestrial","Terrestrial",
                                                 "Aquatic","Terrestrial",
                                                 "Terrestrial","Aquatic",
                                                 "Terrestrial","Aquatic","Terrestrial")

res_for_model_Species_Groups$Species_Groups <- factor(res_for_model_Species_Groups$Species_Groups,levels = c("Plant","Invertebrate","Vertebrate"))
mod_AUC_elbow <- stats::lm(Nb_dim_AUC_elbow ~ logS + logNT + NA_perc + quanti_perc + mean_cor + Species_Groups + Ecosystem_type , 
                           data = res_for_model_Species_Groups, na.action = "na.omit")

aov_AUC_elbow <- data.frame(car::Anova(mod_AUC_elbow))
aov_AUC_elbow <- data.frame(term = rownames(aov_AUC_elbow), aov_AUC_elbow)
aov_AUC_elbow <- aov_AUC_elbow[ , -3]
aov_AUC_elbow <- aov_AUC_elbow[-8, ]

aov_AUC_elbow$term <- c("log(Number of Species)", "log(Number of Traits)", 
                        "Percentage of NA", "% Quantitative Variables",
                        "Correlation","Species_Groups","Ecosystem_type")

colnames(aov_AUC_elbow) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

#step <- MASS::stepAIC(mod_AUC_elbow)

#var_vis <- names(step$coefficients[-1])


Species_Groups_plot_elbow <- visreg::visreg(mod_AUC_elbow, "Species_Groups", scale = "response", partial = TRUE, 
                                            xlab = " ", ylab = "Dimensionality AUC elbow", gg = TRUE, line = list(col = "#830042FF"), 
                                            fill = list(fill = "#830042FF", alpha = 0.3), 
                                            points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))

Ecosystem_type_plot_elbow <- visreg::visreg(mod_AUC_elbow, "Ecosystem_type", scale = "response", partial = TRUE, 
                                            xlab = " ", ylab = " ", gg = TRUE, line = list(col = "#830042FF"), 
                                            fill = list(fill = "#830042FF", alpha = 0.3), 
                                            points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))


#--------
mod_AUC_0.7 <- stats::lm(Nb_dim_AUC_0.7 ~ logS + logNT + NA_perc + quanti_perc + mean_cor + Species_Groups + Ecosystem_type,
                         data = res_for_model_Species_Groups, na.action = "na.omit")

aov_AUC_0.7 <- data.frame(car::Anova(mod_AUC_0.7))
aov_AUC_0.7 <- data.frame(term = rownames(aov_AUC_0.7), aov_AUC_0.7)
aov_AUC_0.7 <- aov_AUC_0.7[ , -3]
aov_AUC_0.7 <- aov_AUC_0.7[-8, ]

aov_AUC_0.7$term <- c("log(Number of Species)", "log(Number of Traits)", 
                      "Percentage of NA", "% Quantitative Variables",
                      "Correlation","Species_Groups","Ecosystem_type")

colnames(aov_AUC_0.7) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_AUC_0.7)

var_vis <- names(step$coefficients[-1])


Species_Groups_plot_AUC_0.7 <- visreg::visreg(mod_AUC_0.7, "Species_Groups", scale = "response", partial = TRUE, 
                                              xlab = " ", ylab = "Dimensionality AUC 0.7", gg = TRUE, line = list(col = "#830042FF"), 
                                              fill = list(fill = "#830042FF", alpha = 0.3), 
                                              points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))

Ecosystem_type_plot_AUC_0.7 <- visreg::visreg(mod_AUC_0.7, "Ecosystem_type", scale = "response", partial = TRUE, 
                                              xlab = " ", ylab = " ", gg = TRUE, line = list(col = "#830042FF"), 
                                              fill = list(fill = "#830042FF", alpha = 0.3), 
                                              points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))




#--------
mod_AUClostwhen50percTraitdepleted <- stats::lm(rowAUClostwhen50percTraitdepleted ~ logS + logNT + NA_perc + quanti_perc + mean_cor + Species_Groups + Ecosystem_type, 
                                                data = res_for_model_Species_Groups, na.action = "na.omit")

aov_AUClostwhen50percTraitdepleted <- data.frame(car::Anova(mod_AUClostwhen50percTraitdepleted))
aov_AUClostwhen50percTraitdepleted <- data.frame(term = rownames(aov_AUClostwhen50percTraitdepleted), aov_AUClostwhen50percTraitdepleted)
aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[ , -3]
aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[-8, ]

aov_AUClostwhen50percTraitdepleted$term <- c("log(Number of Species)", "log(Number of Traits)", 
                                             "Percentage of NA", "% Quantitative Variables",
                                             "Correlation","Species_Groups","Ecosystem_type")

colnames(aov_AUClostwhen50percTraitdepleted) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_AUClostwhen50percTraitdepleted)

var_vis <- names(step$coefficients[-1])


Species_Groups_plot_AUClostwhen50percTraitdepleted <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "Species_Groups", scale = "response", partial = TRUE, 
                                                                     xlab = "Species_Groups ", ylab = "AUC loss - 50% traits omission", gg = TRUE, line = list(col = "#830042FF"), 
                                                                     fill = list(fill = "#830042FF", alpha = 0.3), 
                                                                     points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
  theme(axis.text.x  = element_text(), 
        axis.title.y = element_text( face = "bold"),
        axis.title.x = element_text( face = "bold"))

Ecosystem_type_plot_AUClostwhen50percTraitdepleted <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "Ecosystem_type", scale = "response", partial = TRUE, 
                                                                     xlab = "Ecosystem_type", ylab = " ", gg = TRUE, line = list(col = "#830042FF"), 
                                                                     fill = list(fill = "#830042FF", alpha = 0.3), 
                                                                     points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
  theme(axis.text.x  = element_text(), 
        axis.title.y = element_text( face = "bold"),
        axis.title.x = element_text( face = "bold"))



grDevices::pdf(file = here::here("figures", "Figure3.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4
Dimensionality_Species_Groups_plot <- gridExtra::grid.arrange(Species_Groups_plot_elbow, Ecosystem_type_plot_elbow,
                                                              Species_Groups_plot_AUC_0.7,Ecosystem_type_plot_AUC_0.7,
                                                              Species_Groups_plot_AUClostwhen50percTraitdepleted, 
                                                              Ecosystem_type_plot_AUClostwhen50percTraitdepleted,
                                                              nrow = 3, ncol = 2)
dev.off()

#To make a table summarizing  models for supplementary
#aov_table_df <- rbind(aov_AUC_elbow, aov_AUC_0.7,aov_AUClostwhen50percTraitdepleted)
#aov_table_df <- data.frame(Variables = c(rep("Dimensionality AUC Elbow",7),rep("Dimensionality AUC 0.7",7)
#                                         ,rep("AUC - 50% traits omission",7)),aov_table_df)

#aov_table_df[aov_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
#aov_table_df[aov_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
#aov_table_df[aov_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
#aov_table_df[aov_table_df$Term=="Correlation",]$Term <- "Mean Correlation"
#aov_table_df <- aov_table_df %>% dplyr::mutate_at(vars("Sum.Sq","F.statistic","P.value",), dplyr::funs(round(., 3)))

#aov_table_df<-aov_table_df[aov_table_df$Term %in% c("Species_Groups", "Ecosystem_type"),]

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
#  kableExtra::kable( booktabs = T, escape = F)%>% 
#  kableExtra::kable_styling()%>% 
#  kableExtra::collapse_rows()

#table_mod_aov
