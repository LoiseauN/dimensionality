#' FIGURE 3

## Influence of Life Form and Ecosystem type ----
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


Species_Groups_plot_elbow <- visreg::visreg(mod_AUC_elbow, "Species_Groups", scale = "response", 
                                            partial = TRUE, by = "Species_Groups",overlay = TRUE,
                                            gg = TRUE,  plot =FALSE)

Species_Groups_plot_elbow <- ggplot(Species_Groups_plot_elbow$res,aes(y= visregRes, x= Species_Groups,color=Species_Groups))+
  geom_boxplot()+ 
  geom_point(aes(fill = Species_Groups, group = Species_Groups), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  xlab(" ")+
  scale_y_continuous(labels = comma) +
  ylab("Dimensionality AUC elbow")+
  scale_color_manual(values=c("chartreuse4", "red4", "orange")) + 
  theme_bw()+
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"),
        legend.position = "none")

Ecosystem_type_plot_elbow <- visreg::visreg(mod_AUC_elbow, "Ecosystem_type", scale = "response", 
                                            partial = TRUE, by = "Ecosystem_type",overlay = TRUE,
                                            gg = TRUE,  plot =FALSE)

Ecosystem_type_plot_elbow <- ggplot(Ecosystem_type_plot_elbow$res,aes(y= visregRes, x= Ecosystem_type,color=Ecosystem_type))+
  geom_boxplot()+ 
  geom_point(aes(fill = Ecosystem_type, group = Ecosystem_type), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  scale_y_continuous(labels = comma) +
  xlab(" ")+
  ylab(" ")+
  scale_color_manual(values=c("royalblue1", "limegreen")) + 
  theme_bw()+
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"),
        legend.position = "none")

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


Species_Groups_plot_AUC_0.7 <- visreg::visreg(mod_AUC_0.7, "Species_Groups", scale = "response", 
                                              partial = TRUE, by = "Species_Groups",overlay = TRUE,
                                              gg = TRUE,  plot =FALSE)

Species_Groups_plot_AUC_0.7 <- ggplot(Species_Groups_plot_AUC_0.7$res,aes(y= visregRes, x= Species_Groups,color=Species_Groups))+
  geom_boxplot()+ 
  geom_point(aes(fill = Species_Groups, group = Species_Groups), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  scale_y_continuous(labels = comma) +
  xlab(" ")+
  ylab("Dimensionality AUC 0.7")+
  scale_color_manual(values=c("chartreuse4", "red4", "orange")) + 
  theme_bw()+
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"),
        legend.position = "none")


Ecosystem_type_plot_AUC_0.7 <- visreg::visreg(mod_AUC_0.7, "Ecosystem_type", scale = "response", 
                                              partial = TRUE, by = "Ecosystem_type",overlay = TRUE,
                                              gg = TRUE,  plot =FALSE)

Ecosystem_type_plot_AUC_0.7 <- ggplot(Ecosystem_type_plot_AUC_0.7$res,aes(y= visregRes, x= Ecosystem_type,color=Ecosystem_type))+
  geom_boxplot()+ 
  geom_point(aes(fill = Ecosystem_type, group = Ecosystem_type), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  xlab(" ")+
  ylab(" ")+
  scale_color_manual(values=c("royalblue1", "limegreen")) + 
  theme_bw()+
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"),
        legend.position = "none")


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


Species_Groups_plot_AUClostwhen50percTraitdepleted <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "Species_Groups", scale = "response", 
                                                                     partial = TRUE, by = "Species_Groups",overlay = TRUE,
                                                                     gg = TRUE,  plot =FALSE)


Species_Groups_plot_AUClostwhen50percTraitdepleted <- ggplot(Species_Groups_plot_AUClostwhen50percTraitdepleted$res,aes(y= visregRes, x= Species_Groups,color=Species_Groups))+
  geom_boxplot()+ 
  geom_point(aes(fill = Species_Groups, group = Species_Groups), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  xlab("Life forms")+
  ylab("AUC loss - 50% traits omission")+
  scale_color_manual(values=c("chartreuse4", "red4", "orange")) + 
  theme_bw()+
  theme(axis.title.y = element_text( face = "bold"),
        axis.title.x = element_text( face = "bold"),
        legend.position = "none")


Ecosystem_type_plot_AUClostwhen50percTraitdepleted <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "Ecosystem_type", scale = "response", 
                                                                     partial = TRUE, by = "Ecosystem_type",overlay = TRUE,
                                                                     gg = TRUE,  plot =FALSE)

Ecosystem_type_plot_AUClostwhen50percTraitdepleted <- ggplot(Ecosystem_type_plot_AUClostwhen50percTraitdepleted$res,aes(y= visregRes, x= Ecosystem_type,color=Ecosystem_type))+
  geom_boxplot()+ 
  geom_point(aes(fill = Ecosystem_type, group = Ecosystem_type), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  xlab("Ecosystem types")+
  ylab(" ")+
  scale_color_manual(values=c("royalblue1", "limegreen")) + 
  theme_bw()+
  theme(axis.text.x  = element_text(), 
        axis.title.y = element_text( face = "bold"),
        axis.title.x = element_text( face = "bold"),
        legend.position = "none")



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


