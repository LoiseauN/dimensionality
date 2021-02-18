


res_for_model2<-res_for_model
res_for_model2$Kingdoms <- c(NA,
                        "Invertebrate",
                        "Vertebrate",
                        "Vertebrate",
                        "Vertebrate",
                        "Plant",
                        "Vertebrate",
                        "Vertebrate",
                        "Invertebrate",
                        "Invertebrate",
                        "Plant",
                        "Vertebrate",
                        "Plant",
                        "Invertebrate",
                        "Invertebrate",
                        "Invertebrate",
                        "Invertebrate",
                        "Vertebrate",
                        "Vertebrate",
                        "Invertebrate",
                        "Plant",
                        "Plant",
                        "Plant",
                        NA,
                        "Plant",
                        "Invertebrate",
                        "Invertebrate",
                        "Plant",
                        "Vertebrate",
                        "Invertebrate")


mod_AUC_elbow <- stats::lm(Nb_dim_AUC_elbow ~ logS + logNT + NA_perc + quanti_perc + mean_cor + Kingdoms, 
                           data = res_for_model2, na.action = "na.omit")

aov_AUC_elbow <- data.frame(car::Anova(mod_AUC_elbow))
aov_AUC_elbow <- data.frame(term = rownames(aov_AUC_elbow), aov_AUC_elbow)
aov_AUC_elbow <- aov_AUC_elbow[ , -3]
aov_AUC_elbow <- aov_AUC_elbow[-7, ]

aov_AUC_elbow$term <- c("log(Number of Species)", "log(Number of Traits)", 
                      "Percentage of NA", "% Quantitative Variables",
                      "Correlation","Kingdoms")

colnames(aov_AUC_elbow) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_AUC_elbow)

var_vis <- names(step$coefficients[-1])


Kingdoms_plot_elbow <- visreg::visreg(mod_AUC_elbow, "Kingdoms", scale = "response", partial = TRUE, 
                            xlab = " ", ylab = "Dimensionality AUC elbow", gg = TRUE, line = list(col = "gray78"), 
                            fill = list(fill = "gray90", alpha = 0.8), 
                            points = list(size = 2, col = "gray78"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))

#--------
mod_AUC_0.7 <- stats::lm(Nb_dim_AUC_0.7 ~ logS + logNT + NA_perc + quanti_perc + mean_cor +Kingdoms,
                         data = res_for_model2, na.action = "na.omit")

aov_AUC_0.7 <- data.frame(car::Anova(mod_AUC_0.7))
aov_AUC_0.7 <- data.frame(term = rownames(aov_AUC_0.7), aov_AUC_0.7)
aov_AUC_0.7 <- aov_AUC_0.7[ , -3]
aov_AUC_0.7 <- aov_AUC_0.7[-7, ]

aov_AUC_0.7$term <- c("log(Number of Species)", "log(Number of Traits)", 
                      "Percentage of NA", "% Quantitative Variables",
                      "Correlation","Kingdoms")

colnames(aov_AUC_0.7) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_AUC_0.7)

var_vis <- names(step$coefficients[-1])


Kingdoms_plot_AUC_elbow <- visreg::visreg(mod_AUC_0.7, "Kingdoms", scale = "response", partial = TRUE, 
                                        xlab = " ", ylab = "Dimensionality AUC 0.7", gg = TRUE, line = list(col = "gray78"), 
                                        fill = list(fill = "gray90", alpha = 0.8), 
                                        points = list(size = 2, col = "gray78"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))

#--------
mod_AUClostwhen50percTraitdepleted <- stats::lm(rowAUClostwhen50percTraitdepleted ~ logS + logNT + NA_perc + quanti_perc + mean_cor +Kingdoms, 
                                                data = res_for_model2, na.action = "na.omit")

aov_AUClostwhen50percTraitdepleted <- data.frame(car::Anova(mod_AUClostwhen50percTraitdepleted))
aov_AUClostwhen50percTraitdepleted <- data.frame(term = rownames(aov_AUClostwhen50percTraitdepleted), aov_AUClostwhen50percTraitdepleted)
aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[ , -3]
aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[-7, ]

aov_AUClostwhen50percTraitdepleted$term <- c("log(Number of Species)", "log(Number of Traits)", 
                      "Percentage of NA", "% Quantitative Variables",
                      "Correlation","Kingdoms")

colnames(aov_AUClostwhen50percTraitdepleted) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_AUClostwhen50percTraitdepleted)

var_vis <- names(step$coefficients[-1])


Kingdoms_plot_AUClostwhen50percTraitdepleted <- visreg::visreg(mod_AUClostwhen50percTraitdepleted, "Kingdoms", scale = "response", partial = TRUE, 
                                                               xlab = "Kingdoms ", ylab = "AUC loss - 50% traits omission", gg = TRUE, line = list(col = "gray78"), 
                                                               fill = list(fill = "gray90", alpha = 0.8), 
                                                               points = list(size = 2, col = "gray78"))  + theme_bw()  +
  theme(axis.text.x  = element_text(), 
        axis.title.y = element_text( face = "bold"),
        axis.title.x = element_text( face = "bold"))


Dimensionality_kingdim_plot <- gridExtra::grid.arrange(Kingdoms_plot_AUC_elbow, Kingdoms_plot_AUC_0.7,
                                                       Kingdoms_plot_AUClostwhen50percTraitdepleted, 
                                               nrow = 3, ncol = 1)








mod_logNC <- stats::lm(logNC ~ logS + logNT + NA_perc + quanti_perc + mean_cor + Kingdoms, 
                       data = res_for_model2, na.action = "na.omit")


aov_logNC <- data.frame(car::Anova(mod_logNC))
aov_logNC <- data.frame(term = rownames(aov_logNC), aov_logNC)
aov_logNC <- aov_logNC[ , -3]
aov_logNC <- aov_logNC[-7, ]

aov_logNC$term <- c("log(Number of Species)", "log(Number of Traits)", 
                        "Percentage of NA", "% Quantitative Variables",
                        "Correlation","Kingdoms")

colnames(aov_logNC) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_logNC)

var_vis <- names(step$coefficients[-1])


Kingdoms_plot_logNC <- visreg::visreg(mod_logNC, "Kingdoms", scale = "response", partial = TRUE, 
                                      xlab = " ", ylab = "Number of clusters (log)", gg = TRUE, line = list(col = "gray78"), 
                                      fill = list(fill = "gray90", alpha = 0.8), 
                                      points = list(size = 2, col = "gray78"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))

#----------
mod_PropC1 <- stats::lm(PropC1 ~ logS + logNT + NA_perc + quanti_perc + mean_cor+ Kingdoms, 
                        data = res_for_model2, na.action = "na.omit")

aov_PropC1 <- data.frame(car::Anova(mod_PropC1))
aov_PropC1 <- data.frame(term = rownames(aov_PropC1), aov_PropC1)
aov_PropC1 <- aov_PropC1[ , -3]
aov_PropC1 <- aov_PropC1[-7, ]

aov_PropC1$term <- c("log(Number of Species)", "log(Number of Traits)", 
                    "Percentage of NA", "% Quantitative Variables",
                    "Correlation","Kingdoms")

colnames(aov_PropC1) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_PropC1)

var_vis <- names(step$coefficients[-1])


Kingdoms_plot_PropC1 <- visreg::visreg(mod_PropC1, "Kingdoms", scale = "response", partial = TRUE, 
                                      xlab = " ", ylab = "% Cluster #1", gg = TRUE, line = list(col = "gray78"), 
                                      fill = list(fill = "gray90", alpha = 0.8), 
                                      points = list(size = 2, col = "gray78"))  + theme_bw()  +
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"))


#----------


mod_PropSin <- stats::lm(PropSin ~ logS + logNT + NA_perc + quanti_perc + mean_cor+ Kingdoms, 
                         data = res_for_model2, na.action = "na.omit")

aov_PropSin <- data.frame(car::Anova(mod_PropSin))
aov_PropSin <- data.frame(term = rownames(aov_PropSin), aov_PropSin)
aov_PropSin <- aov_PropSin[ , -3]
aov_PropSin <- aov_PropSin[-7, ]

aov_PropSin$term <- c("log(Number of Species)", "log(Number of Traits)", 
                    "Percentage of NA", "% Quantitative Variables",
                    "Correlation","Kingdoms")

colnames(aov_PropSin) <- c("Term", "Sum.Sq", "F-statistic", "P.value")

step <- MASS::stepAIC(mod_PropSin)

var_vis <- names(step$coefficients[-1])


Kingdoms_plot_PropSin <- visreg::visreg(mod_PropSin, "Kingdoms", scale = "response", partial = TRUE, 
                                      xlab = "Kingdoms", ylab = "% uniques", gg = TRUE, line = list(col = "gray78"), 
                                      fill = list(fill = "gray90", alpha = 0.8), 
                                      points = list(size = 2, col = "gray78"))  + theme_bw()  +
  theme(axis.text.x  = element_text(), 
        axis.title.y = element_text( face = "bold"),
        axis.title.x = element_text( face = "bold"))


Dimensionality_kingdim_plot <- gridExtra::grid.arrange(Kingdoms_plot_logNC, Kingdoms_plot_PropC1,
                                                       Kingdoms_plot_PropSin, 
                                                       nrow = 3, ncol = 1)








df <- res_for_model[,c("Nb_dim_AUC_elbow","Nb_dim_AUC_0.7","rowAUClostwhen50percTraitdepleted","logNC","PropC1","PropSin")]
df <- data.frame(value = c(df[,"Nb_dim_AUC_elbow"],df[,"Nb_dim_AUC_0.7"],df[,"rowAUClostwhen50percTraitdepleted"],df[,"logNC"],df[,"PropC1"],df[,"PropSin"]),
                 variable =  c(rep("Elbow-AUC",30),
                               rep("Dimensionality AUC 0.7",30),
                               rep("AUC 50% traits omission",30),
                               rep("number of clusters (log)",30),
                               rep("% in cluster #1",30),
                               rep("% of uniques",30)))


df$variable <-factor(df$variable,levels = c("% of uniques",
                                            "% in cluster #1",
                                            "number of clusters (log)",
                                            "AUC 50% traits omission",
                                            "Dimensionality AUC 0.7",
                                            "Elbow-AUC"))  

ggplot(df,aes(x = value, y = variable,fill = variable)) +
  ggridges::geom_density_ridges()+
  theme_bw()  +
  theme(legend.position = "none",axis.title.y = element_blank()) 
dev.off()




files <- list.files(path = here::here("outputs"), pattern = "_dim.rds$",
                    full.names = TRUE)
list_res <- lapply(files, function(x) readRDS(x))

res_for_graph_dim <- data.frame(do.call(rbind, lapply(1:length(list_res),function(i){
  print(i)
  res <- data.frame(list_res[[i]][,c("MAD","AUC")],data=rep(rownames(res_for_model)[i],nrow(list_res[[i]])))
  return(res)})))

ggplot(res_for_graph_dim,aes(x=MAD,y=AUC))+geom_point(size=2,col="#3D7688")+
  geom_smooth(method="lm",alpha=0.3,color="#3D7688",fill="#D6EBEC")+ theme_bw()+theme(legend.position="none",
                                                                                      axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12, face="bold"))

ggplot(res_for_graph_dim,aes(x=MAD,y=AUC,colour=data))+geom_point(size=2)+
  geom_smooth(method="lm",alpha=0.3)+ theme_bw()+theme(legend.position="none",
                                                       axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12, face="bold"))+
  scale_colour_hp_d(option = "LunaLovegood")
