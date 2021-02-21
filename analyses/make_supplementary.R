#' Supplementary materials


## Import Data ----

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

## Distribution of modeled variables ----              
              df <- res_for_model[,c("Nb_dim_AUC_elbow","Nb_dim_AUC_0.7","rowAUClostwhen50percTraitdepleted","logNC","PropC1","PropSin")]
              df <- data.frame(value = c(df[,"Nb_dim_AUC_elbow"],df[,"Nb_dim_AUC_0.7"],df[,"rowAUClostwhen50percTraitdepleted"],
                                         df[,"logNC"],df[,"PropC1"]*100,df[,"PropSin"]*100),
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
                ggplot(df, aes(x=value,   fill=variable)) +
                geom_histogram()+
                facet_wrap(~ variable, scales = "free") +
                theme_bw()+theme(legend.position="none")
    
              
## Correlation between drivers ----             
  Drivers <-  res_for_model[,c("logS","logNT","NA_perc","quanti_perc","mean_cor")]
 PerformanceAnalytics::chart.Correlation(Drivers)

 ## Relation between Threshol0.7 & AUC ----   
              
ggplot(res_for_model,aes(x = Nb_dim_AUC_elbow,y = Nb_dim_AUC_0.7)) +
  geom_point(size = 2, col = "#3D7688") + 
  geom_smooth(method = lm, col = "#3D7688") + 
  theme_bw()+ 
  xlab("Dimensionality AUC Elbow") + ylab("Dimensionality AUC 0.7")+
   theme(axis.title.y = element_text( face = "bold"), 
     axis.title.x = element_text( face = "bold"))


## Relation between AUC & MAD ----             
              
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


## Relation between AUC & Percentage of variance explained PCOA ---- 
#Peut-on plutôt avoir le % des axes en X et non en Y avec une valeur 
# de R2 pour savoir à combien il explique nos nouvelles métriques 
#mais aussi mettre ces plots dans un repère orthonormé genre 10-10 
#pour voir le décalage avec la diagonale de pente 1   
 
              res_pcoa <- data.frame(do.call(rbind, lapply(1:length(list_res_pcoa),function(i){
                
                print(i)
                
                res_pcoa1 <- data.frame(list_res_pcoa[[i]]$values)
                inertia_pcoa <- res_pcoa1$Eigenvalues
                inertia_pcoa <- inertia_pcoa[inertia_pcoa>0]
                inertia_pcoa <- inertia_pcoa/sum(inertia_pcoa)
                cumul_inertia <- cumsum(inertia_pcoa) 
                varexplain0.5 <- length(cumul_inertia[cumul_inertia<0.5])+1
                return(varexplain0.5)
              })))
              
              
              res_for_model$dim_varexplain0.5 <- res_pcoa[,1]
             
              formatgg <- data.frame(Nb_dim_AUC_0.5 = res_for_model$Nb_dim_AUC_0.5,
                                     Nb_dim_AUC_0.7 = res_for_model$Nb_dim_AUC_0.7,
                                     Nb_dim_AUC_elbow = res_for_model$Nb_dim_AUC_elbow,
                                     dim_varexplain0.5 = res_for_model$dim_varexplain0.5)
              
              #summary(lm(formatgg$Nb_dim_AUC_0.5 ~ formatgg$dim_varexplain0.5))
              a <-  ggplot(formatgg,aes(y = Nb_dim_AUC_0.5,x = dim_varexplain0.5)) +
                geom_point(size = 2, col = "#3D7688") + 
                geom_smooth(method = lm, col = "#3D7688", fill = "#D6EBEC") + 
                theme_bw()+ 
                xlab("") + ylab("Dimensionality AUC 0.5")+
                theme(axis.title.y = element_text(size = 12, face = "bold"),
                      axis.title.x = element_text(size = 12, face = "bold")) +
                geom_label(data = res_for_model,
                           aes(label = paste0("R-squared = ", 
                                              0.79),
                               y = 2, x = 5), size = 4, hjust = 0) +
                ylim(0, 7.5)+xlim(0, 7.5)+
                geom_abline(slope = 1, intercept = 0)
              
              #summary(lm(formatgg$Nb_dim_AUC_0.7 ~ formatgg$dim_varexplain0.5))
              b <- ggplot(formatgg,aes(y = Nb_dim_AUC_0.7,x = dim_varexplain0.5)) +
                geom_point(size = 2, col = "#3D7688") + 
                geom_smooth(method = lm, col = "#3D7688", fill = "#D6EBEC") + 
                theme_bw()+ 
                xlab("") + ylab("Dimensionality AUC 0.7")+
                theme(axis.title.y = element_text(size = 12, face = "bold"),
                      axis.title.x = element_text(size = 12, face = "bold")) +
                geom_label(data = res_for_model,
                           aes(label = paste0("R-squared = ", 
                                              0.82),
                               y = 2.5, x = 11), size = 4, hjust = 0) +
                ylim(0, 16)+xlim(0, 16) +
                geom_abline(slope = 1, intercept = 0)
              
              #summary(lm(formatgg$Nb_dim_AUC_elbow ~ formatgg$dim_varexplain0.5))
              c <- ggplot(formatgg,aes(y = Nb_dim_AUC_elbow,x = dim_varexplain0.5)) +
                geom_point(size = 2, col = "#3D7688") + 
                geom_smooth(method = lm, col = "#3D7688", fill = "#D6EBEC") + 
                theme_bw()+ 
                 xlab("Dimensionality 50% of Explained Variance") + ylab("Dimensionality AUC Elbow")+
                theme(axis.title.y = element_text(size = 12, face = "bold"),
                      axis.title.x = element_text(size = 12, face = "bold")) +
                geom_label(data = res_for_model,
                           aes(label = paste0("R-squared = ", 
                                              0.18),
                               y = 2, x = 5), size = 4, hjust = 0) +
                ylim(0, 7.5)+xlim(0, 7.5)+
                geom_abline(slope = 1, intercept = 0)
              
            
              grid.arrange(a,b,c,nrow = 3)
              
              
## Influence of Species_Groups and Ecosystem_type ----
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
              
              
              
              
              Dimensionality_Species_Groups_plot <- gridExtra::grid.arrange(Species_Groups_plot_elbow, Ecosystem_type_plot_elbow,
                                                                      Species_Groups_plot_AUC_0.7,Ecosystem_type_plot_AUC_0.7,
                                                                      Species_Groups_plot_AUClostwhen50percTraitdepleted, 
                                                                      Ecosystem_type_plot_AUClostwhen50percTraitdepleted,
                                                                      nrow = 3, ncol = 2)
              
              
              #To make a table summarizing  models for supplementary
              aov_table_df <- rbind(aov_AUC_elbow, aov_AUC_0.7,aov_AUClostwhen50percTraitdepleted)
              aov_table_df <- data.frame(Variables = c(rep("Dimensionality AUC Elbow",7),rep("Dimensionality AUC 0.7",7)
                                                       ,rep("AUC - 50% traits omission",7)),aov_table_df)
              
              aov_table_df[aov_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
              aov_table_df[aov_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
              aov_table_df[aov_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
              aov_table_df[aov_table_df$Term=="Correlation",]$Term <- "Mean Correlation"
              aov_table_df <- aov_table_df %>% dplyr::mutate_at(vars("Sum.Sq","F.statistic","P.value",), dplyr::funs(round(., 3)))
              
              aov_table_df<-aov_table_df[aov_table_df$Term %in% c("Species_Groups", "Ecosystem_type"),]
              
              for(i in 1:nrow(aov_table_df)){ 
                if(aov_table_df[i, 5]<0.001 )    { 
                 aov_table_df[i, 5] <- "<0.001"
                  aov_table_df[i, 5] <- kableExtra::cell_spec(aov_table_df[i, 5],  bold = T)
              } 
              
               if(aov_table_df[i, 5]<0.05 & aov_table_df[i, 5]>0.001)     {  
                  aov_table_df[i, 5] <- kableExtra::cell_spec(aov_table_df[i, 5],  bold = T)
                } 
              }
                            table_mod_aov<-pixiedust::dust(aov_table_df) %>% 
               kableExtra::kable( booktabs = T, escape = F)%>% 
               kableExtra::kable_styling()%>% 
               kableExtra::collapse_rows()
              
              table_mod_aov
              
              #-----------------------------------------------------------------------------------------------  
              
              
              
              
              #-----------------------------------------------------------------------------------------------  
              mod_logNC <- stats::lm(logNC ~ logS + logNT + NA_perc + quanti_perc + mean_cor + Species_Groups + Ecosystem_type, 
                                     data = res_for_model_Species_Groups, na.action = "na.omit")
              
              aov_logNC <- data.frame(car::Anova(mod_logNC))
              aov_logNC <- data.frame(term = rownames(aov_logNC), aov_logNC)
              aov_logNC <- aov_logNC[ , -3]
              aov_logNC <- aov_logNC[-8, ]
              
              aov_logNC$term <- c("log(Number of Species)", "log(Number of Traits)", 
                                  "Percentage of NA", "% Quantitative Variables",
                                  "Correlation","Species_Groups","Ecosystem_type")
              
              colnames(aov_logNC) <- c("Term", "Sum.Sq", "F-statistic", "P.value")
              
              step <- MASS::stepAIC(mod_logNC)
              
              var_vis <- names(step$coefficients[-1])
              
              
              Species_Groups_plot_logNC <- visreg::visreg(mod_logNC, "Species_Groups", scale = "response", partial = TRUE, 
                                                    xlab = " ", ylab = "Number of clusters (log)", gg = TRUE, line = list(col = "#830042FF"), 
                                                    fill = list(fill = "#830042FF", alpha = 0.3), 
                                                    points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
                theme(axis.text.x  = element_blank(), 
                      axis.title.y = element_text( face = "bold"))
              
              Ecosystem_type_plot_logNC <- visreg::visreg(mod_logNC, "Ecosystem_type", scale = "response", partial = TRUE, 
                                                     xlab = " ", ylab = " ", gg = TRUE, line = list(col = "#830042FF"), 
                                                     fill = list(fill = "#830042FF", alpha = 0.3), 
                                                     points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
                theme(axis.text.x  = element_blank(), 
                      axis.title.y = element_text( face = "bold"))
              
           
              #----------
              mod_PropC1 <- stats::lm(PropC1 ~ logS + logNT + NA_perc + quanti_perc + mean_cor+ Species_Groups + Ecosystem_type, 
                                      data = res_for_model_Species_Groups, na.action = "na.omit")
              
              aov_PropC1 <- data.frame(car::Anova(mod_PropC1))
              aov_PropC1 <- data.frame(term = rownames(aov_PropC1), aov_PropC1)
              aov_PropC1 <- aov_PropC1[ , -3]
              aov_PropC1 <- aov_PropC1[-8, ]
              
              aov_PropC1$term <- c("log(Number of Species)", "log(Number of Traits)", 
                                   "Percentage of NA", "% Quantitative Variables",
                                   "Correlation","Species_Groups","Ecosystem_type")
              
              colnames(aov_PropC1) <- c("Term", "Sum.Sq", "F-statistic", "P.value")
              
              step <- MASS::stepAIC(mod_PropC1)
              
              var_vis <- names(step$coefficients[-1])
              
              
              Species_Groups_plot_PropC1 <- visreg::visreg(mod_PropC1, "Species_Groups", scale = "response", partial = TRUE, 
                                                     xlab = " ", ylab = "% Cluster #1", gg = TRUE, line = list(col = "#830042FF"), 
                                                     fill = list(fill = "#830042FF", alpha = 0.3), 
                                                     points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
                theme(axis.text.x  = element_blank(), 
                      axis.title.y = element_text( face = "bold"))
              
              Ecosystem_type_plot_PropC1 <- visreg::visreg(mod_PropC1, "Ecosystem_type", scale = "response", partial = TRUE, 
                                                      xlab = " ", ylab = " ", gg = TRUE, line = list(col = "#830042FF"), 
                                                      fill = list(fill = "#830042FF", alpha = 0.3), 
                                                      points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
                theme(axis.text.x  = element_blank(), 
                      axis.title.y = element_text( face = "bold"))
              
              
              
              
              #----------
              
              
              mod_PropSin <- stats::lm(PropSin ~ logS + logNT + NA_perc + quanti_perc + mean_cor+ Species_Groups + Ecosystem_type, 
                                       data = res_for_model_Species_Groups, na.action = "na.omit")
              
              aov_PropSin <- data.frame(car::Anova(mod_PropSin))
              aov_PropSin <- data.frame(term = rownames(aov_PropSin), aov_PropSin)
              aov_PropSin <- aov_PropSin[ , -3]
              aov_PropSin <- aov_PropSin[-8, ]
              
              aov_PropSin$term <- c("log(Number of Species)", "log(Number of Traits)", 
                                    "Percentage of NA", "% Quantitative Variables",
                                    "Correlation","Species_Groups","Ecosystem_type")
              
              colnames(aov_PropSin) <- c("Term", "Sum.Sq", "F-statistic", "P.value")
              
              step <- MASS::stepAIC(mod_PropSin)
              
              var_vis <- names(step$coefficients[-1])
              
              
              Species_Groups_plot_PropSin <- visreg::visreg(mod_PropSin, "Species_Groups", scale = "response", partial = TRUE, 
                                                      xlab = "Species_Groups", ylab = "% uniques", gg = TRUE, line = list(col = "#830042FF"), 
                                                      fill = list(fill = "#830042FF", alpha = 0.3), 
                                                      points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
                theme(axis.text.x  = element_text(), 
                      axis.title.y = element_text( face = "bold"),
                      axis.title.x = element_text( face = "bold"))
              
              Ecosystem_type_plot_PropSin <- visreg::visreg(mod_PropSin, "Ecosystem_type", scale = "response", partial = TRUE, 
                                                       xlab = "Ecosystem_type", ylab = " ", gg = TRUE, line = list(col = "#830042FF"), 
                                                       fill = list(fill = "#830042FF", alpha = 0.3), 
                                                       points = list(size = 2, col = "#830042FF"))  + theme_bw()  +
                theme(axis.text.x  = element_text(), 
                      axis.title.y = element_text( face = "bold"),
                      axis.title.x = element_text( face = "bold"))
              
              
             
              
              Cluster_Species_Groups_plot <- gridExtra::grid.arrange(Species_Groups_plot_logNC, Ecosystem_type_plot_logNC,
                                                               Species_Groups_plot_PropC1,Ecosystem_type_plot_PropC1,
                                                               Species_Groups_plot_PropSin, Ecosystem_type_plot_PropSin,
                                                               nrow = 3, ncol = 2)
              
              
              
              aov_cluster_table_df <- rbind(aov_logNC, aov_PropSin,aov_PropC1)
              aov_cluster_table_df <- data.frame(Variables = c(rep("Number of Cluster (log)",7),rep("% uniques",7)
                                                               ,rep("% Cluster #1",7)),aov_cluster_table_df)
              
              aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
              aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
              aov_cluster_table_df[aov_cluster_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
              aov_cluster_table_df[aov_cluster_table_df$Term=="Correlation",]$Term <- "Mean Correlation"
              
              aov_cluster_table_df <- aov_cluster_table_df %>% dplyr::mutate_at(vars("Sum.Sq","F.statistic","P.value",), dplyr::funs(round(., 3)))
              aov_cluster_table_df<-aov_cluster_table_df[aov_cluster_table_df$Term %in% c("Species_Groups", "Ecosystem_type"),]
              
              for(i in 1:nrow(aov_cluster_table_df)){ 
                if(aov_cluster_table_df[i, 5]<0.001 )    { 
                  aov_cluster_table_df[i, 5] <- "<0.001"
                  aov_cluster_table_df[i, 5] <- kableExtra::cell_spec(aov_cluster_table_df[i, 5],  bold = T)
                } 
                
                if(aov_cluster_table_df[i, 5]<0.05 & aov_cluster_table_df[i, 5]>0.001)     {  
                  aov_cluster_table_df[i, 5] <- kableExtra::cell_spec(aov_cluster_table_df[i, 5],  bold = T)
                } 
                
                
              }
              
              table_cluster_aov<-pixiedust::dust(aov_cluster_table_df) %>% 
                kableExtra::kable( booktabs = T, escape = F)%>% 
                kableExtra::kable_styling()%>% 
                kableExtra::collapse_rows()
              table_cluster_aov
’              