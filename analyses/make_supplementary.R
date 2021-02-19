#' Supplementary materials


## Import Data ----

load(file = here::here("outputs", "res_for_model.RData"))

## Distribution of modeled variables ----              
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
              
              ggplot(df,aes(x = value, y = variable,fill = variable))+geom_point(size=2,col="#3D7688")+
                geom_smooth(method="lm",alpha=0.3,color="#3D7688",fill="#D6EBEC")+ theme_bw()+theme(legend.position="none",
                                                                                                    
         
              
              
              
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
                scale_colour_hp_d(option = "LunaLovegood")
              

## Relation between AUC & Percentage of variance explained PCOA ----  
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
              
              
              a <- ggplot(formatgg,aes(x = Nb_dim_AUC_0.5,y = dim_varexplain0.5)) +
                geom_point(size = 2, col = "#3D7688") + 
                geom_smooth(method = lm, col = "#3D7688") + 
                theme_bw()+ 
                ylab("Dimensionality 50% of Explained Variance") + xlab("Dimensionality 50% AUC 0.5")  +
                ylim(0, 15)+ theme(axis.text.y  = element_text(), 
                                   axis.title.x = element_text( face = "bold"),
                                   axis.title.y = element_text( face = "bold"))
              
              b <- ggplot(formatgg,aes(x = Nb_dim_AUC_0.7,y = dim_varexplain0.5)) +
                geom_point(size = 2, col = "#3D7688") + 
                geom_smooth(method = lm, col = "#3D7688") + 
                theme_bw()+ 
                 xlab("Dimensionality 50% AUC 0.7") + ylab("")+
                ylim(0, 15)+ theme(axis.text.y  = element_blank(), 
                                   axis.title.x = element_text( face = "bold"))
              
              c <- ggplot(formatgg,aes(x = Nb_dim_AUC_elbow,y = dim_varexplain0.5)) +
                geom_point(size = 2, col = "#3D7688") + 
                geom_smooth(method = lm, col = "#3D7688") + 
                theme_bw()+ 
                 xlab("Dimensionality AUC Elbow") + ylab("")+
                ylim(0, 15)+ theme(axis.text.y  = element_blank(), 
                                   axis.title.x = element_text( face = "bold"))
              
            
              grid.arrange(a,b,c,ncol = 3)
              
              
## Influence of Kingdoms ----
              res_for_model_kingdoms<-res_for_model
              res_for_model_kingdoms$Kingdoms <- c(NA,
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
                                         data = res_for_model_kingdoms, na.action = "na.omit")
              
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
                                       data = res_for_model_kingdoms, na.action = "na.omit")
              
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
                                                              data = res_for_model_kingdoms, na.action = "na.omit")
              
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
              
              
              Dimensionality_kingdoms_plot <- gridExtra::grid.arrange(Kingdoms_plot_AUC_elbow, Kingdoms_plot_AUC_0.7,
                                                                     Kingdoms_plot_AUClostwhen50percTraitdepleted, 
                                                             nrow = 3, ncol = 1)
              
              
              #To make a table summarizing  models for supplementary
              #aov_table_df <- rbind(aov_AUC_elbow, aov_AUC_0.7,aov_AUClostwhen50percTraitdepleted)
              #aov_table_df <- data.frame(Variables = c(rep("Dimensionality AUC Elbow",6),rep("Dimensionality AUC 0.7",6)
              #                                         ,rep("AUC - 50% traits omission",6)),aov_table_df)
              
              #aov_table_df[aov_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
              #aov_table_df[aov_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
              #aov_table_df[aov_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
              #aov_table_df[aov_table_df$Term=="Correlation",]$Term <- "Mean Correlation"
              #aov_table_df <- aov_table_df %>% dplyr::mutate_at(vars("Sum.Sq","F.statistic","P.value",), dplyr::funs(round(., 3)))
              
              #aov_table_df<-subset(aov_table_df,aov_table_df$Term == "Kingdoms")
              
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
              
              #table_mod_aov
              
    #------   
              mod_logNC <- stats::lm(logNC ~ logS + logNT + NA_perc + quanti_perc + mean_cor + Kingdoms, 
                                     data = res_for_model_kingdoms, na.action = "na.omit")
              
              
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
                                      data = res_for_model_kingdoms, na.action = "na.omit")
              
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
                                       data = res_for_model_kingdoms, na.action = "na.omit")
              
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
              
              
              Cluster_kingdoms_plot <- gridExtra::grid.arrange(Kingdoms_plot_logNC, Kingdoms_plot_PropC1,
                                                                     Kingdoms_plot_PropSin, 
                                                                     nrow = 3, ncol = 1)



              #aov_cluster_table_df <- rbind(aov_logNC, aov_PropSin,aov_PropC1)
              #aov_cluster_table_df <- data.frame(Variables = c(rep("Number of Cluster (log)",6),rep("% uniques",6)
               #                                                ,rep("% Cluster #1",6)),aov_cluster_table_df)
              
              #aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
              #aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
              #aov_cluster_table_df[aov_cluster_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
              #aov_cluster_table_df[aov_cluster_table_df$Term=="Correlation",]$Term <- "Mean Correlation"
              
              #aov_cluster_table_df <- aov_cluster_table_df %>% dplyr::mutate_at(vars("Sum.Sq","F.statistic","P.value",), dplyr::funs(round(., 3)))
              #aov_cluster_table_df<-subset(aov_cluster_table_df,aov_cluster_table_df$Term == "Kingdoms")
              
              #for(i in 1:nrow(aov_cluster_table_df)){ 
              #  if(aov_cluster_table_df[i, 5]<0.001 )    { 
              #    aov_cluster_table_df[i, 5] <- "<0.001"
              #    aov_cluster_table_df[i, 5] <- kableExtra::cell_spec(aov_cluster_table_df[i, 5],  bold = T)
              #  } 
              
               # if(aov_cluster_table_df[i, 5]<0.05 & aov_cluster_table_df[i, 5]>0.001)     {  
                #  aov_cluster_table_df[i, 5] <- kableExtra::cell_spec(aov_cluster_table_df[i, 5],  bold = T)
                #} 
              
              
              #}
              
              #table_cluster_aov<-pixiedust::dust(aov_cluster_table_df) %>% 
                # kableExtra::kable( booktabs = T, escape = F)%>% 
                #  kableExtra::kable_styling()%>% 
                #  kableExtra::collapse_rows()
              #table_cluster_aov
              


              






