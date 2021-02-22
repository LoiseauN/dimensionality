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
              
              

              #-----------------------------------------------------------------------------------------------  
              
              
              
              
              #-----------------------------------------------------------------------------------------------  
              
          