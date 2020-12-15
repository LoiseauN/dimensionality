
# Install and load required packages ---------------------------------------------------------------------
pkgs <- c("spdep","corrgram","MuMIn","visreg","sjPlot","car",
              "rsq","caret","relimp","MASS","musculusColors","harrypotter","gridExtra","grid","pixiedust",
          "tidyverse","knitr","kableExtra","dplyr")
nip  <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

# Load Data--------------------------------------------------------------------
load(file=file.path(results_dir,"res_for_model.RData"))

 log

logS<-log10(res_for_model$S)

logNT=log10(res_for_model$Nb_trait)

logNS=log10(res_for_model$Nb_single)

PropSin=res_for_model$Nb_single/res_for_model$S

logNC=log10(res_for_model$Nb_cluster)

log1C=log10(res_for_model$NbS_Cluster1)

PropC1=res_for_model$NbS_Cluster1/res_for_model$S

FR=res_for_model$S/res_for_model$Nb_cluster

res_for_model=cbind(res_for_model,logS,logNT,logNS,logNC,log1C,PropSin,PropC1,FR)


# var to model
var_to_mod <- c("Nb_dim_AUC_elbow","Nb_dim_AUC_0.7","rowAUClostwhen50percTraitdepleted","rowAUClostwhen20percTraitdepleted","logNS","PropSin")

#lapply(1:length(var_to_mod),function(i){


#' ---------------------------------------------------------------------------------@RawRelation 
#' ---------------------------------------------------------@LogforStructure  

a <- ggplot(data = res_for_model,aes(logS,logNC)) +geom_point(size=2,col="#3D7688") +
  theme_bw()+
  stat_smooth(method = "lm",  size = 1, se = T, color="#3D7688",fill="#D6EBEC")+ 
  labs(y="Number of clusters (log)" , x = " ")+ 
  theme(axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12,  face="bold"),axis.text.x=element_blank())+
  geom_label(data = res_for_model, aes(label= paste0("Slope = ",round(lm(logNC~logS)$coefficients[2],2)),
                                           y =3.8,x=1), size=4, hjust = 0)+
  
  expand_limits(x = c(1,4),y= c(1,4))+
  geom_abline(slope=1, intercept=0)

b <- ggplot(data = res_for_model,aes(logS,log1C)) +geom_point(size=2,col="#3D7688")+
  theme_bw()+
  stat_smooth(method = "lm",  size = 1, se = T, color="#3D7688",fill="#D6EBEC")+ 
  labs(y="Number of species in cluster #1 (log)" , x = " ")+ 
  theme(axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12,  face="bold"),axis.text.x=element_blank())+
  geom_label(data = res_for_model, aes(label= paste0("Slope = ",round(lm(log1C~logS)$coefficients[2],2)),
                                           y =3.8,x=1), size=4, hjust = 0)+
  
  expand_limits(x = c(1,4),y= c(1,4))+
  geom_abline(slope=1, intercept=0)

c <- ggplot(data = res_for_model,aes(logS, logNS)) +geom_point(size=2,col="#3D7688")+
  theme_bw()+
  stat_smooth(method = "lm",  size = 1, se = T, color="#3D7688",fill="#D6EBEC")+ 
  #stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = T, color="#3D7688",fill="#D6EBEC")+ 
  labs(y="Number of uniques (log)" , x = "Number of species (log)")+ 
  theme(axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12, face="bold"))+
  geom_label(data = res_for_model, aes(label= paste0("Slope = ",round(lm(logNS~logS)$coefficients[2],2)),
                                           y =3.8,x=1), size=4, hjust = 0)+

  expand_limits(x = c(1,4),y= c(1,4))+
  geom_abline(slope=1, intercept=0)

myplot1 <- arrangeGrob(a, top = textGrob("(a)", x = unit(1, "npc")
                                               , y   = unit(1, "npc"), just=c("right","top"),
                                               gp=gpar(col="black", fontsize=18)))

myplot2 <- arrangeGrob(b, top = textGrob("(b)", x = unit(1, "npc")
                                               , y = unit(1, "npc"), just=c("right","top"),
                                               gp=gpar(col="black", fontsize=18)))

myplot3 <- arrangeGrob(c, top = textGrob("(c)", x = unit(1, "npc")
                                               , y  = unit(1, "npc"), just=c("right","top"),
                                               gp=gpar(col="black", fontsize=18)))

pdf(file=file.path(fig_dir,"Figure6.pdf"), width = 5, height = 8)
#grid.arrange(a,b,c,ncol=1)
#plot_grid(a, b, c,labels = c('(a)', '(b)','(c)'),nrow=3)
grid.arrange(myplot1,myplot2,myplot3,ncol=1)
dev.off()


#' ---------------------------------------------------------@LogforStructure  

d <- ggplot(data = res_for_model,aes(logS,logNT)) +geom_point(size=2,col="#3D7688") +
  theme_bw()+
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = T, color="#3D7688",fill="#D6EBEC")+
  labs(y="Number of groups" , x = "")+ 
  theme(axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12,  face="bold"),axis.text.x=element_blank())

e <- ggplot(data = res_for_model,aes(logS,log1C)) +geom_point(size=2,col="#3D7688")+
  theme_bw()+
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = T, color="#3D7688",fill="#D6EBEC")+
  labs(y="Number of species in cluster 1" , x = "")+ 
  theme(axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12,  face="bold"),axis.text.x=element_blank())


f <- ggplot(data = res_for_model,aes(logS, logNS)) +geom_point(size=2,col="#3D7688")+
  theme_bw()+
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = T, color="#3D7688",fill="#D6EBEC")+ 
  labs(y="Number of sigletons" , x = "Number of species")+ 
  theme(axis.title.y = element_text(size=12, face="bold"),axis.title.x = element_text(size=12, face="bold"))

pdf(file=file.path(fig_dir,"Figure6_loglog.pdf"), width = 5, height = 8)
grid.arrange(d,e,f,ncol=1)
dev.off()

#' ---------------------------------------------------------------------------------------------------@DIMENSIONALITY
#' 
#' ---------------------------------------------------------@Nb_dim_AUC_elbow  

#---mod
 mod_AUC_elbow   <- lm(Nb_dim_AUC_elbow ~ logS+logNT+NA_perc+quanti_perc+mean_cor, data = res_for_model,na.action="na.fail")
 
 aov_AUC_elbow<- data.frame(Anova(mod_AUC_elbow))
 aov_AUC_elbow <- data.frame(term=rownames(aov_AUC_elbow),aov_AUC_elbow)
 aov_AUC_elbow <- aov_AUC_elbow[,-3]
 aov_AUC_elbow <- aov_AUC_elbow[-6,]
 
 aov_AUC_elbow$term <- c("log(Number of Species)","log(Number of Traits)","Percentage of NA", "% Quantitative Variables",
                         "Correlation")
 colnames(aov_AUC_elbow) <- c("Term","Sum.Sq","F-statistic","P.value")

 step <- stepAIC(mod_AUC_elbow)
 
#var selected by step AIC for visreg
 var_vis <- names(step$coefficients[-1])
 length(var_vis)
 
#---plot 
  S_plot <- visreg(mod_AUC_elbow,"logS",scale="response",partial=TRUE,ylab="Dimensionality AUC Elbow",
         gg=TRUE, line=list(col="gray78"),
         fill=list(fill="gray90",alpha=0.5),
         points=list(size=2,col="gray78"))   + theme_bw() + xlab(" ") +
         theme(axis.text.x=element_blank(),
               axis.title.y = element_text( face="bold"))+
    scale_y_continuous(breaks = seq(1,9,2))+
    expand_limits(y = c(1,9))
    
  
  NT_plot <- visreg(mod_AUC_elbow,"logNT",scale="response",partial=TRUE,
                    gg=TRUE, line=list(col="gray78"),
                    fill=list(fill="gray90",alpha=0.5),
                    points=list(size=2,col="gray78"))   + theme_bw() + xlab(" ")+ ylab(" ")+
    theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
    scale_y_continuous(breaks = seq(1,9,2))+
    expand_limits(y = c(1,9))+
    theme(axis.text.y=element_blank())
  
  
  NA_plot <- visreg(mod_AUC_elbow,"NA_perc",scale="response",partial=TRUE,
                    gg=TRUE, line=list(col="gray78"),
                    fill=list(fill="gray90",alpha=0.5),
                    points=list(size=2,col="gray78"))   + theme_bw() + xlab(" ")+ ylab(" ")+
    theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
    scale_y_continuous(breaks = seq(1,9,2))+
    expand_limits(y = c(1,9))+
    theme(axis.text.y=element_blank())
  
  Quanti_plot <- visreg(mod_AUC_elbow,"quanti_perc",scale="response",partial=TRUE,
                        gg=TRUE, line=list(col="gray78"),
                        fill=list(fill="gray90",alpha=0.5),
                        points=list(size=2,col="gray78"))   + theme_bw() + xlab(" ")+ ylab(" ")+
    theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
    scale_y_continuous(breaks = seq(1,9,2))+
    expand_limits(y = c(1,9))+
    theme(axis.text.y=element_blank())
  
  cor_plot <- visreg(mod_AUC_elbow,"mean_cor",scale="response",partial=TRUE,
         gg=TRUE, line=list(col="gray78"),
         fill=list(fill="gray90",alpha=0.5),
         points=list(size=2,col="gray78"))   + theme_bw() + xlab(" ")+ ylab(" ")+
    theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
    scale_y_continuous(breaks = seq(1,9,2))+
    expand_limits(y = c(1,9))+
    theme(axis.text.y=element_blank())
  
  plot_AUC_elbow  <- grid.arrange(S_plot,NT_plot,NA_plot,Quanti_plot,cor_plot,ncol=5)
  
  
#' ---------------------------------------------------------@Nb_dim_AUC_0.7  
 
#---mod
  mod_AUC_0.7   <- lm(Nb_dim_AUC_0.7 ~ logS+logNT+NA_perc+quanti_perc+mean_cor, data = res_for_model,na.action="na.omit")
  
  aov_AUC_0.7<- data.frame(Anova(mod_AUC_0.7))
  aov_AUC_0.7 <- data.frame(term=rownames(aov_AUC_0.7),aov_AUC_0.7)
  aov_AUC_0.7 <- aov_AUC_0.7[,-3]
  aov_AUC_0.7 <- aov_AUC_0.7[-6,]
  
  aov_AUC_0.7$term <- c("log(Number of Species)","log(Number of Traits)","Percentage of NA", "% Quantitative Variables",
                          "Correlation")
  colnames(aov_AUC_0.7) <- c("Term","Sum.Sq","F-statistic","P.value")

  step <- stepAIC(mod_AUC_0.7)
  
  #var selected by step AIC for visreg
  var_vis <- names(step$coefficients[-1])
  length(var_vis)

  #---plot 
  S_plot <- visreg(mod_AUC_0.7,"logS",scale="response",partial=TRUE,xlab=" ",ylab="Dimensionality AUC 0.7"
                   ,gg=TRUE, line=list(col="gray78"),
                   fill=list(fill="gray90",alpha=0.5),
                   points=list(size=2,col="gray78"))   + theme_bw() +
    theme(axis.title.y = element_text( face="bold"),axis.text.x=element_blank())+
    scale_y_continuous(breaks = seq(1,18,3))+
    expand_limits(y = c(1,18))
  
  NT_plot <- visreg(mod_AUC_0.7,"logNT",scale="response",partial=TRUE,xlab=" ",
                    gg=TRUE, line=list(col="#3D7688"),
                    fill=list(fill="#D6EBEC",alpha=0.5),
                    points=list(size=2,col="#3D7688"))   + theme_bw()+ylab(" ")+
    theme(axis.text.y=element_blank())+
    scale_y_continuous(breaks = seq(1,18,3))+
    expand_limits(y = c(1,18))+
    theme(axis.text.y=element_blank(),axis.text.x=element_blank())
  
  NA_plot <- visreg(mod_AUC_0.7,"NA_perc",scale="response",partial=TRUE,xlab=" ",
                    gg=TRUE, line=list(col="gray78"),
                    fill=list(fill="gray90",alpha=0.5),
                    points=list(size=2,col="gray78"))   + theme_bw()+ylab(" ")+
    theme(axis.text.y=element_blank())+
    scale_y_continuous(breaks = seq(1,18,3))+
    expand_limits(y = c(1,18))+
    theme(axis.text.y=element_blank(),axis.text.x=element_blank())
  
  Quanti_plot <- visreg(mod_AUC_0.7,"quanti_perc",scale="response",partial=TRUE,xlab=" ",
                        gg=TRUE, line=list(col="gray78"),
                        fill=list(fill="gray90",alpha=0.5),
                        points=list(size=2,col="gray78"))   + theme_bw()+ylab(" ")+
    theme(axis.text.y=element_blank())+
    scale_y_continuous(breaks = seq(1,18,3))+
    expand_limits(y = c(1,18))+
    theme(axis.text.y=element_blank(),axis.text.x=element_blank())
  
  cor_plot <- visreg(mod_AUC_0.7,"mean_cor",scale="response",partial=TRUE,xlab=" ",
                     gg=TRUE, line=list(col="#3D7688"),
                     fill=list(fill="#D6EBEC",alpha=0.5),
                     points=list(size=2,col="#3D7688"))  + theme_bw()+ylab(" ")+
    scale_y_continuous(breaks = seq(1,18,3))+
    expand_limits(y = c(1,18))+
    theme(axis.text.y=element_blank(),axis.text.x=element_blank())
  
  plot_AUC_0.7  <- grid.arrange(S_plot,NT_plot,NA_plot,Quanti_plot,cor_plot,ncol=5)



#' ---------------------------------------------------------@rowAUClostwhen50percTraitdepleted  
  
  #---mod
  mod_AUClostwhen50percTraitdepleted   <- lm(rowAUClostwhen50percTraitdepleted ~ logS+logNT+NA_perc+quanti_perc+mean_cor, data = res_for_model,na.action="na.omit")
  
  aov_AUClostwhen50percTraitdepleted<- data.frame(Anova(mod_AUClostwhen50percTraitdepleted))
  aov_AUClostwhen50percTraitdepleted <- data.frame(term=rownames(aov_AUClostwhen50percTraitdepleted),aov_AUClostwhen50percTraitdepleted)
  aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[,-3]
  aov_AUClostwhen50percTraitdepleted <- aov_AUClostwhen50percTraitdepleted[-6,]
  
  aov_AUClostwhen50percTraitdepleted$term <- c("log(Number of Species)","log(Number of Traits)","Percentage of NA", "% Quantitative Variables",
                        "Correlation")
  colnames(aov_AUClostwhen50percTraitdepleted) <- c("Term","Sum.Sq","F-statistic","P.value")
  
  step <- stepAIC(mod_AUClostwhen50percTraitdepleted)
  
  #var selected by step AIC for visreg
  var_vis <- names(step$coefficients[-1])
  length(var_vis)
  
  #---plot 
  S_plot <- visreg(mod_AUClostwhen50percTraitdepleted,"logS",scale="response",partial=TRUE,xlab="Number of Species (log)",ylab="AUC loss - 50% traits omission",
                   gg=TRUE, line=list(col="gray78"),
                   fill=list(fill="gray90",alpha=0.5),
                   points=list(size=2,col="gray78"))   + theme_bw() +
    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(0.2,1))+
  theme(axis.title.x  = element_text(face="bold"), axis.title.y  = element_text(face="bold"))
  
  NT_plot <- visreg(mod_AUClostwhen50percTraitdepleted,"logNT",scale="response",partial=TRUE,xlab="Number of Traits (log)",ylab=" ",
                    gg=TRUE, line=list(col="#3D7688"),
                    fill=list(fill="#D6EBEC",alpha=0.5),
                    points=list(size=2,col="#3D7688"))   + theme_bw()+
    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(0.2,1))+
    theme(axis.text.y=element_blank(),axis.title.x  = element_text( face="bold"))
  
  NA_plot <- visreg(mod_AUClostwhen50percTraitdepleted,"NA_perc",scale="response",partial=TRUE,xlab="% of Missing Values",ylab=" ",
                    gg=TRUE, line=list(col="#3D7688"),
                    fill=list(fill="#D6EBEC",alpha=0.5),
                    points=list(size=2,col="#3D7688"))   + theme_bw()+
    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(0.2,1))+
    theme(axis.text.y=element_blank(), axis.title.x  = element_text( face="bold"))
  
  Quanti_plot <- visreg(mod_AUClostwhen50percTraitdepleted,"quanti_perc",scale="response",partial=TRUE,xlab="% Quantitative Variables",ylab=" ",
                        gg=TRUE, line=list(col="gray78"),
                        fill=list(fill="gray90",alpha=0.5),
                        points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(0.2,1))+
    theme(axis.text.y=element_blank(),axis.title.x  = element_text( face="bold"))
  
  cor_plot <- visreg(mod_AUClostwhen50percTraitdepleted,"mean_cor",scale="response",partial=TRUE,xlab="Mean Correlation",ylab=" ",
                     gg=TRUE, line=list(col="#3D7688"),
                     fill=list(fill="#D6EBEC",alpha=0.5),
                     points=list(size=2,col="#3D7688"))   + theme_bw()+
    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(0.2,1))+
    theme(axis.text.y=element_blank(),axis.title.x  = element_text( face="bold"))
  
  plot_AUClostwhen50percTraitdepleted <- grid.arrange(S_plot,NT_plot,NA_plot,Quanti_plot,cor_plot,ncol=5)

  
pdf(file=file.path(fig_dir,"Figure3.pdf"), width = 11.7, height = 8.3)
  Dimensionality_plot <- grid.arrange(plot_AUC_elbow,plot_AUC_0.7,plot_AUClostwhen50percTraitdepleted,nrow = 3, ncol=1)
  dev.off()
  
  
  #Table for all
  aov_table_df <- rbind(aov_AUC_elbow, aov_AUC_0.7,aov_AUClostwhen50percTraitdepleted)
  aov_table_df <- data.frame(Variables = c(rep("Dimensionality AUC Elbow",5),rep("Dimensionality AUC 0.7",5)
                                          ,rep("AUC - 50% traits omission",5)),aov_table_df)
  
  aov_table_df[aov_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
  aov_table_df[aov_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
  aov_table_df[aov_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
  aov_table_df[aov_table_df$Term=="Correlation",]$Term <- "Mean Correlation"
  aov_table_df <- aov_table_df %>% mutate_at(vars("Sum.Sq","F.statistic","P.value",), funs(round(., 3)))

  for(i in 1:nrow(aov_table_df)){ 
    if(aov_table_df[i, 5]<0.001 )    { 
      aov_table_df[i, 5] <- "<0.001"
      aov_table_df[i, 5] <- cell_spec(aov_table_df[i, 5],  bold = T)
    } 
    
    if(aov_table_df[i, 5]<0.05 & aov_table_df[i, 5]>0.001)     {  
      aov_table_df[i, 5] <- cell_spec(aov_table_df[i, 5],  bold = T)
    } 
  }
  
  table_mod_aov<-dust(aov_table_df) %>% 
    #sprinkle(col=3:5,round =3) %>% 
    #sprinkle(col=5,fn=quote(pvalString(value))) %>%  
    kable( booktabs = T, escape = F)%>% 
    kable_styling()%>% 
    collapse_rows()
  
  table_mod_aov
#
  
  ##' ---------------------------------------------------------@rowAUClostwhen20percTraitdepleted  
  
  #---mod
#  mod_AUClostwhen20percTraitdepleted   <- lm(rowAUClostwhen20percTraitdepleted ~ logS+logNT+NA_perc+quanti_perc+mean_cor, data = res_for_model,na.action="na.omit")
  
  #  aov_AUClostwhen20percTraitdepleted <- Anova(mod_AUClostwhen20percTraitdepleted)
  
  #  table_mod_AUClostwhen20percTraitdepleted<-dust(mod_AUClostwhen20percTraitdepleted) %>% 
  #    sprinkle(col=2:4,round =3) %>% 
  #   sprinkle(col=5,fn=quote(pvalString(value))) %>%  
  #   sprinkle_colnames(term="Term",
  #                  estimate="Estimate",
  #                   std.error="SE",
  #                   statistic="T-statistic",
  #                   p.value="P-value")%>% 
  # kable()%>% 
  #  kable_styling()
  
  # step <- stepAIC(mod_AUClostwhen20percTraitdepleted)
  
  #var selected by step AIC for visreg
  #var_vis <- names(step$coefficients[-1])
  #length(var_vis)
  
  #---plot 
  #S_plot <- visreg(mod_AUClostwhen20percTraitdepleted,"logS",scale="response",partial=TRUE,xlab="log(Number of Species)",ylab="AUC - 20% depleted traits",
#                gg=TRUE, line=list(col="gray78"),
#                  fill=list(fill="gray90",alpha=0.5),
#                  points=list(size=2,col="gray78"))   + theme_bw() +
#   scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
#   expand_limits(y = c(0.2,1))
# 
# NT_plot <- visreg(mod_AUClostwhen20percTraitdepleted,"logNT",scale="response",partial=TRUE,xlab="log(Number of Traits)",ylab=" ",
#                   gg=TRUE, line=list(col="#3D7688"),
#                   fill=list(fill="#D6EBEC",alpha=0.5),
#                   points=list(size=2,col="#3D7688"))   + theme_bw()+
#   scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
#   expand_limits(y = c(0.2,1))+
#   theme(axis.text.y=element_blank())
# 
# NA_plot <- visreg(mod_AUClostwhen20percTraitdepleted,"NA_perc",scale="response",partial=TRUE,xlab="Percentage of NA",ylab=" ",
#                   gg=TRUE, line=list(col="#3D7688"),
#                   fill=list(fill="#D6EBEC",alpha=0.5),
#                    points=list(size=2,col="#3D7688"))   + theme_bw()+
#    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
#    expand_limits(y = c(0.2,1))+
#    theme(axis.text.y=element_blank())
  
#  Quanti_plot <- visreg(mod_AUClostwhen20percTraitdepleted,"quanti_perc",scale="response",partial=TRUE,xlab="% Quantitative Variables",ylab=" ",
#                        gg=TRUE, line=list(col="gray78"),
#                        fill=list(fill="gray90",alpha=0.5),
#                        points=list(size=2,col="gray78"))   + theme_bw()+
#    scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
#   expand_limits(y = c(0.2,1))+
#   theme(axis.text.y=element_blank())
  
# cor_plot <- visreg(mod_AUClostwhen20percTraitdepleted,"mean_cor",scale="response",partial=TRUE,xlab="Correlation",ylab=" ",
#                    gg=TRUE, line=list(col="#3D7688"),
#                    fill=list(fill="#D6EBEC",alpha=0.5),
#                    points=list(size=2,col="#3D7688"))   + theme_bw()+
#   scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1))+
#   expand_limits(y = c(0.2,1))+
#   theme(axis.text.y=element_blank())
# 
# plot_AUClostwhen20percTraitdepleted <- grid.arrange(S_plot,NT_plot,NA_plot,Quanti_plot,cor_plot,ncol=5)
# Depletion_plot <- grid.arrange(plot_AUClostwhen50percTraitdepleted,plot_AUClostwhen20percTraitdepleted,nrow = 2, ncol=1,
#                                top = textGrob("Depletion",gp=gpar(fontsize=20,font=3)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#' ---------------------------------------------------------------------------------------------------@STRUCTURE
#' 
#' ----------------------------------------------------------------------------@logNC  
  
  #---mod
  mod_logNC  <- lm(logNC ~ logS+logNT+NA_perc+quanti_perc+mean_cor, data = res_for_model,na.action="na.omit")
  
  aov_logNC <- data.frame(Anova(mod_logNC ))
  aov_logNC  <- data.frame(term=rownames(aov_logNC ),aov_logNC )
  aov_logNC  <- aov_logNC [,-3]
  aov_logNC  <- aov_logNC [-6,]
  
  aov_logNC $term <- c("log(Number of Species)","log(Number of Traits)","Percentage of NA", "% Quantitative Variables",
                        "Correlation")
  colnames(aov_logNC ) <- c("Term","Sum.Sq","F-statistic","P.value")
  
  
  step <- stepAIC(mod_logNC)
  
  #var selected by step AIC for visreg
  var_vis <- names(step$coefficients[-1])
  length(var_vis)
  
  #---plot 
  S_plot <- visreg(mod_logNC,"logS",scale="response",partial=TRUE,xlab=" ",ylab="Number of clusters (log)",
                   gg=TRUE, line=list(col="#3D7688"),
                   fill=list(fill="#D6EBEC",alpha=0.5),
                   points=list(size=2,col="#3D7688"))   + theme_bw() +
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3))+
    expand_limits(y = c(0,2.8))+
    theme(axis.text.x=element_blank(),axis.title.y = element_text( face="bold"))#+
 # geom_label(data = res_for_graph_dim, aes(label= paste0("slope = ",round(mod_logNC$coefficients["logS"],2)),
 #                                          y =0.3,x=1.5), size=3, hjust = 0) 
    

  NT_plot <- visreg(mod_logNC,"logNT",scale="response",partial=TRUE,xlab=" ",ylab=" ",
                    gg=TRUE, line=list(col="gray78"),
                    fill=list(fill="gray90",alpha=0.5),
                    points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3))+
    expand_limits(y = c(0,2.8)) + 
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  NA_plot <- visreg(mod_logNC,"NA_perc",scale="response",partial=TRUE,xlab=" ",ylab=" ",
                    gg=TRUE, line=list(col="#3D7688"),
                    fill=list(fill="#D6EBEC",alpha=0.5),
                    points=list(size=2,col="#3D7688"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3))+
    expand_limits(y = c(0,2.8)) + 
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  Quanti_plot <- visreg(mod_logNC,"quanti_perc",scale="response",partial=TRUE,xlab=" ",ylab=" ",
                        gg=TRUE, line=list(col="gray78"),
                        fill=list(fill="gray90",alpha=0.5),
                        points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3))+
    expand_limits(y = c(0,2.8))+ 
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  cor_plot <- visreg(mod_logNC,"mean_cor",scale="response",partial=TRUE,xlab=" ",ylab=" ",
                     gg=TRUE, line=list(col="gray78"),
                     fill=list(fill="gray90",alpha=0.5),
                     points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3))+
    expand_limits(y = c(0,2.8))+ 
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  plot_logNC <- grid.arrange(S_plot,NT_plot,NA_plot,Quanti_plot,cor_plot,ncol=5)
  

  
  
#' ----------------------------------------------------------------------------@PropC1_FirstCluster
  
  #---mod
  mod_PropC1  <- lm(PropC1 ~ logS+logNT+NA_perc+quanti_perc+mean_cor, data = res_for_model,na.action="na.omit")
  
  aov_PropC1 <- data.frame(Anova(mod_PropC1 ))
  aov_PropC1  <- data.frame(term=rownames(aov_PropC1 ),aov_PropC1 )
  aov_PropC1  <- aov_PropC1 [,-3]
  aov_PropC1  <- aov_PropC1 [-6,]
  
  aov_PropC1$term <- c("log(Number of Species)","log(Number of Traits)","Percentage of NA", "% Quantitative Variables",
                         "Correlation")
  colnames(aov_PropC1 ) <- c("Term","Sum.Sq","F-statistic","P.value")
  
  step <- stepAIC(mod_PropC1)
  
  #var selected by step AIC for visreg
  var_vis <- names(step$coefficients[-1])
  length(var_vis)
  
  #---plot 
  S_plot <- visreg(mod_PropC1,"logS",scale="response",partial=TRUE,xlab="",ylab="% Cluster #1",
                   gg=TRUE, line=list(col="#3D7688"),
                   fill=list(fill="#D6EBEC",alpha=0.5),
                   points=list(size=2,col="#3D7688"))   + theme_bw() +
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,1))+
    theme(axis.text.x=element_blank(),axis.title.y = element_text( face="bold"))
   # geom_label(data = res_for_graph_dim, aes(label= paste0("slope = ",round(mod_PropC1$coefficients["logS"],2)),
    #                                         y =-0.15,x=1.5), size=3, hjust = 0) 
  
  NT_plot <- visreg(mod_PropC1,"logNT",scale="response",partial=TRUE,xlab=" ",ylab = " ",
                    gg=TRUE, line=list(col="gray78"),
                    fill=list(fill="gray90",alpha=0.5),
                    points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,1))+
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  NA_plot <- visreg(mod_PropC1,"NA_perc",scale="response",partial=TRUE,xlab=" ",ylab = " ",
                    gg=TRUE, line=list(col="#3D7688"),
                    fill=list(fill="#D6EBEC",alpha=0.5),
                    points=list(size=2,col="#3D7688"))    + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,1))+
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  Quanti_plot <- visreg(mod_PropC1,"quanti_perc",scale="response",partial=TRUE,xlab=" ",ylab = " ",
                        gg=TRUE, line=list(col="gray78"),
                        fill=list(fill="gray90",alpha=0.5),
                        points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,1))+
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  cor_plot <- visreg(mod_PropC1,"mean_cor",scale="response",partial=TRUE,xlab=" ",ylab = " ",
                     gg=TRUE, line=list(col="gray78"),
                     fill=list(fill="gray90",alpha=0.5),
                     points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,1))+
    theme(axis.text.y=element_blank())+
    theme(axis.text.x=element_blank())
  
  
  plot_PropC1  <- grid.arrange(S_plot,NT_plot,NA_plot,Quanti_plot,cor_plot,ncol=5)
  
  #' ----------------------------------------------------------------------------@PropSin
  
  #---mod
  mod_PropSin  <- lm(PropSin ~ logS+logNT+NA_perc+quanti_perc+mean_cor, data = res_for_model,na.action="na.omit")
  
  aov_PropSin <- data.frame(Anova(mod_PropSin ))
  aov_PropSin  <- data.frame(term=rownames(aov_PropSin ),aov_PropSin )
  aov_PropSin  <- aov_PropSin [,-3]
  aov_PropSin  <- aov_PropSin [-6,]
  
  aov_PropSin $term <- c("log(Number of Species)","log(Number of Traits)","Percentage of NA", "% Quantitative Variables",
                         "Correlation")
  colnames(aov_PropSin ) <- c("Term","Sum.Sq","F-statistic","P.value")
  
  step <- stepAIC(mod_PropSin)
  
  #var selected by step AIC for visreg
  var_vis <- names(step$coefficients[-1])
  length(var_vis)
  
  #---plot 
  S_plot <- visreg(mod_PropSin,"logS",scale="response",partial=TRUE,ylab = "% uniques",xlab="Number of Species (log)",
                   gg=TRUE, line=list(col="#3D7688"),
                   fill=list(fill="#D6EBEC",alpha=0.5),
                   points=list(size=2,col="#3D7688")) + theme_bw() +
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,0.8)) +theme(axis.title.x = element_text( face="bold"),axis.title.y = element_text( face="bold"))
    #geom_label(data = res_for_graph_dim, aes(label= paste0("slope = ",round(mod_PropSin$coefficients["logS"],2)),
    #                                         y =0,x=1.5), size=3, hjust = 0) 
  
  NT_plot <- visreg(mod_PropSin,"logNT",scale="response",partial=TRUE,ylab = " ",xlab="Number of Traits (log)",
                    gg=TRUE, line=list(col="gray78"),
                    fill=list(fill="gray90",alpha=0.5),
                    points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,0.8)) +
    theme(axis.text.y=element_blank(),axis.title.x = element_text( face="bold"))
  
  NA_plot <- visreg(mod_PropSin,"NA_perc",scale="response",partial=TRUE,ylab = " ",xlab="% of Missing Values",
                    gg=TRUE, line=list(col="#3D7688"),
                    fill=list(fill="#D6EBEC",alpha=0.5),
                    points=list(size=2,col="#3D7688"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,0.8))+
    theme(axis.text.y=element_blank(),axis.title.x = element_text( face="bold"))
  
  Quanti_plot <- visreg(mod_PropSin,"quanti_perc",scale="response",partial=TRUE,ylab = " ",xlab="% Quantitative Variables",
                        gg=TRUE, line=list(col="gray78"),
                        fill=list(fill="gray90",alpha=0.5),
                        points=list(size=2,col="gray78"))   + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,0.8))+
    theme(axis.text.y=element_blank(),axis.title.x = element_text( face="bold"))
  
  cor_plot <- visreg(mod_PropSin,"mean_cor",scale="response",partial=TRUE,ylab = " ",xlab="Mean Correlation",
                     gg=TRUE, line=list(col="gray78"),
                     fill=list(fill="gray90",alpha=0.5),
                     points=list(size=2,col="gray78"))    + theme_bw()+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+
    expand_limits(y = c(-0.05,0.8)) +
    theme(axis.text.y=element_blank(),axis.title.x = element_text( face="bold"))
  
  
  plot_PropSin  <- grid.arrange(S_plot,NT_plot,NA_plot,Quanti_plot,cor_plot,ncol=5)
  
  
  pdf(file=file.path(fig_dir,"Figure5.pdf"), width = 11.7, height = 8.3)#SAVE A4  
  structure_plot <- grid.arrange(plot_logNC,plot_PropC1,plot_PropSin,nrow = 3, ncol=1)  
  dev.off()
  
  
  #Table for all
  aov_cluster_table_df <- rbind(aov_logNC, aov_PropSin,aov_PropC1)
  aov_cluster_table_df <- data.frame(Variables = c(rep("Number of Cluster (log)",5),rep("% uniques",5)
                                           ,rep("% Cluster #1",5)),aov_cluster_table_df)
  
  aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
  aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
  aov_cluster_table_df[aov_cluster_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
  aov_cluster_table_df[aov_cluster_table_df$Term=="Correlation",]$Term <- "Mean Correlation"

  aov_cluster_table_df <- aov_cluster_table_df %>% mutate_at(vars("Sum.Sq","F.statistic","P.value",), funs(round(., 3)))
  
  for(i in 1:nrow(aov_cluster_table_df)){ 
    if(aov_cluster_table_df[i, 5]<0.001 )    { 
                  aov_cluster_table_df[i, 5] <- "<0.001"
                  aov_cluster_table_df[i, 5] <- cell_spec(aov_cluster_table_df[i, 5],  bold = T)
                } 
    
    if(aov_cluster_table_df[i, 5]<0.05 & aov_cluster_table_df[i, 5]>0.001)     {  
      aov_cluster_table_df[i, 5] <- cell_spec(aov_cluster_table_df[i, 5],  bold = T)
              } 
   
   
  }
  
  table_cluster_aov<-dust(aov_cluster_table_df) %>% 
    #sprinkle(col=3:5,round =3) %>% 
    #sprinkle(col=5,fn=quote(pvalString(value))) %>%  
    kable( booktabs = T, escape = F)%>% 
    kable_styling()%>% 
    collapse_rows()
  table_cluster_aov
  
