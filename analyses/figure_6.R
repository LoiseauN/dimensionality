#'FIGURE 6
#'
#' Run figure 3 first

## Influence of Species_Groups and Ecosystem_type ----

## Prepare Data ----

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

#step <- MASS::stepAIC(mod_logNC)

var_vis <- names(step$coefficients[-1])


Species_Groups_plot_logNC <- visreg::visreg(mod_logNC, "Species_Groups", scale = "response",
                                            partial = TRUE, by = "Species_Groups",overlay = TRUE,
                                            gg = TRUE,  plot =FALSE)

Species_Groups_plot_logNC <- ggplot(Species_Groups_plot_logNC$res,aes(y= visregRes, x= Species_Groups,color=Species_Groups))+
  geom_boxplot()+ 
  geom_point(aes(fill = Species_Groups, group = Species_Groups), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  xlab(" ")+
  ylab("Number of clusters (log)")+
  scale_color_manual(values=c("chartreuse4", "red4", "orange")) + 
  theme_bw()+
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"),
        legend.position = "none")


Ecosystem_type_plot_logNC <- visreg::visreg(mod_logNC, "Ecosystem_type", scale = "response", 
                                            partial = TRUE, by = "Ecosystem_type",overlay = TRUE,
                                            gg = TRUE,  plot =FALSE)

Ecosystem_type_plot_logNC <- ggplot(Ecosystem_type_plot_logNC$res,aes(y= visregRes, x= Ecosystem_type,color=Ecosystem_type))+
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

#step <- MASS::stepAIC(mod_PropC1)

var_vis <- names(step$coefficients[-1])


Species_Groups_plot_PropC1 <- visreg::visreg(mod_PropC1, "Species_Groups", scale = "response", 
                                             partial = TRUE, by = "Species_Groups",overlay = TRUE,
                                             gg = TRUE,  plot =FALSE)

Species_Groups_plot_PropC1 <- ggplot(Species_Groups_plot_PropC1$res,aes(y= visregRes, x= Species_Groups,color=Species_Groups))+
  geom_boxplot()+ 
  geom_point(aes(fill = Species_Groups, group = Species_Groups), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  xlab(" ")+
  ylab("% Cluster #1")+
  scale_color_manual(values=c("chartreuse4", "red4", "orange")) + 
  theme_bw()+
  theme(axis.text.x  = element_blank(), 
        axis.title.y = element_text( face = "bold"),
        legend.position = "none")


Ecosystem_type_plot_PropC1 <- visreg::visreg(mod_PropC1, "Ecosystem_type", scale = "response", 
                                             partial = TRUE, by = "Ecosystem_type",overlay = TRUE,
                                             gg = TRUE,  plot =FALSE)

Ecosystem_type_plot_PropC1 <- ggplot(Ecosystem_type_plot_PropC1$res,aes(y= visregRes, x= Ecosystem_type,color=Ecosystem_type))+
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


Species_Groups_plot_PropSin <- visreg::visreg(mod_PropSin, "Species_Groups", scale = "response", 
                                              partial = TRUE, by = "Species_Groups",overlay = TRUE,
                                              gg = TRUE,  plot =FALSE)

Species_Groups_plot_PropSin <- ggplot(Species_Groups_plot_PropSin$res,aes(y= visregRes, x= Species_Groups,color=Species_Groups))+
  geom_boxplot()+ 
  geom_point(aes(fill = Species_Groups, group = Species_Groups), 
             alpha  =0.5, size=2, 
             position = position_jitterdodge(jitter.width = .1, dodge.width = 1)) +
  xlab("Life forms")+
  ylab("% uniques")+
  scale_color_manual(values=c("chartreuse4", "red4", "orange")) + 
  theme_bw()+
  theme(axis.title.y = element_text( face = "bold"),
        axis.title.x = element_text( face = "bold"),
        legend.position = "none")

Ecosystem_type_plot_PropSin <- visreg::visreg(mod_PropSin, "Ecosystem_type", scale = "response", 
                                              partial = TRUE, by = "Ecosystem_type",overlay = TRUE,
                                              gg = TRUE,  plot =FALSE)

Ecosystem_type_plot_PropSin <- ggplot(Ecosystem_type_plot_PropSin$res,aes(y= visregRes, x= Ecosystem_type,color=Ecosystem_type))+
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

grDevices::pdf(file = here::here("figures", "Figure6.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4

Cluster_Species_Groups_plot <- gridExtra::grid.arrange(Species_Groups_plot_logNC, Ecosystem_type_plot_logNC,
                                                       Species_Groups_plot_PropC1,Ecosystem_type_plot_PropC1,
                                                       Species_Groups_plot_PropSin, Ecosystem_type_plot_PropSin,
                                                       nrow = 3, ncol = 2)
dev.off()


#aov_cluster_table_df <- rbind(aov_logNC, aov_PropSin,aov_PropC1)
#aov_cluster_table_df <- data.frame(Variables = c(rep("Number of Cluster (log)",7),rep("% uniques",7)
#                                                 ,rep("% Cluster #1",7)),aov_cluster_table_df)

#aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Species)",]$Term <- "Number of Species (log)"
#aov_cluster_table_df[aov_cluster_table_df$Term=="log(Number of Traits)",]$Term <- "Number of Traits (log)"
#aov_cluster_table_df[aov_cluster_table_df$Term=="Percentage of NA",]$Term <- "% of Missing Values"
#aov_cluster_table_df[aov_cluster_table_df$Term=="Correlation",]$Term <- "Mean Correlation"

#aov_cluster_table_df <- aov_cluster_table_df %>% dplyr::mutate_at(vars("Sum.Sq","F.statistic","P.value",), dplyr::funs(round(., 3)))
#aov_cluster_table_df<-aov_cluster_table_df[aov_cluster_table_df$Term %in% c("Species_Groups", "Ecosystem_type"),]

#for(i in 1:nrow(aov_cluster_table_df)){ 
#  if(aov_cluster_table_df[i, 5]<0.001 )    { 
#    aov_cluster_table_df[i, 5] <- "<0.001"
#    aov_cluster_table_df[i, 5] <- kableExtra::cell_spec(aov_cluster_table_df[i, 5],  bold = T)
#  } 
  
#  if(aov_cluster_table_df[i, 5]<0.05 & aov_cluster_table_df[i, 5]>0.001)     {  
#    aov_cluster_table_df[i, 5] <- kableExtra::cell_spec(aov_cluster_table_df[i, 5],  bold = T)
#  } 
  
  
#}

#table_cluster_aov<-pixiedust::dust(aov_cluster_table_df) %>% 
#  kableExtra::kable( booktabs = T, escape = F)%>% 
#  kableExtra::kable_styling()%>% 
#  kableExtra::collapse_rows()
#table_cluster_aov