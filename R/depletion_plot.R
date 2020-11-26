
################################################################################
#' Function to draw graph
#' Allow to test influence of each trait and sensibility of the functional space to a specific functional trait
#'
#' @param data           trait data.frame  output of function compute_missing_trait_distance
#' @param version        3 types of graphs : 1,2 or 3
#'          
#################################################################################

depletion_plot <- function(data, version){      
  
  if (version == 1){
    
    #Plot 1  ---   
  mantel_r<- ggplot(data, aes(miss_percent, mantel_r)) +
      geom_jitter(color="magenta2",alpha=0.4,position=position_jitter(0.02))+ ylim(0,1)  +
      stat_summary(fun = mean, geom="line",color="magenta2",size=1.3,alpha=0.4)+
      stat_summary(fun.data = "mean_sdl", colour = "grey40", size=1.4)+
      theme_classic()+labs(x = "Percentage of depletion")+labs(y = "Mantel R")+
      scale_x_continuous(breaks = seq(0.15, 0.9, by = 0.15))
    
  MAD<- ggplot(data, aes(miss_percent, MAD)) +
    geom_jitter(color="dodgerblue1",alpha=0.4,position=position_jitter(0.02)) +
    stat_summary(fun = mean, geom="line",color="dodgerblue1",size=1.3,alpha=0.4)+
    stat_summary(fun.data = "mean_sdl", colour = "grey40", size=1.4)+
    theme_classic()+labs(x = "Percentage of depletion")+labs(y = "MAD")+
    scale_x_continuous(breaks = seq(0.15, 0.9, by = 0.15))
  
  AUC<- ggplot(data, aes(miss_percent, AUC)) +
    geom_jitter(color="chartreuse1",alpha=0.4,position=position_jitter(0.02)) + ylim(0,1)  +
    stat_summary(fun = mean, geom="line",color="chartreuse1",size=1.3,alpha=0.4)+
    stat_summary(fun.data = "mean_sdl", colour = "grey40", size=1.4)+
    theme_classic()+labs(x = "Percentage of depletion")+labs(y = "AUC")+
    scale_x_continuous(breaks = seq(0.15, 0.9, by = 0.15))

  grid.arrange(mantel_r,MAD,AUC,ncol=3,top = textGrob("Influence of trait depletion" ,gp=gpar(fontsize=20,font=3)))
  }
  # ---  
  
  if (version == 2){
    
    #Plot 2  ---        
    mantel_r2 <- ggplot(data, aes(x=as.factor(miss_percent), y=mantel_r,fill=as.factor(miss_percent))) + 
      geom_boxplot()+scale_fill_brewer(palette="YlOrRd")+theme_bw()+theme(legend.position = "none") +
      labs(x = "Percentage of depletion")+ labs(y = "Mantel R")+ ylim(0,1)
    
    MAD2 <- ggplot(data, aes(x=as.factor(miss_percent), y=MAD,fill=as.factor(miss_percent))) + 
      geom_boxplot()+scale_fill_brewer(palette="YlOrRd")+theme_bw()+theme(legend.position = "none") +
      labs(x = "Percentage of depletion")+ labs(y = "MAD")
    
    AUC2 <- ggplot(data, aes(x=as.factor(miss_percent), y=AUC,fill=as.factor(miss_percent))) + 
      geom_boxplot()+scale_fill_brewer(palette="YlOrRd")+theme_bw()+theme(legend.position = "none") +
      labs(x = "Percentage of depletion")+ labs(y = "AUC")
    
    grid.arrange(mantel_r2,MAD2,AUC2, ncol=3,top = textGrob("Influence of trait depletion" ,gp=gpar(fontsize=20,font=3)))
  }
  # --- 
  
  if (version == 3){
    
    #Plot 3  ---   
    mantel_r3<- ggplot(data, aes(x=as.factor(miss_percent), y=mantel_r, fill=as.factor(miss_percent), color=as.factor(miss_percent))) +
      geom_violin(width=1, size=0.2) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      viridis::scale_color_viridis(discrete=TRUE) +
      theme_bw() +
      theme(legend.position="none") +
      xlab("Percentage of depletion") +
      ylab("Mantel R")
    
    MAD3<- ggplot(data, aes(x=as.factor(miss_percent), y=MAD, fill=as.factor(miss_percent), color=as.factor(miss_percent))) +
      geom_violin(width=1, size=0.2) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      viridis::scale_color_viridis(discrete=TRUE) +
      theme_bw() +
      theme(legend.position="none") +
      xlab("Percentage of depletion") +
      ylab("MAD")
    
    AUC3<- ggplot(data, aes(x=as.factor(miss_percent), y=AUC, fill=as.factor(miss_percent), color=as.factor(miss_percent))) +
      geom_violin(width=1, size=0.2) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      viridis::scale_color_viridis(discrete=TRUE) +
      theme_bw() +
      theme(legend.position="none") +
      xlab("Percentage of depletion") +
      ylab("AUC")
    
    grid.arrange(mantel_r3,MAD3,AUC3,ncol=3,top = textGrob("Influence of trait depletion" ,gp=gpar(fontsize=20,font=3)))
  }
  
  
}                      
