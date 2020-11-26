
################################################################################
#' Function to draw graph of dimensionality
#' #'
#' @param data           trait data.frame  output of function compute_missing_trait_distance
#' @param version        3 types of graphs : 1,2 or 3
#'          
#################################################################################

dim_plot <- function(data_miss,dim_pcoa,metric){      
  
      #Plot 1  ---   
  #msd_plot<-ggplot(data_miss, aes(x=dim, y=quality_fspaces_msd)) + 
  #  stat_summary(fun = mean, geom="line",color="magenta2",size=1.3,alpha=0.4)+
  #  stat_summary(color="magenta2",size=1)+theme_bw()+labs(x = "Number of dimensions")+labs(y = "msd")+
  #  scale_x_continuous(limits=c(0, dim_pcoa),breaks=seq(1,dim_pcoa,1)) 
if (metric == "MAD")   {
 plot<-ggplot(data_miss, aes(x=dim, y=MAD)) + 
    stat_summary(fun = "mean", geom="line",color="dodgerblue1",size=1.3,alpha=0.4)+
    stat_summary(fun = "mean",color="dodgerblue1",size=1)+theme_bw()+labs(x = "Number of dimensions")+labs(y = "MAD")+
    scale_x_continuous(limits=c(0, dim_pcoa),breaks=seq(1,dim_pcoa,1)) 

} else {
  plot<-ggplot(data_miss, aes(x=dim, y=AUC)) + 
    stat_summary(fun = "mean", geom="line",color="chartreuse1",size=1.3,alpha=0.4)+
    stat_summary(fun = "mean",color="chartreuse1",size=1)+theme_bw()+labs(x = "Number of dimensions")+labs(y = "AUC")+
    scale_x_continuous(limits=c(0, dim_pcoa),breaks=seq(1,dim_pcoa,1)) 
  }

  grid.arrange(plot, ncol=1,top = textGrob("Influence of number of dimensions considered" ,gp=gpar(fontsize=20,font=3))) #msd_plot
  }

