
# Load Png
files <- list.files(path = png_dir, pattern = "*.png", full.names=TRUE)
all_im <- lapply(files, readPNG )
names(all_im) <- gsub(".*/icons/", "", files) 
names(all_im)  <- gsub(".png", "",names(all_im)) 

#function to add picture in wrap  
  annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ layer(data = data, stat = StatIdentity, position = PositionIdentity, 
                                       geom = ggplot2:::GeomCustomAnn,
                                       inherit.aes = TRUE, params = list(grob = grob, 
                                                                         xmin = xmin, xmax = xmax, 
                                                                         ymin = ymin, ymax = ymax))}                                                                                                            

# --------------------------------------------------------------------
# Results for model
files <- list.files(path=results_dir,pattern = "_res.RData",full.names = T)
list_res <- lapply(files, function(x) mget(load(x)))
files <- list.files(path=results_dir,pattern = "cor_",full.names = T)
list_res_cor <- lapply(files, function(x) mget(load(x)))
files <- list.files(path=results_dir,pattern = "_sigle.RData",full.names = T)
list_res_sigle <- lapply(files, function(x) mget(load(x)))
files <- list.files(path=results_dir,pattern = "_pcoa.RData",full.names = T)
list_res_pcoa <- lapply(files, function(x) mget(load(x)))


res_for_model <- t(data.frame(do.call(cbind, lapply(1:length(list_res),function(i){
    
  print(i)

  res <- data.frame(list_res[[i]][[1]][[1]])

    #Add information of PCOA Axis and correlation between traits
    res_pcoa1 <- data.frame(list_res_pcoa[[i]][[1]]$values[1,2])
    colnames(res_pcoa1) <- paste0("Relative_eig","_axe1")

    res_pcoa2 <- data.frame(list_res_pcoa[[i]][[1]]$values[2,2])
    colnames(res_pcoa2) <- paste0("Relative_eig","_axe2")
    
    res_pcoa3 <- data.frame(list_res_pcoa[[i]][[1]]$values[3,2])
    colnames(res_pcoa3) <- paste0("Relative_eig","_axe3") 
    
    res_cor<- data.frame(list_res_cor[[i]][[1]])
    
    #Add information on redundancy per cluster
    
    res_sigle <- t(data.frame(table(list_res_sigle[[i]][[1]]$cluster_core))[2:4,2])
    colnames(res_sigle) <- c("NbS_Cluster1","NbS_Cluster2", "NbS_Cluster3")
    res <- t(cbind(res,res_pcoa1,res_pcoa2,res_pcoa3,res_cor,res_sigle))
    
    return(res)
}))))


#at home rownames(res_for_model) <- gsub("/Users/nicolasloiseau/Dropbox/Clustering&Dimensionality/Dimension&Depletion/results/", "", files)
rownames(res_for_model) <- gsub("/Users/nloiseau/Dropbox/Clustering&Dimensionality/Dimension&Depletion/results/", "", files) 
rownames(res_for_model) <- gsub("_pcoa.RData", "", rownames(res_for_model)) 
res_for_model <- as.data.frame(res_for_model) 

res_for_model$Percentage_lostAUC_depleted0.5 <- res_for_model$Percentage_lostAUC_depleted0.5/100
res_for_model$Percentage_lostAUC_depleted0.20 <- res_for_model$Percentage_lostAUC_depleted0.20/100
colnames(res_for_model)[14] <- "rowAUClostwhen50percTraitdepleted"
colnames(res_for_model)[15] <- "rowAUClostwhen20percTraitdepleted"
#save(res_for_model,file=file.path(results_dir,"res_for_model.RData"))

load(file=file.path(results_dir,"res_for_model.RData"))
################################################################################################
#####################################Results and plot for dimension#############################
################################################################################################

#Figure 1 ---
      
      files <- list.files(path=results_dir,pattern = "_res.RData",full.names = T)
      list_res <- lapply(files, function(x) mget(load(x)))
      for(i in 1:length(list_res)){
        colnames(list_res[[i]][[1]][[3]])[2] <-"MAD"
               } 
      
      res_for_graph_dim <- data.frame(do.call(rbind, lapply(1:length(list_res),function(i){
        res <- data.frame(list_res[[i]][[1]][[3]])
        res <- data.frame(res,taxa=rep(gsub("_res", "", names(list_res[[i]]),nrow(res))))
        return(res)
      })))
       
      
      
      #Filoutage pour pouvoir trier les graph en fonction du nombre d'esp??ces ou du nombre de traits
      res_for_graph_dim[res_for_graph_dim$taxa=="birds",8] <- "birdstrait"
      res_for_graph_dim[res_for_graph_dim$taxa=="freshfish",8] <- "freshfishtrait"
      
      res_for_graph_dim$SP <-NA 
      res_for_graph_dim$trait <-NA 
      for (i in 1:nrow(res_for_graph_dim)){ 
        print(i)
      res_for_graph_dim$SP[i]<- res_for_model[rownames(res_for_model) %in% res_for_graph_dim$taxa[i],]$S
      res_for_graph_dim$trait[i]<- res_for_model[rownames(res_for_model) %in% res_for_graph_dim$taxa[i],]$Nb_trait
      res_for_graph_dim$elbow[i]<- res_for_model[rownames(res_for_model) %in% res_for_graph_dim$taxa[i],]$Nb_dim_AUC_elbow
      
      }
      
      res_for_graph_dim$selec_elbow_graph <-NA
      res_for_graph_dim$AUCwhenelbow <-NA
      for (i in 1:nrow(res_for_graph_dim)){ 
        print(i)
        if(res_for_graph_dim$dim[i] == res_for_graph_dim$elbow[i]) {
          
          res_for_graph_dim$selec_elbow_graph[i] <- res_for_graph_dim$dim[i]
          res_for_graph_dim$AUCwhenelbow[i] <- res_for_graph_dim$AUC[i]
        }
      }
      #To sort plot by number of species or traits
      res_for_graph_dim$taxa = factor(res_for_graph_dim$taxa, levels=unique(res_for_graph_dim$taxa[order(res_for_graph_dim$SP)]), ordered=TRUE)
      res_for_graph_dim <-res_for_graph_dim[order(res_for_graph_dim$taxa,decreasing = F),]
      
      #res_for_model$taxa <-rownames(res_for_model)
      #res_for_model$taxa = factor(res_for_model$taxa, levels=unique(res_for_model$taxa[order(res_for_graph_dim$SP)]), ordered=TRUE)
      #res_for_graph_dim <-res_for_graph_dim[order(res_for_graph_dim$taxa,decreasing = F),]
      #res_for_graph_dim <- merge(res_for_graph_dim,res_for_model, by.x="taxa",by.y="row.names",all.x=T)
      
      
      p <- ggplot(res_for_graph_dim, aes(x=dim, y=AUC,colour = taxa )) + 
        stat_summary(fun = "mean", geom="line",size=1,alpha=0.4)+
        stat_summary(fun = "mean",size=0.88)+theme_bw()+labs(x = "Number of dimensions")+labs(y = "Quality of species trait space (AUC)")+
        #geom_label(data = res_for_model, aes(0.5, 5, hjust = 1, 
         #              "Adj R2 = ",
         #               S,"\n",
         #                "Intercept =",Nb_trait)) +
      #geom_segment(data=res_for_graph_dim, aes(x = Nb_dim_AUC_0.5 , y = 0, 
      #              xend = Nb_dim_AUC_0.5, yend = AUC))+
      #geom_vline(data = res_for_model, mapping = aes(xintercept = Nb_dim_AUC_0.5)) +
      #geom_vline(data = res_for_model, mapping = aes(xintercept = Nb_dim_AUC_elbow),linetype="dotted")+
      
        #scale_x_continuous(limits=c(0, 20),breaks=seq(0,20,2))+ 
        facet_wrap(~ taxa,ncol = 6)  + theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.x = element_text( size=14, face="bold"),
              axis.title.y = element_text( size=14, face="bold"),
              panel.background = element_blank(),
              legend.position = "none")+  scale_colour_hp_d(option = "LunaLovegood")+
      geom_segment(data = ddply(res_for_graph_dim, "taxa", summarize, wavg = AUC), 
                  aes(x=res_for_graph_dim$selec_elbow_graph,xend=res_for_graph_dim$selec_elbow_graph,
                    y=0 ,yend=res_for_graph_dim$AUCwhenelbow ),color="black",linetype="dotted",size=1) +
      geom_segment(data = ddply(res_for_graph_dim, "taxa", summarize, wavg = AUC), 
                    aes(y=res_for_graph_dim$AUCwhenelbow,yend=res_for_graph_dim$AUCwhenelbow ,
                        x=0 ,xend=res_for_graph_dim$selec_elbow_graph),color="black",linetype="dotted",size=1) +
      geom_point(data = ddply(res_for_graph_dim, "taxa", summarize, wavg = AUC), 
                      aes(y=res_for_graph_dim$AUCwhenelbow,x=res_for_graph_dim$selec_elbow_graph),color="black",size=4,shape=19)+
      geom_label(data = res_for_graph_dim, aes(label= paste0("#S = ", SP ,"\n","#T = ",trait), y =0.12,x=5), size=3, hjust = 0) 

      
      
      a1 = annotation_custom2(rasterGrob(all_im$Eallonardo2013, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[1],])
      a2 = annotation_custom2(rasterGrob(all_im$Beetle, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[2],])
      a3 = annotation_custom2(rasterGrob(all_im$Gibb2015, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[3],])
      a4 = annotation_custom2(rasterGrob(all_im$Goncalves2014, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[4],])
      a5 = annotation_custom2(rasterGrob(all_im$Jeliazkov2013, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[5],])
      a6 = annotation_custom2(rasterGrob(all_im$Yates2014, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[6],])
      a7 = annotation_custom2(rasterGrob(all_im$Bartonova2016, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[7],])
      a8 = annotation_custom2(rasterGrob(all_im$NorthSeaTraits, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[8],])
      a9 = annotation_custom2(rasterGrob(all_im$Pakeman2011, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[9],])
      a10 = annotation_custom2(rasterGrob(all_im$Clearly2016, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[10],])
      a11 = annotation_custom2(rasterGrob(all_im$Flua, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[11],])
      a12 = annotation_custom2(rasterGrob(all_im$Diaz2008, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[12],])
      a13 = annotation_custom2(rasterGrob(all_im$Jeliazkov2013, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[13],])
      a14 = annotation_custom2(rasterGrob(all_im$ThermalFauna, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[14],])
      a15 = annotation_custom2(rasterGrob(all_im$coral, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[15],])
      a16 = annotation_custom2(rasterGrob(all_im$bacteria, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[16],])
      a17 = annotation_custom2(rasterGrob(all_im$Chondri, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[17],])
      a18 = annotation_custom2(rasterGrob(all_im$phytoplankton, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[18],])
      a19 = annotation_custom2(rasterGrob(all_im$USDA_plant, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[19],])
      a20 = annotation_custom2(rasterGrob(all_im$PalmTraits, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[20],])
      a21 = annotation_custom2(rasterGrob(all_im$plant_alps, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[21],])
      a22 = annotation_custom2(rasterGrob(all_im$mammalstrait, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[22],])
      a23 = annotation_custom2(rasterGrob(all_im$freshfish, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[23],])
      a24 = annotation_custom2(rasterGrob(all_im$birds, interpolate=TRUE), xmin=15, xmax=21, ymin=0.01, ymax=0.34, data=res_for_graph_dim[res_for_graph_dim$taxa==unique(res_for_graph_dim$taxa)[24],])
      
      pdf(file=file.path(fig_dir,"Figure1.pdf"), width = 11.7, height = 8.3)#SAVE A4
      p + a1 + a2 + a3 + a4 + a5 + a6  + a7 + a8 + a9 + a10 + 
        a11 + a12 + a13 + a14 + a15 + a16  + a17 + a18 + a19 + a20 + 
        a21 + a22 + a23 + a24
      dev.off()
      


 
#Figure 3 ---

files <- list.files(path=results_dir,pattern = "_miss.RData",full.names = T)
list_res <- lapply(files, function(x) mget(load(x)))

res_for_graph_miss <- na.omit(data.frame(do.call(rbind, lapply(1:length(list_res),function(i){
res <- data.frame(list_res[[i]][[1]])
res <- data.frame(res,taxa=rep(gsub("_miss", "", names(list_res[[i]]),nrow(res))))
return(res)
}))))

#Filoutage pour pouvoir trier les graph en fonction du nombre d'esp??ces ou du nombre de traits
res_for_graph_miss[res_for_graph_miss$taxa=="birds",7] <- "birdstrait"
res_for_graph_miss[res_for_graph_miss$taxa=="freshfish",7] <- "freshfishtrait"


res_for_graph_miss$SP <-NA 
res_for_graph_miss$trait <-NA 

for (i in 1:nrow(res_for_graph_miss)){ 
print(i)
res_for_graph_miss$SP[i]<- res_for_model[rownames(res_for_model) %in% res_for_graph_miss$taxa[i],]$S
res_for_graph_miss$trait[i]<- res_for_model[rownames(res_for_model) %in% res_for_graph_miss$taxa[i],]$Nb_trait
}


res_for_graph_miss$taxa <- factor(res_for_graph_miss$taxa, levels=unique(res_for_graph_miss$taxa[order(res_for_graph_miss$SP)]), ordered=TRUE)
res_for_graph_miss <- res_for_graph_miss[order(res_for_graph_miss$taxa,decreasing = F),]

p2 <- ggplot(res_for_graph_miss, aes(x=miss_percent*100, y=AUC,fill=as.factor(miss_percent*100))) + 
  geom_boxplot()+theme_bw()+labs(x = "Trait depletion (%)",y = "Quality of species trait space (AUC)",size = 14) +
  scale_fill_hp_d(option = "ronweasley2",direction = -1)+
  #geom_vline(data = res_for_model, mapping = aes(xintercept = Perc_miss_AUC_0.5)) +
  #geom_vline(data = res_for_model, mapping = aes(xintercept = Percentage_lostAUC_depleted0.5),linetype="dotted")+
  facet_wrap(~ taxa,ncol = 6)  + theme_bw() +
  scale_y_continuous(limits=c(0, 1),breaks=seq(0,1,0.25))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.position = "none")


b1 = annotation_custom2(rasterGrob(all_im$Eallonardo2013, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[1],])
b2 = annotation_custom2(rasterGrob(all_im$Beetle, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[2],])
b3 = annotation_custom2(rasterGrob(all_im$Gibb2015, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[3],])
b4 = annotation_custom2(rasterGrob(all_im$Goncalves2014, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[4],])
b5 = annotation_custom2(rasterGrob(all_im$Jeliazkov2013, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[5],])
b6 = annotation_custom2(rasterGrob(all_im$Yates2014, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[6],])
b7 = annotation_custom2(rasterGrob(all_im$Bartonova2016, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[7],])
b8 = annotation_custom2(rasterGrob(all_im$NorthSeaTraits, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[8],])
b9 = annotation_custom2(rasterGrob(all_im$Pakeman2011, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[9],])
b10 = annotation_custom2(rasterGrob(all_im$Clearly2016, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[10],])
b11 = annotation_custom2(rasterGrob(all_im$Flua, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[11],])
b12 = annotation_custom2(rasterGrob(all_im$Diaz2008, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[12],])
b13 = annotation_custom2(rasterGrob(all_im$Jeliazkov2013, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[13],])
b14 = annotation_custom2(rasterGrob(all_im$ThermalFauna, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[14],])
b15 = annotation_custom2(rasterGrob(all_im$coral, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[15],])
b16 = annotation_custom2(rasterGrob(all_im$bacteria, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[16],])
b17 = annotation_custom2(rasterGrob(all_im$Chondri, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[17],])
b18 = annotation_custom2(rasterGrob(all_im$phytoplankton, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[18],])
b19 = annotation_custom2(rasterGrob(all_im$USDA_plant, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[19],])
b20 = annotation_custom2(rasterGrob(all_im$PalmTraits, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[20],])
b21 = annotation_custom2(rasterGrob(all_im$plant_alps, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[21],])
b22 = annotation_custom2(rasterGrob(all_im$mammalstrait, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[22],])
b23 = annotation_custom2(rasterGrob(all_im$freshfish, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[23],])
b24 = annotation_custom2(rasterGrob(all_im$birds, interpolate=TRUE), xmin=60, xmax=85, ymin=0.78, ymax=1, data=res_for_graph_miss[res_for_graph_miss$taxa==unique(res_for_graph_miss$taxa)[24],])

pdf(file=file.path(fig_dir,"Figure3.pdf"), width = 11.7, height = 8.3)#SAVE A4
p2 + b1 + b2 + b3 + b4 + b5 + b6  + b7 + b8 + b9 + b10 + 
  b11 + b12 + b13 + b14 + b15 + b16  + b17 + b18 + b19 + b20 + 
  b21 + b22 + b23 + b24
dev.off()




#Figure 6---
    files <- list.files(path=results_dir,pattern = "_sigle.RData",full.names = T)
    list_res_sigle <- lapply(files, function(x) mget(load(x)))
    files <- list.files(path=results_dir,pattern = "_pcoa.RData",full.names = T)
    list_res_pcoa <- lapply(files, function(x) mget(load(x)))
    
    res_for_graph_sigle <- na.omit(data.frame(do.call(rbind, lapply(1:length(list_res_sigle),function(i){
      print(i)
      res_sigle <- data.frame(list_res_sigle[[i]][[1]]$cluster_core)
      # cluster_core = 1 === singleton
      res_sigle[res_sigle[,1]>0,]<-2
      res_sigle[res_sigle[,1]==0,]<-1
      res_sigle[res_sigle[,1]>1,]<-0
      res_sigle <- data.frame(res_sigle,taxa=rep(gsub("_sigle", "", names(list_res_sigle[[i]]),nrow(res_sigle))))
      
      res_pcoa <- data.frame(list_res_pcoa[[i]][[1]]$vectors)[,c(1:3)]
    
      res_cluster <- list_res_sigle[[i]][[1]]$cluster_core
      res <- cbind(res_sigle,res_pcoa,res_cluster)
      colnames(res) <- c("Single","taxa", "Pcoa1", "Pcoa2", "Pcoa3","cluster_ID")
      return(res)
    }))))
    
    #res_for_graph_sigle[res_for_graph_sigle$taxa=="birdstrait",2] <- "birds"
    #res_for_graph_sigle[res_for_graph_sigle$taxa=="freshfishtrait",2] <- "freshfish"
    res_for_graph_sigle$SP <-NA 
    res_for_graph_sigle$trait <-NA 
    
    for (i in 1:nrow(res_for_graph_sigle)){ 
      print(i)
      res_for_graph_sigle$SP[i]<- res_for_model[rownames(res_for_model) %in% res_for_graph_sigle$taxa[i],]$S
      res_for_graph_sigle$trait[i]<- res_for_model[rownames(res_for_model) %in% res_for_graph_sigle$taxa[i],]$Nb_trait
    }
    
    
    res_for_graph_sigle$taxa = factor(res_for_graph_sigle$taxa, levels=unique(res_for_graph_sigle$taxa[order(res_for_graph_sigle$SP)]), ordered=TRUE)
    res_for_graph_sigle <-res_for_graph_sigle[order(res_for_graph_sigle$taxa,decreasing = F),]
    
    res_for_graph_sigle$Pcoa1   <- jitter(as.numeric(as.character(res_for_graph_sigle$Pcoa1)),factor=50)
    res_for_graph_sigle$Pcoa2   <- jitter(as.numeric(as.character(res_for_graph_sigle$Pcoa2)),factor=50)
    res_for_graph_sigle$Pcoa3   <- as.numeric(as.character(res_for_graph_sigle$Pcoa3))
    res_for_graph_sigle$Single  <- as.factor(as.character(res_for_graph_sigle$Single))
    res_for_graph_sigle$cluster_ID[res_for_graph_sigle$cluster_ID!=1] <- 0
    
    
    #jitval=50000
    #res_for_graph_sigle$Pcoa1 <- jitter(res_for_graph_sigle$Pcoa1,jitval)
    #res_for_graph_sigle$Pcoa2 <- jitter(res_for_graph_sigle$Pcoa2,jitval)
    
    # cluster_core = 1 === singleton
    p3 <- ggplot(res_for_graph_sigle, aes(x=Pcoa1, y=Pcoa2,colour = taxa )) + 
        geom_point(aes(alpha=Single,shape=Single),size=0.7)+ #
          scale_shape_manual(values=c(4, 16))+
         scale_alpha_manual(values=c(0.3, 0.8))+
        geom_encircle(s_shape = 1, expand = 0,size=3,
                     alpha = 0.7, show.legend = FALSE)+
           theme_bw()+labs(x = "PCoA axis 1")+labs(y = "PCoA axis 2") +
        facet_wrap(~ taxa,ncol = 6, scales = "free")  +
        scale_colour_hp_d(option = "LunaLovegood",direction = 1)+
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              panel.grid.major = element_blank(), 
               panel.background = element_blank(),
              legend.position = "none",
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_text( size=14, face="bold"),
                axis.title.y = element_text( size=14, face="bold"),
                axis.ticks = element_blank()
              )
    
    
    hull <- NULL
    for (i in 1:length(unique(res_for_graph_sigle$taxa))){
      sub <- res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[i],]
      sub_hull <- sub[sub$cluster_ID==1,] %>%
        slice(chull(Pcoa1, Pcoa2)) 
      hull <-rbind(hull,sub_hull)
    }
    
    
    # Overlay the convex hull of the most important cluster
    #p3 <- p3 + geom_point(data = hull, alpha = 0.5,size=1,color="black",fill = NA, 
    #                 show.legend = FALSE,shape=4) +geom_polygon(data = hull, alpha = 0.2,size=0.8,color="black",fill = NA, 
    #                                                   show.legend = FALSE) 
    
    
    
    c1 = annotation_custom2(rasterGrob(all_im$Eallonardo2013, interpolate=TRUE), xmin=0.2, xmax=0.32, ymin=0, ymax=0.12, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[1],])
    c2 = annotation_custom2(rasterGrob(all_im$Beetle, interpolate=TRUE), xmin=0.2, xmax=0.3, ymin=0.2, ymax=0.3, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[2],])
    c3 = annotation_custom2(rasterGrob(all_im$Gibb2015, interpolate=TRUE), xmin=-0.6, xmax=-0.35, ymin=-0.25, ymax=-0.07, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[3],])
    c4 = annotation_custom2(rasterGrob(all_im$Goncalves2014, interpolate=TRUE), xmin=0.1, xmax=0.3, ymin=-0.4, ymax=-0.25, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[4],])
    c5 = annotation_custom2(rasterGrob(all_im$Jeliazkov2013, interpolate=TRUE), xmin=0.1, xmax=0.2, ymin=0.17, ymax=0.28, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[5],])
    c6 = annotation_custom2(rasterGrob(all_im$Yates2014, interpolate=TRUE), xmin=0.22, xmax=0.45, ymin=0.05, ymax=0.2, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[6],])
    c7 = annotation_custom2(rasterGrob(all_im$Bartonova2016, interpolate=TRUE), xmin=0.35, xmax=0.52, ymin=-0.25, ymax=-0.11, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[7],])
    c8 = annotation_custom2(rasterGrob(all_im$NorthSeaTraits, interpolate=TRUE), xmin=0.17, xmax=0.32, ymin=0.1, ymax=0.25, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[8],])
    c9 = annotation_custom2(rasterGrob(all_im$Pakeman2011, interpolate=TRUE), xmin=-0.3, xmax=-0.2, ymin=-0.3, ymax=-0.11, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[9],])
    c10 = annotation_custom2(rasterGrob(all_im$Clearly2016, interpolate=TRUE), xmin=0.26, xmax=0.55, ymin=0.4, ymax=0.6, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[10],])
    c11 = annotation_custom2(rasterGrob(all_im$Flua, interpolate=TRUE), xmin=0.075, xmax=0.22, ymin=-0.32, ymax=-0.18, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[11],])
    c12 = annotation_custom2(rasterGrob(all_im$Diaz2008, interpolate=TRUE), xmin=0.18, xmax=0.32, ymin=0.07, ymax=0.18, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[12],])
    c13 = annotation_custom2(rasterGrob(all_im$Jeliazkov2013, interpolate=TRUE), xmin=0.1, xmax=0.25, ymin=0.2, ymax=0.35, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[13],])
    c14 = annotation_custom2(rasterGrob(all_im$ThermalFauna, interpolate=TRUE), xmin=0.35, xmax=0.6, ymin=0.2, ymax=0.5, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[14],])
    c15 = annotation_custom2(rasterGrob(all_im$coral, interpolate=TRUE), xmin=0.5, xmax=1, ymin=0.3, ymax=0.65, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[15],])
    c16 = annotation_custom2(rasterGrob(all_im$bacteria, interpolate=TRUE), xmin=0.18, xmax=0.3, ymin=0.15, ymax=0.27, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[16],])
    c17 = annotation_custom2(rasterGrob(all_im$Chondri, interpolate=TRUE), xmin=0.27, xmax=0.58, ymin=-0.05, ymax=0.2, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[17],])
    c18 = annotation_custom2(rasterGrob(all_im$phytoplankton, interpolate=TRUE), xmin=-0.05, xmax=-0.2, ymin=-0.35, ymax=-0.2, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[18],])
    c19 = annotation_custom2(rasterGrob(all_im$USDA_plant, interpolate=TRUE), xmin=0.15, xmax=0.3, ymin=0.1, ymax=0.25, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[19],])
    c20 = annotation_custom2(rasterGrob(all_im$PalmTraits, interpolate=TRUE), xmin=0.25, xmax=0.5, ymin=0.25, ymax=0.45, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[20],])
    c21 = annotation_custom2(rasterGrob(all_im$plant_alps, interpolate=TRUE), xmin=0.24, xmax=0.5, ymin=-0.45, ymax=-0.25, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[21],])
    c22 = annotation_custom2(rasterGrob(all_im$mammalstrait, interpolate=TRUE), xmin=-0.3, xmax=-0.15, ymin=-0.3, ymax=-0.15, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[22],])
    c23 = annotation_custom2(rasterGrob(all_im$freshfish, interpolate=TRUE), xmin=-0.1, xmax=-0.25, ymin=-0.3, ymax=-0.2, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[23],])
    c24 = annotation_custom2(rasterGrob(all_im$birds, interpolate=TRUE), xmin=0.075, xmax=0.15, ymin=-0.13, ymax=-0.09, data=res_for_graph_sigle[res_for_graph_sigle$taxa==unique(res_for_graph_sigle$taxa)[24],])
    
    pdf(file=file.path(fig_dir,"Figure6.pdf"), width = 11.7, height = 8.3)#SAVE A4
    p3 + c1 + c2 + c3 + c4 + c5 + c6  + c7 + c8 + c9 + c10 + 
        c11 + c12 + c13 + c14 + c15 + c16  + c17 + c18 + c19 + c20 + 
        c21 + c22 + c23 + c24
    dev.off()  
     
