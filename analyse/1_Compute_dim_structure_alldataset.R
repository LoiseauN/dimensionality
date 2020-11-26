# Set 1 --------------------------------------------------------------------Bartonova2016
        
          Bartonova2016 <- read.csv2(file.path(data_dir,"Bartonova2016.csv"),  header=TRUE,row.names = 1)
         
          #Trait prep and cat --------------------------------------------------------------------
          for (i in 1: ncol(Bartonova2016)){
            Bartonova2016[,i] <- as.numeric(as.character(Bartonova2016[,i]))
          }

          Bartonova2016_cat <- data.frame(
          trait_name        = colnames(head(Bartonova2016)),
          trait_type        = rep(NA,ncol(Bartonova2016)),
          fuzzy_name        = rep(NA,ncol(Bartonova2016)),
          stringsAsFactors  = FALSE)
          
          cor_Bartonova2016 <- matrix(0,ncol(Bartonova2016),ncol(Bartonova2016))
          
          for ( i in 1:ncol(Bartonova2016))
          {
            for (j in i:ncol(Bartonova2016))
            {
              cor_Bartonova2016[i,j] <-  cor(rank(Bartonova2016[,i]), rank(Bartonova2016[,j]),method="kendall")
            }
          }
          cor_Bartonova2016[lower.tri(cor_Bartonova2016)] <- t(cor_Bartonova2016)[lower.tri(cor_Bartonova2016)]
          diag(cor_Bartonova2016)=NA
          cor_Bartonova2016<- data.frame(mean_cor=mean(abs(cor_Bartonova2016),na.rm=T),sd_cor=sd(abs(cor_Bartonova2016),na.rm=T),
                                         max_cor=max(abs(cor_Bartonova2016),na.rm=T),min_cor=min(abs(cor_Bartonova2016),na.rm=T))
          save(cor_Bartonova2016,file=file.path(results_dir,"cor_Bartonova2016.RData"))

          #Depletion -----------------------------------------------------------
          Bartonova2016_miss <- compute_missing_trait_distance(Bartonova2016, Bartonova2016_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                               classical_gower = TRUE,pos= 100)
          save(Bartonova2016_miss,file=file.path(results_dir,"Bartonova2016_miss.RData"))
                  #Plot  --------
                  depletion_plot(Bartonova2016_miss,version=1)

                  #Table result Depletion  --------
                  Bartonova2016_miss_final <- Bartonova2016_miss
                  Bartonova2016_miss_final <-  aggregate(x = Bartonova2016_miss_final, by =Bartonova2016_miss_final["miss_percent"], FUN = mean)[,-2]
                  
          #Dimension  -----------------------------------------------------------
         
          Bartonova2016_dim <- dimension_funct(Bartonova2016, dim_pcoa = 20,  metric_scaled = TRUE, Bartonova2016_cat, classical_gower=TRUE,rep=999 ,cores = 3)
          
                  #Plot  --------
                  dim_plot(Bartonova2016_dim,dim_pcoa = 20)
           save(Bartonova2016_dim,file=file.path(results_dir,"Bartonova2016_dim.RData"))
          #Sigleton  -----------------------------------------------------------
      
          Bartonova2016_dist <- as.dist(as.matrix(daisy(Bartonova2016,metric = "gower")))
          Bartonova2016_pcoa <- pcoa(Bartonova2016_dist)
          save(Bartonova2016_pcoa,file=file.path(results_dir,"Bartonova2016_pcoa.RData"))
          
          Bartonova2016_sigle <-DPC(Bartonova2016_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
          save(Bartonova2016_sigle,file=file.path(results_dir,"Bartonova2016_sigle.RData"))
                 
           #Table result Sigleton  --------
                  Bartonova2016_sigle <- data.frame(rho          = Bartonova2016_sigle$rho,
                                                    cluster_core = Bartonova2016_sigle$cluster_core,
                                                    cluster      = Bartonova2016_sigle$cluster)
   
          #All results  -----------------------------------------------------------
                #Summary res-----------------------------------------------------------
                Bartonova2016_summary_res <-  synth_results(trait_df =Bartonova2016,miss_final_df = Bartonova2016_miss_final, dim_df = Bartonova2016_dim,single_df = Bartonova2016_sigle)
               
                Bartonova2016_res <- list(summary_res    = Bartonova2016_summary_res,
                                    depletion = Bartonova2016_miss_final,
                                    dimension = Bartonova2016_dim,
                                    cluster = Bartonova2016_sigle)
          
              save(Bartonova2016_res,file=file.path(results_dir,"Bartonova2016_res.RData"))
# Set 2 --------------------------------------------------------------------Clearly2016
        
          Clearly2016 <- read.csv2(file.path(data_dir,"Clearly2016.csv"),  header=TRUE,row.names = 1)
          
          #Trait prep and cat --------------------------------------------------------------------
          for (i in c(1:10,15)){
            Clearly2016[,i] <- as.numeric(as.character(Clearly2016[,i]))
          }
          
          for (i in c(11,12)){
            Clearly2016[,i] <- factor(as.character(Clearly2016[,i],level=TRUE))
          }
          
          Clearly2016_cat <- data.frame(
            trait_name        = colnames(head(Clearly2016)),
            trait_type        = rep(NA,ncol(Clearly2016)),
            fuzzy_name        = rep(NA,ncol(Clearly2016)),
            stringsAsFactors  = FALSE)
          
          #Correlation Traits -----------------------------------------------------------
          cor_Clearly2016 <- matrix(0,ncol(Clearly2016),ncol(Clearly2016))
          
          for ( i in 1:ncol(Clearly2016))
          {
            for (j in i:ncol(Clearly2016))
            {
              cor_Clearly2016[i,j] <-  cor(rank(Clearly2016[,i]), rank(Clearly2016[,j]),method="kendall")
            }
          }
          cor_Clearly2016[lower.tri(cor_Clearly2016)] <- t(cor_Clearly2016)[lower.tri(cor_Clearly2016)]
          diag(cor_Clearly2016)=NA
          cor_Clearly2016<- data.frame(mean_cor=mean(abs(cor_Clearly2016),na.rm=T),sd_cor=sd(abs(cor_Clearly2016),na.rm=T),
                                         max_cor=max(abs(cor_Clearly2016),na.rm=T),min_cor=min(abs(cor_Clearly2016),na.rm=T))
          save(cor_Clearly2016,file=file.path(results_dir,"cor_Clearly2016.RData"))
          
          #Depletion -----------------------------------------------------------
          Clearly2016_miss <- compute_missing_trait_distance(Clearly2016, Clearly2016_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                               classical_gower = TRUE,pos= 100)
          save(Clearly2016_miss,file=file.path(results_dir,"Clearly2016_miss.RData"))
          
          #Plot  --------
          depletion_plot(Clearly2016_miss,version=1)
          
          #Table result Depletion  --------
          Clearly2016_miss_final <- Clearly2016_miss
          Clearly2016_miss_final <-  aggregate(x = Clearly2016_miss_final, by =Clearly2016_miss_final["miss_percent"], FUN = mean)[,-2]
          
          #Dimension  -----------------------------------------------------------
          
          Clearly2016_dim <- dimension_funct(Clearly2016, dim_pcoa = 20,  metric_scaled = TRUE, Clearly2016_cat, classical_gower=TRUE,rep=999 ,cores = 3)
          
          #Plot  --------
          dim_plot(Clearly2016_dim,dim_pcoa = 20)
          
          save(Clearly2016_dim,file=file.path(results_dir,"Clearly2016_dim.RData"))
          #Sigleton  -----------------------------------------------------------
          
          Clearly2016_dist <- as.dist(as.matrix(daisy(Clearly2016,metric = "gower")))
          Clearly2016_pcoa <- pcoa(Clearly2016_dist)
          save(Clearly2016_pcoa,file=file.path(results_dir,"Clearly2016_pcoa.RData"))
          
          Clearly2016_sigle <-DPC(Clearly2016_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
          save(Clearly2016_sigle,file=file.path(results_dir,"Clearly2016_sigle.RData"))
          #Table result Sigleton  --------
          Clearly2016_sigle <- data.frame(rho          = Clearly2016_sigle$rho,
                                            cluster_core = Clearly2016_sigle$cluster_core,
                                            cluster      = Clearly2016_sigle$cluster)
          
          #All results  -----------------------------------------------------------
          #Summary res-----------------------------------------------------------
          Clearly2016_summary_res <-  synth_results(trait_df =Clearly2016,miss_final_df = Clearly2016_miss_final, dim_df = Clearly2016_dim,single_df = Clearly2016_sigle)
          
          Clearly2016_res <- list(summary_res    = Clearly2016_summary_res,
                                    depletion = Clearly2016_miss_final,
                                    dimension = Clearly2016_dim,
                                    cluster = Clearly2016_sigle)
          
          save(Clearly2016_res,file=file.path(results_dir,"Clearly2016_res.RData"))
# Set 3 --------------------------------------------------------------------Ribera2001
          Ribera2001 <- read.csv2(file.path(data_dir,"Ribera2001.csv"),  header=TRUE,row.names = 1)
          
          #Trait prep and cat --------------------------------------------------------------------
          for (i in c(1:10)){
            Ribera2001[,i] <- as.numeric(as.character(Ribera2001[,i]))
          }
          
          for (i in c(11:ncol(Ribera2001))){
            Ribera2001[,i] <- factor(as.character(Ribera2001[,i]))
          }
          
          Ribera2001_cat <- data.frame(
            trait_name        = colnames(head(Ribera2001)),
            trait_type        = rep(NA,ncol(Ribera2001)),
            fuzzy_name        = rep(NA,ncol(Ribera2001)),
            stringsAsFactors  = FALSE)
          
          #Correlation Traits -----------------------------------------------------------
          cor_Ribera2001 <- matrix(0,ncol(Ribera2001),ncol(Ribera2001))
          
          for ( i in 1:ncol(Ribera2001))
          {
            for (j in i:ncol(Ribera2001))
            {
              cor_Ribera2001[i,j] <-  cor(rank(Ribera2001[,i]), rank(Ribera2001[,j]),method="kendall")
            }
          }
          cor_Ribera2001[lower.tri(cor_Ribera2001)] <- t(cor_Ribera2001)[lower.tri(cor_Ribera2001)]
          diag(cor_Ribera2001)=NA
          cor_Ribera2001<- data.frame(mean_cor=mean(abs(cor_Ribera2001),na.rm=T),sd_cor=sd(abs(cor_Ribera2001),na.rm=T),
                                       max_cor=max(abs(cor_Ribera2001),na.rm=T),min_cor=min(abs(cor_Ribera2001),na.rm=T))
          save(cor_Ribera2001,file=file.path(results_dir,"cor_Ribera2001.RData"))
          
          
          #Depletion -----------------------------------------------------------
          Ribera2001_miss <- compute_missing_trait_distance(Ribera2001, Ribera2001_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                               classical_gower = TRUE,pos= 100)
          save(Ribera2001_miss,file=file.path(results_dir,"Ribera2001_miss.RData"))
          
          #Plot  --------
          depletion_plot(Ribera2001_miss,version=1)
          
          #Table result Depletion  --------
          Ribera2001_miss_final <- Ribera2001_miss
          Ribera2001_miss_final <-  aggregate(x = Ribera2001_miss_final, by =Ribera2001_miss_final["miss_percent"], FUN = mean)[,-2]
          
          #Dimension  -----------------------------------------------------------
          
          Ribera2001_dim <- dimension_funct(Ribera2001, dim_pcoa = 20,  metric_scaled = TRUE, Ribera2001_cat, classical_gower=TRUE,rep=999 ,cores = 3)
          
          #Plot  --------
          dim_plot(Ribera2001_dim,dim_pcoa = 20)
          
          save(Ribera2001_dim,file=file.path(results_dir,"Ribera2001_dim.RData"))
          #Sigleton  -----------------------------------------------------------
          
          Ribera2001_dist <- as.dist(as.matrix(daisy(Ribera2001,metric = "gower")))
          Ribera2001_pcoa <- pcoa(Ribera2001_dist)
          save(Ribera2001_pcoa,file=file.path(results_dir,"Ribera2001_pcoa.RData"))
          
          Ribera2001_sigle <-DPC(Ribera2001_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
          save(Ribera2001_sigle,file=file.path(results_dir,"Ribera2001_sigle.RData"))
          
          #Table result Sigleton  --------
          Ribera2001_sigle <- data.frame(rho          = Ribera2001_sigle$rho,
                                            cluster_core = Ribera2001_sigle$cluster_core,
                                            cluster      = Ribera2001_sigle$cluster)
          
          #All results  -----------------------------------------------------------
          #Summary res-----------------------------------------------------------
          Ribera2001_summary_res <-  synth_results(trait_df =Ribera2001,miss_final_df = Ribera2001_miss_final, dim_df = Ribera2001_dim,single_df = Ribera2001_sigle)
          
          Ribera2001_res <- list(summary_res    = Ribera2001_summary_res,
                                    depletion = Ribera2001_miss_final,
                                    dimension = Ribera2001_dim,
                                    cluster = Ribera2001_sigle)
          
          save(Ribera2001_res,file=file.path(results_dir,"Ribera2001_res.RData"))           
          
# Set 4 --------------------------------------------------------------------Pakeman2011
          Pakeman2011 <- read.csv2(file.path(data_dir,"Pakeman2011.csv"),  header=TRUE,row.names = 1)
          
          #Trait prep and cat --------------------------------------------------------------------
          for (i in c(3:4,23:28)){
            Pakeman2011[,i] <- as.numeric(as.character(Pakeman2011[,i]))
          }
          
          for (i in c(5:22)){
            Pakeman2011[,i] <- factor(as.character(Pakeman2011[,i],level=TRUE))
          }
          
          Pakeman2011_cat <- data.frame(
            trait_name        = colnames(head(Pakeman2011)),
            trait_type        = rep(NA,ncol(Pakeman2011)),
            fuzzy_name        = rep(NA,ncol(Pakeman2011)),
            stringsAsFactors  = FALSE)
         
          #Correlation Traits -----------------------------------------------------------
          cor_Pakeman2011 <- matrix(0,ncol(Pakeman2011),ncol(Pakeman2011))
          
          for ( i in 1:ncol(Pakeman2011))
          {
            for (j in i:ncol(Pakeman2011))
            {
              cor_Pakeman2011[i,j] <-  cor(rank(Pakeman2011[,i]), rank(Pakeman2011[,j]),method="kendall")
            }
          }
          cor_Pakeman2011[lower.tri(cor_Pakeman2011)] <- t(cor_Pakeman2011)[lower.tri(cor_Pakeman2011)]
          diag(cor_Pakeman2011)=NA
          cor_Pakeman2011<- data.frame(mean_cor=mean(abs(cor_Pakeman2011),na.rm=T),sd_cor=sd(abs(cor_Pakeman2011),na.rm=T),
                                       max_cor=max(abs(cor_Pakeman2011),na.rm=T),min_cor=min(abs(cor_Pakeman2011),na.rm=T))
          save(cor_Pakeman2011,file=file.path(results_dir,"cor_Pakeman2011.RData"))
          
          #Depletion -----------------------------------------------------------
          Pakeman2011_miss <- compute_missing_trait_distance(Pakeman2011, Pakeman2011_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                               classical_gower = TRUE,pos= 100)
          save(Pakeman2011_miss,file=file.path(results_dir,"Pakeman2011_miss.RData"))
          #Plot  --------
          depletion_plot(Pakeman2011_miss,version=1)
          
          #Table result Depletion  --------
          Pakeman2011_miss_final <- Pakeman2011_miss
          Pakeman2011_miss_final <-  aggregate(x = Pakeman2011_miss_final, by =Pakeman2011_miss_final["miss_percent"], FUN = mean)[,-2]
          
          #Dimension  -----------------------------------------------------------
          
          Pakeman2011_dim <- dimension_funct(Pakeman2011, dim_pcoa = 20,  metric_scaled = TRUE, Pakeman2011_cat, classical_gower=TRUE,rep=999 ,cores = 3)
          
          #Plot  --------
          dim_plot(Pakeman2011_dim,dim_pcoa = 20)
          save(Pakeman2011_dim,file=file.path(results_dir,"Pakeman2011_dim.RData"))
          
          #Sigleton  -----------------------------------------------------------
          
          Pakeman2011_dist <- as.dist(as.matrix(daisy(Pakeman2011,metric = "gower")))
          Pakeman2011_pcoa <- pcoa(Pakeman2011_dist)
          save(Pakeman2011_pcoa,file=file.path(results_dir,"Pakeman2011_pcoa.RData"))
          
          Pakeman2011_sigle <-DPC(Pakeman2011_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
          save(Pakeman2011_sigle,file=file.path(results_dir,"Pakeman2011_sigle.RData"))
          
          #Table result Sigleton  --------
          Pakeman2011_sigle <- data.frame(rho          = Pakeman2011_sigle$rho,
                                            cluster_core = Pakeman2011_sigle$cluster_core,
                                            cluster      = Pakeman2011_sigle$cluster)
          
          #All results  -----------------------------------------------------------
          #Summary res-----------------------------------------------------------
          Pakeman2011_summary_res <-  synth_results(trait_df =Pakeman2011,miss_final_df = Pakeman2011_miss_final, dim_df = Pakeman2011_dim,single_df = Pakeman2011_sigle)
          
          Pakeman2011_res <- list(summary_res    = Pakeman2011_summary_res,
                                    depletion = Pakeman2011_miss_final,
                                    dimension = Pakeman2011_dim,
                                    cluster = Pakeman2011_sigle)
          
          save(Pakeman2011_res,file=file.path(results_dir,"Pakeman2011_res.RData"))
          
# Set 5 --------------------------------------------------------------------Goncalves2014
          Goncalves2014 <- read.csv2(file.path(data_dir,"Goncalves2014.csv"),  header=TRUE,row.names = 1)
          
          #Trait prep and cat --------------------------------------------------------------------
          for (i in 1){
            Goncalves2014[,i] <- as.factor(as.character(Goncalves2014[,i]))
          }
          
          for (i in c(2:6)){
            Goncalves2014[,i] <- as.numeric(as.character(Goncalves2014[,i]))
          }
          
          Goncalves2014_cat <- data.frame(
            trait_name        = colnames(head(Goncalves2014)),
            trait_type        = rep(NA,ncol(Goncalves2014)),
            fuzzy_name        = rep(NA,ncol(Goncalves2014)),
            stringsAsFactors  = FALSE)
          
          #Correlation Traits -----------------------------------------------------------
          cor_Goncalves2014 <- matrix(0,ncol(Goncalves2014),ncol(Goncalves2014))
          
          for ( i in 1:ncol(Goncalves2014))
          {
            for (j in i:ncol(Goncalves2014))
            {
              cor_Goncalves2014[i,j] <-  cor(rank(Goncalves2014[,i]), rank(Goncalves2014[,j]),method="kendall")
            }
          }
          cor_Goncalves2014[lower.tri(cor_Goncalves2014)] <- t(cor_Goncalves2014)[lower.tri(cor_Goncalves2014)]
          diag(cor_Goncalves2014)=NA
          cor_Goncalves2014<- data.frame(mean_cor=mean(abs(cor_Goncalves2014),na.rm=T),sd_cor=sd(abs(cor_Goncalves2014),na.rm=T),
                                       max_cor=max(abs(cor_Goncalves2014),na.rm=T),min_cor=min(abs(cor_Goncalves2014),na.rm=T))
          save(cor_Goncalves2014,file=file.path(results_dir,"cor_Goncalves2014.RData"))
          
          #Depletion -----------------------------------------------------------
          Goncalves2014_miss <- compute_missing_trait_distance(Goncalves2014, Goncalves2014_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                               classical_gower = TRUE,pos= 100)
          save(Goncalves2014_miss,file=file.path(results_dir,"Goncalves2014_miss.RData"))
          
          #Plot  --------
          depletion_plot(Goncalves2014_miss,version=1)
          
          #Table result Depletion  --------
          Goncalves2014_miss_final <- Goncalves2014_miss
          Goncalves2014_miss_final <-  aggregate(x = Goncalves2014_miss_final, by =Goncalves2014_miss_final["miss_percent"], FUN = mean)[,-2]
          
          #Dimension  -----------------------------------------------------------
          
          Goncalves2014_dim <- dimension_funct(Goncalves2014, dim_pcoa = 20,  metric_scaled = TRUE, Goncalves2014_cat, classical_gower=TRUE,rep=999 ,cores = 3)
          
          #Plot  --------
          dim_plot(Goncalves2014_dim,dim_pcoa = 20)
          
          save(Goncalves2014_dim,file=file.path(results_dir,"Goncalves2014_dim.RData"))
          
          #Sigleton  -----------------------------------------------------------
          
          Goncalves2014_dist <- as.dist(as.matrix(daisy(Goncalves2014,metric = "gower")))
          Goncalves2014 <- pcoa(Goncalves2014_dist)
          save(Goncalves2014,file=file.path(results_dir,"Goncalves2014_pcoa.RData"))
          
          Goncalves2014_sigle <-DPC(Goncalves2014_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
          save(Goncalves2014_sigle,file=file.path(results_dir,"Goncalves2014_sigle.RData"))
          
          #Table result Sigleton  --------
          Goncalves2014_sigle <- data.frame(rho          = Goncalves2014_sigle$rho,
                                            cluster_core = Goncalves2014_sigle$cluster_core,
                                            cluster      = Goncalves2014_sigle$cluster)
          
          #All results  -----------------------------------------------------------
          #Summary res-----------------------------------------------------------
          Goncalves2014_summary_res <-  synth_results(trait_df =Goncalves2014,miss_final_df = Goncalves2014_miss_final, dim_df = Goncalves2014_dim,single_df = Goncalves2014_sigle)
          
          Goncalves2014_res <- list(summary_res    = Goncalves2014_summary_res,
                                    depletion = Goncalves2014_miss_final,
                                    dimension = Goncalves2014_dim,
                                    cluster = Goncalves2014_sigle)
          
          save(Goncalves2014_res,file=file.path(results_dir,"Goncalves2014_res.RData")) 
          
# Set 6 --------------------------------------------------------------------Diaz2008
          Diaz2008 <- read.csv2(file.path(data_dir,"Diaz2008.csv"),  header=TRUE,row.names = 1)
          
          #Trait prep and cat --------------------------------------------------------------------
          for (i in c(1:ncol(Diaz2008))){
            Diaz2008[,i] <- factor(as.character(Diaz2008[,i],level=TRUE))
          }
          
          Diaz2008_cat <- data.frame(
            trait_name        = colnames(head(Diaz2008)),
            trait_type        = rep(NA,ncol(Diaz2008)),
            fuzzy_name        = rep(NA,ncol(Diaz2008)),
            stringsAsFactors  = FALSE)
          
         
          #Correlation Traits -----------------------------------------------------------
          cor_Diaz2008 <- matrix(0,ncol(Diaz2008),ncol(Diaz2008))
          
          for ( i in 1:ncol(Diaz2008))
          {
            for (j in i:ncol(Diaz2008))
            {
              cor_Diaz2008[i,j] <-  cor(rank(Diaz2008[,i]), rank(Diaz2008[,j]),method="kendall")
            }
          }
          cor_Diaz2008[lower.tri(cor_Diaz2008)] <- t(cor_Diaz2008)[lower.tri(cor_Diaz2008)]
          diag(cor_Diaz2008)=NA
          cor_Diaz2008<- data.frame(mean_cor=mean(abs(cor_Diaz2008),na.rm=T),sd_cor=sd(abs(cor_Diaz2008),na.rm=T),
                                       max_cor=max(abs(cor_Diaz2008),na.rm=T),min_cor=min(abs(cor_Diaz2008),na.rm=T))
          save(cor_Diaz2008,file=file.path(results_dir,"cor_Diaz2008.RData"))
          
          #Depletion -----------------------------------------------------------
          Diaz2008_miss <- compute_missing_trait_distance(Diaz2008, Diaz2008_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                               classical_gower = TRUE,pos= 100)
          
          save(Diaz2008_miss,file=file.path(results_dir,"Diaz2008_miss.RData"))
          
          #Plot  --------
          depletion_plot(Diaz2008_miss,version=1)
          
          #Table result Depletion  --------
          Diaz2008_miss_final <- Diaz2008_miss
          Diaz2008_miss_final <-  aggregate(x = Diaz2008_miss_final, by =Diaz2008_miss_final["miss_percent"], FUN = mean)[,-2]
          
          #Dimension  -----------------------------------------------------------
          
          Diaz2008_dim <- dimension_funct(Diaz2008, dim_pcoa = 20,  metric_scaled = TRUE, Diaz2008_cat, classical_gower=TRUE,rep=999 ,cores = 3)
          
          #Plot  --------
          dim_plot(Diaz2008_dim,dim_pcoa = 20)
          
          save(Diaz2008_dim,file=file.path(results_dir,"Diaz2008_dim.RData"))
          
          #Sigleton  -----------------------------------------------------------
          
          Diaz2008_dist <- as.dist(as.matrix(daisy(Diaz2008,metric = "gower")))
          Diaz2008_pcoa <- pcoa(Diaz2008_dist)
          save(Diaz2008_pcoa,file=file.path(results_dir,"Diaz2008_pcoa.RData"))
          
          Diaz2008_sigle <-DPC(Diaz2008_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
          save(Diaz2008_sigle,file=file.path(results_dir,"Diaz2008_sigle.RData"))
          
          #Table result Sigleton  --------
          Diaz2008_sigle <- data.frame(rho          = Diaz2008_sigle$rho,
                                            cluster_core = Diaz2008_sigle$cluster_core,
                                            cluster      = Diaz2008_sigle$cluster)
          
          #All results  -----------------------------------------------------------
          #Summary res-----------------------------------------------------------
          Diaz2008_summary_res <-  synth_results(trait_df =Diaz2008,miss_final_df = Diaz2008_miss_final, dim_df = Diaz2008_dim,single_df = Diaz2008_sigle)
          
          Diaz2008_res <- list(summary_res    = Diaz2008_summary_res,
                                    depletion = Diaz2008_miss_final,
                                    dimension = Diaz2008_dim,
                                    cluster = Diaz2008_sigle)
          
          save(Diaz2008_res,file=file.path(results_dir,"Diaz2008_res.RData"))
          
# Set 7 --------------------------------------------------------------------NorthSeaTraits
   
          NorthSeaTraits <- read.csv2(file.path(data_dir,"NorthSeaTraits.csv"),  header=TRUE,row.names = 1)
          
          #Trait prep and cat --------------------------------------------------------------------
          for (i in c(4,5,7:11,13:14)){
            NorthSeaTraits[,i] <- as.numeric(as.character(NorthSeaTraits[,i]))
          }
          
          for (i in c(1:3,6)){
            NorthSeaTraits[,i] <- as.factor(as.character(NorthSeaTraits[,i]))
          }
          
          for (i in c(12)){
            NorthSeaTraits[,i] <- factor(as.character(NorthSeaTraits[,i],level=TRUE))
            
          }
         
          
          NorthSeaTraits_cat <- data.frame(
            trait_name        = colnames(head(NorthSeaTraits)),
            trait_type        = rep(NA,ncol(NorthSeaTraits)),
            fuzzy_name        = rep(NA,ncol(NorthSeaTraits)),
            stringsAsFactors  = FALSE)
          
          #Correlation Traits -----------------------------------------------------------
          cor_NorthSeaTraits <- matrix(0,ncol(NorthSeaTraits),ncol(NorthSeaTraits))
          
          for ( i in 1:ncol(NorthSeaTraits))
          {
            for (j in i:ncol(NorthSeaTraits))
            {
              cor_NorthSeaTraits[i,j] <-  cor(rank(NorthSeaTraits[,i]), rank(NorthSeaTraits[,j]),method="kendall")
            }
          }
          cor_NorthSeaTraits[lower.tri(cor_NorthSeaTraits)] <- t(cor_NorthSeaTraits)[lower.tri(cor_NorthSeaTraits)]
          diag(cor_NorthSeaTraits)=NA
          cor_NorthSeaTraits<- data.frame(mean_cor=mean(abs(cor_NorthSeaTraits),na.rm=T),sd_cor=sd(abs(cor_NorthSeaTraits),na.rm=T),
                                       max_cor=max(abs(cor_NorthSeaTraits),na.rm=T),min_cor=min(abs(cor_NorthSeaTraits),na.rm=T))
          save(cor_NorthSeaTraits,file=file.path(results_dir,"cor_NorthSeaTraits.RData"))
          
          
          #Depletion -----------------------------------------------------------
          NorthSeaTraits_miss <- compute_missing_trait_distance(NorthSeaTraits, NorthSeaTraits_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 2, 
                                                               classical_gower = TRUE,pos= 100)
          
          save(NorthSeaTraits_miss,file=file.path(results_dir,"NorthSeaTraits_miss.RData"))
          
          #Plot  --------
          depletion_plot(NorthSeaTraits_miss,version=1)
          
          #Table result Depletion  --------
          NorthSeaTraits_miss_final <- NorthSeaTraits_miss
          NorthSeaTraits_miss_final <-  aggregate(x = NorthSeaTraits_miss_final, by =NorthSeaTraits_miss_final["miss_percent"], FUN = mean)[,-2]
          
          #Dimension  -----------------------------------------------------------
          
          NorthSeaTraits_dim <- dimension_funct(NorthSeaTraits, dim_pcoa = 20,  metric_scaled = TRUE, NorthSeaTraits_cat, classical_gower=TRUE,rep=999 ,cores = 3)
          save(NorthSeaTraits_dim,file=file.path(results_dir,"NorthSeaTraits_dim.RData"))
          #Plot  --------
          dim_plot(NorthSeaTraits_dim,dim_pcoa = 20)
          
          #Sigleton  -----------------------------------------------------------
          
          NorthSeaTraits_dist <- as.dist(as.matrix(daisy(NorthSeaTraits,metric = "gower")))
          NorthSeaTraits_pcoa <- pcoa(NorthSeaTraits_dist)
          save(NorthSeaTraits_pcoa,file=file.path(results_dir,"NorthSeaTraits_pcoa.RData"))
          
          NorthSeaTraits_sigle <-DPC(NorthSeaTraits_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
          save(NorthSeaTraits_sigle,file=file.path(results_dir,"NorthSeaTraits_sigle.RData"))
          
          #Table result Sigleton  --------
          NorthSeaTraits_sigle <- data.frame(rho          = NorthSeaTraits_sigle$rho,
                                            cluster_core = NorthSeaTraits_sigle$cluster_core,
                                            cluster      = NorthSeaTraits_sigle$cluster)
          
          #All results  -----------------------------------------------------------
          #Summary res-----------------------------------------------------------
          NorthSeaTraits_summary_res <-  synth_results(trait_df =NorthSeaTraits,miss_final_df = NorthSeaTraits_miss_final, dim_df = NorthSeaTraits_dim,single_df = NorthSeaTraits_sigle)
          
          NorthSeaTraits_res <- list(summary_res    = NorthSeaTraits_summary_res,
                                    depletion = NorthSeaTraits_miss_final,
                                    dimension = NorthSeaTraits_dim,
                                    cluster = NorthSeaTraits_sigle)
          
          save(NorthSeaTraits_res,file=file.path(results_dir,"NorthSeaTraits_res.RData"))
          
# Set 8 --------------------------------------------------------------------Invertebrate NZ  
         trait_nz_invertebrate <- read.csv2(file.path(data_dir,"nz_insect.csv"),  header=TRUE)
         rownames(trait_nz_invertebrate) <-trait_nz_invertebrate$ID_sp
         trait_nz_invertebrate <- trait_nz_invertebrate[,-c(1:5)]
          
         for (i in c(1:ncol(trait_nz_invertebrate))){
           trait_nz_invertebrate[,i] <- factor(as.character(trait_nz_invertebrate[,i],level=TRUE))
           
         }
         
         
         trait_nz_invertebrate_cat = data.frame(
         trait_name = colnames(trait_nz_invertebrate),
         trait_type = rep(NA, ncol(trait_nz_invertebrate)),
         fuzzy_name = rep(NA,ncol(trait_nz_invertebrate)),
         stringsAsFactors = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_trait_nz_invertebrate <- matrix(0,ncol(trait_nz_invertebrate),ncol(trait_nz_invertebrate))
         
         for ( i in 1:ncol(trait_nz_invertebrate))
         {
           for (j in i:ncol(trait_nz_invertebrate))
           {
             cor_trait_nz_invertebrate[i,j] <-  cor(rank(trait_nz_invertebrate[,i]), rank(trait_nz_invertebrate[,j]),method="kendall")
           }
         }
         cor_trait_nz_invertebrate[lower.tri(cor_trait_nz_invertebrate)] <- t(cor_trait_nz_invertebrate)[lower.tri(cor_trait_nz_invertebrate)]
         diag(cor_trait_nz_invertebrate)=NA
         cor_trait_nz_invertebrate<- data.frame(mean_cor=mean(abs(cor_trait_nz_invertebrate),na.rm=T),sd_cor=sd(abs(cor_trait_nz_invertebrate),na.rm=T),
                                      max_cor=max(abs(cor_trait_nz_invertebrate),na.rm=T),min_cor=min(abs(cor_trait_nz_invertebrate),na.rm=T))
         save(cor_trait_nz_invertebrate,file=file.path(results_dir,"cor_trait_nz_invertebrate.RData"))
         
         #Depletion -----------------------------------------------------------
         trait_nz_invertebrate_miss <- compute_missing_trait_distance(trait_nz_invertebrate, trait_nz_invertebrate_cat,
                                                              percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                              classical_gower = TRUE,pos= 100)
         
         save(trait_nz_invertebrate_miss,file=file.path(results_dir,"trait_nz_invertebrate_miss.RData"))
         
         #Plot  --------
         depletion_plot(trait_nz_invertebrate_miss,version=1)
         
         #Table result Depletion  --------
         trait_nz_invertebrate_miss_final <- trait_nz_invertebrate_miss
         trait_nz_invertebrate_miss_final <-  aggregate(x = trait_nz_invertebrate_miss_final, by =trait_nz_invertebrate_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         trait_nz_invertebrate_dim <- dimension_funct(trait_nz_invertebrate, dim_pcoa = 20,  metric_scaled = TRUE, trait_nz_invertebrate_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(trait_nz_invertebrate_dim,file=file.path(results_dir,"trait_nz_invertebrate_dim.RData"))
         #Plot  --------
         dim_plot(trait_nz_invertebrate_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         trait_nz_invertebrate_dist <- as.dist(as.matrix(daisy(trait_nz_invertebrate,metric = "gower")))
         trait_nz_invertebrate_pcoa <- pcoa(trait_nz_invertebrate_dist)
         save(trait_nz_invertebrate_pcoa,file=file.path(results_dir,"trait_nz_invertebrate_pcoa.RData"))
         
         trait_nz_invertebrate_sigle <-DPC(trait_nz_invertebrate_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(trait_nz_invertebrate_sigle,file=file.path(results_dir,"trait_nz_invertebrate_sigle.RData"))
         
         #Table result Sigleton  --------
         trait_nz_invertebrate_sigle <- data.frame(rho          = trait_nz_invertebrate_sigle$rho,
                                           cluster_core = trait_nz_invertebrate_sigle$cluster_core,
                                           cluster      = trait_nz_invertebrate_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         trait_nz_invertebrate_summary_res <-  synth_results(trait_df =trait_nz_invertebrate,miss_final_df = trait_nz_invertebrate_miss_final, dim_df = trait_nz_invertebrate_dim,single_df = trait_nz_invertebrate_sigle)
         
         trait_nz_invertebrate_res <- list(summary_res    = trait_nz_invertebrate_summary_res,
                                   depletion = trait_nz_invertebrate_miss_final,
                                   dimension = trait_nz_invertebrate_dim,
                                   cluster = trait_nz_invertebrate_sigle)
         
         save(trait_nz_invertebrate_res,file=file.path(results_dir,"trait_nz_invertebrate_res.RData"))
               
         
# Set 9 --------------------------------------------------------------------Krasnov2015
         Krasnov2015 <- read.csv2(file.path(data_dir,"Krasnov2015.csv"),  header=TRUE,row.names = 1)
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(1:6)){
           Krasnov2015[,i] <- as.numeric(as.character(Krasnov2015[,i]))
         }
         
         Krasnov2015_cat <- data.frame(
           trait_name        = colnames(head(Krasnov2015)),
           trait_type        = rep(NA,ncol(Krasnov2015)),
           fuzzy_name        = rep(NA,ncol(Krasnov2015)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_Krasnov2015 <- matrix(0,ncol(Krasnov2015),ncol(Krasnov2015))
         
         for ( i in 1:ncol(Krasnov2015))
         {
           for (j in i:ncol(Krasnov2015))
           {
             cor_Krasnov2015[i,j] <-  cor(rank(Krasnov2015[,i]), rank(Krasnov2015[,j]),method="kendall")
           }
         }
         cor_Krasnov2015[lower.tri(cor_Krasnov2015)] <- t(cor_Krasnov2015)[lower.tri(cor_Krasnov2015)]
         diag(cor_Krasnov2015)=NA
         cor_Krasnov2015<- data.frame(mean_cor=mean(abs(cor_Krasnov2015),na.rm=T),sd_cor=sd(abs(cor_Krasnov2015),na.rm=T),
                                      max_cor=max(abs(cor_Krasnov2015),na.rm=T),min_cor=min(abs(cor_Krasnov2015),na.rm=T))
         save(cor_Krasnov2015,file=file.path(results_dir,"cor_Krasnov2015.RData"))
         
       
         #Depletion -----------------------------------------------------------
         Krasnov2015_miss <- compute_missing_trait_distance(Krasnov2015, Krasnov2015_cat,
                                                         percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                         classical_gower = TRUE,pos= 100)
         
         save(Krasnov2015_miss,file=file.path(results_dir,"Krasnov2015_miss.RData"))
         
         #Plot  --------
         depletion_plot(Krasnov2015_miss,version=1)
         
         #Table result Depletion  --------
         Krasnov2015_miss_final <- Krasnov2015_miss
         Krasnov2015_miss_final <-  aggregate(x = Krasnov2015_miss_final, by =Krasnov2015_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         Krasnov2015_dim <- dimension_funct(Krasnov2015, dim_pcoa = 20,  metric_scaled = TRUE, Krasnov2015_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(Krasnov2015_dim,file=file.path(results_dir,"Krasnov2015_dim.RData"))
         #Plot  --------
         dim_plot(Krasnov2015_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         Krasnov2015_dist <- as.dist(as.matrix(daisy(Krasnov2015,metric = "gower")))
         Krasnov2015_pcoa <- pcoa(Krasnov2015_dist)
         save(Krasnov2015_pcoa,file=file.path(results_dir,"Krasnov2015_pcoa.RData"))
         
         Krasnov2015_sigle <-DPC(Krasnov2015_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(Krasnov2015_sigle,file=file.path(results_dir,"Krasnov2015_sigle.RData"))
         
         #Table result Sigleton  --------
         Krasnov2015_sigle <- data.frame(rho          = Krasnov2015_sigle$rho,
                                      cluster_core = Krasnov2015_sigle$cluster_core,
                                      cluster      = Krasnov2015_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         Krasnov2015_summary_res <-  synth_results(trait_df =Krasnov2015,miss_final_df = Krasnov2015_miss_final, dim_df = Krasnov2015_dim,single_df = Krasnov2015_sigle)
         
         Krasnov2015_res <- list(summary_res    = Krasnov2015_summary_res,
                              depletion = Krasnov2015_miss_final,
                              dimension = Krasnov2015_dim,
                              cluster = Krasnov2015_sigle)
         
         save(Krasnov2015_res,file=file.path(results_dir,"Krasnov2015_res.RData"))

# Set 10 --------------------------------------------------------------------USDA_plant
         
         USDA_plant <- read.csv2(file.path(data_dir,"usda_plant_traits.csv"),  header=TRUE)
         USDA_plant <- USDA_plant[,-c(1:4)]
         rownames(USDA_plant) <- USDA_plant[,1]
         USDA_plant <- USDA_plant[,-1]
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(10:18)){
           USDA_plant[,i] <- as.numeric(as.character(USDA_plant[,i]))
         }
         
         for (i in c(19:ncol(USDA_plant))){
           USDA_plant[,i] <- factor(as.character(USDA_plant[,i]))
           
         }
         
         
         USDA_plant_cat <- data.frame(
           trait_name        = colnames(head(USDA_plant)),
           trait_type        = rep(NA,ncol(USDA_plant)),
           fuzzy_name        = rep(NA,ncol(USDA_plant)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_USDA_plant <- matrix(0,ncol(USDA_plant),ncol(USDA_plant))
         
         for ( i in 1:ncol(USDA_plant))
         {
           for (j in i:ncol(USDA_plant))
           {
             cor_USDA_plant[i,j] <-  cor(rank(USDA_plant[,i]), rank(USDA_plant[,j]),method="kendall")
           }
         }
         cor_USDA_plant[lower.tri(cor_USDA_plant)] <- t(cor_USDA_plant)[lower.tri(cor_USDA_plant)]
         diag(cor_USDA_plant)=NA
         cor_USDA_plant<- data.frame(mean_cor=mean(abs(cor_USDA_plant),na.rm=T),sd_cor=sd(abs(cor_USDA_plant),na.rm=T),
                                      max_cor=max(abs(cor_USDA_plant),na.rm=T),min_cor=min(abs(cor_USDA_plant),na.rm=T))
         save(cor_USDA_plant,file=file.path(results_dir,"cor_USDA_plant.RData"))
         
         #Depletion -----------------------------------------------------------
         USDA_plant_miss <- compute_missing_trait_distance(USDA_plant, USDA_plant_cat,
                                                            percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                            classical_gower = TRUE,pos= 100)
         
         save(USDA_plant_miss,file=file.path(results_dir,"USDA_plant_miss.RData"))
         
         #Plot  --------
         depletion_plot(USDA_plant_miss,version=1)
         
         #Table result Depletion  --------
         USDA_plant_miss_final <- USDA_plant_miss
         USDA_plant_miss_final <-  aggregate(x = USDA_plant_miss_final, by =USDA_plant_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         USDA_plant_dim <- dimension_funct(USDA_plant, dim_pcoa = 20,  metric_scaled = TRUE, USDA_plant_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(USDA_plant_dim,file=file.path(results_dir,"USDA_plant_dim.RData"))
         #Plot  --------
         dim_plot(USDA_plant_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         USDA_plant_dist <- as.dist(as.matrix(daisy(USDA_plant,metric = "gower")))
         USDA_plant_pcoa <- pcoa(USDA_plant_dist)
         save(USDA_plant_pcoa,file=file.path(results_dir,"USDA_plant_pcoa.RData"))
         
         USDA_plant_sigle <-DPC(USDA_plant_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(USDA_plant_sigle,file=file.path(results_dir,"USDA_plant_sigle.RData"))
         
         #Table result Sigleton  --------
         USDA_plant_sigle <- data.frame(rho          = USDA_plant_sigle$rho,
                                         cluster_core = USDA_plant_sigle$cluster_core,
                                         cluster      = USDA_plant_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         USDA_plant_summary_res <-  synth_results(trait_df =USDA_plant,miss_final_df = USDA_plant_miss_final, dim_df = USDA_plant_dim,single_df = USDA_plant_sigle)
         
         USDA_plant_res <- list(summary_res    = USDA_plant_summary_res,
                                 depletion = USDA_plant_miss_final,
                                 dimension = USDA_plant_dim,
                                 cluster = USDA_plant_sigle)
         
         save(USDA_plant_res,file=file.path(results_dir,"USDA_plant_res.RData"))
         
# Set 11 --------------------------------------------------------------------Palm Tree
         
         
         PalmTraits <- read.csv2(file.path(data_dir,"PalmTraits_1.0.csv"), row.names = 1, header=TRUE)
         PalmTraits <- PalmTraits[,-c(1:4)]

         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(7,8,11:19)){
           PalmTraits[,i] <- as.numeric(as.character(PalmTraits[,i]))
         }
         
         for (i in c(9,20:ncol(PalmTraits))){
           PalmTraits[,i] <- factor(as.character(PalmTraits[,i]))
         }
         
         PalmTraits_cat <- data.frame(
           trait_name        = colnames(head(PalmTraits)),
           trait_type        = rep(NA,ncol(PalmTraits)),
           fuzzy_name        = rep(NA,ncol(PalmTraits)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_PalmTraits <- matrix(0,ncol(PalmTraits),ncol(PalmTraits))
         
         for ( i in 1:ncol(PalmTraits))
         {
           for (j in i:ncol(PalmTraits))
           {
             cor_PalmTraits[i,j] <-  cor(rank(PalmTraits[,i]), rank(PalmTraits[,j]),method="kendall")
           }
         }
         cor_PalmTraits[lower.tri(cor_PalmTraits)] <- t(cor_PalmTraits)[lower.tri(cor_PalmTraits)]
         diag(cor_PalmTraits)=NA
         cor_PalmTraits<- data.frame(mean_cor=mean(abs(cor_PalmTraits),na.rm=T),sd_cor=sd(abs(cor_PalmTraits),na.rm=T),
                                      max_cor=max(abs(cor_PalmTraits),na.rm=T),min_cor=min(abs(cor_PalmTraits),na.rm=T))
         save(cor_PalmTraits,file=file.path(results_dir,"cor_PalmTraits.RData"))
         
         
         #Depletion -----------------------------------------------------------
         PalmTraits_miss <- compute_missing_trait_distance(PalmTraits, PalmTraits_cat,
                                                           percent_list, max_comb = 100,n_perm = 100,cores = 4, 
                                                           classical_gower = TRUE,pos= 100)
         
         save(PalmTraits_miss,file=file.path(results_dir,"PalmTraits_miss.RData"))
         
         #Plot  --------
         depletion_plot(PalmTraits_miss,version=1)
         
         #Table result Depletion  --------
         PalmTraits_miss_final <- PalmTraits_miss
         PalmTraits_miss_final <-  aggregate(x = PalmTraits_miss_final, by =PalmTraits_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         PalmTraits_dim <- dimension_funct(PalmTraits, dim_pcoa = 20,  metric_scaled = TRUE, PalmTraits_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(PalmTraits_dim,file=file.path(results_dir,"PalmTraits_dim.RData"))
         #Plot  --------
         dim_plot(PalmTraits_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         PalmTraits_dist <- as.dist(as.matrix(daisy(PalmTraits,metric = "gower")))
         PalmTraits_pcoa <- pcoa(PalmTraits_dist)
         save(PalmTraits_pcoa,file=file.path(results_dir,"PalmTraits_pcoa.RData"))
         
         PalmTraits_sigle <-DPC(PalmTraits_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(PalmTraits_sigle,file=file.path(results_dir,"PalmTraits_sigle.RData"))
         
         #Table result Sigleton  --------
         PalmTraits_sigle <- data.frame(rho          = PalmTraits_sigle$rho,
                                        cluster_core = PalmTraits_sigle$cluster_core,
                                        cluster      = PalmTraits_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         PalmTraits_summary_res <-  synth_results(trait_df =PalmTraits,miss_final_df = PalmTraits_miss_final, dim_df = PalmTraits_dim,single_df = PalmTraits_sigle)
         
         PalmTraits_res <- list(summary_res    = PalmTraits_summary_res,
                                depletion = PalmTraits_miss_final,
                                dimension = PalmTraits_dim,
                                cluster = PalmTraits_sigle)
         
         save(PalmTraits_res,file=file.path(results_dir,"PalmTraits_res.RData"))
         
# Set 12 --------------------------------------------------------------------Freshwater fish
         
         load(file.path(data_dir,"freshfishtrait.RData"))
         
         
         #---- remove line with too much NA more than 8
         freshfishtrait <- freshfishtrait[rowSums(is.na(freshfishtrait)) < 5, ]
         
         #Trait prep and cat --------------------------------------------------------------------
         freshfishtrait_cat <- data.frame(
           trait_name        = colnames(head(freshfishtrait)),
           trait_type        = rep(NA,ncol(freshfishtrait)),
           fuzzy_name        = rep(NA,ncol(freshfishtrait)),
           stringsAsFactors  = FALSE)
      
         
         #Correlation Traits -----------------------------------------------------------
         cor_freshfishtrait <- matrix(0,ncol(freshfishtrait),ncol(freshfishtrait))
         
         for ( i in 1:ncol(freshfishtrait))
         {
           for (j in i:ncol(freshfishtrait))
           {
             cor_freshfishtrait[i,j] <-  cor(rank(freshfishtrait[,i]), rank(freshfishtrait[,j]),method="kendall")
           }
         }
         cor_freshfishtrait[lower.tri(cor_freshfishtrait)] <- t(cor_freshfishtrait)[lower.tri(cor_freshfishtrait)]
         diag(cor_freshfishtrait)=NA
         cor_freshfishtrait<- data.frame(mean_cor=mean(abs(cor_freshfishtrait),na.rm=T),sd_cor=sd(abs(cor_freshfishtrait),na.rm=T),
                                      max_cor=max(abs(cor_freshfishtrait),na.rm=T),min_cor=min(abs(cor_freshfishtrait),na.rm=T))
         save(cor_freshfishtrait,file=file.path(results_dir,"cor_freshfishtrait.RData"))
         
         
         #Depletion -----------------------------------------------------------
         freshfishtrait_miss <- compute_missing_trait_distance(freshfishtrait, freshfishtrait_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 16, 
                                                               classical_gower = TRUE,pos= 100)
         #Plot  --------
         depletion_plot(freshfishtrait_miss,version=1)
         
         #Table result Depletion  --------
         freshfish_miss_final <- freshfish_miss
         freshfish_miss_final <-  aggregate(x = freshfish_miss_final, by =freshfish_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         freshfishtrait_dim <- dimension_funct(freshfishtrait, dim_pcoa = 20,  metric_scaled = TRUE, freshfishtrait_cat, classical_gower=TRUE,rep=999 ,cores = 16)
      
         
         #Plot  --------
         dim_plot(freshfishtrait_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         freshfishtrait_dist <- as.dist(as.matrix(daisy(freshfishtrait,metric = "gower")))
         freshfishtrait_pcoa <- pcoa(freshfishtrait_dist)
         save(freshfishtrait_pcoa,file=file.path(results_dir,"freshfishtrait_pcoa.RData"))
         
         freshfishtrait_sigle <-DPC(freshfishtrait_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(freshfishtrait_sigle,file=file.path(results_dir,"freshfishtrait_sigle.RData"))
         #Table result Sigleton  --------
         freshfishtrait_sigle <- data.frame(rho          = freshfishtrait_sigle$rho,
                                            cluster_core = freshfishtrait_sigle$cluster_core,
                                            cluster      = freshfishtrait_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         freshfishtrait_summary_res <-  synth_results(trait_df =freshfishtrait,miss_final_df = freshfish_miss_final, dim_df = freshfish_dim,single_df = freshfishtrait_sigle)
         
         freshfishtrait_res <- list(summary_res    = freshfishtrait_summary_res,
                                    depletion = freshfish_miss_final,
                                    dimension = freshfish_dim,
                                    cluster = freshfishtrait_sigle)
         
         save(freshfishtrait_res,file=file.path(results_dir,"freshfishtrait_res.RData"))
         
    
         
# Set 13 --------------------------------------------------------------------Birds
         load(file.path(data_dir,"birdstrait.RData"))
         
         #Trait prep and cat --------------------------------------------------------------------
         birdstrait_cat <- data.frame(
           trait_name        = colnames(head(birdstrait)),
           trait_type        = rep(NA,ncol(birdstrait)),
           fuzzy_name        = rep(NA,ncol(birdstrait)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_birdstrait <- matrix(0,ncol(birdstrait),ncol(birdstrait))
         
         for ( i in 1:ncol(birdstrait))
         {
           for (j in i:ncol(birdstrait))
           {
             cor_birdstrait[i,j] <-  cor(rank(birdstrait[,i]), rank(birdstrait[,j]),method="kendall")
           }
         }
         cor_birdstrait[lower.tri(cor_birdstrait)] <- t(cor_birdstrait)[lower.tri(cor_birdstrait)]
         diag(cor_birdstrait)=NA
         cor_birdstrait<- data.frame(mean_cor=mean(abs(cor_birdstrait),na.rm=T),sd_cor=sd(abs(cor_birdstrait),na.rm=T),
                                      max_cor=max(abs(cor_birdstrait),na.rm=T),min_cor=min(abs(cor_birdstrait),na.rm=T))
         save(cor_birdstrait,file=file.path(results_dir,"cor_birdstrait.RData"))
         
         #Depletion -----------------------------------------------------------
         birdstrait_miss <- compute_missing_trait_distance(birdstrait, birdstrait_cat,
                                                           percent_list, max_comb = 100,n_perm = 100,cores = 45, 
                                                           classical_gower = TRUE,pos= 100)
         #Plot  --------
         depletion_plot(tab_miss,version=3)
         
         #Table result Depletion  --------
         birdstrait_miss_final <- birds_miss
         birdstrait_miss_final <-  aggregate(x = birdstrait_miss_final, by =birdstrait_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         birdstrait_dim <- dimension_funct(birdstrait, dim_pcoa = 20,  metric_scaled = TRUE, birdstrait_cat, classical_gower=TRUE,rep=2 ,cores = 3)
         
         #Plot  --------
         dim_plot(birdstrait_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         birdstrait_dist <- as.dist(as.matrix(daisy(birdstrait,metric = "gower")))
         birdstrait_pcoa <- pcoa(birdstrait_dist)
         save(birdstrait_pcoa,file=file.path(results_dir,"birdstrait_pcoa.RData"))
         
         birdstrait_sigle <-DPC(birdstrait_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         
         #Table result Sigleton  --------
         birdstrait_sigle <- data.frame(rho          = birdstrait_sigle$rho,
                                        cluster_core = birdstrait_sigle$cluster_core,
                                        cluster      = birdstrait_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         birdstrait_summary_res <-  synth_results(trait_df =birdstrait,miss_final_df = birdstrait_miss_final, dim_df = birds_dim,single_df = birdstrait_sigle)
         
         birdstrait_res <- list(summary_res    = birdstrait_summary_res,
                                depletion = birdstrait_miss_final,
                                dimension = birds_dim,
                                cluster = birdstrait_sigle)
         
         save(birdstrait_res,file=file.path(results_dir,"birdstrait_res.RData"))
      
# Set 14 --------------------------------------------------------------------Terrestrial mammals
         
         load(file.path(data_dir,"mammalstrait.RData"))
         
         #Trait prep and cat --------------------------------------------------------------------
         mammalstrait_cat <- data.frame(
           trait_name        = colnames(head(mammalstrait)),
           trait_type        = rep(NA,ncol(mammalstrait)),
           fuzzy_name        = rep(NA,ncol(mammalstrait)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_mammalstrait <- matrix(0,ncol(mammalstrait),ncol(mammalstrait))
         
         for ( i in 1:ncol(mammalstrait))
         {
           for (j in i:ncol(mammalstrait))
           {
             cor_mammalstrait[i,j] <-  cor(rank(mammalstrait[,i]), rank(mammalstrait[,j]),method="kendall")
           }
         }
         cor_mammalstrait[lower.tri(cor_mammalstrait)] <- t(cor_mammalstrait)[lower.tri(cor_mammalstrait)]
         diag(cor_mammalstrait)=NA
         cor_mammalstrait<- data.frame(mean_cor=mean(abs(cor_mammalstrait),na.rm=T),sd_cor=sd(abs(cor_mammalstrait),na.rm=T),
                                      max_cor=max(abs(cor_mammalstrait),na.rm=T),min_cor=min(abs(cor_mammalstrait),na.rm=T))
         save(cor_mammalstrait,file=file.path(results_dir,"cor_mammalstrait.RData"))
         
         #Depletion -----------------------------------------------------------
         mammalstrait_miss <- compute_missing_trait_distance(mammalstrait, mammalstrait_cat,
                                                           percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                           classical_gower = TRUE,pos= 100)
         #Plot  --------
         depletion_plot(mammalstrait_miss,version=1)
         
         #Table result Depletion  --------
         mammalstrait_miss_final <- mammalstrait_miss
         mammalstrait_miss_final <-  aggregate(x = mammalstrait_miss_final, by =mammalstrait_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         mammalstrait_dim <- dimension_funct(mammalstrait, dim_pcoa = 20,  metric_scaled = TRUE, mammalstrait_cat, classical_gower=TRUE,rep=10 ,cores = 3)
         save(mammalstrait_dim,file=file.path(results_dir,"mammalstrait_dim.RData"))
         #Plot  --------
         dim_plot(mammalstrait_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         mammalstrait_dist <- as.dist(as.matrix(daisy(mammalstrait,metric = "gower")))
         mammalstrait_pcoa <- pcoa(mammalstrait_dist)
         save(mammalstrait_pcoa,file=file.path(results_dir,"mammalstrait_pcoa.RData"))
         
         
         mammalstrait_sigle <-DPC(mammalstrait_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(mammalstrait_sigle,file=file.path(results_dir,"mammalstrait_sigle.RData"))
         
         #Table result Sigleton  --------
         mammalstrait_sigle <- data.frame(rho          = mammalstrait_sigle$rho,
                                        cluster_core = mammalstrait_sigle$cluster_core,
                                        cluster      = mammalstrait_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         mammalstrait_summary_res <-  synth_results(trait_df =mammalstrait,miss_final_df = mammalstrait_miss_final, dim_df = mammalstrait_dim,single_df = mammalstrait_sigle)
         
         mammalstrait_res <- list(summary_res    = mammalstrait_summary_res,
                                depletion = mammalstrait_miss_final,
                                dimension = mammalstrait_dim,
                                cluster = mammalstrait_sigle)
         
         save(mammalstrait_res,file=file.path(results_dir,"mammalstrait_res.RData"))
       
# Set 15 -------------------------------------------------------------------- Scleratinian corals 
         
         coral <- read.csv2(file.path(data_dir,"coraltraitsv3.csv"),  header=TRUE,row.names = 1)
        
                  #Trait prep and cat --------------------------------------------------------------------
         for (i in c(2:5,13)){
           coral[,i] <- as.numeric(as.character(coral[,i]))
         }
         
         for (i in c(1,6:12)){
           coral[,i] <- factor(as.character(coral[,i]))
         }
         
         coral <- coral[,-10]
         coral <- coral[,-13]
         
         coral_cat <- data.frame(
           trait_name        = colnames(head(coral)),
           trait_type        = rep(NA,ncol(coral)),
           fuzzy_name        = rep(NA,ncol(coral)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_coral <- matrix(0,ncol(coral),ncol(coral))
         
         for ( i in 1:ncol(coral))
         {
           for (j in i:ncol(coral))
           {
             cor_coral[i,j] <-  cor(rank(coral[,i]), rank(coral[,j]),method="kendall")
           }
         }
         cor_coral[lower.tri(cor_coral)] <- t(cor_coral)[lower.tri(cor_coral)]
         diag(cor_coral)=NA
         cor_coral<- data.frame(mean_cor=mean(abs(cor_coral),na.rm=T),sd_cor=sd(abs(cor_coral),na.rm=T),
                                      max_cor=max(abs(cor_coral),na.rm=T),min_cor=min(abs(cor_coral),na.rm=T))
         save(cor_coral,file=file.path(results_dir,"cor_coral.RData"))
         
         #Depletion -----------------------------------------------------------
         coral_miss <- compute_missing_trait_distance(coral, coral_cat,
                                                           percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                           classical_gower = TRUE,pos= 100)
         
         save(coral_miss,file=file.path(results_dir,"coral_miss.RData"))
         
         #Plot  --------
         depletion_plot(coral_miss,version=1)
         
         #Table result Depletion  --------
         coral_miss_final <- coral_miss
         coral_miss_final <-  aggregate(x = coral_miss_final, by =coral_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         coral_dim <- dimension_funct(coral, dim_pcoa = 20,  metric_scaled = TRUE, coral_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(coral_dim,file=file.path(results_dir,"coral_dim.RData"))
         #Plot  --------
         dim_plot(coral_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         coral_dist <- as.dist(as.matrix(daisy(coral,metric = "gower")))
         coral_pcoa <- pcoa(coral_dist)
         save(coral_pcoa,file=file.path(results_dir,"coral_pcoa.RData"))
         
         coral_sigle <-DPC(coral_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(coral_sigle,file=file.path(results_dir,"coral_sigle.RData"))
         
         #Table result Sigleton  --------
         coral_sigle <- data.frame(rho          = coral_sigle$rho,
                                        cluster_core = coral_sigle$cluster_core,
                                        cluster      = coral_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         coral_summary_res <-  synth_results(trait_df =coral,miss_final_df = coral_miss_final, dim_df = coral_dim,single_df = coral_sigle)
         
         coral_res <- list(summary_res    = coral_summary_res,
                                depletion = coral_miss_final,
                                dimension = coral_dim,
                                cluster = coral_sigle)
         
         save(coral_res,file=file.path(results_dir,"coral_res.RData"))
# Set 16 --------------------------------------------------------------------
         bacteria <- read.csv2(file.path(data_dir,"data_FULL_bacteria.csv"),  header=TRUE,row.names = 1)
         
         #Trait prep and cat --------------------------------------------------------------------
         bacteria[,1] <- factor(as.character(bacteria[,1]))

         
         bacteria_cat <- data.frame(
           trait_name        = colnames(head(bacteria)),
           trait_type        = rep(NA,ncol(bacteria)),
           fuzzy_name        = rep(NA,ncol(bacteria)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_bacteria <- matrix(0,ncol(bacteria),ncol(bacteria))
         
         for ( i in 1:ncol(bacteria))
         {
           for (j in i:ncol(bacteria))
           {
             cor_bacteria[i,j] <-  cor(rank(bacteria[,i]), rank(bacteria[,j]),method="kendall")
           }
         }
         cor_bacteria[lower.tri(cor_bacteria)] <- t(cor_bacteria)[lower.tri(cor_bacteria)]
         diag(cor_bacteria)=NA
         cor_bacteria<- data.frame(mean_cor=mean(abs(cor_bacteria),na.rm=T),sd_cor=sd(abs(cor_bacteria),na.rm=T),
                                      max_cor=max(abs(cor_bacteria),na.rm=T),min_cor=min(abs(cor_bacteria),na.rm=T))
         save(cor_bacteria,file=file.path(results_dir,"cor_bacteria.RData"))
         
         
         #Depletion -----------------------------------------------------------
         bacteria_miss <- compute_missing_trait_distance(bacteria, bacteria_cat,
                                                      percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                      classical_gower = TRUE,pos= 100)
         
         save(bacteria_miss,file=file.path(results_dir,"bacteria_miss.RData"))
         
         #Plot  --------
         depletion_plot(bacteria_miss,version=1)
         
         #Table result Depletion  --------
         bacteria_miss_final <- bacteria_miss
         bacteria_miss_final <-  aggregate(x = bacteria_miss_final, by =bacteria_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         bacteria_dim <- dimension_funct(bacteria, dim_pcoa = 20,  metric_scaled = TRUE, bacteria_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(bacteria_dim,file=file.path(results_dir,"bacteria_dim.RData"))
         #Plot  --------
         dim_plot(bacteria_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         bacteria_dist <- as.dist(as.matrix(daisy(bacteria,metric = "gower")))
         bacteria_pcoa <- pcoa(bacteria_dist)
         save(bacteria_pcoa,file=file.path(results_dir,"bacteria_pcoa.RData"))
         
         bacteria_sigle <-DPC(bacteria_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(bacteria_sigle,file=file.path(results_dir,"bacteria_sigle.RData"))
         
         #Table result Sigleton  --------
         bacteria_sigle <- data.frame(rho          = bacteria_sigle$rho,
                                   cluster_core = bacteria_sigle$cluster_core,
                                   cluster      = bacteria_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         bacteria_summary_res <-  synth_results(trait_df =bacteria,miss_final_df = bacteria_miss_final, dim_df = bacteria_dim,single_df = bacteria_sigle)
         
         bacteria_res <- list(summary_res    = bacteria_summary_res,
                           depletion = bacteria_miss_final,
                           dimension = bacteria_dim,
                           cluster = bacteria_sigle)
         
         save(bacteria_res,file=file.path(results_dir,"bacteria_res.RData"))
# Set 17 --------------------------------------------------------------------
         Eallonardo2013 <- read.csv2(file.path(data_dir,"Eallonardo2013.csv"),  header=TRUE,row.names = 1)
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(6:11)){
           Eallonardo2013[,i] <- as.numeric(as.character(Eallonardo2013[,i]))
         }
         
         for (i in c(1:5)){
           Eallonardo2013[,i] <- factor(as.character(Eallonardo2013[,i]))
         }
         
         
         Eallonardo2013_cat <- data.frame(
           trait_name        = colnames(head(Eallonardo2013)),
           trait_type        = rep(NA,ncol(Eallonardo2013)),
           fuzzy_name        = rep(NA,ncol(Eallonardo2013)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_Eallonardo2013 <- matrix(0,ncol(Eallonardo2013),ncol(Eallonardo2013))
         
         for ( i in 1:ncol(Eallonardo2013))
         {
           for (j in i:ncol(Eallonardo2013))
           {
             cor_Eallonardo2013[i,j] <-  cor(rank(Eallonardo2013[,i]), rank(Eallonardo2013[,j]),method="kendall")
           }
         }
         cor_Eallonardo2013[lower.tri(cor_Eallonardo2013)] <- t(cor_Eallonardo2013)[lower.tri(cor_Eallonardo2013)]
         diag(cor_Eallonardo2013)=NA
         cor_Eallonardo2013<- data.frame(mean_cor=mean(abs(cor_Eallonardo2013),na.rm=T),sd_cor=sd(abs(cor_Eallonardo2013),na.rm=T),
                                      max_cor=max(abs(cor_Eallonardo2013),na.rm=T),min_cor=min(abs(cor_Eallonardo2013),na.rm=T))
         save(cor_Eallonardo2013,file=file.path(results_dir,"cor_Eallonardo2013.RData"))
         
         
         #Depletion -----------------------------------------------------------
         Eallonardo2013_miss <- compute_missing_trait_distance(Eallonardo2013, Eallonardo2013_cat,
                                                         percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                         classical_gower = TRUE,pos= 100)
         
         save(Eallonardo2013_miss,file=file.path(results_dir,"Eallonardo2013_miss.RData"))
         
         #Plot  --------
         depletion_plot(Eallonardo2013_miss,version=1)
         
         #Table result Depletion  --------
         Eallonardo2013_miss_final <- Eallonardo2013_miss
         Eallonardo2013_miss_final <-  aggregate(x = Eallonardo2013_miss_final, by =Eallonardo2013_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         Eallonardo2013_dim <- dimension_funct(Eallonardo2013, dim_pcoa = 18,  metric_scaled = TRUE, Eallonardo2013_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(Eallonardo2013_dim,file=file.path(results_dir,"Eallonardo2013_dim.RData"))
         
         #Plot  --------
         dim_plot(Eallonardo2013_dim,dim_pcoa = 18)
         
         #Sigleton  -----------------------------------------------------------
         
         Eallonardo2013_dist <- as.dist(as.matrix(daisy(Eallonardo2013,metric = "gower")))
         Eallonardo2013_pcoa <- pcoa(Eallonardo2013_dist)
         save(Eallonardo2013_pcoa,file=file.path(results_dir,"Eallonardo2013_pcoa.RData"))
         
         Eallonardo2013_sigle <-DPC(Eallonardo2013_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(Eallonardo2013_sigle,file=file.path(results_dir,"Eallonardo2013_sigle.RData"))
         
         #Table result Sigleton  --------
         Eallonardo2013_sigle <- data.frame(rho          = Eallonardo2013_sigle$rho,
                                      cluster_core = Eallonardo2013_sigle$cluster_core,
                                      cluster      = Eallonardo2013_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         Eallonardo2013_summary_res <-  synth_results(trait_df =Eallonardo2013,miss_final_df = Eallonardo2013_miss_final, dim_df = Eallonardo2013_dim,single_df = Eallonardo2013_sigle)
         
         Eallonardo2013_res <- list(summary_res    = Eallonardo2013_summary_res,
                              depletion = Eallonardo2013_miss_final,
                              dimension = Eallonardo2013_dim,
                              cluster = Eallonardo2013_sigle)
         
         save(Eallonardo2013_res,file=file.path(results_dir,"Eallonardo2013_res.RData"))
# Set 18 --------------------------------------------------------------------
         Yates2014 <- read.csv2(file.path(data_dir,"Yates2014.csv"),  header=TRUE,row.names = 1)
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(1:11)){
           Yates2014[,i] <- as.numeric(as.character(Yates2014[,i]))
         }
         
         for (i in c(11)){
           Yates2014[,i] <- factor(as.character(Yates2014[,i],level=TRUE))
         }
         
         
         
         Yates2014_cat <- data.frame(
           trait_name        = colnames(head(Yates2014)),
           trait_type        = rep(NA,ncol(Yates2014)),
           fuzzy_name        = rep(NA,ncol(Yates2014)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_Yates2014 <- matrix(0,ncol(Yates2014),ncol(Yates2014))
         
         for ( i in 1:ncol(Yates2014))
         {
           for (j in i:ncol(Yates2014))
           {
             cor_Yates2014[i,j] <-  cor(rank(Yates2014[,i]), rank(Yates2014[,j]),method="kendall")
           }
         }
         cor_Yates2014[lower.tri(cor_Yates2014)] <- t(cor_Yates2014)[lower.tri(cor_Yates2014)]
         diag(cor_Yates2014)=NA
         cor_Yates2014<- data.frame(mean_cor=mean(abs(cor_Yates2014),na.rm=T),sd_cor=sd(abs(cor_Yates2014),na.rm=T),
                                      max_cor=max(abs(cor_Yates2014),na.rm=T),min_cor=min(abs(cor_Yates2014),na.rm=T))
         save(cor_Yates2014,file=file.path(results_dir,"cor_Yates2014.RData"))
         
         
         #Depletion -----------------------------------------------------------
         Yates2014_miss <- compute_missing_trait_distance(Yates2014, Yates2014_cat,
                                                               percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                               classical_gower = TRUE,pos= 100)
         
         save(Yates2014_miss,file=file.path(results_dir,"Yates2014_miss.RData"))
         
         #Plot  --------
         depletion_plot(Yates2014_miss,version=1)
         
         #Table result Depletion  --------
         Yates2014_miss_final <- Yates2014_miss
         Yates2014_miss_final <-  aggregate(x = Yates2014_miss_final, by =Yates2014_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         Yates2014_dim <- dimension_funct(Yates2014, dim_pcoa = 20,  metric_scaled = TRUE, Yates2014_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(Yates2014_dim,file=file.path(results_dir,"Yates2014_dim.RData"))
         #Plot  --------
         dim_plot(Yates2014_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         Yates2014_dist <- as.dist(as.matrix(daisy(Yates2014,metric = "gower")))
         Yates2014_pcoa <- pcoa(Yates2014_dist)
         save(Yates2014_pcoa,file=file.path(results_dir,"Yates2014_pcoa.RData"))
         
         Yates2014_sigle <-DPC(Yates2014_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(Yates2014_sigle,file=file.path(results_dir,"Yates2014_sigle.RData"))
         
         #Table result Sigleton  --------
         Yates2014_sigle <- data.frame(rho          = Yates2014_sigle$rho,
                                            cluster_core = Yates2014_sigle$cluster_core,
                                            cluster      = Yates2014_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         Yates2014_summary_res <-  synth_results(trait_df =Yates2014,miss_final_df = Yates2014_miss_final, dim_df = Yates2014_dim,single_df = Yates2014_sigle)
         
         Yates2014_res <- list(summary_res    = Yates2014_summary_res,
                                    depletion = Yates2014_miss_final,
                                    dimension = Yates2014_dim,
                                    cluster = Yates2014_sigle)
         
         save(Yates2014_res,file=file.path(results_dir,"Yates2014_res.RData"))
# Set 19 --------------------------------------------------------------------Jeliazkov2013
         Jeliazkov2013 <- read.csv2(file.path(data_dir,"Jeliazkov2013.csv"),  header=TRUE,row.names = 1)
         
         #Trait prep and cat --------------------------------------------------------------------
        for (i in c(1:89)){
           Jeliazkov2013[,i] <- factor(as.character(Jeliazkov2013[,i],level=TRUE))
         }
         
         
         Jeliazkov2013_cat <- data.frame(
           trait_name        = colnames(head(Jeliazkov2013)),
           trait_type        = rep(NA,ncol(Jeliazkov2013)),
           fuzzy_name        = rep(NA,ncol(Jeliazkov2013)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_Jeliazkov2013 <- matrix(0,ncol(Jeliazkov2013),ncol(Jeliazkov2013))
         
         for ( i in 1:ncol(Jeliazkov2013))
         {
           for (j in i:ncol(Jeliazkov2013))
           {
             cor_Jeliazkov2013[i,j] <-  cor(rank(Jeliazkov2013[,i]), rank(Jeliazkov2013[,j]),method="kendall")
           }
         }
         cor_Jeliazkov2013[lower.tri(cor_Jeliazkov2013)] <- t(cor_Jeliazkov2013)[lower.tri(cor_Jeliazkov2013)]
         diag(cor_Jeliazkov2013)=NA
         cor_Jeliazkov2013<- data.frame(mean_cor=mean(abs(cor_Jeliazkov2013),na.rm=T),sd_cor=sd(abs(cor_Jeliazkov2013),na.rm=T),
                                      max_cor=max(abs(cor_Jeliazkov2013),na.rm=T),min_cor=min(abs(cor_Jeliazkov2013),na.rm=T))
         save(cor_Jeliazkov2013,file=file.path(results_dir,"cor_Jeliazkov2013.RData"))
         
         #Depletion -----------------------------------------------------------
         Jeliazkov2013_miss <- compute_missing_trait_distance(Jeliazkov2013, Jeliazkov2013_cat,
                                                          percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                          classical_gower = TRUE,pos= 100)
         
         save(Jeliazkov2013_miss,file=file.path(results_dir,"Jeliazkov2013_miss.RData"))
         
         #Plot  --------
         depletion_plot(Jeliazkov2013_miss,version=1)
         
         #Table result Depletion  --------
         Jeliazkov2013_miss_final <- Jeliazkov2013_miss
         Jeliazkov2013_miss_final <-  aggregate(x = Jeliazkov2013_miss_final, by =Jeliazkov2013_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         Jeliazkov2013_dim <- dimension_funct(Jeliazkov2013, dim_pcoa = 20,  metric_scaled = TRUE, Jeliazkov2013_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(Jeliazkov2013_dim,file=file.path(results_dir,"Jeliazkov2013_dim.RData"))
         
         #Plot  --------
         dim_plot(Jeliazkov2013_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         Jeliazkov2013_dist <- as.dist(as.matrix(daisy(Jeliazkov2013,metric = "gower")))
         Jeliazkov2013_pcoa <- pcoa(Jeliazkov2013_dist)
         save(Jeliazkov2013_pcoa,file=file.path(results_dir,"Jeliazkov2013_pcoa.RData"))
         
         Jeliazkov2013_sigle <-DPC(Jeliazkov2013_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(Jeliazkov2013_sigle,file=file.path(results_dir,"Jeliazkov2013_sigle.RData"))
         
         #Table result Sigleton  --------
         Jeliazkov2013_sigle <- data.frame(rho          = Jeliazkov2013_sigle$rho,
                                       cluster_core = Jeliazkov2013_sigle$cluster_core,
                                       cluster      = Jeliazkov2013_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         Jeliazkov2013_summary_res <-  synth_results(trait_df =Jeliazkov2013,miss_final_df = Jeliazkov2013_miss_final, dim_df = Jeliazkov2013_dim,single_df = Jeliazkov2013_sigle)
         
         Jeliazkov2013_res <- list(summary_res    = Jeliazkov2013_summary_res,
                               depletion = Jeliazkov2013_miss_final,
                               dimension = Jeliazkov2013_dim,
                               cluster = Jeliazkov2013_sigle)
         
         save(Jeliazkov2013_res,file=file.path(results_dir,"Jeliazkov2013_res.RData"))



# Set 20 --------------------------------------------------------------------
         Gibb2015 <- read.csv2(file.path(data_dir,"Gibb2015.csv"),  header=TRUE,row.names = 1)
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(1:10)){
           Gibb2015[,i] <- as.numeric(as.character(Gibb2015[,i]))
         }
         
         Gibb2015_cat <- data.frame(
           trait_name        = colnames(head(Gibb2015)),
           trait_type        = rep(NA,ncol(Gibb2015)),
           fuzzy_name        = rep(NA,ncol(Gibb2015)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_Gibb2015 <- matrix(0,ncol(Gibb2015),ncol(Gibb2015))
         
         for ( i in 1:ncol(Gibb2015))
         {
           for (j in i:ncol(Gibb2015))
           {
             cor_Gibb2015[i,j] <-  cor(rank(Gibb2015[,i]), rank(Gibb2015[,j]),method="kendall")
           }
         }
         cor_Gibb2015[lower.tri(cor_Gibb2015)] <- t(cor_Gibb2015)[lower.tri(cor_Gibb2015)]
         diag(cor_Gibb2015)=NA
         cor_Gibb2015<- data.frame(mean_cor=mean(abs(cor_Gibb2015),na.rm=T),sd_cor=sd(abs(cor_Gibb2015),na.rm=T),
                                      max_cor=max(abs(cor_Gibb2015),na.rm=T),min_cor=min(abs(cor_Gibb2015),na.rm=T))
         save(cor_Gibb2015,file=file.path(results_dir,"cor_Gibb2015.RData"))
         
         #Depletion -----------------------------------------------------------
         Gibb2015_miss <- compute_missing_trait_distance(Gibb2015, Gibb2015_cat,
                                                          percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                          classical_gower = TRUE,pos= 100)
         
         save(Gibb2015_miss,file=file.path(results_dir,"Gibb2015_miss.RData"))
         
         #Plot  --------
         depletion_plot(Gibb2015_miss,version=1)
         
         #Table result Depletion  --------
         Gibb2015_miss_final <- Gibb2015_miss
         Gibb2015_miss_final <-  aggregate(x = Gibb2015_miss_final, by =Gibb2015_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         Gibb2015_dim <- dimension_funct(Gibb2015, dim_pcoa = 20,  metric_scaled = TRUE, Gibb2015_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(Gibb2015_dim,file=file.path(results_dir,"Gibb2015_dim.RData"))
         
         #Plot  --------
         dim_plot(Gibb2015_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         Gibb2015_dist <- as.dist(as.matrix(daisy(Gibb2015,metric = "gower")))
         Gibb2015_pcoa <- pcoa(Gibb2015_dist)
         save(Gibb2015_pcoa,file=file.path(results_dir,"Gibb2015_pcoa.RData"))
         
         Gibb2015_sigle <-DPC(Gibb2015_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(Gibb2015_sigle,file=file.path(results_dir,"Gibb2015_sigle.RData"))
         
         #Table result Sigleton  --------
         Gibb2015_sigle <- data.frame(rho          = Gibb2015_sigle$rho,
                                       cluster_core = Gibb2015_sigle$cluster_core,
                                       cluster      = Gibb2015_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         Gibb2015_summary_res <-  synth_results(trait_df =Gibb2015,miss_final_df = Gibb2015_miss_final, dim_df = Gibb2015_dim,single_df = Gibb2015_sigle)
         
         Gibb2015_res <- list(summary_res    = Gibb2015_summary_res,
                               depletion = Gibb2015_miss_final,
                               dimension = Gibb2015_dim,
                               cluster = Gibb2015_sigle)
         
         save(Gibb2015_res,file=file.path(results_dir,"Gibb2015_res.RData"))
         
         
 # Set 21 --------------------------------------------------------------------
         ThermalFauna <- read.csv2(file.path(data_dir,"Thermal Vent Fauna.csv"),  header=TRUE,row.names = 1)
         
         #Trait prep and cat --------------------------------------------------------------------
        for (i in c(3:8,13:16)){
           ThermalFauna[,i] <- factor(as.character(ThermalFauna[,i],level=TRUE))
         }
         
         ThermalFauna[,1] <- factor(as.character(ThermalFauna[,1],level=TRUE))
         
         
         ThermalFauna_cat <- data.frame(
           trait_name        = colnames(head(ThermalFauna)),
           trait_type        = rep(NA,ncol(ThermalFauna)),
           fuzzy_name        = rep(NA,ncol(ThermalFauna)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_ThermalFauna <- matrix(0,ncol(ThermalFauna),ncol(ThermalFauna))
         
         for ( i in 1:ncol(ThermalFauna))
         {
           for (j in i:ncol(ThermalFauna))
           {
             cor_ThermalFauna[i,j] <-  cor(rank(ThermalFauna[,i]), rank(ThermalFauna[,j]),method="kendall")
           }
         }
         cor_ThermalFauna[lower.tri(cor_ThermalFauna)] <- t(cor_ThermalFauna)[lower.tri(cor_ThermalFauna)]
         diag(cor_ThermalFauna)=NA
         cor_ThermalFauna<- data.frame(mean_cor=mean(abs(cor_ThermalFauna),na.rm=T),sd_cor=sd(abs(cor_ThermalFauna),na.rm=T),
                                      max_cor=max(abs(cor_ThermalFauna),na.rm=T),min_cor=min(abs(cor_ThermalFauna),na.rm=T))
         save(cor_ThermalFauna,file=file.path(results_dir,"cor_ThermalFauna.RData"))
         
         #Depletion -----------------------------------------------------------
         ThermalFauna_miss <- compute_missing_trait_distance(ThermalFauna, ThermalFauna_cat,
                                                          percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                          classical_gower = TRUE,pos= 100)
         
         save(ThermalFauna_miss,file=file.path(results_dir,"ThermalFauna_miss.RData"))
         
         #Plot  --------
         depletion_plot(ThermalFauna_miss,version=1)
         
         #Table result Depletion  --------
         ThermalFauna_miss_final <- ThermalFauna_miss
         ThermalFauna_miss_final <-  aggregate(x = ThermalFauna_miss_final, by =ThermalFauna_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         ThermalFauna_dim <- dimension_funct(ThermalFauna, dim_pcoa = 20,  metric_scaled = TRUE, ThermalFauna_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(ThermalFauna_dim,file=file.path(results_dir,"ThermalFauna_dim.RData"))
         
         #Plot  --------
         dim_plot(ThermalFauna_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         ThermalFauna_dist <- as.dist(as.matrix(daisy(ThermalFauna,metric = "gower")))
         ThermalFauna_pcoa <- pcoa(ThermalFauna_dist)
         save(ThermalFauna_pcoa,file=file.path(results_dir,"ThermalFauna_pcoa.RData"))
         
         ThermalFauna_sigle <-DPC(ThermalFauna_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(ThermalFauna_sigle,file=file.path(results_dir,"ThermalFauna_sigle.RData"))
         
         #Table result Sigleton  --------
         ThermalFauna_sigle <- data.frame(rho       = ThermalFauna_sigle$rho,
                                       cluster_core = ThermalFauna_sigle$cluster_core,
                                       cluster      = ThermalFauna_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         ThermalFauna_summary_res <-  synth_results(trait_df =ThermalFauna,miss_final_df = ThermalFauna_miss_final, dim_df = ThermalFauna_dim,single_df = ThermalFauna_sigle)
         
         ThermalFauna_res <- list(summary_res    = ThermalFauna_summary_res,
                               depletion = ThermalFauna_miss_final,
                               dimension = ThermalFauna_dim,
                               cluster = ThermalFauna_sigle)
         
         save(ThermalFauna_res,file=file.path(results_dir,"ThermalFauna_res.RData"))
         
         
         
         
         
# Set 21 --------------------------------------------------------------------
         plant_alps <- read.csv2(file.path(data_dir,"plant_alps.csv"),  header=TRUE,row.names = 1)
         #Remove to much NA
         #---- remove traits with too much NA (more than 60%)
         plant_alps <- plant_alps[,colSums(is.na(plant_alps)) < nrow(plant_alps)*0.6 ]
         #---- remove line with too much NA more than 50%
         plant_alps <- plant_alps[rowSums(is.na(plant_alps)) < ncol(plant_alps)*0.5, ]
         
        
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(1:3,5,7,10,13,15,17,20:27,31:33)){
           plant_alps[,i] <- factor(as.character(plant_alps[,i],level=FALSE))
         }
         
         for (i in c(4,6,8,9,11,12,14,16,19)){
           plant_alps[,i] <- factor(as.character(plant_alps[,i],level=TRUE))
         }
         
         for (i in c(18,28:30)){
           plant_alps[,i] <- as.numeric(as.character(plant_alps[,i]))
         }
        
         
         
         plant_alps_cat <- data.frame(
           trait_name        = colnames(head(plant_alps)),
           trait_type        = rep(NA,ncol(plant_alps)),
           fuzzy_name        = rep(NA,ncol(plant_alps)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_plant_alps <- matrix(0,ncol(plant_alps),ncol(plant_alps))
         
         for ( i in 1:ncol(plant_alps))
         {
           for (j in i:ncol(plant_alps))
           {
             cor_plant_alps[i,j] <-  cor(rank(plant_alps[,i]), rank(plant_alps[,j]),method="kendall")
           }
         }
         cor_plant_alps[lower.tri(cor_plant_alps)] <- t(cor_plant_alps)[lower.tri(cor_plant_alps)]
         diag(cor_plant_alps)=NA
         cor_plant_alps<- data.frame(mean_cor=mean(abs(cor_plant_alps),na.rm=T),sd_cor=sd(abs(cor_plant_alps),na.rm=T),
                                      max_cor=max(abs(cor_plant_alps),na.rm=T),min_cor=min(abs(cor_plant_alps),na.rm=T))
         save(cor_plant_alps,file=file.path(results_dir,"cor_plant_alps.RData"))
         
         #Depletion -----------------------------------------------------------
         plant_alps_miss <- compute_missing_trait_distance(plant_alps, plant_alps_cat,
                                                             percent_list, max_comb = 100,n_perm = 100,cores = 3, 
                                                             classical_gower = TRUE,pos= 100)
         
         save(plant_alps_miss,file=file.path(results_dir,"plant_alps_miss.RData"))
         
         #Plot  --------
         depletion_plot(plant_alps_miss,version=1)
         
         #Table result Depletion  --------
         plant_alps_miss_final <- plant_alps_miss
         plant_alps_miss_final <-  aggregate(x = plant_alps_miss_final, by =plant_alps_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         plant_alps_dim <- dimension_funct(plant_alps, dim_pcoa = 20,  metric_scaled = TRUE, plant_alps_cat, classical_gower=TRUE,rep=100 ,cores = 3)
         save(plant_alps_dim,file=file.path(results_dir,"plant_alps_dim.RData"))
         
         #Plot  --------
         dim_plot(plant_alps_dim,dim_pcoa = 20,metric = "MAD")
         
         #Sigleton  -----------------------------------------------------------
         
         plant_alps_dist <- as.dist(as.matrix(daisy(plant_alps,metric = "gower")))
         plant_alps_pcoa <- pcoa(plant_alps_dist)
         save(plant_alps_pcoa,file=file.path(results_dir,"plant_alps_pcoa.RData"))
         
         plant_alps_sigle <-DPC(plant_alps_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(plant_alps_sigle,file=file.path(results_dir,"plant_alps_sigle.RData"))
         
         #Table result Sigleton  --------
         plant_alps_sigle <- data.frame(rho       = plant_alps_sigle$rho,
                                          cluster_core = plant_alps_sigle$cluster_core,
                                          cluster      = plant_alps_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         plant_alps_summary_res <-  synth_results(trait_df =plant_alps,miss_final_df = plant_alps_miss_final, dim_df = plant_alps_dim,single_df = plant_alps_sigle)
         
         plant_alps_res <- list(summary_res    = plant_alps_summary_res,
                                  depletion = plant_alps_miss_final,
                                  dimension = plant_alps_dim,
                                  cluster = plant_alps_sigle)
         
         save(plant_alps_res,file=file.path(results_dir,"plant_alps_res.RData"))
         
         
# Set 23 --------------------------------------------------------------------CHondricthyens
         Chondri <- read.csv2(file.path(data_dir,"Chondrichtyens_traits.csv"),  header=TRUE,row.names = 1)
         #Remove to much NA
         #---- remove traits with too much NA (more than 60%)
         Chondri <- Chondri[,colSums(is.na(Chondri)) < 672 ]
         #---- remove line with too much NA more than 50%
         Chondri <- Chondri[rowSums(is.na(Chondri)) < 7, ]
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(1:7,11:14)){
           Chondri[,i] <- as.numeric(as.character(Chondri[,i]))
         }
         
         for (i in 8:10){
           Chondri[,i] <- factor(as.character(Chondri[,i]))
         }
         
         Chondri_cat <- data.frame(
           trait_name        = colnames(head(Chondri)),
           trait_type        = rep(NA,ncol(Chondri)),
           fuzzy_name        = rep(NA,ncol(Chondri)),
           stringsAsFactors  = FALSE)
         
         #Correlation Traits -----------------------------------------------------------
         cor_Chondri <- matrix(0,ncol(Chondri),ncol(Chondri))
         
         for ( i in 1:ncol(Chondri))
         {
           for (j in i:ncol(Chondri))
           {
             cor_Chondri[i,j] <-  cor(rank(Chondri[,i]), rank(Chondri[,j]),method="kendall")
           }
         }
         cor_Chondri[lower.tri(cor_Chondri)] <- t(cor_Chondri)[lower.tri(cor_Chondri)]
         diag(cor_Chondri)=NA
         cor_Chondri<- data.frame(mean_cor=mean(abs(cor_Chondri),na.rm=T),sd_cor=sd(abs(cor_Chondri),na.rm=T),
                                      max_cor=max(abs(cor_Chondri),na.rm=T),min_cor=min(abs(cor_Chondri),na.rm=T))
         save(cor_Chondri,file=file.path(results_dir,"cor_Chondri.RData"))
         
         
         
         #Depletion -----------------------------------------------------------
         Chondri_miss <- compute_missing_trait_distance(Chondri, Chondri_cat,
                                                        percent_list, max_comb = 100,n_perm = 100,cores = 4, 
                                                        classical_gower = TRUE,pos= 100)
         #Plot  --------
         depletion_plot(Chondri_miss,version=1)
         save(Chondri_miss,file=file.path(results_dir,"Chondri_miss.RData"))
         #Table result Depletion  --------
         Chondri_miss_final <- Chondri_miss
         Chondri_miss_final <-  aggregate(x = Chondri_miss_final, by =Chondri_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         Chondri_dim <- dimension_funct(Chondri, dim_pcoa = 20,  metric_scaled = TRUE, Chondri_cat, classical_gower=TRUE,rep=999 ,cores = 3)
         save(Chondri_dim,file=file.path(results_dir,"Chondri_dim.RData"))
         
         #Plot  --------
         dim_plot(Chondri_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         Chondri_dist <- as.dist(as.matrix(daisy(Chondri,metric = "gower")))
         Chondri_pcoa <- pcoa(Chondri_dist)
         save(Chondri_pcoa,file=file.path(results_dir,"Chondri_pcoa.RData"))
         
         Chondri_sigle <-DPC(Chondri_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(Chondri_sigle,file=file.path(results_dir,"Chondri_sigle.RData"))
         
         #Table result Sigleton  --------
         Chondri_sigle <- data.frame(rho          = Chondri_sigle$rho,
                                     cluster_core = Chondri_sigle$cluster_core,
                                     cluster      = Chondri_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         Chondri_summary_res <-  synth_results(trait_df =Chondri,miss_final_df = Chondri_miss_final, dim_df = Chondri_dim,single_df = Chondri_sigle)
         
         Chondri_res <- list(summary_res    = Chondri_summary_res,
                             depletion = Chondri_miss_final,
                             dimension = Chondri_dim,
                             cluster = Chondri_sigle)
         
         save(Chondri_res,file=file.path(results_dir,"Chondri_res.RData"))
         
         
# Set 24 -------------------------------------------------------------------- Phytoplankton
         Phytoplankton <- read.csv2(file.path(data_dir,"Phytoplankton_traits.csv"), header=TRUE,row.names = 1)
         #Remove to much NA
         #---- remove traits with too much NA (more than 60%)
         Phytoplankton <- Phytoplankton[,colSums(is.na(Phytoplankton)) < nrow(Phytoplankton)*0.6 ]
         #---- remove line with too much NA more than 50%
         Phytoplankton <- Phytoplankton[rowSums(is.na(Phytoplankton)) < ncol(Phytoplankton)*0.5, ]
         
         #Trait prep and cat --------------------------------------------------------------------
         for (i in c(1:3,5:7)){
           Phytoplankton[,i] <- as.numeric(as.character(Phytoplankton[,i]))
         }
         
         for (i in c(4,8:15)){
           Phytoplankton[,i] <- as.factor(as.character(Phytoplankton[,i]))
         }
         
         Phytoplankton_cat <- data.frame(
           trait_name        = colnames(head(Phytoplankton)),
           trait_type        = rep(NA,ncol(Phytoplankton)),
           fuzzy_name        = rep(NA,ncol(Phytoplankton)),
           stringsAsFactors  = FALSE)
         
            
         cor_Phytoplankton <- matrix(0,ncol(Phytoplankton),ncol(Phytoplankton))
         
         for ( i in 1:ncol(Phytoplankton))
         {
           for (j in i:ncol(Phytoplankton))
           {
             cor_Phytoplankton[i,j] <-  cor(rank(Phytoplankton[,i]), rank(Phytoplankton[,j]),method="kendall")
           }
         }
         cor_Phytoplankton[lower.tri(cor_Phytoplankton)] <- t(cor_Phytoplankton)[lower.tri(cor_Phytoplankton)]
         diag(cor_Phytoplankton)=NA
         cor_Phytoplankton<- data.frame(mean_cor=mean(abs(cor_Phytoplankton),na.rm=T),sd_cor=sd(abs(cor_Phytoplankton),na.rm=T),
                                      max_cor=max(abs(cor_Phytoplankton),na.rm=T),min_cor=min(abs(cor_Phytoplankton),na.rm=T))
         save(cor_Phytoplankton,file=file.path(results_dir,"cor_Phytoplankton.RData"))
         
         #Depletion -----------------------------------------------------------
         Phytoplankton_miss <- compute_missing_trait_distance(Phytoplankton, Phytoplankton_cat,
                                                        percent_list, max_comb = 100,n_perm = 100,cores = 4, 
                                                        classical_gower = TRUE,pos= 100)
         #Plot  --------
         depletion_plot(Phytoplankton_miss,version=1)
         save(Phytoplankton_miss,file=file.path(results_dir,"Phytoplankton_miss.RData"))
         #Table result Depletion  --------
         Phytoplankton_miss_final <- Phytoplankton_miss
         Phytoplankton_miss_final <-  aggregate(x = Phytoplankton_miss_final, by =Phytoplankton_miss_final["miss_percent"], FUN = mean)[,-2]
         
         #Dimension  -----------------------------------------------------------
         
         Phytoplankton_dim <- dimension_funct(Phytoplankton, dim_pcoa = 20,  metric_scaled = TRUE, Phytoplankton_cat, classical_gower=TRUE,rep=99 ,cores = 3)
         save(Phytoplankton_dim,file=file.path(results_dir,"Phytoplankton_dim.RData"))
         
         #Plot  --------
         dim_plot(Phytoplankton_dim,dim_pcoa = 20)
         
         #Sigleton  -----------------------------------------------------------
         
         Phytoplankton_dist <- as.dist(as.matrix(daisy(Phytoplankton,metric = "gower")))
         Phytoplankton_pcoa <- ape::pcoa(Phytoplankton_dist)
         save(Phytoplankton_pcoa,file=file.path(results_dir,"Phytoplankton_pcoa.RData"))
         
         Phytoplankton_sigle <-DPC(Phytoplankton_dist,metric = "predefined", radius="automatic",density_filter="global_threshold",rho_threshold=1)
         save(Phytoplankton_sigle,file=file.path(results_dir,"Phytoplankton_sigle.RData"))
         
         #Table result Sigleton  --------
         Phytoplankton_sigle <- data.frame(rho          = Phytoplankton_sigle$rho,
                                     cluster_core = Phytoplankton_sigle$cluster_core,
                                     cluster      = Phytoplankton_sigle$cluster)
         
         #All results  -----------------------------------------------------------
         #Summary res-----------------------------------------------------------
         Phytoplankton_summary_res <-  synth_results(trait_df =Phytoplankton,miss_final_df = Phytoplankton_miss_final, dim_df = Phytoplankton_dim,single_df = Phytoplankton_sigle)
         
         Phytoplankton_res <- list(summary_res    = Phytoplankton_summary_res,
                             depletion = Phytoplankton_miss_final,
                             dimension = Phytoplankton_dim,
                             cluster = Phytoplankton_sigle)
         
         save(Phytoplankton_res,file=file.path(results_dir,"Phytoplankton_res.RData"))
