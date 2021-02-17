#' Test Influence of Number of Traits and S on Simulated Data

dim_simulation <- function(traits, tr_cat, threshold, cut, core) {
  
  do.call(rbind, lapply(threshold, function(j) { 
  
    # vary number of randomly selected species

    do.call(rbind, lapply(cutS, function(i) {   # 3:nrow(traits)
      
      traits_sub <- traits[sample(1:nsp, i), ]
      
      traits_sub_dist <- as.dist(as.matrix(daisy(traits_sub, metric = "gower")))
      
      traits_sub_pcoa <- pcoa(traits_sub_dist)
      
      
      # limit to 40 dimension to avoid to long computation

      if (ncol(traits_sub_pcoa$vectors) < 40) {
        
        pcoa_dim <- ncol(traits_sub_pcoa$vectors)
        
      } else {
        
        pcoa_dim <- 40
      }
      
      out <- dimension_funct(traits_sub, dim_pcoa = pcoa_dim, tr_cat, 
                             classical_gower = TRUE, metric_scaled = TRUE, 
                             rep = 1, cores = core)
      
      naxes <- out[min(which(out$AUC >= j)), "dim"]
      
      data.frame(threshold = j, nsp = nrow(traits_sub), naxes = naxes)
      
    }))
  }))
}
