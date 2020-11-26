
################################################################################
#' Function to compute the number of dimension 
#'
#' @param data           trait data.frame  output of function dimension_funct
#' @param version        3 types of graphs : 1,2 or 3
#'          
#################################################################################


dimension_funct<- function(trait_df, dim_pcoa = 10,rep = 99,cores=3,
                           metric_scaled = TRUE,trait_category_df,classical_gower) {
  
  #  trait_df <- Bartonova2016
  # classical_gower <- TRUE
  # trait_category_df <- Bartonova2016_cat
  
  
  sp_trdist = calc_dist(trait_df, trait_category_df, colnames(trait_df),classical_gower)
  sp_trdist=  quasieuclid(sp_trdist)
  # computing PCoA-based functional spaces:
  
  pcoa_trdist <- pcoa(sp_trdist)
  # number of dimensions to keep given the input from user and number of PC...
  # ... with positive eigenvalues:
  nbdim <- min(dim_pcoa, ncol(pcoa_trdist$vectors))
  # keeping species coordinates on the 'nbdim' axes and renaming axes:
  sp_coord <- pcoa_trdist$vectors[, 1:nbdim]
  colnames(sp_coord) <- paste("PC", 1:nbdim, sep = "")
  
  # computing quality of multidimensional spaces: storing trait-based...
  # ... distance (=input) in a 3-variables dataframe with 1 row for each...
  # ... pair of species (names stored in the 2 first columns):
  distsp_df <- dendextend::dist_long(sp_trdist)
  names(distsp_df) <- c("sp.x", "sp.y", "distsp_df")
  
  # create a vector to store names of all tested spaces:
  fspaces_nm <- vector()
  
  # increase number of PCoA dimensions:
  for (k in 1:nbdim) {
    # computing Euclidean distance between species:
    dist_sp_k <- dist(sp_coord[, 1:k])
    # storing these distances as additional column to previous dataframe:
    distsp_df[, paste0("pcoa_", k, "dim")] <- dendextend::dist_long(
      dist_sp_k)$distance
    # adding name of funct space:
    fspaces_nm <- c( fspaces_nm, paste0("pcoa_", k, "dim"))
    
    
  }
  
  # Compute Coranking index
  computeAUCandNullmod<- lapply(1:nbdim, function(i) {
    
    pcoa_axes <- pcoa_trdist$vectors[ , 1:i]
    
    D_dimen <- dist(pcoa_axes)
    
    Co_rank <- coranking(sp_trdist, D_dimen, input_Xi = "dist")
    
    NX  <- R_NX(Co_rank)
    
    AUC <- AUC_ln_K(NX)
    
    dat <- data.frame(Axe = i, AUC = round(AUC, 3))
    
    if (i == 1) { rand_table <- NA }
      
    if (i != 1) { 
      ui_done(paste0("Compute null model for AUC, Have a break, Have a kit kat, nbdim = ", i))
      
      if (i == nbdim) { 
        ui_done(paste0("Last one, end of the kit kat!"))}
      #rand_table = matrix(NA,rep,nbdim)
      
      rand_table =  do.call(rbind, pbmclapply(1:rep, function (j) {
        
        real_axes <- pcoa_axes[,-i]
        
        rand_axis <- sample(pcoa_axes[,i])
        
        new_axes <- data.frame(real_axes, rand_axis)
        
        D_dimen_rand <- dist(new_axes)
        
        Co_rank_rand=coranking(sp_trdist,D_dimen_rand,input_Xi = "dist")
        
        NX_rand=R_NX(Co_rank_rand)
        
        AUCrand = AUC_ln_K(NX_rand)
        
        #rand_table[j,] <- AUCrand
        return(AUCrand)
      },mc.cores = cores)
      )
    }
    res <- list(dat,rand_table)
  } ) 
  
  
  AUC_SES_pval <- do.call(cbind,pbmclapply(1:nbdim,function(k){
    
    AUC_obs <- computeAUCandNullmod[[k]][[1]][,2] 
    
    if (k == 1){
      SES  <- NA
      pval <- NA
      
    }else{ 
      SES <- (computeAUCandNullmod[[k]][[1]][,2] - mean(computeAUCandNullmod[[k]][[2]]))/sd(computeAUCandNullmod[[k]][[2]])
      # p-value = proportion of null value inferior to obs Beta (+1)
      pval <- length(which(computeAUCandNullmod[[k]][[2]]>computeAUCandNullmod[[k]][[1]]$AUC )) / (length(computeAUCandNullmod[[k]][[2]])+1)  }
    
    res_final <- rbind(AUC_obs,SES,pval)
    return <- res_final
  },mc.cores = cores))
  
  
  ## Detect inflexion point ----
  axes_table <- data.frame(Axe = 1:nbdim, AUC = AUC_SES_pval[1,])
  ebow_meth <- elbow(axes_table, xmin = 0, ymin = 0)
  
  # compute the choosen quality metric for each functional space:
  if (metric_scaled == FALSE) {
    # compute deviance between distance in each functional space and ...
    # ... trait-based distance:
    dev_distsp <- data.frame (distsp_df[, c("sp.x", "sp.y")],
                              distsp_df[, fspaces_nm] -
                                distsp_df[, "distsp_df"] )
    
    # compute squared deviance:
    # sqr_dev_distsp <- data.frame (dev_distsp[, c("sp.x", "sp.y")],
    # (dev_distsp[, fspaces_nm])^2)
    # mean squared deviation:
    # qual_metric_msd <- apply(sqr_dev_distsp[, fspaces_nm], 2, mean)
    
    # compute absolute deviance:
    abs_dev_distsp <- data.frame (dev_distsp[, c("sp.x", "sp.y")],
                                  abs(dev_distsp[, fspaces_nm]))
    # mean absolute deviation:
    qual_metric_mad <- apply(abs_dev_distsp[, fspaces_nm], 2, mean)
    
    
  }
  
  if (metric_scaled == TRUE) {
    
    # compute deviance between distance in each functional space and ...
    # ... trait-based distance:
    scdistsp <- apply(distsp_df[, fspaces_nm], 2, function(x) {  x / max(x) *
        max(distsp_df[, "distsp_df"])})
    # compute deviance:
    dev_scdistsp <- data.frame (distsp_df[, c("sp.x", "sp.y")],
                                scdistsp[, fspaces_nm] -
                                  distsp_df[, "distsp_df"])
    
    # compute squared deviance:
    # sqr_dev_scdistsp <- data.frame (dev_scdistsp[, c("sp.x", "sp.y")],
    # (dev_scdistsp[, fspaces_nm])^2)
    # compute mean squared deviation:
    # qual_metric_msd <- apply(sqr_dev_scdistsp[, fspaces_nm], 2, mean)
    
    
    # compute absolute deviance:
    abs_dev_scdistsp <- data.frame (dev_scdistsp[, c("sp.x", "sp.y")],
                                    abs(dev_scdistsp[, fspaces_nm]))
    # compute mean absolute deviation:
    qual_metric_mad <- apply(abs_dev_scdistsp[, fspaces_nm], 2, mean)
    
  }
  
  # grouping and returning results:
  res <- data.frame(dim                = c(1:dim_pcoa),
                    #quality_fspaces_msd  = round(qual_metric_msd,3),
                    MAD  = round(qual_metric_mad,3),
                    AUC                  = ebow_meth$AUC, 
                    benefit_AUCebow_meth = ebow_meth$benefits,
                    SelectedbyAUCelbow   = ebow_meth$SelectedorNot,
                    SES    = AUC_SES_pval[2,],
                    pvalue = AUC_SES_pval[3,])
  
  return(res)
}

