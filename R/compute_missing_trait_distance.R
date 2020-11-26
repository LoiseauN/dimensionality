################################################################################
#' Function compute trait distance with missing traits
#'
#' @param trait_df          a trait data.frame with species as rows and traits as
#'                          columns
#' @param trait_category_df a data.frame describing the categories of each traits with
#'                          3 columns: trait name, trait category and fuzzy category if
#'                          trait is fuzzy
#' @param percent_missing   a numeric vector of percent of missing trait that should
#'                          be considered /!\ should be between 0 and 1
#' @param max_comb          maximum number of combinations to sample
#' @param n_perm            number of permutations for Mantel test
#' @param cores             number of cores used in `mclapply()` 
#' @param classical_gower   if true, do not use modified Gower from Pavoine
#' @param pos               number of combination to consider    
#################################################################################
compute_missing_trait_distance = function(trait_df, trait_category_df,
                                          percent_missing, max_comb = 20,
                                          n_perm = 20, cores = 1,classical_gower, pos=20) {
  
  #trait_df <- Krasnov2015 
  #trait_category_df <- Krasnov2015_cat 
  #percent_missing=0.8
  #classical_gower=T
  
  initial_dist = calc_dist(trait_df, trait_category_df, colnames(trait_df),classical_gower)
  
  n_species = nrow(trait_df)
  
  all_trait_comb = get_trait_combinations(trait_category_df,
                                          percent_list = percent_missing,pos)
  
  selected_trait_comb = sample_trait_combinations(all_trait_comb, max_comb)
  
  selected_dist = lapply(names(selected_trait_comb), function(comb_type) {
    
    comb = selected_trait_comb[[comb_type]]
    
    ui_done(paste0("Depletioooooooooon running! Coffee time! ",comb_type))
    
    given_comb_stat <- pbmclapply(comb, function(x) {
      miss_trait_dist <- calc_dist(trait_df, trait_category_df, x,classical_gower)
      
      if(sum(is.na(miss_trait_dist))>0){
        
        res<- data.frame(miss_percent  = NA,
                         n_trait       = NA,
                         mantel_r      = NA,
                         mantel_signif = NA,
                         MAD           = NA,
                         AUC           = NA)
        return(res)
      } else{ 
        
        # Compute Mantel test between initial distance and distance obtained
        # with missing traits
        mant_test = vegan::mantel(initial_dist, miss_trait_dist,
                                  permutations = n_perm,na.rm=TRUE)
        
        # Compute MAD
        yst = (miss_trait_dist/max(na.omit(miss_trait_dist))) * max(na.omit(initial_dist))
        MAD = (sum(abs(na.omit(as.dist(as.matrix(initial_dist) -  as.matrix(yst)))))) / (n_species * (n_species - 1)/2)
        
        
        # Compute AUC rank
        pcoa_miss <- tryCatch( pcoa_miss<-pcoa(miss_trait_dist),
                               error=function(err){result="NA"})
        
        if((is(pcoa_miss)[1]=="character")=="TRUE"){ 
          
          res<- data.frame(miss_percent  = NA,
                           n_trait       = NA,
                           mantel_r      = NA,
                           mantel_signif = NA,
                           MAD           = NA,
                           AUC           = NA)
          return(res)
        } else{ 
          
          D_dimen <- dist(pcoa_miss$vectors)
          Co_rank <- coranking(initial_dist, D_dimen, input_Xi = "dist")
          
          NX  <- R_NX(Co_rank)
          AUC <- AUC_ln_K(NX)
          
          miss_part = parse_miss_trait(comb_type)
          
          res = data.frame(miss_percent  = miss_part$miss_percent,
                           n_trait       = miss_part$n_trait,
                           mantel_r      = mant_test$statistic,
                           mantel_signif = mant_test$signif,
                           MAD           = MAD,
                           AUC           = AUC)
          return(res)} }
    },mc.cores=cores)
    
    do.call(rbind, given_comb_stat)
  })
  
  do.call(rbind, selected_dist)
}


#-------------
