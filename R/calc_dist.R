################################################################################
#' Function to compute distance between species
#'
#' For a given combination of traits return the distance matrix
#' @param trait_df  a trait data.frame with species as rows and traits as
#'                  columns
#' @param trait_category_df    a data.frame describing the categories of each traits with
#'                  3 columns: trait name, trait category and fuzzy category if
#'                  trait is fuzzy
#' @param comb_name a character vector of the trait combination to test
#' @param classical_gower if true, do not use modified Gower from Pavoine
#################################################################################
calc_dist <- function(trait_df, trait_category_df, comb_name,classical_gower) {
  
  # Similarly to what is needed by 'ade4::dist.ktab()' we divide the selected
  # traits into sub data frames
  
  # Keep only needed traits and categories
  trait_df <- trait_df[, comb_name, drop = FALSE]
  
  if(classical_gower==TRUE){ktab_dist <- cluster::daisy (trait_df,"gower")
  
  }else{
    
    trait_category_df <- trait_category_df[trait_category_df$trait_name %in% comb_name,, drop = FALSE]
    
    # Quantitative traits
    quant_trait <- NULL
    
    if (any(trait_category_df$trait_type == "Q")) {
      quant_trait <- trait_df[, trait_category_df$trait_name[trait_category_df$trait_type == "Q"],
                              drop = FALSE]
    }
    
    # Ordinal Traits
    ord_trait <- NULL
    
    if (any(trait_category_df$trait_type == "O")) {
      ord_trait <- trait_df[, trait_category_df$trait_name[trait_category_df$trait_type == "O"],
                            drop = FALSE]
    }
    
    
    # Circular traits
    circ_trait <- NULL
    
    if (any(trait_category_df$trait_type == "C")) {
      circ_trait <- trait_df[, trait_category_df$trait_name[trait_category_df$trait_type == "C"],
                             drop = FALSE]
      
      circ_trait <- ade4::prep.circular(circ_trait,1,12)
    }
    
    
    # Fuzzy traits (basically several categories that are considered a single
    #               trait)
    fuzz_trait <- NULL
    
    if (any(trait_category_df$trait_type == "F")) {
      
      # Select the fuzzy traits
      fuzz_trait <- trait_df[, trait_category_df$trait_name[trait_category_df$trait_type == "F"],
                             drop = FALSE]
      
      # Count the number of fuzzy categories
      fuzz_cat <- table(trait_category_df$fuzzy_name)
      
      # Order the trait names based on the order of the categories
      fuzz_names_ordered <- unlist(lapply(names(fuzz_cat),
                                          function(x) {
                                            trait_category_df$trait_name[trait_category_df$fuzzy_name == x & !is.na(trait_category_df$fuzzy_name)]
                                          }))
      
      # Reorder the traits according to the names
      fuzz_trait <- fuzz_trait[, fuzz_names_ordered]
      
      fuzz_trait <- ade4::prep.fuzzy(fuzz_trait,
                                     col.blocks = as.numeric(fuzz_cat),
                                     labels     = names(fuzz_cat))
    }
    
    # Binary traits
    bin_trait <- NULL
    
    if (any(trait_category_df$trait_type == "B")) {
      bin_trait <- trait_df[, trait_category_df$trait_name[trait_category_df$trait_type == "B"], drop = FALSE]
      
      bin_trait <- ade4::prep.binary(bin_trait, col.blocks = ncol(bin_trait))
    }
    
    # Norminal traits
    nom_trait <- NULL
    
    if (any(trait_category_df$trait_type == "N")) {
      nom_trait <- trait_df[, trait_category_df$trait_name[trait_category_df$trait_type == "N"], drop = FALSE]
    }
    
    # Combine all traits (remove 'NULL' data.frames using 'plyr::compact()')
    all_trait <- plyr::compact(list(Q = quant_trait,
                                    O = ord_trait,
                                    F = fuzz_trait,
                                    B = bin_trait,
                                    N = nom_trait,
                                    C = circ_trait))
    
    ktab_list <- ade4::ktab.list.df(all_trait)
    
    # Compute the final distance using the retained categories
    ktab_dist <- ade4::dist.ktab(ktab_list, names(all_trait),
                                 option = "scaledBYrange")
  }
  
  return(ktab_dist)
}

