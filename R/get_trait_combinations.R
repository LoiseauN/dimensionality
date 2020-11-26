
################################################################################
#' Function to get all trait combinations
#'
#' Return a list of all combinations with the given percentage of missing traits
#' @param trait_trait_category_df a data.frame describing the categories of each
#'                                traits with 3 columns: trait name, trait
#'                                category and fuzzy category if trait is fuzzy
#' @param percent_list a numeric vector of percent of missing trait that should
#'                     be considered /!\ should be between 0 and 1
#' @param pos  number of combination to consider        
#################################################################################
get_trait_combinations = function(trait_trait_category_df, percent_list,pos) {
  
  # Get the names of fuzzy categories
  fuzzy_names = unique(trait_trait_category_df$fuzzy_name[!is.na(trait_trait_category_df$fuzzy_name)])
  
  # Names of traits that are not fuzzy
  non_fuzzy_traits = trait_trait_category_df$trait_name[is.na(trait_trait_category_df$fuzzy_name)]
  
  all_traits = c(fuzzy_names, non_fuzzy_traits)
  
  # Compute percent missing
  names(percent_list) = paste0("misstrait_", round(100 * percent_list, 0),
                               "%")
  
  # Convert percent missing to number of traits
  nb_traits <- lapply(percent_list, function(x) {
    round(length(all_traits) * (1 - x), digits = 0)
  })
  
  # When to values rounded are equal keep unique values
  nb_traits = nb_traits[nb_traits != 0]
  nb_traits = nb_traits[!duplicated(nb_traits)]
  names(nb_traits) = paste0(names(nb_traits), "_n", nb_traits)
  
  # All combinations of trait categories with fixed missing trait proportion
  
  all_comb <- lapply(nb_traits, function(x) {
    numb_possibilities <- factorial(length(all_traits))/(factorial(x)*factorial(length(all_traits)-x)) # need this to randomly sample 
    #combn(all_traits, x, simplify = FALSE)
    #chosencombn(all_traits, x, pos)
    
    if (x==1) { res<- as.list(as.data.frame(t(chosencombn(sample(all_traits), x, round(runif(pos, 1, numb_possibilities))))))  
    }else{ res<- as.list(as.data.frame(chosencombn(sample(all_traits), x, round(runif(pos, 1, numb_possibilities))))) }
    #  if (x==1) { res<- apply(matrix(chosencombn(sample(all_traits), x, round(runif(pos, 1, numb_possibilities)))),1,list)
    # }else{ res<- apply(chosencombn(sample(all_traits), x, round(runif(pos, 1, numb_possibilities))),2,list) }
    lapply(res,as.character)
  })
  
  all_comb<-lapply(all_comb,unname)
  
  #all_comb2 <- lapply(nb_traits, function(x) { #OLD VERSION
  #combn(all_traits, x, simplify = FALSE)
  #})
  
  # Replace fuzzy trait category by trait name
  final_comb <- lapply(all_comb, function(x) {
    lapply(x, function(trait_list) {
      fuzz_cat = trait_list[trait_list %in% fuzzy_names]
      fuzz_traits = trait_trait_category_df$trait_name[trait_trait_category_df$fuzzy_name %in% fuzz_cat]
      
      other_traits = trait_list[!(trait_list %in% fuzzy_names)]
      
      c(fuzz_traits, other_traits)
    })
  })
  
  return(final_comb)
}

