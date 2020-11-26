################################################################################
#' Function to sample a max. number of combinations in all combinations
#'
#' @param combinations_list a list of all combinations of traits
#' @param max_n             maximum number of combinations to sample
#################################################################################
sample_trait_combinations = function(combinations_list, max_n = 100) {
  
  lapply(combinations_list, function(trait_list) {
    comb_number = length(trait_list)
    
    if (comb_number > max_n) {
      comb_number = max_n
    }
    
    selected_comb = sample(seq_along(trait_list), comb_number,
                           replace = FALSE)
    
    trait_list[selected_comb]
  })
}
