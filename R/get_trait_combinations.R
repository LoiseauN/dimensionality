#' Get All Traits Combinations
#'
#' This function returns a list of all combinations with the given percentage of
#'   missing traits.
#'
#' @param trait_trait_category_df a data frame describing the categories of each
#'   traits with 3 columns: trait name, trait category and fuzzy category if
#'   trait is fuzzy
#' @param percent_list a numeric vector of percent of missing traits that should
#'   be considered (should be between 0 and 1)
#' @param pos number of combination to consider
#'
#' @return __WHAT?__
#'
#' @export

get_trait_combinations <- function(trait_trait_category_df, percent_list, pos) {

  # Get the names of fuzzy categories ----

  na_rows     <- !is.na(trait_trait_category_df$"fuzzy_name")
  fuzzy_names <- unique(trait_trait_category_df$"fuzzy_name"[na_rows])


  # Names of traits that are not fuzzy ----

  nf_rows          <- is.na(trait_trait_category_df$"fuzzy_name")
  non_fuzzy_traits <- trait_trait_category_df$"trait_name"[nf_rows]

  all_traits <- c(fuzzy_names, non_fuzzy_traits)


  # Compute percent missing ---

  names(percent_list) <- paste0("misstrait_", round(100 * percent_list, 0), "%")


  # Convert percent missing to number of traits ---

  nb_traits <- lapply(percent_list, function(x) {
    round(length(all_traits) * (1 - x), digits = 0)
  })


  # When to values rounded are equal keep unique values ----

  nb_traits <- nb_traits[nb_traits != 0]
  nb_traits <- nb_traits[!duplicated(nb_traits)]

  names(nb_traits) <- paste0(names(nb_traits), "_n", nb_traits)


  # All combinations of trait categories with fixed missing trait proportion ----

  all_comb <- lapply(nb_traits, function(x) {

    n_traits   <- length(all_traits)
    n_possible <- factorial(n_traits) / (factorial(x) * factorial(n_traits - x)) # need this to randomly sample

    if (x == 1) {

      res <- as.list(
        as.data.frame(
          t(
            chose_n_combn(sample(all_traits), x, round(runif(pos, 1, n_possible)))
          )
        )
      )

    } else {

      res <- as.list(
        as.data.frame(
          chose_n_combn(sample(all_traits), x, round(runif(pos, 1, n_possible)))
        )
      )
    }

    lapply(res,as.character)
  })

  all_comb <- lapply(all_comb, unname)


  # Replace fuzzy trait category by trait name ----

  final_comb <- lapply(all_comb, function(x) {

    lapply(x, function(trait_list) {

      fuzz_cat     <- trait_list[trait_list %in% fuzzy_names]
      fuzz_traits  <- trait_trait_category_df$"fuzzy_name" %in% fuzz_cat
      fuzz_traits  <- trait_trait_category_df$"trait_name"[fuzz_traits]

      other_traits <- trait_list[!(trait_list %in% fuzzy_names)]

      c(fuzz_traits, other_traits)
    })
  })

  return(final_comb)
}
