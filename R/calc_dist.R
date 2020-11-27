#' Function to Compute Distance between Species
#'
#' For a given combination of traits, this function returns a distance matrix.
#'
#' @param trait_df a traits data frame with species as rows and traits as
#'   columns
#' @param trait_category_df a data frame describing the categories of each
#'   traits with 3 columns: trait name, trait category, and fuzzy category if
#'   trait is fuzzy
#' @param comb_name a character vector of the traits combination to test
#' @param classical_gower if TRUE, do not use modified Gower distance from
#'   Pavoine
#'
#' @return a distance matrix
#'
#' @export
#' @importFrom ade4 prep.circular prep.fuzzy prep.binary ktab.list.df dist.ktab
#' @importFrom cluster daisy
#' @importFrom plyr compact

calc_dist <- function(trait_df, trait_category_df, comb_name, classical_gower) {

  # Similarly to what is needed by 'ade4::dist.ktab()' we divide the selected
  # traits into sub data frames


  # Keep only needed traits and categories ----

  trait_df <- trait_df[ , comb_name, drop = FALSE]

  if (classical_gower) {

    ktab_dist <- cluster::daisy(trait_df, "gower")

  } else {

    selected_rows     <- trait_category_df$"trait_name" %in% comb_name
    trait_category_df <- trait_category_df[selected_rows, , drop = FALSE]


    # Quantitative traits ----

    quant_trait <- NULL

    if (any(trait_category_df$"trait_type" == "Q")) {

      quant_trait <- trait_category_df$"trait_type" == "Q"
      quant_trait <- trait_category_df$"trait_name"[quant_trait]
      quant_trait <- trait_df[ , quant_trait, drop = FALSE]
    }


    # Ordinal Traits ----

    ord_trait <- NULL

    if (any(trait_category_df$"trait_type" == "O")) {

      ord_trait <- trait_category_df$"trait_type" == "O"
      ord_trait <- trait_category_df$"trait_name"[ord_trait]
      ord_trait <- trait_df[ , ord_trait, drop = FALSE]
    }


    # Circular traits ----

    circ_trait <- NULL

    if (any(trait_category_df$"trait_type" == "C")) {

      circ_trait <- trait_category_df$"trait_type" == "C"
      circ_trait <- trait_category_df$"trait_name"[circ_trait]
      circ_trait <- trait_df[ , circ_trait, drop = FALSE]

      circ_trait <- ade4::prep.circular(circ_trait, 1, 12)
    }


    # Fuzzy traits ----
    # (basically several categories that are considered a single trait)

    fuzz_trait <- NULL

    if (any(trait_category_df$"trait_type" == "F")) {

      fuzz_trait <- trait_category_df$"trait_type" == "F"
      fuzz_trait <- trait_category_df$"trait_name"[fuzz_trait]
      fuzz_trait <- trait_df[ , fuzz_trait, drop = FALSE]


      # Number of fuzzy categories

      fuzz_cat <- table(trait_category_df$"fuzzy_name")


      # Order the trait names based on the order of the categories

      fuzz_names_ordered <- unlist(lapply(names(fuzz_cat), function(x) {
        selected_traits <- trait_category_df$"fuzzy_name" == x &
                           !is.na(trait_category_df$"fuzzy_name")
        return(trait_category_df$"trait_name"[selected_traits])
      }))


      # Reorder the traits according to the names

      fuzz_trait <- fuzz_trait[ , fuzz_names_ordered]

      fuzz_trait <- ade4::prep.fuzzy(
        df         = fuzz_trait,
        col.blocks = as.numeric(fuzz_cat),
        labels     = names(fuzz_cat)
      )
    }


    # Binary traits ----

    bin_trait <- NULL

    if (any(trait_category_df$"trait_type" == "B")) {

      bin_trait <- trait_category_df$"trait_type" == "B"
      bin_trait <- trait_category_df$"trait_name"[bin_trait]
      bin_trait <- trait_df[ , bin_trait, drop = FALSE]

      bin_trait <- ade4::prep.binary(bin_trait, col.blocks = ncol(bin_trait))
    }


    # Nominal traits ----

    nom_trait <- NULL

    if (any(trait_category_df$"trait_type" == "N")) {

      nom_trait <- trait_category_df$"trait_type" == "N"
      nom_trait <- trait_category_df$"trait_name"[nom_trait]
      nom_trait <- trait_df[ , nom_trait, drop = FALSE]
    }


    # Combine all traits (remove NULL data.frames using 'plyr::compact()') ----

    all_trait <- plyr::compact(
      list(
        Q = quant_trait,
        O = ord_trait,
        F = fuzz_trait,
        B = bin_trait,
        N = nom_trait,
        C = circ_trait
      )
    )

    ktab_list <- ade4::ktab.list.df(all_trait)


    # Compute the final distance using the retained categories ----

    ktab_dist <- ade4::dist.ktab(
      x    = ktab_list,
      type = names(all_trait),
      option = "scaledBYrange"
    )
  }

  return(ktab_dist)
}
