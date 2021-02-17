#' Compute the Number of Dimensions
#'
#' __ADD DESCRIPTION__
#'
#' @param trait_df a traits data frame with species as rows and traits as
#'   columns
#' @param dim_pcoa number of PCoA axes
#' @param rep number of replicates
#' @param cores number of cores (parallelization)
#' @param metric_scaled a boolean
#' @param trait_category_df a data frame describing the categories of each
#'   traits with 3 columns: trait name, trait category, and fuzzy category if
#'   trait is fuzzy
#' @param classical_gower if TRUE, do not use modified Gower distance from
#'   Pavoine
#'
#' @export
#' @importFrom ade4 quasieuclid
#' @importFrom ape pcoa
#' @importFrom coRanking coranking R_NX AUC_ln_K
#' @importFrom dendextend dist_long
#' @importFrom pbmcapply pbmclapply
#' @importFrom stats dist
#' @importFrom usethis ui_done

dimension_funct <- function(trait_df, dim_pcoa = 10, rep = 99, cores = 3,
                            metric_scaled = TRUE, trait_category_df,
                            classical_gower) {

  sp_trdist <- calc_dist(trait_df, trait_category_df, colnames(trait_df),
                         classical_gower)
  sp_trdist <- ade4::quasieuclid(sp_trdist)


  # Computing PCoA-based functional spaces ----

  pcoa_trdist <- ape::pcoa(sp_trdist)


  # Number of dimensions to keep given the input from user and number of PC
  # with positive eigenvalues

  nbdim <- min(dim_pcoa, ncol(pcoa_trdist$"vectors"))


  # Keeping species coordinates on the 'nbdim' axes and renaming axes

  sp_coord <- pcoa_trdist$"vectors"[ , 1:nbdim]
  colnames(sp_coord) <- paste0("PC", 1:nbdim)

  # Computing quality of multidimensional spaces: storing trait-based distance
  # (=input) in a 3-variables dataframe with 1 row for each pair of species
  # (names stored in the 2 first columns)

  distsp_df <- dendextend::dist_long(sp_trdist)
  names(distsp_df) <- c("sp.x", "sp.y", "distsp_df")

  fspaces_nm <- vector()

  # Increase Nnmber of PCoA dimensions

  for (k in 1:nbdim) {

    # Computing Euclidean distance between species
    dist_sp_k <- dist(sp_coord[ , 1:k])

    # Storing these distances as additional column to previous data frame
    value <- dendextend::dist_long(dist_sp_k)$"distance"
    distsp_df[ , paste0("pcoa_", k, "dim")] <- value

    # Adding name of funct space
    fspaces_nm <- c(fspaces_nm, paste0("pcoa_", k, "dim"))
  }


  # Compute coranking index ----

  computeAUCandNullmod <- lapply(1:nbdim, function(i) {

    pcoa_axes <- pcoa_trdist$"vectors"[ , 1:i]
    D_dimen   <- stats::dist(pcoa_axes)

    Co_rank   <- coRanking::coranking(sp_trdist, D_dimen, input_Xi = "dist")
    NX        <- coRanking::R_NX(Co_rank)
    AUC       <- coRanking::AUC_ln_K(NX)

    dat <- data.frame(Axe = i, AUC = round(AUC, 3))

    if (i == 1) rand_table <- NA

    if (i != 1) {

      usethis::ui_done(
        paste("Compute NULL model for AUC, nbdim =", i)
      )

      if (i == nbdim) usethis::ui_done(paste0("Last one!"))

      rand_table <- do.call(rbind, pbmcapply::pbmclapply(1:rep, function (j) {

        real_axes <- pcoa_axes[ , -i]
        rand_axis <- sample(pcoa_axes[ , i])
        new_axes  <- data.frame(real_axes, rand_axis)

        D_dimen_rand <- dist(new_axes)

        Co_rank_rand <- coRanking::coranking(sp_trdist, D_dimen_rand,
                                             input_Xi = "dist")
        NX_rand      <- coRanking::R_NX(Co_rank_rand)
        AUCrand      <- coRanking::AUC_ln_K(NX_rand)

        return(AUCrand)

      }, mc.cores = cores))
    }

    res <- list(dat, rand_table)
  })


  AUC_SES_pval <- do.call(cbind, pbmcapply::pbmclapply(1:nbdim, function(k) {

    AUC_obs <- computeAUCandNullmod[[k]][[1]][ , 2]

    if (k == 1) {

      SES  <- NA
      pval <- NA

    } else {

      SES <- (computeAUCandNullmod[[k]][[1]][ , 2] -
              mean(computeAUCandNullmod[[k]][[2]])) /
              sd(computeAUCandNullmod[[k]][[2]])

      # p-value = proportion of null value inferior to obs Beta (+1)
      pval <- length(
        which(computeAUCandNullmod[[k]][[2]] >
              computeAUCandNullmod[[k]][[1]]$"AUC")) /
        (length(computeAUCandNullmod[[k]][[2]]) + 1)
    }

    res_final <- rbind(AUC_obs, SES, pval)
    return(res_final)

  }, mc.cores = cores))


  # Detect inflexion point ----

  axes_table <- data.frame(Axe = 1:nbdim, AUC = AUC_SES_pval[1, ])
  ebow_meth  <- elbow(axes_table, xmin = 0, ymin = 0)


  # Compute the choosen quality metric for each functional space ----

  if (!metric_scaled) {

    # Compute deviance between distance in each functional space and
    # trait-based distance

    dev_distsp <- data.frame(
      distsp_df[ , c("sp.x", "sp.y")],
      distsp_df[ , fspaces_nm] - distsp_df[ , "distsp_df"]
    )


    # Compute absolute deviance ----

    abs_dev_distsp <- data.frame(
      dev_distsp[ , c("sp.x", "sp.y")],
      abs(dev_distsp[, fspaces_nm])
    )

    # mean absolute deviation:

    qual_metric_mad <- apply(abs_dev_distsp[ , fspaces_nm], 2, mean)

  } else {

    # Compute deviance between distance in each functional space and
    # trait-based distance:

    scdistsp <- apply(distsp_df[ , fspaces_nm], 2, function(x) {
      x / max(x) * max(distsp_df[ , "distsp_df"])
    })


    # Compute deviance ----

    dev_scdistsp <- data.frame(
      distsp_df[ , c("sp.x", "sp.y")],
      scdistsp[, fspaces_nm] - distsp_df[, "distsp_df"]
    )

    # compute squared deviance:
    # sqr_dev_scdistsp <- data.frame (dev_scdistsp[, c("sp.x", "sp.y")],
    # (dev_scdistsp[, fspaces_nm])^2)
    # compute mean squared deviation:
    # qual_metric_msd <- apply(sqr_dev_scdistsp[, fspaces_nm], 2, mean)


    # Compute absolute deviance ----

    abs_dev_scdistsp <- data.frame(
      dev_scdistsp[ , c("sp.x", "sp.y")], abs(dev_scdistsp[ , fspaces_nm])
    )


    # Compute mean absolute deviation ----
    qual_metric_mad <- apply(abs_dev_scdistsp[ , fspaces_nm], 2, mean)
  }


  # Return results ----

  res <- data.frame(
    dim                  = 1:nbdim,
    # quality_fspaces_msd  = round(qual_metric_msd,3),
    MAD                  = round(qual_metric_mad,3),
    AUC                  = ebow_meth$"AUC",
    benefit_AUCebow_meth = ebow_meth$"benefits",
    SelectedbyAUCelbow   = ebow_meth$"SelectedorNot",
    SES                  = AUC_SES_pval[2, ],
    pvalue               = AUC_SES_pval[3, ]
  )

  return(res)
}
