#' Summarize Results
#'
#' __ADD DESCRIPTION__
#'
#' @param trait_df __ADD DESCRIPTION__
#' @param single_df __ADD DESCRIPTION__
#' @param miss_final_df __ADD DESCRIPTION__
#' @param dim_df __ADD DESCRIPTION__
#'
#' @return __ADD DESCRIPTION__
#'
#' @export

synth_results <- function(trait_df, single_df, miss_final_df, dim_df) {


  # S and nb trait ----

  S           <- nrow(trait_df)
  Nb_trait    <- ncol(trait_df)
  NA_perc     <- (sum(is.na(trait_df)) / length(is.na(trait_df))) * 100
  quanti_perc <- (sum(do.call(c, lapply(1:ncol(trait_df), function(i) {
    is.numeric(trait_df[ , i])
  }))) / ncol(trait_df)) * 100


  # Dimension results ----

  Nb_dim_AUC_0.5   <- dim_df$"dim"[tail(which(dim_df$"AUC" < 0.5), n = 1) + 1]

  if (is.integer(Nb_dim_AUC_0.5) && length(Nb_dim_AUC_0.5) == 0) {
    Nb_dim_AUC_0.5 <- NA
  }

  Nb_dim_AUC_0.7    <- dim_df$"dim"[tail(which(dim_df$"AUC" < 0.7), n = 1) + 1]
  Nb_dim_AUC_elbow  <- dim_df$"dim"[tail(which(dim_df$"SelectedbyAUCelbow" =="Selected"), n = 1)]
  Nb_dim_AUC_SES    <- length(which(dim_df$"SES" > 1.96)) + 1
  Nb_dim_AUC_pvalue <- length(which(dim_df$"pvalue" < 0.05)) + 1


  # Depletion results ----

  Nb_miss_AUC_0.5  <- miss_final_df$"miss_percent"[tail(which(miss_final_df$"AUC" > 0.5), n = 1)]
  if (is_empty(Nb_miss_AUC_0.5) == TRUE) Nb_miss_AUC_0.5 <- NA

  Nb_miss_AUC_0.7  <- miss_final_df$"miss_percent"[tail(which(miss_final_df$"AUC" > 0.7), n = 1)]
  if (is_empty(Nb_miss_AUC_0.7) == TRUE) Nb_miss_AUC_0.7 <- NA

  Perclost_AUC_50perctrait <- (1 - miss_final_df[miss_final_df$"miss_percent" == 0.5, ]$"AUC") * 100
  Perclost_AUC_20perctrait <- (1 - miss_final_df[miss_final_df$"miss_percent" == 0.2, ]$"AUC") * 100

  Nb_miss_Mantel_0.5 <- miss_final_df$"miss_percent"[tail(which(miss_final_df$"mantel_r" > 0.5), n = 1)]
  Nb_miss_Mantel_0.7 <- miss_final_df$"miss_percent"[tail(which(miss_final_df$"mantel_r" > 0.7), n = 1)]


  # Singleton results ----

  Nb_single  <- length(which(single_df$"cluster_core" ==0))
  Nb_cluster <- length(unique(single_df$"cluster_core"[single_df$"cluster_core" > 0]))

  res <- data.frame(
    S                               = S,
    Nb_trait                        = Nb_trait,
    NA_perc                         = NA_perc,
    quanti_perc                     = quanti_perc,
    Nb_dim_AUC_0.5                  = Nb_dim_AUC_0.5,
    Nb_dim_AUC_0.7                  = Nb_dim_AUC_0.7,
    Nb_dim_AUC_elbow                = Nb_dim_AUC_elbow,
    Nb_dim_AUC_SES                  = Nb_dim_AUC_SES,
    Nb_dim_AUC_pvalue               = Nb_dim_AUC_pvalue,
    Perc_miss_AUC_0.5               = as.numeric(as.character(Nb_miss_AUC_0.5)) * 100,
    Perc_miss_AUC_0.7               = as.numeric(as.character(Nb_miss_AUC_0.7)) * 100,
    Perc_miss_Mantel_0.5            = as.numeric(as.character(Nb_miss_Mantel_0.5)) * 100,
    Perc_miss_Mantel_0.7            = as.numeric(as.character(Nb_miss_Mantel_0.7)) * 100,
    Percentage_lostAUC_depleted0.5  = Perclost_AUC_50perctrait,
    Percentage_lostAUC_depleted0.20 = Perclost_AUC_20perctrait,
    Nb_single                       = Nb_single,
    Nb_cluster                      = Nb_cluster
  )

  return(res)
}
