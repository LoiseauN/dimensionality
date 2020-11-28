run_analysis <- function(dataset, folder = "outputs", name, plot = FALSE) {

  dataset_cat <- data.frame(
    trait_name       = colnames(head(dataset)),
    trait_type       = rep(NA, ncol(dataset)),
    fuzzy_name       = rep(NA, ncol(dataset)),
    stringsAsFactors = FALSE
  )


  # Correlation between Traits ----

  dataset_cor <- matrix(0, ncol(dataset), ncol(dataset))

  for (i in 1:ncol(dataset)) {

    for (j in i:ncol(dataset)) {

      dataset_cor[i, j] <- stats::cor(
        x      = rank(dataset[ , i]),
        y      = rank(dataset[ , j]),
        method = "kendall"
      )
    }
  }

  dataset_cor[lower.tri(dataset_cor)] <- t(
    dataset_cor)[lower.tri(dataset_cor)]

  diag(dataset_cor) <- NA

  dataset_cor <- data.frame(
    mean_cor = mean(abs(dataset_cor), na.rm = TRUE),
    sd_cor   = sd(abs(dataset_cor),   na.rm = TRUE),
    max_cor  = max(abs(dataset_cor),  na.rm = TRUE),
    min_cor  = min(abs(dataset_cor),  na.rm = TRUE)
  )

  saveRDS(dataset_cor, file = here::here(folder, paste0(name, "_cor.rds")))


  # Depletion ----

  dataset_miss <- compute_missing_trait_distance(
    trait_df          = dataset,
    trait_category_df = dataset_cat,
    percent_missing   = percent_list,
    max_comb          = 100,
    n_perm            = 100,
    cores             = 3,
    classical_gower   = TRUE,
    pos               = 100
  )

  saveRDS(dataset_miss, file = here::here(folder, paste0(name, "_miss.rds")))


  # Plot  ----

  if (plot) print(depletion_plot(dataset_miss, version = 1))


  # Table Result Depletion  ----

  dataset_miss_final <- dataset_miss
  dataset_miss_final <- stats::aggregate(
    x   = dataset_miss_final,
    by  = list(miss_percent = dataset_miss_final$"miss_percent"),
    FUN = mean
  )
  dataset_miss_final <- dataset_miss_final[ , -2]


  # Dimension  ----

  dataset_dim <- dimension_funct(
    trait_df          = dataset,
    trait_category_df = dataset_cat,
    dim_pcoa          = 20,
    metric_scaled     = TRUE,
    classical_gower   = TRUE,
    rep               = 999,
    cores             = 3
  )

  saveRDS(dataset_dim, file = here::here(folder, paste0(name, "_dim.rds")))


  # Plot  ----

  if (plot) print(dim_plot(dataset_dim, dim_pcoa = 20))


  # Singleton  ----

  dataset_dist <- as.matrix(cluster::daisy(dataset, metric = "gower"))
  dataset_dist <- stats::as.dist(dataset_dist)

  dataset_pcoa <- ape::pcoa(dataset_dist)

  assign(paste0(name, "_pcoa"), dataset_pcoa)
  saveRDS(eval(parse(text = paste0(name, "_pcoa"))),
          file = here::here(folder, paste0(name, "_pcoa.rds")))

  dataset_single <- DPC(dataset_dist,
    metric         = "predefined",
    radius         = "automatic",
    density_filter = "global_threshold",
    rho_threshold  = 1
  )

  saveRDS(dataset_single, here::here(folder, paste0(name, "_single.rds")))

  # Table result Sigleton ----

  dataset_single <- data.frame(
    rho          = dataset_single$"rho",
    cluster_core = dataset_single$"cluster_core",
    cluster      = dataset_single$"cluster"
  )


  # All results ----

  dataset_summary_res <- synth_results(
    trait_df      = dataset,
    miss_final_df = dataset_miss_final,
    dim_df        = dataset_dim,
    single_df     = dataset_single
  )

  dataset_res <- list(
    summary_res = dataset_summary_res,
    depletion   = dataset_miss_final,
    dimension   = dataset_dim,
    cluster     = dataset_single
  )

  saveRDS(dataset_res, here::here(folder, paste0(name, "_res.rds")))
}
