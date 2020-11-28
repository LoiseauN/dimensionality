Bartonova2016 <- read.csv2(
  file      = here::here("data", "Bartonova2016.csv"),
  header    = TRUE,
  row.names = 1
)

# Trait preparation and category ----

for (i in 1:ncol(Bartonova2016)) {

  Bartonova2016[ , i] <- as.numeric(as.character(Bartonova2016[ , i]))
}

Bartonova2016_cat <- data.frame(
  trait_name       = colnames(head(Bartonova2016)),
  trait_type       = rep(NA, ncol(Bartonova2016)),
  fuzzy_name       = rep(NA, ncol(Bartonova2016)),
  stringsAsFactors = FALSE
)

cor_Bartonova2016 <- matrix(0, ncol(Bartonova2016), ncol(Bartonova2016))

for (i in 1:ncol(Bartonova2016)) {

  for (j in i:ncol(Bartonova2016)) {

    cor_Bartonova2016[i, j] <- stats::cor(
      x      = rank(Bartonova2016[ , i]),
      y      = rank(Bartonova2016[ , j]),
      method = "kendall"
    )
  }
}

cor_Bartonova2016[lower.tri(cor_Bartonova2016)] <- t(
  cor_Bartonova2016)[lower.tri(cor_Bartonova2016)]

diag(cor_Bartonova2016) <- NA

cor_Bartonova2016 <- data.frame(
  mean_cor = mean(abs(cor_Bartonova2016), na.rm = TRUE),
  sd_cor   = sd(abs(cor_Bartonova2016), na.rm = TRUE),
  max_cor  = max(abs(cor_Bartonova2016), na.rm = TRUE),
  min_cor  = min(abs(cor_Bartonova2016), na.rm = TRUE)
)

save(cor_Bartonova2016, file = here::here("results", "cor_Bartonova2016.RData"))


# Depletion ----

Bartonova2016_miss <- compute_missing_trait_distance(
  trait_df          = Bartonova2016,
  trait_category_df = Bartonova2016_cat,
  percent_missing   = percent_list,
  max_comb          = 100,
  n_perm            = 100,
  cores             = 3,
  classical_gower   = TRUE,
  pos               = 100
)

save(Bartonova2016_miss,file = here::here("results", "Bartonova2016_miss.RData"))


# Plot  ----

print(depletion_plot(Bartonova2016_miss, version = 1))


# Table Result Depletion  ----

Bartonova2016_miss_final <- Bartonova2016_miss
Bartonova2016_miss_final <- stats::aggregate(
  x   = Bartonova2016_miss_final,
  by  = list(miss_percent = Bartonova2016_miss_final$"miss_percent"),
  FUN = mean
)
Bartonova2016_miss_final <- Bartonova2016_miss_final[ , -2]


# Dimension  ----

Bartonova2016_dim <- dimension_funct(
  trait_df          = Bartonova2016,
  trait_category_df = Bartonova2016_cat,
  dim_pcoa          = 20,
  metric_scaled     = TRUE,
  classical_gower   = TRUE,
  rep               = 999,
  cores             = 3
)

save(Bartonova2016_dim, file = here::here("results", "Bartonova2016_dim.RData"))


# Plot  ----

print(dim_plot(Bartonova2016_dim, dim_pcoa = 20))


# Singleton  ----

Bartonova2016_dist <- as.matrix(cluster::daisy(Bartonova2016, metric = "gower"))
Bartonova2016_dist <- stats::as.dist(Bartonova2016_dist)

Bartonova2016_pcoa <- ape::pcoa(Bartonova2016_dist)

save(Bartonova2016_pcoa, file = here::here("results", "Bartonova2016_pcoa.RData"))

Bartonova2016_sigle <- DPC(Bartonova2016_dist,
  metric         = "predefined",
  radius         = "automatic",
  density_filter = "global_threshold",
  rho_threshold  = 1
)

save(Bartonova2016_sigle, file = here::here("results", "Bartonova2016_sigle.RData"))


# Table result Sigleton ----

Bartonova2016_sigle <- data.frame(
  rho          = Bartonova2016_sigle$"rho",
  cluster_core = Bartonova2016_sigle$"cluster_core",
  cluster      = Bartonova2016_sigle$"cluster"
)


# All results ----

Bartonova2016_summary_res <- synth_results(
  trait_df      = Bartonova2016,
  miss_final_df = Bartonova2016_miss_final,
  dim_df        = Bartonova2016_dim,
  single_df     = Bartonova2016_sigle
)

Bartonova2016_res <- list(
  summary_res = Bartonova2016_summary_res,
  depletion   = Bartonova2016_miss_final,
  dimension   = Bartonova2016_dim,
  cluster     = Bartonova2016_sigle
)

save(Bartonova2016_res, file = here::here("results", "Bartonova2016_res.RData"))
