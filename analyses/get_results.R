## Import Results ----

files <- list.files(path = here::here("outputs"), pattern = "_res.rds$",
                    full.names = TRUE)
list_res <- lapply(files, function(x) readRDS(x))

files <- list.files(path = here::here("outputs"), pattern = "_cor.rds$",
                    full.names = TRUE)
list_res_cor <- lapply(files, function(x) readRDS(x))

files <- list.files(path = here::here("outputs"), pattern = "_single.rds$",
                    full.names = TRUE)
list_res_sigle <- lapply(files, function(x) readRDS(x))

files <- list.files(path = here::here("outputs"), pattern = "_pcoa.rds$",
                    full.names = TRUE)
list_res_pcoa <- lapply(files, function(x) readRDS(x))

filenames <- list.files(path = here::here("outputs"), pattern = "_pcoa.rds$",
                        full.names = FALSE)


## Create Full Table ----

res_for_model <- t(data.frame(do.call(cbind, lapply(1:length(list_res), 
                                                    function(i) {
  
  res <- data.frame(list_res[[i]][[1]])
  
  # Add information of PCOA Axis and correlation between traits
  res_pcoa1 <- data.frame(list_res_pcoa[[i]]$values[1, 2])
  colnames(res_pcoa1) <- paste0("Relative_eig","_axe1")
  
  res_pcoa2 <- data.frame(list_res_pcoa[[i]]$values[2, 2])
  colnames(res_pcoa2) <- paste0("Relative_eig","_axe2")
  
  res_pcoa3 <- data.frame(list_res_pcoa[[i]]$values[3, 2])
  colnames(res_pcoa3) <- paste0("Relative_eig","_axe3") 
  
  res_cor<- data.frame(list_res_cor[[i]])
  
  # Add information on redundancy per cluster
  
  res_sigle <- t(data.frame(table(list_res_sigle[[i]]$cluster_core))[2:4, 2])
  colnames(res_sigle) <- c("NbS_Cluster1", "NbS_Cluster2", "NbS_Cluster3")
  res <- t(cbind(res, res_pcoa1, res_pcoa2, res_pcoa3, res_cor, res_sigle))
  
  return(res)
}))))

res_for_model <- as.data.frame(res_for_model)
rownames(res_for_model) <- gsub("_pcoa\\.rds", "", filenames)


res_for_model$Percentage_lostAUC_depleted0.5  <- res_for_model$Percentage_lostAUC_depleted0.5  / 100
res_for_model$Percentage_lostAUC_depleted0.20 <- res_for_model$Percentage_lostAUC_depleted0.20 / 100

colnames(res_for_model)[14] <- "rowAUClostwhen50percTraitdepleted"
colnames(res_for_model)[15] <- "rowAUClostwhen20percTraitdepleted"


save(res_for_model, file = here::here("outputs", "res_for_model.RData"))
