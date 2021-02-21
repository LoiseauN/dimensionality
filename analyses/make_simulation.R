#' @header *********************************************************************
#' @dataset Simulation to test influence of Nbtrait and S on dimensionlity
#' @dataset Suggested by Referee 1 
#' @header *********************************************************************


#Function to generate correlated traits following uniform distribution
runifcor <- function(x, rho){
  hw <- function(r){ 
    tmp <- (3-sqrt(1+8*abs(r)))/4 
    return(tmp * sign(r))
  } 
  y <- (x*sign(rho) + runif(length(x),-hw(abs(rho)),hw(rho*sign(rho)))) %% 1 
  return(y)
}

# number of traits (columns)
ntraits <- 10

# number of species
nsp <- 1000

# reproductibility
set.seed(666)


#UNIFORM DISTRIBUTION
# generate trait matrix without correlated traits
traits <- matrix(runif(nsp*ntraits), nrow = nsp, ncol = ntraits)
rownames(traits) <- paste0("sp", 1:nsp)
colnames(traits) <- paste0("tr", 1:ntraits)

# generate trait matrix with correlated traits
traits_cor <- traits
for (i in 2:ncol(traits_cor)){
  traits_cor[,i] <-  runifcor(x = traits[,1], rho = sample(seq(0.3,0.7,0.05),1))
}

# generate trait matrix without correlated traits but increase number of traits
traits_20 <- data.frame(traits,matrix(runif(nsp*ntraits), nrow = nsp, ncol = ntraits))
rownames(traits_20) <- paste0("sp", 1:nsp)
colnames(traits_20) <- paste0("tr", 1:(ntraits*2))

#To vizualize correlation between traits
#PerformanceAnalytics::chart.Correlation(traits_cor)
tr_cat <- data.frame(
  trait_name        = colnames(traits) ,
  trait_type        = rep(NA,ncol(traits)),
  fuzzy_name        = rep(NA,ncol(traits)),
  stringsAsFactors  = FALSE) 

tr_cat_20 <- data.frame(
  trait_name        = colnames(traits_20) ,
  trait_type        = rep(NA,ncol(traits_20)),
  fuzzy_name        = rep(NA,ncol(traits_20)),
  stringsAsFactors  = FALSE) 


cutS <- c(seq(3,50,5),seq(50,200,10),seq(201,1000,100))

#UNIFORM TEST 
res_traits <- dim_simulation(traits,tr_cat,threshold = seq(0.5, 0.9, 0.1),core=30 ,cut = cutS)

res_traits_cor <- dim_simulation(traits_cor,tr_cat,threshold = seq(0.5, 0.9, 0.1),core=30 ,cut = cutS)

res_traits_20 <- dim_simulation(traits_20,tr_cat_20,threshold = seq(0.5, 0.9, 0.1),core=30 ,cut = cutS)

res_simulation <- list(res_traits,
                       res_traits_cor,
                       res_traits_20)

res <- data.frame(nsp = c(res_simulation[[1]]$nsp,res_simulation[[2]]$nsp,res_simulation[[3]]$nsp),
                  dimension = c(res_simulation[[1]]$naxes,res_simulation[[2]]$naxes,res_simulation[[3]]$naxes),
                  threshold = c(res_simulation[[1]]$threshold,res_simulation[[2]]$threshold,res_simulation[[3]]$threshold),
                  datatrait = c(rep("10 uncorrelated traits",nrow(res_simulation[[1]])),
                                rep("10 correlated traits",nrow(res_simulation[[1]])),
                                rep("20 uncorrelated traits",nrow(res_simulation[[1]]))))


ggplot(subset(res, res$threshold == 0.7), aes( x= nsp, y = dimension,color=datatrait)) +
  geom_point() +
  #stat_smooth() +
  theme_bw(base_size = 12)+
  ylim(0,20)+ggtitle("Simulation")

