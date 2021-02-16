#' Identify Isolated Points and Compute Density Peak Clustering
#'
#' Implementation of the density peak clustering method. The algorithm makes the 
#' following steps:
#' 
#' 1. The N x N density matrix D is computed from the N x M coordinate matrix, 
#'   where N is the number of points and m the number of coordinates;
#'   alternatively, D is directly taken as input.
#'
#' 2. For each point i, the density rho_i is computed. The density is defined as 
#'   the number of neighbors, i.e. the number of points j at a distance lower 
#'   than a given radius radius `d_ij < radius`. Usually, the radius is fixed in 
#'   such a way that the average density corresponds to a small fraction, 1% to 
#'   2%, of the total number of points `rho = fraction x N`.
#'
#' 3. For each point i, one considers the distances `d_ij` between `i` and all 
#'   points j with a higher density `rho_j > rho_i`. The minimum distance in t
#'   his set is called `delta_i`.
#'
#' 4. The cluster centers are identified. By default, all points with 
#'   `delta_i > radius` are cluster centers (i.e. all points that have a density 
#'   higher than their neighbors'. Alternatively, one can inspect a 
#'   'decision graph' where delta_i is plotted against `rho_i`. Cluster centers 
#'   are selected as points with high `rho_i` and `delta_i`: based on inspection 
#'   of a decision graph, two thresholds `rhomin` and `deltamin` can be fixed, 
#'   such that all points with rho_i > rhomin and delta_i > deltamin are cluster 
#'   centers. `rhomin` and `deltamin` should be chosen in such a way that points 
#'   with `rho_i > rhomin` and `delta_i > deltamin` are well isolated in the 
#'   decision graph.
#'
#' 5. All non-isolated points (i.e. all points with rho_i > 0) are assigned to 
#'   one of the clusters. All points from which one can reach a center c through 
#'   a path of monotonically increasing density are considered as forming the 
#'   corresponding cluster. Thus, starting from a point i, one follows a path of 
#'   increasing density **by stepping recursively on the nearest point with a 
#'   higher density** until one of the centers is reached.
#'
#' 6. For each cluster c, one identifies the border points: these are points 
#'   which belong to c, but whose neighborhood contains points assigned to a 
#'   different cluster (i.e. within radius one finds points in c but also point 
#'   is in a different cluster c'). The border density is the maximum density of 
#'   border points. Whenever the border density is higher than the density of 
#'   the cluster center, c cannot be considered a robust cluster (it is a 
#'   density fluctuation rather than a true peak). In this case, c is merged 
#'   within the cluster c'. This merging procedure is repeated until all 
#'   clusters are robust.
#'
#' 7. Each cluster is 'cleaned' by retaining only points in the cluster core. 
#'   By default, only points with a density higher than the density of the 
#'   border density are considered as part of the core. Alternatively, one can 
#'   fix a global density threshold rhog and  exclude from the clusters all 
#'   points with rho_i < rhog (these are points with very low density that are 
#'   not considered a part of the clusters).
#'  
#' @param data if `metric = "predefined"` `X` is the N x N distance matrix. 
#' Otherwise `X` is the N x M coordinate matrix.
#' 
#' @param metric if `"predefined"` the distance matrix `D` is copied from `X`. 
#' Otherwise `D` is computed on the coordinates with the function dist with 
#' option `metric` (by default `metric = "euclidean"`).
#' 
#' @param radius this is the distance threshold to define neighborhood of a 
#' point (neighbors are points with distance < radius). If `radius = "automatic"` 
#' the radius is automatically fixed such that, on average, a neighborhood 
#' includes a fraction `f` of all points, where `f` is fixed by input parameter 
#' `fraction`.
#' 
#' @param fraction if `radius = "automatic"` the radius is automatically fixed 
#' such that, on average, a neighborhood includes this fraction of all points. 
#' By default `fraction = 0.01`, i.e. 1% of points are on average in the 
#' neighborhood.
#' 
#' @param mode if `mode = "interactive"` the decision graph is plotted and the 
#' user can enter the values of `rhomin` and `deltamin.` All points with 
#' `delta > deltamin` and `rho > rhomin` are considered cluster centers. 
#' `rhomin` and `deltamin` should be chosen in such a way that points with 
#' `rho_i > rhomin` and `delta_i > deltamin` are isolated in the decision graph.
#' If `mode = "automatic"` all points with `delta > radius` are cluster centers.
#' 
#' @param rhomin minimum value of rho to consider a point as cluster center. 
#' Unless `mode = "interactive"` this threshold is selected automatically by the 
#' algorithm.
#' 
#' @param deltamin minimum value of delta to consider a point as cluster center. 
#' Unless `mode = "interactive"` this threshold is selected automatically by the 
#' algorithm.
#' 
#' @param density_filter method to clean the clusters. If `saddle`, only the 
#' points in the cluster core (i.e. with a density above the maximum border 
#' density) are retained. If `global`, only points in with a density above a 
#' given fixed threshold (`rho_threshold`) are retained.
#' 
#' @param rho_threshold if `density_filter = "global"` only points in with a 
#' density above a given fixed threshold `rho_threshold` are retained. 
#' 
#' @param removezeroes if `TRUE`, it considers identical points (points at zero 
#' distance) as a single point in the computation of the density.
#'
#' @return A list with:
#' - `rho`: neighbor density of each point 
#' - `delta`: minimum distance of a point with higher density
#' - `cluster`: cluster to which each point is assigned, from 1 to #clusters 
#' (0 for isolated points)
#' - `centers`: list of cluster centers
#' - `cluster_core`: cluster to which each point is assigned, including only 
#' points in th cluster (0 for points in the cluster halos and isolated points)
#' - `border`: matrix of border densities (maximum density on the border between 
#' two clusters c and c').
#'
#' @references 
#' Rodriguez A & Laio A (2014) Clustering by fast search and find of density peaks.
#' _Science_, **344**, 1492-1496.\cr
#' d'Errico M et al. (2018) Automatic topography of high-dimensional data sets 
#' by non-parametric Density Peak Clustering. _arXiv preprint_, **1802**, 10549.
#'                        
#' @export
#' @importFrom igraph graph_from_adjacency_matrix components


DPC <- function(X,metric = "euclidean", radius='automatic', fraction=0.01, mode='automatic', rhomin='automatic',deltamin='automatic', density_filter='saddle', rho_threshold=0, removezeroes=FALSE){

  if(metric!='predefined') {
    D=as.matrix(dist(X, method = metric))
  }
  else {
    if(class(X)=="dist") { D=as.matrix(X) }
    if(class(X)=="list") { D=X[[1]] }
    if(class(X)=="data.frame") { D=as.matrix(X) }
    if(removezeroes==TRUE) { D=removezeroes(D) }
  }

  DG = find_decision_graph(D,radius,fraction)

  rho=as.matrix(DG$rho)
  delta=as.matrix(DG$delta)
  imin=DG$imin
  radius=DG$radius

  if(mode=='interactive') {

    p = ggplot() + geom_point(aes(x=rho,y=delta)) +  xlab("rho") + ylab("delta)")
    print(p)

    cat("insert minimum density: ")
    rhomin <-scan(n=1)
    cat("insert minimum delta: ")
    deltamin <- scan(n=1)
  }

  if(rhomin=='automatic') {
    rhomin=1
  }
  if(deltamin=='automatic') {
    deltamin=radius
  }

  C = find_clusters(rho,delta,rhomin,deltamin,imin)
  cluster=C$cluster
  centers=C$centers

  if(length(centers)>1) {
    border = find_saddle(D,rho,centers,cluster,radius)
    C=merge_clusters(border,rho,centers,cluster,radius)
    cluster=C$cluster
    centers=C$centers
    border=C$border
    cluster_core= clean_clusters(D,rho,centers,cluster,density_filter,radius,rho_threshold,border)
  }
  else {
    cluster_core=cluster
    border = max(rho)
  }

  results <- list('rho' = rho, 'delta' = delta, 'radius'=radius, 'cluster'=cluster,'centers'=centers,'cluster_core'=cluster_core,'border'=border)

}


find_decision_graph <- function(D,radius,fraction) {

  N = dim(D)[1]

  if(radius=='automatic') {
    sorted_dist = sort(D)
    radius=sorted_dist[floor(N*N*fraction+N)]
  }


  rho = colSums(D<=radius)-1
  idx = order(rho)

  D = D[idx,idx]
  D[lower.tri(D, diag = FALSE)]=0
  D=D+1.E7*lower.tri(matrix(1, nrow=N, ncol=N),diag= TRUE)

  delta1 = apply(D,1,min)
  imin1 = apply(D,1,which.min)

  delta = rep(0,N)

  deltamax=max(delta1[1:N-1])
  delta[idx]=delta1
  imin=1:N
  imin[idx]=idx[imin1]
  imin[idx[N]]=idx[N]
  delta[idx[N]]=1.1*deltamax

  DG <- list('rho' = rho, 'delta' = delta, 'imin' = imin, 'radius'=radius)

  return(DG)
}


find_clusters <- function(rho,delta,rhomin,deltamin,imin) {

  N = length(rho)

  centers = which(rho >= rhomin)
  centers = intersect(centers,which(delta >=deltamin))

  A = matrix(0,N,N)

  A[cbind(1:N,imin)]=1

  A[cbind(centers,imin[centers])]=0

  G=igraph::graph_from_adjacency_matrix(A)

  comp = igraph::components(G)

  cluster=comp$membership

  nclusters=length(centers)

  newcenters=rep(0,nclusters)

  for(c1 in 1:nclusters) {
    cl=cluster[centers[c1]]
    newcenters[cl]=centers[c1]
  }

  newcluster=rep(0,N)
  nclusters=max(cluster)

  sortind=order(order(-rho[newcenters]))
  newcenters=newcenters[order(-rho[newcenters])]

  for(c1 in 1:nclusters) {
    clus1=which(cluster==c1)
    newcluster[clus1]=sortind[c1]
  }

  C <- list('cluster' = newcluster, 'centers' = newcenters)

  return(C)
}

find_saddle = function(D,rho,centers,cluster,radius) {
  N = dim(rho)[1]
  nclusters=length(centers)

  border=matrix(0,nclusters,nclusters)
  for (c1 in 1:(nclusters-1)) {
    clus1=which(cluster==c1)
    for (c2 in (c1+1):nclusters) {
      clus2=which(cluster==c2)

      Dc1c2=D[clus1,clus2]

      near = (Dc1c2 <= radius)
      near = as.matrix(near)
      if(dim(near)[1]!=length(clus1)) {near=t(near)}
      b1=which(rowSums(near)>0)
      b2=which(colSums(near)>0)

      if(length(b1)>0) {
        rb1=max(rho[clus1[b1]])
      }
      else { rb1=0}
      if(length(b2)>0) {
        rb2=max(rho[clus2[b2]])
      }
      else {rb2=0}
      rb=max(rb1[1],rb2[1])
      border[c1,c2]=rb
      border[c2,c1]=border[c1,c2]
    }
    border[c1,c1]=max(rho[clus1])
  }
  border[nclusters,nclusters]=rho[centers[nclusters]]

  return(border)
}

merge_clusters=function(border,rho,centers,cluster,radius) {

  nclusters=length(centers)

  cond=TRUE
  while(cond==TRUE) {
    for (c1 in 1:(nclusters-1)) {
      clus1=which(cluster==c1)
      if(length(clus1)==0) { break }
      for (c2 in (c1+1):nclusters) {
        clus2=which(cluster==c2)
        if(length(clus2)==0) { break }
        if(border[c1,c2]>=border[c2,c2] || border[c1,c2]>=border[c1,c1]) {
          cluster[clus2]=c1
          gt=which(cluster>c2)
          cluster[gt]=cluster[gt]-1
          border[c1,]=apply(border[c(c1,c2),],2,max)
          border[,c1]=border[c1,]
          border=border[,-c2]
          border=border[-c2,]
          ctr=c(centers[c1],centers[c2])
          c12=c(c1,c2)
          delcen=c12[which.min(rho[ctr])]
          centers=centers[-delcen]
        }
      }
    }
    nclusters=length(centers)
    cond=FALSE
    for (c1 in 1:(nclusters-1)) {
      for (c2 in (c1+1):nclusters) {
        if(border[c1,c2]>border[c2,c2]) { cond=TRUE}
      }
    }

  }

  for (c1 in 1:nclusters) {
    if(length(which(cluster==c1))==1) {
      cluster[which(cluster==c1)]=0
    }
  }

  C <- list('cluster' = cluster, 'centers' = centers, 'border'=border)
  return(C)

}


clean_clusters= function(D,rho,centers,cluster,density_filter,radius,rho_threshold,border) {
  N = dim(rho)[1]
  nclusters=length(centers)

  cluster_core=rep(0,N)

  if (density_filter=='saddle') {
    for (c1 in 1:nclusters) {
      clus1 = which(cluster==c1)
      maxborder = max(border[c1,])
      cluster_core[intersect(clus1,which(rho>maxborder))]=c1
    }
  }


  if(density_filter=='global_threshold') {
    keep=which(rho>= rho_threshold)
    cluster_core[keep]=cluster[keep]
  }

  cuts_core<-cut(cluster_core, breaks=seq(0,max(cluster_core)))
  counts_core<-c(t(table(cuts_core)))
  singlets = which(counts_core<2)
  for(s in singlets) {
    cluster_core[which(cluster_core==s)]=0
    cluster_core[which(cluster_core>s)]=cluster_core[which(cluster_core>s)]-1
  }

  return(cluster_core)
}



removezeroes <- function(distances) {

  if(class(distances)=="dist") { distances=as.matrix(distances) }
  if(class(distances)=="list") { distances=distances[[1]] }
  if(class(distances)=="data.frame") { distances=as.matrix(distances) }


  distances[is.nan(distances)] <- 1

  d=dim(distances)


  ll <- c()
  isEqual <- rep(0, d[1])
  newidx <- rep(0, d[1])


  for (i in 1:(d[1]-1)){
    #print(i)
    for (j in (i+1):d[2]){
      if(distances[i,j]==0) {
        ll <- c(ll,j)
        isEqual[j]=i
      }
    }
  }

  if(length(ll)>0) {
    distances = distances[,-ll]
    distances = distances[-ll,]
  }

  c=0

  for (i in 1:(d[1])){
    if(isEqual[i]==0) {
      c=c+1
      newidx[i]=c
    }
    else {
      newidx[i]=newidx[isEqual[i]]
    }
  }

  return(distances)

}
