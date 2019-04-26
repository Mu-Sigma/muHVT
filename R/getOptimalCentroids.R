#' getOptimalCentroids
#'
#' Get Optimal Centroids
#'
#' The raw data is first scaled and this scaled data is supplied as input to
#' the vector quantization algorithm. Vector quantization technique uses a
#' parameter called quantization error. This parameter acts as a threshold and
#' determines the number of levels in the hierarchy. It means that, if there
#' are 'n' number of levels in the hierarchy, then all the clusters formed till
#' this level will have quantization error equal or greater than the threshold
#' quantization error. The user can define the number of clusters in the first
#' level of hierarchy and then each cluster in first level is sub-divided into
#' the same number of clusters as there are in the first level. This process
#' continues and each group is divided into smaller clusters as long as the
#' threshold quantization error is met. The output of this technique will be
#' hierarchically arranged vector quantized data.
#'
#' @param x Data Frame. A dataframe of multivariate data. Each row corresponds to an
#' observation, and each column corresponds to a variable. Missing values are
#' not accepted.
#' @param nclust Numeric. Indicating the maximum number of nodes per hierarchy.
#' @param depth Numeric. Indicating the hierarchy depth (or) the depth of the
#' tree (1 = no hierarchy, 2 = 2 levels, etc..)
#' @param quant.err Numeric. The quantization error for the algorithm.
#' @param algorithm String. The type of algorithm used for quantization.
#' Available algorithms are Hartigan and Wong, "Lloyd", "Forgy", "MacQueen".
#' (default is "Hartigan-Wong")
#' @param distance_metric character. The distance metric can be 'L1_Norm" or "L2_Norm". L1_Norm is selected by default.
#' @param error_metric character. The error metric can be "mean" or "max". mean is selected by default
#' @return \item{clusters}{ List. A list showing each ID assigned to a cluster.
#' } \item{nodes.clust}{ List. A list corresponding to nodes' details. }
#' \item{idnodes}{ List. A list of ID and segments similar to
#' \code{nodes.clust} with additional columns for nodes ID. }
#' \item{error.quant}{ List. A list of quantization error for all levels and
#' nodes. } \item{plt.clust}{ List. A list of logical values indicating if the
#' quantization error was met. } \item{summary}{ Summary. Output table with
#' summary. }
#' @author Sangeet Moy Das <sangeet.das@@mu-sigma.com>


getOptimalCentroids <-
  function (x, iter.max,algorithm, nclust,distance_metric = c("L1_Norm","L2_Norm"),error_metric = c("mean","max"),quant.err){
    requireNamespace("dplyr")
    
    # Start with splitting data into three clusters
    nclust_iter <- 3
    outkinit <- list(centers = numeric(), values = logical() , nsize = numeric())
    quantok <- rep(T, nclust)
    
    while (nclust_iter <= nclust & nrow(x) > nclust_iter & (sum(quantok,na.rm = T) > 0)) {
      # print(nclust_iter)
      resplt <- list()
      # if(nrow(x)<3){
      #   nclust_iter = 0
      #   break
      # }
       
      #outkinit will have centroids and datapoints and size of the cluster
      set.seed(100)
      outkinit <- getCentroids(x, kout = stats::kmeans(x, nclust_iter, iter.max=100, algorithm=algorithm), nclust_iter,distance_metric=distance_metric,error_metric=error_metric)
      
      #flag to check for quantization error
      resplt <- unlist(outkinit$cent) > quant.err
      quantok <- unlist(resplt)
      # print(max( unlist(outkinit$cent) ))
      # print(resplt)
      # if(nclust<=nclust_iter | NROW(x) - 1 <= nclust_iter | sum(quantok) == 0 ){
      #   break
      # }
      nclust_iter <- nclust_iter + 1      
    }
    # print(paste0("Optimal cluster: ",nclust_iter - 1))
    if(nrow(x) <= 3) {
      dummy_iter = nclust
    } else {
      dummy_iter = nclust - nclust_iter + 1  
    }
    outkinit[["centers"]] <- c(outkinit[["centers"]],as.list(rep(NA,dummy_iter)))
    outkinit[["values"]] <- c(outkinit[["values"]],as.list(rep(NA,dummy_iter)))
    outkinit[["nsize"]] <- c(outkinit[["nsize"]],as.list(rep(0,dummy_iter)))
    
    #return centroids, datapoints and size of each cluster
    return(outkinit)
  }