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
#' @param nclust Numeric. Indicating the number of nodes per hierarchy.
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
# 
 



getOptimalCentroids <-
  function (x, iter.max,algorithm, nclust,distance_metric,error_metric,quant.err){
    options(warn = -1)
    calculate_euclidean_distance_for_each_cluster <- function(x){
      sqrt(rowSums(scale(x,center = TRUE,scale = FALSE)^2))/ncol(x)
    }
    
    calculate_manhattan_distance_for_each_cluster <- function(x){
      rowSums(abs(scale(x,center = TRUE,scale = FALSE)))/ncol(x)
    }
    
    ## for distance metrics i.e; manhattan or eucleadian
    if(distance_metric == "L1_Norm"){
      function_to_calculate_distance_metric <- calculate_manhattan_distance_for_each_cluster
    } else if(distance_metric == "L2_Norm"){
      function_to_calculate_distance_metric <- calculate_euclidean_distance_for_each_cluster
    } else{
      stop('distance_metric must be L1_Norm (Manhattan), L2_Norm(Euclidean) or custom distance function')
    }
    
    ## for error metric i.e; ,mean or max
    if(error_metric %in% c("mean","max")){
      function_to_calculate_error_metric <- error_metric
    } else{
      stop('error_metric must be max,mean or custom function')
    }
    
    
    
    # Start with splitting data into three clusters
    getCentroids_for_opti <-
      function (x, kout, nclust,distance_metric,error_metric){
        calculate_error<- centl <- list()
        calculate_error <- x %>% dplyr::group_by(kout$clust) %>% dplyr::do(err = function_to_calculate_distance_metric(.))
        centl <-lapply(calculate_error$err,function_to_calculate_error_metric)
        #return centroids
        return(centl)
        
      }

    
    nclust_iter <- 3
    outkinit <- list(centers = numeric(), values = logical() , nsize = numeric())
    quantok <- rep(TRUE, nclust)
    while (nclust_iter <= nclust & nrow(x) > nclust_iter & (sum(quantok,na.rm = TRUE) > 0)) {
      resplt <- list()
      #outkinit will have centroids and datapoints and size of the cluster
      set.seed(100)
      kout<-stats::kmeans(x, nclust_iter, iter.max=10^5, algorithm=algorithm)
      outkinit[[1]]<- getCentroids_for_opti(x,kout, nclust_iter,function_to_calculate_distance_metric,function_to_calculate_error_metric)
      #flag to check for quantization error
      resplt <- unlist(outkinit$centers) > quant.err
      quantok <- unlist(resplt)
      nclust_iter <- nclust_iter + 1      
    }
    clusts <- nclust_iter-1
    outkinit[[2]] <- c(1:clusts) %>% purrr::map(~x[kout$cluster==.x,])
    outkinit[[3]] <- as.list(kout$size)
    outkinit[["centroid_val"]] <- kout$centers
    
    dummy_iter = nclust - nclust_iter + 1
    
    if (dummy_iter !=0){
      outkinit[["centers"]] <- c(outkinit[["centers"]],as.list(rep(NA,dummy_iter)))
      outkinit[["values"]] <- c(outkinit[["values"]],as.list(rep(NA,dummy_iter)))
      outkinit[["nsize"]] <- c(outkinit[["nsize"]],as.list(rep(0,dummy_iter)))
      temprow <- matrix(c(rep.int(NA,length(outkinit[["centroid_val"]][1,]))),nrow=length(nclust_iter:nclust),ncol=length(outkinit[["centroid_val"]][1,]))
      outkinit[["centroid_val"]] <- rbind(outkinit[["centroid_val"]],temprow)
      
    }
    # return centroids, datapoints and size of each cluster
    return(outkinit)
    
  }
