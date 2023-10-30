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
#' @param n_cells Numeric. Indicating the number of nodes per hierarchy.
#' @param quant.err Numeric. The quantization error for the algorithm.
#' @param algorithm String. The type of algorithm used for quantization.
#' Available algorithms are Hartigan and Wong, "Lloyd", "Forgy", "MacQueen".
#' (default is "Hartigan-Wong")
#' @param distance_metric Character. The distance metric to calculate inter point distance. It can be 'L1_Norm" or "L2_Norm". L1_Norm is selected by default.
#' @param function_to_calculate_distance_metric Function. The function is to find 'L1_Norm" or "L2_Norm" distances. L1_Norm is selected by default.
#' @param function_to_calculate_error_metric Character. The error metric can be "mean" or "max". mean is selected by default
#' @param quant_method Character. The quant_method can be "kmeans" or "kmedoids". kmeans is selected by default
#' @return \item{values}{ List. A list showing observations assigned to a cluster.}
#' \item{maxQE}{ List. A list corresponding to maximum QE values for each cell. }
#' \item{meanQE}{ List. A list corresponding to mean QE values for each cell. }
#' \item{centers}{ List. A list of quantization error for all levels and nodes. } 
#' \item{nsize}{ List. A list corresponding to number of observations in respective groups. } 
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>
#' @keywords internal

getOptimalCentroids <-
  function (x, 
            iter.max, 
            algorithm, 
            n_cells, 
            function_to_calculate_distance_metric, 
            function_to_calculate_error_metric=c("mean","max"), 
            quant.err,
            distance_metric = "L1_Norm",
            quant_method=c("kmeans","kmedoids"),...
  ){
    # browser()
    if(quant_method == "kmeans"){
      
      # Start with splitting data into three clusters
      nclust_iter <- 3
      outkinit <- list(centers = numeric(),maxQE = numeric(),meanQE = numeric(), values = logical() , nsize = numeric())
      quantok <- rep(T, n_cells)
      
      # Check if 3 <= No of clusters AND No of cells in a cluster > 3 AND flag to check QE for all clusters
      while(nclust_iter <= n_cells & nrow(x) > nclust_iter & (sum(quantok,na.rm = TRUE) > 0)) {
        resplt <- list()
        #outkinit will have centroids and datapoints and size of the cluster
        set.seed(100)
        kout<-stats::kmeans(x, nclust_iter, iter.max=10^5, algorithm=algorithm)
        result <- getCentroids_for_opti(x,kout, nclust_iter,function_to_calculate_distance_metric,function_to_calculate_error_metric)
        
        outkinit[[1]] <- result[[1]]
        outkinit[[2]] <- result[[2]]
        outkinit[[3]] <- result[[3]]
        #flag to check for quantization error
        resplt <- unlist(outkinit$centers) > quant.err
        quantok <- unlist(resplt)
        nclust_iter <- nclust_iter + 1      
      }
      clusts <- nclust_iter-1
      outkinit[[4]] <- c(1:clusts) %>% purrr::map(~x[kout$cluster==.x,])
      outkinit[[5]] <- as.list(kout$size)
      outkinit[["centroid_val"]] <- kout$centers
      
      dummy_iter = n_cells - nclust_iter + 1
      
      if(dummy_iter !=0){
        outkinit[["centers"]] <- c(outkinit[["centers"]],as.list(rep(NA,dummy_iter)))
        outkinit[["maxQE"]] <- c(outkinit[["maxQE"]],as.list(rep(NA,dummy_iter)))
        outkinit[["meanQE"]] <- c(outkinit[["meanQE"]],as.list(rep(NA,dummy_iter)))
        outkinit[["values"]] <- c(outkinit[["values"]],as.list(rep(NA,dummy_iter)))
        outkinit[["nsize"]] <- c(outkinit[["nsize"]],as.list(rep(0,dummy_iter)))
        temprow <- matrix(c(rep.int(NA,length(outkinit[["centroid_val"]][1,]))),nrow=length(nclust_iter:n_cells),ncol=length(outkinit[["centroid_val"]][1,]))
        outkinit[["centroid_val"]] <- rbind(outkinit[["centroid_val"]],temprow)
      }
      # return centroids, datapoints and size of each cluster
      return(outkinit)
    }else if(quant_method == "kmedoids"){
      # Start with splitting data into three clusters
      # browser()
      nclust_iter <- 3
      outkinit <- list(centers = numeric(),maxQE = numeric(),meanQE = numeric(), values = logical() , nsize = numeric())
      quantok <- rep(T, n_cells)
      if(distance_metric == "L1_Norm") {
        distance_metric = "manhattan"
      }else if(distance_metric == "L2_Norm") {
        distance_metric = "euclidean"
      }
      kout=list()
      while(nclust_iter <= n_cells & nrow(x) > nclust_iter & (sum(quantok,na.rm = TRUE) > 0)) {
        resplt <- list()
        #outkinit will have centroids and datapoints and size of the cluster
        set.seed(100)
        
        kmedoids_model <-
          cluster::pam(
            x = cluster::daisy(x, metric = distance_metric),
            k = nclust_iter,
            diss = TRUE,
            keep.data = FALSE
          )
        kout$cluster=kmedoids_model[["clustering"]]
        result<- getCentroids_for_opti(x,kout, nclust_iter,function_to_calculate_distance_metric,function_to_calculate_error_metric)
        outkinit[[1]] <-result[[1]]
        outkinit[[2]]<- result[[2]]
        outkinit[[3]]<- result[[3]]
        
        
        #flag to check for quantization error
        resplt <- unlist(outkinit$centers) > quant.err
        quantok <- unlist(resplt)
        nclust_iter <- nclust_iter + 1      
      }
      kout$size=as.numeric(table(kmedoids_model[["clustering"]]))
      kout$centers=as.matrix(x[kmedoids_model[["medoids"]],])
      clusts <- nclust_iter-1
      outkinit[[4]] <- c(1:clusts) %>% purrr::map(~x[kout$cluster==.x,])
      outkinit[[5]] <- as.list(kout$size)
      outkinit[["centroid_val"]] <- kout$centers
      
      dummy_iter = n_cells - nclust_iter + 1
      
      if(dummy_iter !=0){
        outkinit[["centers"]] <- c(outkinit[["centers"]],as.list(rep(NA,dummy_iter)))
        outkinit[["maxQE"]] <- c(outkinit[["maxQE"]],as.list(rep(NA,dummy_iter)))
        outkinit[["meanQE"]] <- c(outkinit[["meanQE"]],as.list(rep(NA,dummy_iter)))
        outkinit[["values"]] <- c(outkinit[["values"]],as.list(rep(NA,dummy_iter)))
        outkinit[["nsize"]] <- c(outkinit[["nsize"]],as.list(rep(0,dummy_iter)))
        temprow <- matrix(c(rep.int(NA,length(outkinit[["centroid_val"]][1,]))),nrow=length(nclust_iter:n_cells),ncol=length(outkinit[["centroid_val"]][1,]))
        outkinit[["centroid_val"]] <- rbind(outkinit[["centroid_val"]],temprow)
        
      }
      # return centroids, datapoints and size of each cluster
      return(outkinit)
    }
    
  }
