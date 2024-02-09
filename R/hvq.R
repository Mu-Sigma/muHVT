#' hvq
#' 
#' Hierarchical Vector Quantization
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
#' @param min_compression_perc Numeric. An integer indicating the minimum percent compression rate to
#' be achieved for the dataset
#' @param n_cells Numeric. Indicating the number of nodes per hierarchy.
#' @param depth Numeric. Indicating the hierarchy depth (or) the depth of the
#' tree (1 = no hierarchy, 2 = 2 levels, etc..)
#' @param quant.err Numeric. The quantization error for the algorithm.
#' @param algorithm String. The type of algorithm used for quantization.
#' Available algorithms are Hartigan and Wong, "Lloyd", "Forgy", "MacQueen".
#' (default is "Hartigan-Wong")
#' @param seed Numeric. Random Seed.
#' @param distance_metric character. The distance metric can be 'L1_Norm" or "L2_Norm". L1_Norm is selected by default.
#' @param error_metric character. The error metric can be "mean" or "max". mean is selected by default 
#' @param quant_method character. The quant_method can be "kmeans" or "kmedoids". kmeans is selected by default
#' @return \item{clusters}{ List. A list showing each ID assigned to a cluster.
#' } \item{nodes.clust}{ List. A list corresponding to nodes' details. }
#' \item{idnodes}{ List. A list of ID and segments similar to 
#' \code{nodes.clust} with additional columns for nodes ID. }
#' \item{error.quant}{ List. A list of quantization error for all levels and
#' nodes. } \item{plt.clust}{ List. A list of logical values indicating if the
#' quantization error was met. } \item{summary}{ Summary. Output table with
#' summary. }
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases
#' @examples
#' data("EuStockMarkets")
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#' DAX = EuStockMarkets[, "DAX"],
#' SMI = EuStockMarkets[, "SMI"],
#' CAC = EuStockMarkets[, "CAC"],
#' FTSE = EuStockMarkets[, "FTSE"])
#' dataset_hvt <- dataset[,-c(1)]
#' hvqOutput = hvq(dataset_hvt, n_cells = 5, depth = 2, quant.err = 0.2,
#' distance_metric='L1_Norm',error_metric='mean',quant_method="kmeans")
#' @export hvq
#' @include getCentroids.R
#' @include getOptimalCentroids.R
#' @keywords internal

hvq <-
  function (x,
            min_compression_perc = NA,
            n_cells = NA,
            depth = 3,
            quant.err = 10,
            seed = 300,
            algorithm = "Hartigan-Wong",
            distance_metric = c("L1_Norm", "L2_Norm"),
            error_metric = c("mean", "max"),
            quant_method=c("kmeans","kmedoids")
  ) {    
    requireNamespace("dplyr")
    # browser()  
    rescl <- list()
    resid <- list()
    resm <- list()
    maxqe <- list()
    meanqe <- list()
    resplt <- list()
    ztab3up <- list()
    ztab1 <- ztab2 <- ztabn <- NULL
    ztab11 <- ztab12 <- ztab13 <- NULL
    zdepth <- depth
    if(!is.na(n_cells)){
      quantinit <- rep(F, n_cells)
      ztab3upc<-matrix(0, nrow = ncol(x), n_cells)
      std<-matrix(0, nrow = ncol(x), n_cells)
    }
    set.seed(seed)
    # flog.info("Parameters are initialized")
    #outkinit will have centroids and datapoints and size of the cluster
    # outkinit <- getOptimalCentroids(x, iter.max=100, algorithm=algorithm, n_cells,distance_metric=distance_metric,error_metric=error_metric,quant.err=quant.err)
    colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=stats::sd, na.rm=na.rm)
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
      stop('distance_metric must be L1_Norm (Manhattan), L2_Norm (Euclidean) or a custom distance function')
    }
    
    ## for error metric i.e; mean or max
    if(error_metric %in% c("mean","max")){
      function_to_calculate_error_metric <- error_metric
    } else{
      stop('error_metric must be max,mean or custom function')
    }
    
    if(!is.na(min_compression_perc)){
      depth <- 1
      compress_percent <- 0
      n_cells_compress_perc = 2
      num_data_points <- rep(F, n_cells_compress_perc)
      n_cells_quant_err_list <- list()
      n_cells = NA

      while(n_cells_compress_perc < nrow(x) & compress_percent <= min_compression_perc){
      quantinit <- rep(F, n_cells_compress_perc)
      ztab3upc<-matrix(0, nrow = ncol(x), n_cells_compress_perc)
      std<-matrix(0, nrow = ncol(x), n_cells_compress_perc)
      # print(n_cells_compress_perc)
      outkinit <- getCentroids(x=x,
                               kout = stats::kmeans(x, n_cells_compress_perc, iter.max=10^5, algorithm=algorithm),
                               n_cells=n_cells_compress_perc,
                               function_to_calculate_distance_metric=function_to_calculate_distance_metric,
                               function_to_calculate_error_metric=function_to_calculate_error_metric,
                               distance_metric=distance_metric,
                               quant_method = quant_method)

      n_cells_quant_err_list <- unlist(outkinit$cent)
      noOfCells = sum(!is.na(n_cells_quant_err_list))
      noOfCellsBelowQuantizationError = sum(n_cells_quant_err_list < quant.err,na.rm = TRUE)
      compress_percent = round((noOfCellsBelowQuantizationError/noOfCells) * 100, 2) #percentOfCellsBelowQuantizationErrorThreshold
      n_cells_compress_perc <- n_cells_compress_perc + 1
    }
    n_cells_optimal <- n_cells_compress_perc - 1
    message(paste0("For the given dataset ",min_compression_perc,"% compression rate is achieved at n_cells : ", n_cells_optimal))
    }else{
      outkinit <- getCentroids(x=x,
                             kout = stats::kmeans(x, n_cells, iter.max=10^5, algorithm=algorithm),
                             n_cells=n_cells,
                             function_to_calculate_distance_metric=function_to_calculate_distance_metric,
                             function_to_calculate_error_metric=function_to_calculate_error_metric,
                             distance_metric=distance_metric,
                             quant_method = quant_method)
    
      n_cells_optimal <- n_cells
    }
    # names(outkinit$val) <- seq_along(outkinit$val)
    rescl[[1]] <- outkinit$val
    tet <- lapply(outkinit$val, row.names) 
    # tet <- lapply(1:length(tet),function(k){
    #   data.frame(tet[[as.character(k)]], 1, 1, k)
    # })
    # tet <- lapply(1:length(tet),function(k){
    #   k<- as.character(k)
    #   if(length(tet[[k]])!=0)
    #     data.frame(tet[[as.character(k)]], 1, 1, k)
    #   else data.frame(`tet..as.character.k...`=character(0), `1`=numeric(0),'1'=numeric(0), k=integer(0))
    # })  
    #length(tet) = no of clusters
    for (k in 1:length(tet)){
      #print(k)
      tet[[k]] <- data.frame(tet[[k]], 1, 1, k)
      #print(head(tet[[k]]))
    } 
    names(tet) <- paste(1:length(tet))
    # names(tet) <- seq_along(tet)
    # for (k in 1: length(tet)) 
    #   tet[[as.character(k)]] <- data.frame(tet[[as.character(k)]], 1, 1, k)
    #ID for each datapoint
    resid[[1]] <- tet
    #centroids of each cluster
    # names(outkinit$cent) <- seq_along(outkinit$cent)
    resm[[1]] <- outkinit$cent
    maxqe[[1]] <- outkinit$maxQE
    meanqe[[1]] <- outkinit$meanQE
    #flag to check for quantization error
    resplt[[1]] <- unlist(outkinit$cent) > quant.err
    #initial values for next level k-means
    # names(outkinit$values)<-seq_along(outkinit$values)
    initclust <- outkinit$values  
    if (depth > 1) {
      # flog.info("HVQ calculation started")
      i <- 1
      while (i < depth) {
        ijclust <- NULL
        #stores values of datapoints
        ijrescl <- list()
        #ids of the datapoints
        ijresid <- list()
        #to store the centroids
        ijresm <- list()
        # to store max qe as radius for anomalies
        ijmaxqe <- list()
        ijmeanqe <- list()
        #flag to check the quantization error
        ijresplt <- list()
        #number of datapoints in a cluster
        ijresnsize <- list()
        ijztab3up <- list()
        #flag which checks quantization error
        quantok <- unlist(resplt[[i]])
        j <- 1  
        ## Intervention 1
        while (j < (n_cells^i) + 1) {  
          #if datapoint exceeds quantization error and number of datapoints in that cluster is greater than n_cells
          if (quantok[j] & NROW(initclust[[j]]) > 3) {
            #k-means on the initclust to obtain the next level clustering(sub-clusters)
            z = data.frame(initclust[[j]])
            # try(
            # outk <- getOptimalCentroids_new(z, iter.max = 10^5, algorithm = algorithm, n_cells, n_min_points, function_to_calculate_distance_metric, function_to_calculate_error_metric, quant.err = quant.err, distance_metric = distance_metric, quant_method=quant_method)
            outk <- getOptimalCentroids(z, iter.max = 10^5, algorithm = algorithm, n_cells = n_cells, function_to_calculate_distance_metric = function_to_calculate_distance_metric, function_to_calculate_error_metric = function_to_calculate_error_metric, quant.err = quant.err, distance_metric = distance_metric, quant_method = quant_method)
            # )  
            # outk$centers <- outk$centers[-which(sapply(outk$centers, is.na))]
            # outk$maxQE <- outk$maxQE[-which(sapply(outk$maxQE, is.na))]
            # outk$meanQE <- outk$meanQE[-which(sapply(outk$meanQE, is.na))]
            # outk$values <- outk$values[-which(sapply(outk$values, is.na))]
            # outk$nsize <- outk$nsize[-which(sapply(outk$nsize, is.na))]     
            # browser()
            # outk <- getOptimalCentroids(initclust[[j]], iter.max = 100, algorithm = algorithm, n_cells,distance_metric = distance_metric,error_metric = error_metric,quant.err = quant.err)       
            # outk <- getCentroids(initclust[[j]], kout = stats::kmeans(initclust[[j]], n_cells, iter.max = 100, algorithm = algorithm), n_cells,distance_metric = distance_metric,error_metric = error_metric)
            #store the datapoints
            # names(outk$val)<-seq_along(outk$val)
            ijrescl[[j]] <- outk$val
            # tet <- lapply(outk$val, sapply, row.names)
            tet <- lapply(outk$val, row.names)
            tet[-which(sapply(tet, is.null))]
            # names(tet) <- seq_along(tet)
            #create ID's for each datapoint
            # tet <- lapply(1:length(tet),function(k){
            #   if(length(tet[[as.character(k)]])!=0)
            #     data.frame(tet[[as.character(k)]], i+1, j, k)
            #   else data.frame(`tet..as.character.k...`=character(0), `i...1`=numeric(0), j=numeric(0), k=integer(0))
            # })
            # for (k in 1: length(tet)) {
            #   print(k)
            #   print(nrow(data.frame(tet[[k]])))
            # }
            for (k in 1: length(tet)){
              
              if(!is.null(tet[[k]])){
                tet[[k]] <- data.frame(tet[[k]],i + 1, j, k)  
              }
              else{
                tet[[k]] <- data.frame(tet[[k]]<-numeric(0))
              }
            }           
            # names(tet) <- paste(1:length(tet))
            # names(tet)<-seq_along(tet)
            ijresid[[j]] <- tet
            #store the centroids
            # names(outk$cent) <- seq_along(outk$cent)
            ijresm[[j]] <- outk$centers
            ijmaxqe[[j]] <- outk$maxQE
            ijmeanqe[[j]] <- outk$meanQE
            #size of a cluster
            # names(outk$nsize) <- seq_along(outk$nsize)
            ijresnsize[[j]] <- outk$nsize
            # ijztab3up[[j]] <- sapply(outk$val, mean)  
            ijztab3up[[j]]<-matrix(0, nrow = ncol(x), n_cells)          
            # rownames(ijztab3up[[j]]) <- colnames(x)
            ijztab3up[[j]] <- t(outk$centroid_val)
            # ijztab3up[[j]]<- na.omit(ijztab3up[[j]])            
            #flag to check if the quantization error threshold is exceeded
            ijresplt[[j]] <- unlist(outk$centers) > quant.err
            #store the datapoints
            ijclust <- c(ijclust, outk$values)
          }else {
            ijrescl[[j]] <- rep(NA, n_cells)
            ijresid[[j]] <- NULL
            ijresm[[j]] <- rep(NA, n_cells)
            ijmaxqe[[j]] <- rep(NA, n_cells)
            ijmeanqe[[j]] <- rep(NA, n_cells)
            ijresnsize[[j]] <- rep(0, n_cells)
            ijztab3up[[j]] <- matrix(NA, ncol(x), n_cells)
            ijresplt[[j]] <- rep(F, n_cells)
            ijclust <- c(ijclust, rep(NA, n_cells))
          }
          n_cells_at_j <- length(tet)
          ztab1 <- c(ztab1, paste(i + 1, j, 1:n_cells_at_j, sep = ","))
          ztab11 <- c(ztab11, rep(i + 1, n_cells_at_j))
          ztab12 <- c(ztab12, rep(j, n_cells_at_j))
          ztab13 <- c(ztab13, 1: n_cells_at_j)
          j <- j + 1
        }
        rescl[[i + 1]] <- ijrescl
        resid[[i + 1]] <- ijresid
        resm[[i + 1]] <- ijresm
        maxqe[[i + 1]] <- ijmaxqe
        meanqe[[i + 1]] <- ijmeanqe
        resplt[[i + 1]] <- ijresplt
        ztab2 <- c(ztab2, unlist(ijresnsize))
        ztab3up[[i + 1]] <- data.frame(ijztab3up)
        ztabn <- c(ztabn, unlist(ijresm))
        initclust <- ijclust
        # flog.info("Output for Level %s is ready", i)
        i <- i + 1
        #check if the desired depth is reached
        if (!is.element(T, unlist(ijresplt))) {
          zdepth <- i
          i <- depth
        }
      }
      # flog.info("HVQ for user-defined depth has been calculated")
      #ztab is the output which contains the datapoints and their IDs.
      #initialize ztab
      ztab <- data.frame(matrix(0, nrow = sum(n_cells^(1:zdepth)), 
                                ncol = (ncol(x) + 5)))
      #Segment Level
      ztab[1:n_cells, 1] <- rep(1, n_cells)
      #Segment Parent
      ztab[1:n_cells, 2] <- rep(1, n_cells)
      #Segment Child
      ztab[1:n_cells, 3] <- 1: n_cells
      #Size of the cluster
      ztab[1:n_cells, 4] <- unlist(outkinit$nsize)
      #Centroid/Quantization error of the cluster
      ztab[1:n_cells, 5] <- unlist(outkinit$cent)
      # ztab3upc <- sapply(outkinit$val, mean, na.rm = TRUE)
      # ztab3upc<-matrix(0, nrow = ncol(x), n_cells)
      # std<-matrix(0, nrow = ncol(x), n_cells)
      for(a in 1: n_cells){
        for(b in 1: ncol(x)){
          ztab3upc[b, a] <- as.matrix(mean(outkinit$val[[a]][, b], na.rm = TRUE))
          rownames(ztab3upc) <- colnames(x)
          # Calculating sd
          std[b, a] <- as.matrix(stats::sd(outkinit$val[[a]][, b], na.rm = TRUE))
          rownames(std) <- colnames(x)
        }
      }
      for (l in 1:length(ztab3up)){
        ztab3upc <- cbind(ztab3upc,ztab3up[[l]])
        std <- cbind(std, ztab3up[[l]])
      }    
      ztab[(n_cells + 1):sum(n_cells^(1:zdepth)), 1] <- ztab11
      ztab[(n_cells + 1):sum(n_cells^(1:zdepth)), 2] <- ztab12
      ztab[(n_cells + 1):sum(n_cells^(1:zdepth)), 3] <- ztab13
      ztab[(n_cells + 1):sum(n_cells^(1:zdepth)), 4] <- ztab2
      ztab[, 6: ncol(ztab)] <- t(ztab3upc)
      ztab[(n_cells + 1): sum(n_cells^(1: zdepth)), 5] <- ztabn
      names(ztab) <- c("Segment.Level", "Segment.Parent", "Segment.Child", 
                       "n", "Quant.Error", colnames(x))
    } else {
      # else loop if depth = 1
      ztab <- data.frame(matrix(0, nrow = n_cells_optimal, ncol = (ncol(x) +  5)))                                                          
      ztab[, 1] <- rep(1, n_cells_optimal)
      ztab[, 2] <- rep(1, n_cells_optimal)
      ztab[, 3] <- 1:n_cells_optimal
      ztab[, 4] <- unlist(outkinit$nsize)
      ztab[, 5] <- unlist(outkinit$cent)
      ztab_mean <- t(sapply(outkinit$val, colMeans, 
                            na.rm = TRUE))
      std <- t(sapply(outkinit$val, colSd, 
                      na.rm = TRUE))
      if(quant_method == "kmeans"){
        ztab[, 6:ncol(ztab)] <- ztab_mean # mean values
      } else {
        ztab[, 6:ncol(ztab)] <- outkinit[["sum_val"]] # Medoid/median values
      }
      names(ztab) <- c("Segment.Level", "Segment.Parent", "Segment.Child", 
                       "n", "Quant.Error", colnames(x))
    }
    if(depth > 1){
      meanCol = t(ztab3upc)
      std = t(std)
    }else{
      meanCol =  ztab_mean
      std = data.frame(std)
    }
    ## Calculate compress percentage
    compression_summary <- ztab %>% 
      dplyr::group_by(Segment.Level) %>% 
      dplyr::summarise( noOfCells = sum(!is.na(Quant.Error)), noOfCellsBelowQuantizationError = sum(Quant.Error < quant.err,na.rm = TRUE)) %>% dplyr::mutate(percentOfCellsBelowQuantizationErrorThreshold = (noOfCellsBelowQuantizationError/noOfCells))  
    colnames(compression_summary)[1] <- "segmentLevel" 
    if(any(is.nan(compression_summary$percentOfCellsBelowQuantizationErrorThreshold))){
      compression_summary <- compression_summary[stats::complete.cases(compression_summary),]}
    compression_summary$parameters=paste("n_cells:",n_cells_optimal,"quant.err:",quant.err,"distance_metric:",distance_metric[1],"error_metric:",error_metric[1],"quant_method:",quant_method[1])
    ridnames <- resid
    rclnames <- rescl
    return(list(clusters = initclust, nodes.clust = rescl, idnodes = resid, 
                error.quant = resm, max_QE = maxqe, plt.clust = resplt, summary = ztab, compression_summary = compression_summary,
                meanClust = meanCol, sdClust = std,mean_QE = meanqe ))   
  }