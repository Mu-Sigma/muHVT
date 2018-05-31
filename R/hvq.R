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
#' @param nclust Numeric. Indicating the number of nodes per hierarchy.
#' @param depth Numeric. Indicating the hierarchy depth (or) the depth of the
#' tree (1 = no hierarchy, 2 = 2 levels, etc..)
#' @param quant.err Numeric. The quantization error for the algorithm.
#' @param algorithm String. The type of algorithm used for quantization.
#' Available algorithms are Hartigan and Wong, "Lloyd", "Forgy", "MacQueen".
#' (default is "Hartigan-Wong")
#' @return \item{clusters}{ List. A list showing each ID assigned to a cluster.
#' } \item{nodes.clust}{ List. A list corresponding to nodes' details. }
#' \item{idnodes}{ List. A list of ID and segments similar to
#' \code{nodes.clust} with additional columns for nodes ID. }
#' \item{error.quant}{ List. A list of quantization error for all levels and
#' nodes. } \item{plt.clust}{ List. A list of logical values indicating if the
#' quantization error was met. } \item{summary}{ Summary. Output table with
#' summary. }
#' @author Meet K. Dave <dave.kirankumar@@mu-sigma.com>
#' @seealso \code{\link{hvtHmap}}
#' @examples
#' 
#' data("iris",package="datasets")
#' iris <- iris[,1:2]
#' hvqOutput = hvq(iris, nclust = 2, depth = 3, quant.err = 0.3)
#' 
#' 
#' 
#' @export hvq
hvq <-
  function (x, nclust = 3, depth = 3, quant.err = 10, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")) {
    
    rescl <- list()
    resid <- list()
    resm <- list()
    resplt <- list()
    ztab3up <- list()
    ztab1 <- ztab2 <- ztabn <- NULL
    ztab11 <- ztab12 <- ztab13 <- NULL
    zdepth <- depth
    quantinit <- rep(F, nclust)
    # flog.info("Parameters are initialized")
    #outkinit will have centroids and datapoints and size of the cluster
    outkinit <- getCentroids(x, kout = stats::kmeans(x, nclust, iter.max=100, algo=algorithm), nclust)
    # flog.info("Level 1 cluster memberships are calculated")
    #datapoints grouped into clusters
    rescl[[1]] <- outkinit$val
    tet <- lapply(outkinit$val, row.names)
    for (k in 1:length(tet)) tet[[k]] <- data.frame(tet[[k]], 1, 1, k)
    #ID for each datapoint
    resid[[1]] <- tet
    #centroids of each cluster
    resm[[1]] <- outkinit$cent
    #flag to check for quantization error
    resplt[[1]] <- unlist(outkinit$cent) > quant.err
    #initial values for next level k-means
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
        #flag to check the quantization error
        ijresplt <- list()
        #number of datapoints in a cluster
        ijresnsize <- list()
        ijztab3up <- list()
        #flag which checks quantization error
        quantok <- unlist(resplt[[i]])
        j <- 1
        while (j < (nclust^i) + 1) {
          #if datapoint exceeds quantization error and number of datapoints in
          #that cluster is greater than nclust
          if (quantok[j] & NROW(initclust[[j]]) > nclust) {
            #k-means on the initclust to obtain the next level clustering(sub-clusters)
            outk <- getCentroids(initclust[[j]], kout = stats::kmeans(initclust[[j]], 
                                                               nclust, iter.max = 100, algo = algorithm), nclust)
            #store the datapoints
            ijrescl[[j]] <- outk$val
            tet <- lapply(outk$val, row.names)
            #create ID's for each datapoint
            for (k in 1: length(tet)) tet[[k]] <- data.frame(tet[[k]], 
                                                             i + 1, j, k)
            ijresid[[j]] <- tet
            #store the centroids
            ijresm[[j]] <- outk$cent
            #size of a cluster
            ijresnsize[[j]] <- outk$nsize
            # ijztab3up[[j]] <- sapply(outk$val, mean)
            
            ijztab3up[[j]]<-matrix(0, nrow = ncol(x), nclust)
            for(a in 1: nclust){
              for(b in 1: ncol(x)){
                ijztab3up[[j]][b, a] <- as.matrix(mean(outk$val[[a]][, b]))
                rownames(ijztab3up[[j]]) <- colnames(x)
              }
            }
            #flag to check if the quantization error threshold is exceeded
            ijresplt[[j]] <- unlist(outk$cent) > quant.err
            #store the datapoints
            ijclust <- c(ijclust, outk$values)
          }else {
            ijrescl[[j]] <- rep(NA, nclust)
            ijresid[[j]] <- NULL
            ijresm[[j]] <- rep(NA, nclust)
            ijresnsize[[j]] <- rep(0, nclust)
            ijztab3up[[j]] <- matrix(NA, ncol(x), nclust)
            ijresplt[[j]] <- rep(F, nclust)
            ijclust <- c(ijclust, rep(NA, nclust))
          }
          ztab1 <- c(ztab1, paste(i + 1, j, 1:nclust, sep = ","))
          ztab11 <- c(ztab11, rep(i + 1, nclust))
          ztab12 <- c(ztab12, rep(j, nclust))
          ztab13 <- c(ztab13, 1: nclust)
          j <- j + 1
        }
        rescl[[i + 1]] <- ijrescl
        resid[[i + 1]] <- ijresid
        resm[[i + 1]] <- ijresm
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
      ztab <- data.frame(matrix(0, nrow = sum(nclust^(1:zdepth)), 
                                ncol = (ncol(x) + 5)))
      #Segment Level
      ztab[1:nclust, 1] <- rep(1, nclust)
      #Segment Parent
      ztab[1:nclust, 2] <- rep(1, nclust)
      #Segment Child
      ztab[1:nclust, 3] <- 1: nclust
      #Size of the cluster
      ztab[1:nclust, 4] <- unlist(outkinit$nsize)
      #Centroid/Quantization error of the cluster
      ztab[1:nclust, 5] <- unlist(outkinit$cent)
      # ztab3upc <- sapply(outkinit$val, mean, na.rm = T)
      ztab3upc<-matrix(0, nrow = ncol(x), nclust)
      for(a in 1: nclust){
        for(b in 1: ncol(x)){
          ztab3upc[b, a] <- as.matrix(mean(outkinit$val[[a]][, b], na.rm = T))
          rownames(ztab3upc) <- colnames(x)
        }
      }
      for (l in 1:length(ztab3up)) ztab3upc <- cbind(ztab3upc, 
                                                     ztab3up[[l]])
      ztab[(nclust + 1):sum(nclust^(1:zdepth)), 1] <- ztab11
      ztab[(nclust + 1):sum(nclust^(1:zdepth)), 2] <- ztab12
      ztab[(nclust + 1):sum(nclust^(1:zdepth)), 3] <- ztab13
      ztab[(nclust + 1):sum(nclust^(1:zdepth)), 4] <- ztab2
      ztab[, 6: ncol(ztab)] <- t(ztab3upc)
      ztab[(nclust + 1): sum(nclust^(1: zdepth)), 5] <- ztabn
      names(ztab) <- c("Segment Level", "Segment Parent", "Segment Child", 
                       "n", "Quant Error", colnames(x))
    }
    else {
      ztab <- data.frame(matrix(0, nrow = nclust, ncol = (ncol(x) + 
                                                            5)))
      ztab[, 1] <- rep(1, nclust)
      ztab[, 2] <- rep(1, nclust)
      ztab[, 3] <- 1:nclust
      ztab[, 4] <- unlist(outkinit$nsize)
      ztab[, 6:ncol(ztab)] <- t(sapply(outkinit$val, colMeans, 
                                       na.rm = T))
      ztab[, 5] <- unlist(outkinit$cent)
      names(ztab) <- c("Segment Level", "Segment Parent", "Segment Child", 
                       "n", "Quant Error", colnames(x))
    }
    ridnames <- resid
    rclnames <- rescl
    return(list(clusters = initclust, nodes.clust = rescl, idnodes = resid, 
                error.quant = resm, plt.clust = resplt, summary = ztab))
    
  }
