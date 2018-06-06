requireNamespace("deldir")   

#======================================================================================
# Global variables
#======================================================================================

#quant.err <- 80
errorCode <- 0

#______________________________________________________________________________________
# Splitting the codebooks
#______________________________________________________________________________________

split_codevec = function(codeVec,epsilon){
  #errorValue=errorValue/100
  codeVec <- plyr::llply(codeVec, .fun= function(X) return(list((1+epsilon)*X, (1-epsilon)*X)))
  return(codeVec)  
}

#_____________________________________________________________________________________
# Checks if the MAPE for a given cluster and its codebook is lesser/greater
# than the specified treshold
#_____________________________________________________________________________________

findMape <- function(cluster, centroid){
  mape <- 1/nrow(cluster) * sum(apply(cluster, MARGIN = 1, FUN= function(X){ return(abs(sum((X-centroid), na.rm=TRUE)/length(X)))}))
  return(mape)
}

mapeTest = function(cluster, centroid,errorValue){
  
  if(is.null(dim(cluster))){
    return(TRUE)
  }
  
  mape <- findMape(cluster, centroid)
  #print(paste("Quantization error:", mape))
  
  if(mape < errorValue){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#_____________________________________________________________________________________
# allocate members to their centroids
#_____________________________________________________________________________________
findMembers <- function(dataset, centroid){
  
  # compute the distance of every point in the data from a centroid
  #err <- lapply(centroid, FUN = function(X){ return(apply(dataset, MARGIN=1, FUN= function(Y)sqrt(sum((Y-X)^2))))})
  err = lapply(centroid, FUN=function(x) sqrt(colSums((t(dataset) - x)^2)))
  err <- do.call(rbind, err)
  
  # Allocate the membership of a datum to its closest centroid
  clust.list <- apply(err, MARGIN=2, FUN = function(X) { return(which(X==min(X), arr.ind=T))})
  
  return(clust.list)
}

#_____________________________________________________________________________________
# Function to assign cluster membership of data points
#_____________________________________________________________________________________

findSeque <- function(dataset, clusters){
  
  tryCatch({
    rownames(dataset) <- NULL
    clusters <- lapply(clusters, unname)
    clusters <- lapply(clusters, as.matrix)
    clusters <- lapply(clusters , function(X){colnames(X)<- colnames(dataset); return(X)})
    oneRowIndices <- which(lapply(lapply(clusters, dim), function(X)return(X[2]))==1)
    clusters[oneRowIndices] <- lapply(clusters[oneRowIndices], t)
    pointPos <- lapply(clusters, FUN=function(X)rownames(plyr::match_df(dataset, as.data.frame(X))))
    pointPos <- lapply(pointPos, as.numeric)
    sequeDf <- as.data.frame(1:nrow(dataset))
    lapply(1:length(pointPos), FUN=function(X){sequeDf[pointPos[[X]],]<<- X})
    clust.list <- sequeDf[,1]
  },
           error=function(err){print(err)}, 
           finally={})
  
  return(clust.list)
}

#_____________________________________________________________________________________
# allocate members to their centroids
#_____________________________________________________________________________________

allocateMembers <- function(dataset, centroid){ 
  
  clust.list <- findMembers(dataset, centroid)
  
  return(lapply(unique(clust.list), FUN=function(X)which(clust.list==X)))
}

#_____________________________________________________________________________________
# Nearest neighbour condition - LBGVQ function
#_____________________________________________________________________________________

lbgVQ = function(dataset,quant.err,epsilon){
  
  # mean of the entire data set
  cnew <- list(apply(dataset, MARGIN = 2, FUN= function(X)mean(X, na.rm=TRUE)))
  cFin <- c()
  finalClusters <- c()
  
  # Set the quantization error
  mape <- 1/nrow(dataset) * sum(apply(dataset, MARGIN = 1, FUN= function(X){ return(abs(sum((X-unlist(cnew)), na.rm=TRUE))/length(X))}))
  if(mape != 0){
    errorValue <- mape*quant.err
  } else {
    errorValue <- quant.err
  }
  
  # clust.list is the indices of the members of a cluster
  clust.list <- list(1:nrow(dataset))
  
  while(T){
    
    # cluster is the actual data points in the cluster
    cluster <- lapply(clust.list, FUN=function(Y) { dataset[Y,] })
    
    # codesToSplit is the codebooks which have q error greater than the treshold 
    tryCatch({codesToSplit <- cnew[which(!unlist(lapply(1:length(cluster), FUN= function(i){mapeTest(cluster[[i]], cnew[[i]],errorValue)})))]}, 
             error=function(err){print(err)}, 
             finally={})
    
    # split the codebooks
    tryCatch({cSplit <- unlist(split_codevec(codesToSplit,epsilon), recursive=F)}, 
             error=function(err){print(err)}, 
             finally={})
    
    # remove the final codebook from the list whose q error is lesser than the treshold
    doneFlags <- !(cnew %in% codesToSplit)
    cFin <- c(cFin, cnew[doneFlags])
    if(length(which(doneFlags))!=0){
      
      # Update the finalClusters
      finalClusters <- c(finalClusters, cluster[which(!cnew %in% codesToSplit)])
      cluster[which(!cnew %in% codesToSplit)] <- NULL
      
      
      # Remove the clustered data from the original dataset
      dataset <- dataset[-unlist(clust.list[which(doneFlags)]),]
      
      # Start afresh
      cnew <- list(apply(dataset, MARGIN = 2, FUN= mean))
      clust.list <- list(1:nrow(dataset))
      
      if(length(codesToSplit) == 0){
        break
      }
      next
    }
    
    # update the clust.list with the new split codebooks and re-compute the mean codebook
    tryCatch({clust.list <- allocateMembers(dataset, cSplit)}, 
             error=function(err){print(err)}, 
             finally={})
    
    cnew <- lapply(clust.list, 
                   FUN = function(X) 
                     if(length(X)==1){ return(dataset[X,]) 
                     } else { return(apply(dataset[unlist(X),], MARGIN=2, FUN=mean))} )
  }
  
  cFin <- lapply(cFin, FUN = as.numeric)
  
  return(list(clusters = finalClusters, codebooks = cFin,errorValue=errorValue))
}

#_____________________________________________________________________________________
# Functions to merge the clusters that satisfy the quantization treshold
#_____________________________________________________________________________________

euDistance <- function(index, codebooks){
  return((codebooks[[index[1]]] - codebooks[[index[2]]])^2)
}

#_____________________________________________________________________________________
# Function checks if the clusters should be merged based on the quantization error
#_____________________________________________________________________________________

shouldMergeClusters <- function(X, clusters,errorValue){ 
  
  # merge the two clusters and compute the new mean
  newCluster <- do.call(rbind, list(clusters[[X[1]]], clusters[[X[2]]]))
  meanCb <- apply(newCluster, MARGIN = 2, FUN= mean)
  
  # check for the quantization error
  tryCatch({mapeFlag <- mapeTest(newCluster, meanCb,errorValue)}, 
           error=function(err){print(err)}, 
           finally={})
  
  return(mapeFlag)
}


mergeClusters <- function(codebooks, clusters,errorValue){
  
  # find the index of the centroids that are closest (X)
  pairs <- t(utils::combn(length(codebooks),2))
  
  tryCatch({euD <- sqrt(colSums(apply(pairs, MARGIN=1, FUN=function(X){euDistance(X, codebooks)})))},
           error=function(err){print(err)}, 
           finally={})
  
  X<- pairs[which(euD==min(euD)),]
  
  
  if(shouldMergeClusters(X, clusters,errorValue)){
    # merge the two clusters and compute the new mean
    newCluster <- do.call(rbind, list(clusters[[X[1]]], clusters[[X[2]]]))
    meanCb <- list(apply(newCluster, MARGIN = 2, FUN= mean))
    
    # Update the codebooks and the clusters
    clusters[[X[1]]] <- newCluster
    clusters[[X[2]]] <- NULL
    codebooks[[X[1]]] <- meanCb[[1]]
    codebooks[[X[2]]] <- NULL
    
    # recurse to find the next merge-able codebook pair
    mergeClusters(codebooks, clusters,errorValue)
  } else {
    return(list(clusters =clusters, codebooks = codebooks,errorValue=errorValue))
  }
  
}

#_______________________________________________________________________________________
# Main Function to quantize
#______________________________________________________________________________________

vectorQuantize <- function(dataset,quant.err,epsilon){
  
  tryCatch({lbgvq <- lbgVQ(dataset,quant.err,epsilon)}, 
           error=function(err){print(err)}, 
           finally={})
  
  if(length(lbgvq$codebooks) > 1){
    
    tryCatch({model <- mergeClusters(lbgvq$codebooks, lbgvq$clusters,lbgvq$errorValue)}, 
             error=function(err){print(err)},
             finally={})
    

    return(model)
    
  } else {
    return(lbgvq)
  }
  
}

#==================================================================
# Function to Trilaterate
# Input : a distance matrix or a 'dist' object
# Output : a set of trilaterated points
#==================================================================

trilaterate <- function(d){
  if(class(d)=='dist'){
    d <- as.matrix(d)
  }
  
  # p1, p2, p3 are the initial triangulation points
  p1 <- c(0,0)
  p2 <- c(0, d[1,2])
  
  if(dim(d)[1]==3){  # If there are only 3 points to trilaterate
    
    p3y <- (d[1,3]^2 + d[1,2]^2 - d[2,3]^2)/(2*d[1,2])
    p3x <- sqrt(d[1,3]^2 - p3y^2)
    p3 <- c(p3x,p3y)  
    pdash <- rbind(p1,p2,p3)	
    colnames(pdash) = c('x', 'y')
    return(pdash)
    
  } else if(dim(d)[1] > 3){ # If there are more than 3 points to trilaterate
    
    p3y <- (d[1,3]^2 + d[1,2]^2 - d[2,3]^2)/(2*d[1,2])
    p3x <- sqrt(d[1,3]^2 - p3y^2)
    p3 <- c(p3x,p3y)
    
    # iterator
    i <- 4:dim(d)[1]
    # compute the remaining points
    pdash <- lapply(i, FUN= function(ind) {
      y <- (d[1,ind]^2 + d[1,2]^2 - d[2,ind]^2)/(2*d[1,2])
      x <- (d[1,ind]^2 + d[1,3]^2 - d[3,ind]^2 - 2*y*p3[2])/(2*p3[1])
      return(c(x, y))
    })
    
    pdash<- do.call(rbind, pdash)
    pdash <- rbind(p1,p2, p3,pdash)
    colnames(pdash) = c('x', 'y')
    return(pdash)
  } else {  # If there are one or 2 points to trilaterate
    pdash <- rbind(p1,p2)
    colnames(pdash) = c('x', 'y')
    return(pdash)
  }
}



#===========================================================================
# Main function to call
#===========================================================================







#' VQ_codebookSplit
#' 
#' Vector Quantization by codebook split method
#' 
#' Performs Vector Quantization by codebook split method. Initially, the entire
#' dataset is considered to be one cluster where the codebook is the mean of
#' the cluster. The quantization criteria is checked and the codebook is split
#' such that the new codebooks are (codebook+epsilon) and (codebook-epsilon).
#' The observations are reassigned to these new codebooks based on the nearest
#' neighbour condition and the means recomputed for the new clusters. This is
#' done iteratively until all the clusters meet the quantization criteria.
#' 
#' @param dataset Matrix. A matrix of multivariate data. Each row corresponds
#' to an observation, and each column corresponds to a variable. Missing values
#' are not accepted.
#' @param quant.err Numeric. The quantization error for the algorithm.
#' @param epsilon Numeric. The value to offset the codebooks during the
#' codebook split. Default is NULL, in which case the value is set to quant.err
#' parameter.
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
#' 
#' data("iris",package="datasets")
#' iris <- iris[,1:2]
#' 
#' vqOutput = VQ_codebookSplit(iris, quant.err = 0.5)
#' 
#' 
#' @export VQ_codebookSplit
VQ_codebookSplit <- function(dataset,quant.err=0.5,epsilon=NULL){
  
  model <- list()
  
  if(is.null(epsilon)){
    epsilon = quant.err
  }
  
  if(!is.null(dataset)){
    # Run VQ
    tryCatch({system.time(model <- vectorQuantize(dataset,quant.err,epsilon))}, 
             error=function(err){print(err)}, 
             finally={})
    
  } else {
    #flog.error("Data is missing", name=logNS)	      
  }
  
  if(exists('model')){	  
    
    # Reduce the dimensionality of the codebooks to 2
    codebooks <- do.call(rbind, model$codebooks)
    distances <- stats::dist(codebooks)
    if(length(distances) != 0){
      tryCatch({twoDCodebooks <- trilaterate(distances)}, error=function(err){print(err)}, finally={})
    } else {
      twoDCodebooks <- model$codebooks[[1]][1:2]
    }
    mapes <- lapply(1:length(model$clusters), FUN=function(i){findMape(model$clusters[[i]],model$codebooks[[i]])})
	
    ztab <- data.frame('Segment.Level' = rep(1, length(model$clusters)), 'segment.Parent' = rep(1, length(model$clusters)), 'Segment.Child'= 1:length(model$clusters), 'n'=  unlist(lapply(model$clusters, nrow)),'Quant.Error'= unlist(mapes))
    ztab <- cbind(ztab, do.call(rbind, model$codebooks))
    
    idnodes <- lapply(model$clusters, rownames)
    idnodes <- list(lapply(1:length(idnodes), FUN=function(i){data.frame('tet..k..'=idnodes[[i]], 'X1'=rep(1, length(idnodes[[i]])), 'X1.1'=rep(1, length(idnodes[[i]])), 'k'= i)}))
    
    return(list('clusters'=model$clusters, 'nodes.clust'= list(model$clusters), 'idnodes'= idnodes, 'error.quant'= list(mapes), plt.clust= list(mapes<model$errorValue), 'summary'= ztab))
    
       
  } else {
    return('error')
  }
  
}
