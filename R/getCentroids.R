getCentroids <-
function (x, kout, nclust){
  
  outl <- list()
  nout <- list()
  centl <- list()
  for(i in 1: nclust) {
    #datapoints according to each cluster
    outl[[i]] <- x[kout$cluster == i, ,drop=F ]
    #size of each cluster
    nout[[i]] <- kout$size[i]  
    
    #
    #  Generating lists of outputs
    #
    if(nrow(outl[[i]]) > 1) {
      #intermediate centroid calculated using mean of the datapoints in a cluster
      icent <- apply(outl[[i]], 2, mean)
      #centroid for each cluster
      centl[[i]] <- mean(apply(outl[[i]], 1,FUN =  function(x, y){
        mean(abs(x - y), na.rm = T)}
        , icent), na.rm = T)
    }
    else {
      #no centroids if the number of points is not greater than 1
      centl[[i]] <- 0
      icent <- outl[[i]]
    }
  }
  #return centroids, datapoints and size of each cluster
  return(list(centers = centl, values = outl, nsize = nout))
}
