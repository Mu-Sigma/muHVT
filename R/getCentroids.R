getCentroids <-
  function (x, kout, nclust){
    
    outl <- list()
    nout <- list()
    centl <- list()
    
    x <- data.frame(x)
    # function to calculate centroid for each cluster
    calculate_error_for_each_cluster <- function(x){
      if(nrow(x) > 1){
        mean(sqrt(rowSums(scale(x,center = T,scale = F)^2)))
      }
      else{
        return(0)
      }
    }
    
    calculate_error <- x %>% dplyr::group_by(kout$cluster) %>% dplyr::do(err = calculate_error_for_each_cluster(.))
    centl <- calculate_error$err
    outl <-  c(1:nclust) %>% purrr::map(~x[kout$cluster==.x,])
    nout <- as.list(kout$size)


    
    #return centroids, datapoints and size of each cluster
    return(list(centers = centl, values = outl, nsize = nout))
  }
