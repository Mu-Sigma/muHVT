#' @importFrom magrittr %>%

# The function should take dataframe having m rows and n dimension as input. It should return distance from the center for each row or should return 0 if nrow(df) < 1

getCentroids <-
  function (x, kout, nclust,distance_metric,error_metric){
    
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
    
    
    centl <- nout <- outl <- list()
    options(warn = -1)
    x <- data.frame(x)
    calculate_error <- x %>% dplyr::group_by(kout$cluster) %>% dplyr::do(err = function_to_calculate_distance_metric(.))
    calculate_error_for_each_cluster <- lapply(calculate_error$err,function_to_calculate_error_metric)
    centl <- calculate_error_for_each_cluster
    outl <-  c(1:nclust) %>% purrr::map(~x[kout$cluster==.x,])
    nout <- as.list(kout$size)
    #return centroids, datapoints and size of each cluster
    return(list(centers = centl, values = outl, nsize = nout))
  }
