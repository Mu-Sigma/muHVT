


#' @importFrom magrittr %>%

# The function should take dataframe having m rows and n dimension as input. It should return distance from the center for each row or should return 0 if nrow(df) < 1

getCentroids <-
  function (x, kout, nclust,distance_metric = c("L1_Norm","L2_Norm"),error_metric = c("mean","max")){
    requireNamespace("dplyr")
    
    
    
    outl <- list()
    nout <- list()
    centl <- list()
    
    options(warn = -1)
    
    
    x <- data.frame(x)
    # function to calculate centroid for each cluster
    calculate_euclidean_distance_for_each_cluster <- function(x){
      if(nrow(x) > 1){
        sqrt(rowSums(scale(x,center = T,scale = F)^2))
      }
      else{
        return(0)
      }
    }
    
    calculate_manhattan_distance_for_each_cluster <- function(x){
      if(nrow(x) > 1){
        rowSums(abs(scale(x,center = T,scale = F)))
      }
      else{
        return(0)
      }
    }
    
    
    
    if(is.function(distance_metric)){
      function_to_calculate_distance_metric <- distance_metric
    }
    else{
      distance_id <- switch(match.arg(distance_metric), L1_Norm = 1L, 
                      L2_Norm = 2L)
      if(distance_id==1L){
        function_to_calculate_distance_metric <- calculate_manhattan_distance_for_each_cluster
      }
      else if(distance_id==2L){
        function_to_calculate_distance_metric <- calculate_euclidean_distance_for_each_cluster
      }
      
      else{
        stop('distance_metric must be L1_Norm (Manhattan), L2_Norm(Euclidean) or custom distance function')
      }
    }
    
    if(is.function(error_metric)){
      function_to_calculate_error_metric <- error_metric
    }
    else{
      error_id <- switch(match.arg(error_metric), mean = 1L, 
                            max = 2L)
    if(error_id==1L){
      function_to_calculate_error_metric <- "mean"
    }
    
    else if(error_id==2L){
      function_to_calculate_error_metric <- "max"
    }

    else{
      stop('error_metric must be max,mean or custom function')
    }
    }
    
    
    calculate_error <- x %>% dplyr::group_by(kout$cluster) %>% dplyr::do(err = function_to_calculate_distance_metric(.))
    distance_for_each_dimension_for_each_cluster <- calculate_error$err
    
    calculate_error_for_each_cluster <- lapply(distance_for_each_dimension_for_each_cluster,function_to_calculate_error_metric)
    
    centl <- calculate_error_for_each_cluster
    outl <-  c(1:nclust) %>% purrr::map(~x[kout$cluster==.x,])
    nout <- as.list(kout$size)


    
    #return centroids, datapoints and size of each cluster
    return(list(centers = centl, values = outl, nsize = nout))
  }
