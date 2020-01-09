#' @importFrom magrittr %>%

# The function should take dataframe having m rows and n dimension as input. It should return distance from the center for each row or should return 0 if nrow(df) < 1

getCentroids <-
  function (x, kout, nclust,function_to_calculate_distance_metric,function_to_calculate_error_metric){
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
