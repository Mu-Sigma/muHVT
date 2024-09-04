#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr

getCentroids_for_opti <- function (x, kout, n_cells,function_to_calculate_distance_metric,function_to_calculate_error_metric){
  requireNamespace("dplyr")   
  requireNamespace("tidyr")   
  requireNamespace("magrittr")   
  calculate_error<- centl <- list()
  
  calculate_error <-
    x %>%
    group_by(kout$cluster) %>%
    tidyr::nest()
  
  centroid_data <- kout[["centers"]] %>% as.data.frame()
  calculate_error <- calculate_error %>% arrange(calculate_error$`kout$cluster`)
  
  cluster_distances <- purrr::map2(
    1:nrow(centroid_data),
    calculate_error$data,
    function(i, cluster_data) {
      centroid_row <- centroid_data[i, ]
      apply(cluster_data, 1, function(row) function_to_calculate_distance_metric(centroid_row, row))
    }
  )
  
  
  calculate_error_for_each_cluster <- unlist(lapply(cluster_distances, function_to_calculate_error_metric)) 
  maxQE <- unlist(lapply(cluster_distances, function(x) {   return(max(x))}))
  meanQE <-  unlist(lapply(cluster_distances, function(x) {   return(mean(x))}))
  centl <- calculate_error_for_each_cluster
  
  
  return(list(centl,maxQE,meanQE))
  
}