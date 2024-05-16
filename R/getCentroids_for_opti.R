#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr

getCentroids_for_opti <- function (x, kout, n_cells,function_to_calculate_distance_metric,function_to_calculate_error_metric){
  requireNamespace("dplyr")   
  requireNamespace("tidyr")   
  requireNamespace("magrittr")   
   # browser()
  calculate_error<- centl <- list()
  # calculate_error <- x %>% dplyr::group_by(kout$clust) %>% dplyr::do(err = function_to_calculate_distance_metric(.))
  calculate_error <-
    x %>%
    dplyr::group_by(kout$cluster) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(.x = data, .f = function_to_calculate_distance_metric)) %>%
    dplyr::arrange(`kout$cluster`) %>%
    dplyr::rename(err = data)
  
  centl <-lapply(calculate_error$err,function_to_calculate_error_metric)
  maxQE<- lapply(calculate_error$err, "max")
  meanQE<- lapply(calculate_error$err, "mean")
  
  return(list(centl,maxQE,meanQE))
  
}