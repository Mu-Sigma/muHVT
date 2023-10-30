#' @importFrom magrittr %>%

# The function should take dataframe having m rows and n dimension as input. It should return distance from the center for each row or should return 0 if nrow(df) < 1
# distance_metric=c("L1_Norm","L2_Norm")
# quant_method=c("kmeans","kmedoids")

getCentroids <-
  function (x,
            kout,
            n_cells,
            function_to_calculate_distance_metric,
            function_to_calculate_error_metric,
            distance_metric = "L1_Norm",
            quant_method=c("kmeans","kmedoids"),
            ...) {
    # browser()
    if (quant_method == "kmeans") {
      centl <- nout <- outl <- list()
      x <- data.frame(x)
      # calculate_error <- x %>% dplyr::group_by(kout$cluster) %>% dplyr::do(err = function_to_calculate_distance_metric(.))
      calculate_error <-
        x %>%
        group_by(kout$cluster) %>%
        tidyr::nest() %>%
        # calculate the distance for each datapoint for each cluster wrt to its centroid as per L1 or L2 norm
        dplyr::mutate(data = purrr::map(.x = data, .f = function_to_calculate_distance_metric)) %>%
        # arrange the dataframe by cluster number
        dplyr::arrange(`kout$cluster`) %>%
        dplyr::rename(err = data)
      
      # calcuate the quant error for each cluster as per error_metric = "mean" or "max"
      calculate_error_for_each_cluster <-
        lapply(calculate_error$err, function_to_calculate_error_metric)
      maxQE_each_cluster <- lapply(calculate_error$err, "max")
      meanQE_each_cluster <- lapply(calculate_error$err, "mean")
      centl <- calculate_error_for_each_cluster
      outl <-
        c(1:n_cells) %>% purrr::map( ~ x[kout$cluster == .x, ])
      nout <- as.list(kout$size)
      #return centroids, datapoints and size of each cluster
      return(
        list(
          centers = centl,
          maxQE = maxQE_each_cluster,
          values = outl,
          nsize = nout,
          meanQE = meanQE_each_cluster
        )
      )
      
      ############################## Medoid implementation ###############################################
      
    } else if (quant_method == "kmedoids") {
      if (distance_metric == "L1_Norm") {
        distance_metric = "manhattan"
      } else if (distance_metric == "L2_Norm") {
        distance_metric = "euclidean"
      }
      
      
      kmedoids_model <-
        cluster::pam(
          x = cluster::daisy(x, metric = distance_metric),
          k = n_cells,
          diss = TRUE,
          keep.data = FALSE
        )
      
      centl <- nout <- outl <- list()
      x <- data.frame(x)
      # calculate_error <- x %>% dplyr::group_by(kout$cluster) %>% dplyr::do(err = function_to_calculate_distance_metric(.))
      calculate_error <-
        x %>% dplyr::group_by(kmedoids_model[["clustering"]]) %>% tidyr::nest() %>% dplyr::mutate(data = purrr::map(.x = data, .f = function_to_calculate_distance_metric)) %>% arrange(`kmedoids_model[["clustering"]]`) %>% dplyr::rename(err =
                                                                                                                                                                                                           data)
      calculate_error_for_each_cluster <-
        lapply(calculate_error$err, function_to_calculate_error_metric)
      maxQE_each_cluster <- lapply(calculate_error$err, "max")
      meanQE_each_cluster <- lapply(calculate_error$err, "mean")
      centl <- calculate_error_for_each_cluster
      outl <-
        c(1:n_cells) %>% purrr::map(~ x[kmedoids_model[["clustering"]] == .x,])
      nout <-
        as.list(as.numeric(table(kmedoids_model[["clustering"]])))
      
      sum_val = as.data.frame(x[kmedoids_model[["medoids"]],])
      #return centroids, datapoints and size of each cluster
      return(
        list(
          centers = centl,
          maxQE = maxQE_each_cluster,
          values = outl,
          nsize = nout,
          meanQE = meanQE_each_cluster,
          sum_val = sum_val
        )
      )
    }
  }
