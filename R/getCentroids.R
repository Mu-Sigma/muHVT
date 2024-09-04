#' @importFrom magrittr %>%


getCentroids <-
  function (x,
            kout,
            n_cells,
            function_to_calculate_distance_metric,
            function_to_calculate_error_metric,
            distance_metric = "L1_Norm",
            error_metric,
            quant_method=c("kmeans","kmedoids"),
            ...) {
    set.seed(279)
    
#browser()

    if (quant_method == "kmeans") {
      centl <- nout <- outl <- list()
      x <- data.frame(x) %>% stats::na.omit()
      col_dim <- ncol(x)
  ###############################################################    
      
      calculate_error <-
        x %>%
        group_by(kout$cluster) %>%
        tidyr::nest()
    
      centroid_data <- kout$centers %>% as.data.frame()
      calculate_error <- calculate_error %>% arrange(calculate_error$`kout$cluster`)
      
      
      cluster_distances <- purrr::map2(
        1:nrow(centroid_data),
        calculate_error$data,
        function(i, cluster_data) {
          centroid_row <- centroid_data[i, ]
          apply(cluster_data, 1, function(row) function_to_calculate_distance_metric(centroid_row, row))
        }
      )
     
      calculate_error_for_each_cluster <- unlist(lapply(cluster_distances, function(x) {
        if (error_metric == "mean") {
          return(mean(x)/col_dim)
        } else if (error_metric == "max") {
          return(max(x)/col_dim)
        } else {
          return(error_metric(x))
        }
      }))
      

      maxQE_each_cluster <- unlist(lapply(cluster_distances, function(x) {   return(max(x))}))
      meanQE_each_cluster <-  unlist(lapply(cluster_distances, function(x) {   return(mean(x))}))
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
          meanQE = meanQE_each_cluster,
          dataframe_clusters = centroid_data,
          cluster_distance = cluster_distances
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
