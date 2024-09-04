#' @name trainHVT
#' @title Constructing Hierarchical Voronoi Tessellations
#' @description This is the main function to construct hierarchical voronoi tessellations.
#' This is done using hierarchical vector quantization(hvq). The data is represented in 2D
#' coordinates and the tessellations are plotted using these coordinates as
#' centroids. For subsequent levels, transformation is performed on the 2D
#' coordinates to get all the points within its parent tile. Tessellations are
#' plotted using these transformed points as centroids.
#' @param dataset Data frame. A data frame, with numeric columns (features) will be used for training the model.
#' @param min_compression_perc Numeric. An integer, indicating the minimum compression percentage to be achieved for the dataset. 
#' It indicates the desired level of reduction in dataset size compared to its original size.
#' @param n_cells Numeric. An integer, indicating the number of cells per hierarchy (level).
#' @param depth Numeric. An integer, indicating the number of levels. A depth of 1 means no hierarchy (single level), 
#' while higher values indicate multiple levels (hierarchy).
#' @param quant.err Numeric. A number indicating the quantization error threshold.
#' A cell will only breakdown into further cells if the quantization error of the cell is 
#' above the defined quantization error threshold.
#' @param normalize Logical. A logical value indicating if the dataset should be normalized. When set to TRUE, 
#' scales the values of all features to have a mean of 0 and a standard deviation of 1 (Z-score).
#' @param distance_metric Character. The distance metric can be L1_Norm(Manhattan) or L2_Norm(Eucledian). L1_Norm is selected by default.
#' The distance metric is used to calculate the distance between an n dimensional point and centroid.
#' @param error_metric Character. The error metric can be mean or max. max is selected by default. 
#' max will return the max of m values and mean will take mean of m values where
#' each value is a distance between a point and centroid of the cell.
#' @param quant_method Character. The quantization method can be kmeans or kmedoids. Kmeans uses means (centroids) as cluster centers
#'  while Kmedoids uses actual data points (medoids) as cluster centers. kmeans is selected by default.
#' @param scale_summary List. A list with user-defined mean and standard deviation values for all the features in the dataset. 
#' Pass the scale summary when normalize is set to FALSE.
#' @param projection.scale Numeric. A number indicating the scale factor for the tessellations to visualize the sub-tessellations
#' well enough. It helps in adjusting the visual representation of the hierarchy to make the sub-tessellations more visible.
#' Default is 10.
#' @param diagnose Logical. A logical value indicating whether user wants to perform diagnostics on the model. 
#' Default value is FALSE.
#' @param hvt_validation Logical. A logical value indicating whether user wants to holdout a validation set and find 
#' mean absolute deviation of the validation points from the centroid. Default value is FALSE.
#' @param train_validation_split_ratio  Numeric. A numeric value indicating train validation split ratio. 
#' This argument is only used when hvt_validation has been set to TRUE. Default value for the argument is 0.8.
#' @param dim_reduction_method Character.The dim_reduction_method can be one of "tsne", "umap", "sammon".
#' @param tsne_perplexity Numeric.The tsne_perplexity is only used when dim_reduction_method is set to "tsne".
#' Default value is 30 and common values are between between 30 and 50.
#' @param tsne_theta Numeric.The tsne_theta is only used when dim_reduction_method is set to "tsne".
#' Default value is 0.5 and common values are between 0.2 and 0.5.
#' @param tsne_eta Numeric.The tsne_eta are used only when dim_reduction method is set to "tsne".
#' Default value is 200.
#' @param tsne_verbose Logical. A logical value which indicates the t-SNE algorithm to print 
#' detailed information about its progress to the console.
#' @param tsne_max_iter Numeric.The tsne_max_iter is used only when dim_reduction_method is set to "tsne".
#' Default value is 1000.More iterations can improve results but increase computation time.
#' @param umap_n_neighbors Integer.The umap_n_neighbors is used only when dim_reduction_method is set to "umap".
#' Default value is 15.Controls the balance between local and global structure in data.
#' @param umap_n_components Integer.The umap_n_components is used only when dim_reduction_method is set to "umap".
#' Default value is 2.Indicates the number of dimensions for embedding.
#' @param umap_min_dist Numeric.The umap_map_dist is used only when dim_reduction_method is set to "umap".
#' Default value is 0.1.Controls how tightly UMAP packs points together.
#' @return A Nested list that contains the hierarchical tessellation information. This
#' list has to be given as input argument to plot the tessellations.
#' \item{[[1]] }{A list containing information related to plotting tessellations. 
#' This information will include coordinates, boundaries, and other details necessary for visualizing the tessellations} 
#' \item{[[2]] }{A list containing information related to Sammon’s projection coordinates of the data points
#' in the reduced-dimensional space.}
#' \item{[[3]] }{A list containing detailed information about the hierarchical vector quantized data along with 
#' a summary section containing no of points, Quantization Error and the centroids for each cell.}
#' \item{[[4]] }{A list that contains all the diagnostics information of the model when diagnose is set to TRUE. 
#' Otherwise NA.}
#' \item{[[5]] }{A list that contains all the information required to generates a Mean Absolute Deviation (MAD) plot, 
#' if hvt_validation is set to TRUE. Otherwise NA}
#' \item{[[6]]}{A list containing detailed information about the hierarchical vector quantized data along with a
#'  summary section containing no of points, Quantization Error and the centroids for each cell which is the output of `hvq`}
#' \item{[[7]]}{model info: A list that contains model-generated timestamp, input parameters passed to the model ,
#' the validation results and the dimensionality reduction evaluation metrics table.}
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>, Shantanu Vaidya <shantanu.vaidya@@mu-sigma.com>,Bidesh Ghosh <bidesh.gosh@@mu-sigma.com>,Alimpan Dey <alimpan.dey@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords Training_or_Compression
#' @importFrom magrittr %>%
#' @examples
#' data("EuStockMarkets")
#' hvt.results <- trainHVT(EuStockMarkets, n_cells = 60, depth = 1, quant.err = 0.1, 
#'                        distance_metric = "L1_Norm", error_metric = "max",
#'                        normalize = TRUE,quant_method="kmeans")
#' @include hvq.R getCellId.R madPlot.R
#' @include Add_boundary_points.R  Corrected_Tessellations.R  DelaunayInfo.R  Delete_Outpoints.R diagPlot.R
#' @export trainHVT


trainHVT <-
  function(dataset,
           min_compression_perc = NA,
           n_cells = NA,
           depth = 1,
           quant.err = 0.2,
           normalize = FALSE,
           distance_metric = c("L1_Norm", "L2_Norm"),
           error_metric = c("mean", "max"),
           quant_method=c("kmeans","kmedoids"),
           scale_summary = NA,
           projection.scale = 10,
           diagnose=FALSE,
           hvt_validation=FALSE,
           train_validation_split_ratio=0.8,
           dim_reduction_method = "sammon",
           tsne_theta = 0.2,
           tsne_eta = 200,
           tsne_perplexity = 30,
           tsne_verbose = TRUE,
           tsne_max_iter = 500,
           umap_n_neighbors = 60,
           umap_n_components = 2, 
           umap_min_dist = 0.1
  ) {
    
    set.seed(279)
    #browser()
    requireNamespace("deldir")       #deldir function
    requireNamespace("Hmisc")        #ceil function
    requireNamespace("grDevices")    #chull function
    requireNamespace("splancs")      #csr function
    requireNamespace("sp")           #point.in.polygon function
    requireNamespace("conf.design")  #factorize functionScreenshot from 2022-10-26 16-15-44
    # requireNamespace("Rtsne")        
    # requireNamespace("umap")
    # requireNamespace("uwot")
    # requireNamespace("MASS")
    # requireNamespace("dplyr")
    # requireNamespace("scales")
    # requireNamespace("Matrix")
    # requireNamespace("stats")
    # requireNamespace("cluster")
    # requireNamespace("magrittr")
    # requireNamespace("kableExtra")
    # requireNamespace("FNN")
    
    if(quant_method=="kmedoids"){message(' K-Medoids: Run time for vector quantization using K-Medoids is very high for large number of clusters.')}
    # browser()
    dataset <- as.data.frame(dataset)
    # dataset <- as.data.frame(sapply(dataset[,1:length(dataset[1,])], as.numeric))
    dataset=data.frame(
      lapply(dataset, as.numeric),
      check.names = FALSE,
      row.names = rownames(dataset)
    )
    #browser()
    data_structure <- dim(dataset)
    
    ## Diagnose Function
    if(hvt_validation){
      if(train_validation_split_ratio >1 | train_validation_split_ratio <0){stop("The train_validation_split_ratio should be in range 0-1 ")}
      message(paste0("Check MAD parameter has been set to TRUE, the train data will be randomly split by the ratio of  ",train_validation_split_ratio*100,":",(1-train_validation_split_ratio)*100))
      
      set.seed(273)
      smp_size <- floor(train_validation_split_ratio * nrow(dataset))
      train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
      
      validation_data <- dataset[-train_ind, ]
      dataset <- dataset[train_ind, ]
      
    }
    
    
    if (normalize) {
      scaledata <- scale(dataset, scale = TRUE, center = TRUE)
      rownames(scaledata) <- rownames(dataset)
      
      mean_data <- attr(scaledata, "scaled:center")
      std_data <- attr(scaledata, "scaled:scale")
      
      scale_summary <- list(mean_data = mean_data, std_data = std_data)
      
      # flog.info("scaling is done")
    } else {
      scaledata <- as.matrix(dataset)
      rownames(scaledata) <- rownames(dataset)
      # flog.info("The data is not scaled as per the user requirement")
      if(!all(is.na(scale_summary))){
        scale_summary = scale_summary
      }else{
        scale_summary = NA
        # stop("Please pass scale_summary to the function or set normalize parameter to TRUE")
      }
      
    }
    
    polinfo <- hvqdata <- list()
    # browser()
    hvq_k <- hvq(
      scaledata,
      min_compression_perc = min_compression_perc,
      n_cells = n_cells,
      depth = depth,
      quant.err = quant.err,
      distance_metric = distance_metric,
      error_metric = error_metric,
      quant_method=quant_method
    )
    
    if(!is.na(min_compression_perc)){
      n_cells <- hvq_k$compression_summary$noOfCells
      depth <- 1
    }
    
    # flog.info("HVQ output is ready")
    hvqoutput <- hvq_k$summary
    
    gdata <- hvqoutput  #assign the output of hvq file to gdata
    #cleaning the data by deleting the rows containing NA's
    #gdata <- gdata[-which(is.na(gdata[, 5])), ]
    gdata <- hvqoutput[stats::complete.cases(hvqoutput), ]
    hvqdata <- gdata
    # flog.info("NA's are removed from the HVQ output")
    
    #to store hvqdata according to each level
    tessdata <- input.tessdata <- list()
    #stores sammon co-ordinates and segregated according to each level
    points2d <- list()
    #sammon datapoints to be stored in hierarchy
    rawdeldata <- list()
    #rawdeldata is transformed to be within its parent tile and stored in new_rawdeldata
    new_rawdeldata <- list()
    #contains the vertices of the parent polygon
    pol_info <- polygon_info <- list()
    #number of levels
    #browser()
    nlevel <- length(unique(gdata[, "Segment.Level"]))
    #verify if the transformed points are correct
    transpoints <- list()
    
    # New columns added to the dataset by the hvq function
    newcols <-  ncol(hvqoutput) - ncol(dataset)
    # Variable to store information on mapping
    par_map <- list()
    
    for (i in 1:nlevel) {
      #hvqdata segregated according to different levels
      tessdata[[i]] <- gdata[which(gdata[, "Segment.Level"]==i),]
      # tessdata[[i]] <- tessdata[[i]] %>% mutate(across(where(is.double), round, 8))
      # tessdata[[i]] <- tessdata[[i]] %>% mutate_if(is.double, round, 8)
    }
    
    # data to be used as input to sammon function
    # input.tessdata[[i]] <- tessdata[[i]][, (newcols+1): ncol(hvqoutput)]
    # d<-cmdscale()
    #  }
    #  }
    # # tessdata[[i]] <- round(tessdata[[i]], digits = 8)
    counter <- c(1:nlevel)
    
    
    dim_reduction <- function(x){
      data <- unique(tessdata[[x]][,(newcols +1):ncol(hvqoutput)])
      
      if (dim_reduction_method == "sammon"){
        return( projection.scale * MASS::sammon(stats::dist(data),niter = 10^5)$points)
      }else if (dim_reduction_method == "tsne"){
        set.seed(232)
        tsne_data <- (projection.scale * Rtsne::Rtsne(data,
                                                      theta = tsne_theta,
                                                      eta = tsne_eta,
                                                      perplexity = tsne_perplexity,
                                                      verbose = tsne_verbose,
                                                      max_iter = tsne_max_iter)$Y)
        rownames(tsne_data) <- rownames(tessdata[[x]])
        return(tsne_data)
      }else if (dim_reduction_method == "umap"){
        set.seed(232)
        return(projection.scale * umap::umap(data,
                                             n_neighbors = umap_n_neighbors,
                                             n_components = umap_n_components, 
                                             min_dist = umap_min_dist)$layout)
      }
    }
    start_time <- Sys.time()
    points2d <- lapply(counter,dim_reduction)
    end_time <-Sys.time()
    
    time_for_projection <- as.numeric(end_time - start_time)
    
    ## Function for Trustworthiness
    calculate_trustworthiness <- function(X, X_embedded, k = 5) {
      n <- nrow(X)
      # Compute the pairwise distance matrices
      dist_original <- as.matrix(dist(X))
      dist_embedded <- as.matrix(dist(X_embedded))
      # Compute ranks
      rank_original <- t(apply(dist_original, 1, rank))
      rank_embedded <- t(apply(dist_embedded, 1, rank))
      # Determine which points are in the top k in the embedded space but not in the original space
      U_k_matrix <- (rank_embedded <= k) & !(rank_original <= k)
      # Calculate the rank differences
      rank_differences <- (rank_original - k) * U_k_matrix
      # Sum all rank differences
      trustworthiness_sum <- sum(rank_differences[rank_differences > 0])
      # Normalize the trustworthiness score
      trustworthiness <- 1 - (2 / (n * k * (2 * n - 3 * k - 1)) * trustworthiness_sum)
      return(trustworthiness)
    }
    
    ## Function For Continuity
    calculate_continuity <- function(X, X_embedded, k = 5) {
      n <- nrow(X)
      # Compute the pairwise distance matrices
      dist_original <- as.matrix(dist(X))
      dist_embedded <- as.matrix(dist(X_embedded))
      # Compute ranks
      rank_original <- t(apply(dist_original, 1, rank))
      rank_embedded <- t(apply(dist_embedded, 1, rank))
      # Determine which points are in the top k in the original space but not in the embedded space
      U_k_matrix <- (rank_original <= k) & !(rank_embedded <= k)
      # Calculate the rank differences
      rank_differences <- (rank_embedded - k) * U_k_matrix
      # Sum all rank differences
      continuity_sum <- sum(rank_differences[rank_differences > 0])
      # Normalize the continuity score
      continuity <- 1 - (2 / (n * k * (2 * n - 3 * k - 1)) * continuity_sum)
      return(continuity)
    }
    
    ## RMSE
    calculate_normalized_rmse <- function(embeddingss) {
      
      # Ensure the original data is in matrix form
      if (is.list(embeddingss[["original"]])) {
        original_data <- do.call(cbind, embeddingss[["original"]])  # Convert list to matrix
      } else {
        original_data <- as.matrix(embeddingss[["original"]])  # Convert data frame to matrix if needed
      }
      
      # Replace the 'original' entry with the matrix
      embeddingss[["original"]] <- original_data
      
      # Check the number of points in the original and reduced data
      if (nrow(embeddingss[["original"]]) != nrow(embeddingss[["points_df"]])) {
        stop("The number of points in the original data and the reduced 2D data do not match.")
      }
      
      # Compute pairwise distances in the original space (3D)
      original_distances <- as.matrix(dist(embeddingss[["original"]], method = "euclidean"))
      
      # Compute pairwise distances in the reduced space (2D)
      reduced_distances <- as.matrix(dist(embeddingss[["points_df"]], method = "euclidean"))
      
      # Check if the distance matrices are conformable
      if (!all(dim(original_distances) == dim(reduced_distances))) {
        stop("The distance matrices are not conformable for RMSE calculation.")
      }
      
      # return(rmse_normalized)
      difference_matrix <- original_distances - reduced_distances
      
      # Square the differences
      squared_differences <- difference_matrix^2
      
      # Calculate the mean of the squared differences
      mean_squared_error <- mean(squared_differences)
      
      # Calculate the RMSE by taking the square root of the mean squared error
      rmse <- sqrt(mean_squared_error)
      
      return(rmse)
    }
    
    ###### Silhouette Score
    calculate_average_silhouette <- function(data, centers = 4, seed = 123) {
      
      set.seed(seed)
      
      # Apply k-means clustering
      kmeans_result <- kmeans(data, centers = centers)
      
      # Vectorized function to compute Silhouette score manually
      calculate_silhouette_vectorized <- function(data, clusters) {
        dist_matrix <- as.matrix(dist(data))
        
        # Compute the intra-cluster distance (a_i)
        a_i <- sapply(1:length(clusters), function(i) {
          cluster_points <- which(clusters == clusters[i])
          if (length(cluster_points) > 1) {
            mean(dist_matrix[i, cluster_points])
          } else {
            0
          }
        })
        
        # Compute the nearest-cluster distance (b_i)
        b_i <- sapply(1:length(clusters), function(i) {
          other_clusters <- unique(clusters)[unique(clusters) != clusters[i]]
          min(sapply(other_clusters, function(cluster) {
            cluster_points <- which(clusters == cluster)
            mean(dist_matrix[i, cluster_points])
          }))
        })
        
        # Calculate Silhouette scores
        sil_scores <- (b_i - a_i) / pmax(a_i, b_i)
        return(sil_scores)
      }
      
      # Calculate Silhouette scores
      silhouette_scores <- calculate_silhouette_vectorized(data, kmeans_result$cluster)
      
      # Calculate the average Silhouette score
      average_silhouette_score <- mean(silhouette_scores)
      
      return(average_silhouette_score)
    }
    
    ## Sammon's Stresss
    sammon_stress <- function(original_distances, projected_embeddings) {
      # Calculate pairwise distances for projected embeddings
      projected_distances <- dist(projected_embeddings)
      # Filter out zero distances to avoid division by zero
      mask <- original_distances > 0
      original_distances <- original_distances[mask]
      projected_distances <- projected_distances[mask]
      # Compute the numerator and denominator
      numerator <- sum(((original_distances - projected_distances) / original_distances) ^2)
      denominator <- sum(original_distances ^ 2)  # Usually just the sum of squared original distances
      # print(sum(original_distances))
      # print((sum(((original_distances - projected_distances) / original_distances) ^2)/sum(original_distances ^ 2)))
      stress <-  numerator/denominator
      return(stress)
    }
    
    ## KNN_retention function
    knn_retention <- function(high_dim_embeddings, low_dim_embeddings, k = 5) {
      # Convert embeddings to matrices (if not already)
      high_dim_embeddings <- as.matrix(high_dim_embeddings)
      low_dim_embeddings <- as.matrix(low_dim_embeddings)
      # Find k-nearest neighbors in high-dimensional space
      high_dim_nn <- FNN::knn.index(high_dim_embeddings, k = k)
      # Find k-nearest neighbors in low-dimensional space
      low_dim_nn <- FNN::knn.index(low_dim_embeddings, k = k)
      # Calculate overlap
      total_overlap <- 0
      n <- nrow(high_dim_embeddings)
      # Calculate total overlap without using a for loop
      total_overlap <- sum(sapply(1:n, function(i) {
        length(intersect(high_dim_nn[i, ], low_dim_nn[i, ]))
      }))
      # Calculate average retention
      avg_retention <- total_overlap / (n * k)
      return(avg_retention)
    }
    
    if(depth == 1){
      points2d_df = as.data.frame(points2d)
      new_tessdata = unique(tessdata[[1]][,(newcols +1):ncol(hvqoutput)])
      
      ## Trustworthiness
      trustworthiness <- calculate_trustworthiness(new_tessdata, points2d_df, k = 5)
      
      ## Continuity Score
      continuity_score <- calculate_continuity(new_tessdata, points2d_df, k = 5)
      
      ## RMSE Score
      embeddingss <- list(
        original = new_tessdata,  
        points_df = points2d_df
      )
      rmse_value<- calculate_normalized_rmse(embeddingss)
      # print(embeddingss)
      
      ## Silhouette Score
      average_silhouette_score <- calculate_average_silhouette(points2d_df)
      
      ## Sammon's Stress
      # Calculate pairwise distances in the original space
      original_distances <- dist(embeddingss$original)
      # Calculate Sammon's Stress 
      stress_score <- sammon_stress(original_distances, embeddingss$points_df)
      
      ## KNN Retention
      avg_retention_score <- knn_retention(embeddingss[["original"]], embeddingss[["points_df"]], k = 8)
      
      ## Creating the Metrics df with the values.
      metrics_df<- data.frame(
        `L1_Metrics` = c("Structure Preservation Metrics", "Structure Preservation Metrics", "Structure Preservation Metrics", "Distance Preservation Metrics", "Interpretive Quality Metrics", "Interpretive Quality Metrics","Computational Efficiency Metrics"),
        `L2_Metrics` = c("Trustworthiness", "Continuity", "Sammon's Stress", "RMSE", "Silhouette Score", "KNN Retention Score","Execution Duration(sec)"),
        Value = c(trustworthiness, continuity_score, stress_score, rmse_value, average_silhouette_score, avg_retention_score, round(time_for_projection,4))
        # stringsAsFactors = FALSE
      )
    }else{
      
    }
    
   
    
    for (i in 1:nlevel) {
      # #hvqdata segregated according to different levels
      # tessdata[[i]] <- gdata[which(gdata[, "Segment.Level"] == i), ]
      #
      # #data to be used as input to sammon function
      # input.tessdata[[i]] <- tessdata[[i]][, (newcols+1): ncol(hvqoutput)]
      #
      # #sammon function output is 2d coordinates which are saved level-wise
      # points2d[[i]] <- projection.scale * (MASS::sammon(stats::dist(unique(input.tessdata[[i]])),niter = 10^5,trace=FALSE)$points)
      #
      # #sammon datapoints grouped according to the hierarchy
      intermediate.rawdata <- list()
      rn = row.names(points2d[[i]])
      vec = as.integer(rn) - sum(n_cells ^ (0:(i - 1))) + 1
      
      # for(j in 1: length(unique(tessdata[[i]][, "Segment.Parent"]))) {
      index = 1
      for (j in unique(tessdata[[i]][, "Segment.Parent"])) {
        # print(((n_cells * (j - 1)) + 1): (j * n_cells))
        # intermediate.rawdata[[j]] <- cbind(points2d[[i]][((n_cells * (j - 1)) + 1): (j * n_cells), 1],
        #                                    points2d[[i]][((n_cells * (j - 1)) + 1): (j * n_cells), 2])
        if (any(dplyr::between(j - vec / n_cells, 0, 0.9999))) {
          intermediate.rawdata[[index]] <-
            cbind(points2d[[i]][rn[dplyr::between(j - vec / n_cells, 0, 0.9999)], 1],
                  points2d[[i]][rn[dplyr::between(j - vec /
                                                    n_cells, 0, 0.9999)], 2])
          index = index + 1
        }
      }
      rawdeldata[[i]] <- intermediate.rawdata
      
    }
    rm(intermediate.rawdata)
    # flog.info("Sammon points for all the levels have been calculated")
    
    #new_rawdeldata contains the transformed points of rawdeldata
    new_rawdeldata[[1]] <- rawdeldata[[1]]
    
    #deldat1 is the output of the deldir::deldir function and contains the tessellation information
    deldat1 <- deldat2 <- list()
    #deldir function of deldir package gives the tessellations output
    deldat2[[1]] <- deldir::deldir(new_rawdeldata[[1]][[1]][, 1],
                                   new_rawdeldata[[1]][[1]][, 2])
    deldat1[[1]] <- deldat2
    rm(deldat2)
    # flog.info("Tessellations are calculated for the first level")
    #plotting the tessellation
    #plot(deldat1[[1]][[1]], wlines = "tess", lty = 1, lwd = 4)
    
    #polygon_info stores parent tile vertices information
    #polygon_info is the modified tile.list output except for first level.
    pol_info[[1]] <- deldir::tile.list(deldat1[[1]][[1]])
    
    #loop throught the polygon and  add the row name as id variable
    # row names of info in rawdeldata is preserved from the hvqoutput and this row name ties the rawdeldata back to the hierarchy
    # in the hvqoutput dataset
    pol_info[[1]] <- mapply(function(info, id) {
      info$id = as.numeric(id)
      info <-
        append(info, as.list(gdata[row.names(gdata) == id, c('Segment.Level', 'Segment.Parent', 'Segment.Child')]))
      return(info)
    }, pol_info[[1]], as.list(row.names(new_rawdeldata[[1]][[1]])), SIMPLIFY = FALSE)
    
    polygon_info[[1]] <- pol_info
    rm(pol_info)
    par_tile_indices <- n_par_tile <- list()
    par_tile_indices[[1]] <-
      unique(tessdata[[1]][, "Segment.Parent"])
    n_par_tile[[1]] <-
      length(unique(tessdata[[1]][, "Segment.Parent"]))
    if (nlevel < 2) {
      polinfo <- polygon_info
      
      fin_out <- list()
      
      fin_out[[1]] <- deldat1
      fin_out[[2]] <- polinfo
      fin_out[[3]] <- hvq_k
      fin_out[[6]] <- hvq_k
      fin_out[[3]][['scale_summary']] <- scale_summary
      level = 1
      level_names <- list()
      level_names[[level]] <- fin_out[[2]][[level]] %>% purrr::map(~ {
        this <- .
        element <- this[[1]]
        return(element$Segment.Parent)
      }) %>% unlist()
      
      names(fin_out[[2]][[level]]) <- level_names[[level]]
      
      ####### Adding Code for Diagnosis Plot
      diag_Suggestion=NA
      fin_out[[4]] = NA
      fin_out[[5]] <- NA
      if(diagnose){
        # fin_out[[4]] = NA
        diag_list =  diagPlot(hvt.results = fin_out,
                              data = dataset,
                              level = depth,
                              quant.err = quant.err,
                              distance_metric=distance_metric,
                              error_metric=error_metric
        )
        diag_Suggestion =  diagSuggestion(hvt.results = fin_out,
                                          data = dataset,
                                          level = depth,
                                          quant.err = quant.err,
                                          distance_metric=distance_metric,
                                          error_metric=error_metric
        )
        fin_out[[4]] = diag_list
        
      }
      
      ####### Adding Code for MAD Plot  
      # browser()
      if(hvt_validation){
        ####### MAD Plot ################
        
        predictions_validation = list()
        predictions_validation <- scoreHVT(
          dataset = validation_data,
          hvt.results.model=fin_out,
          child.level = depth,
          line.width = c(0.6, 0.4, 0.2),
          color.vec = c("#141B41", "#6369D1", "#D8D2E1"),
          mad.threshold = quant.err,
          distance_metric = distance_metric,
          error_metric = error_metric
        )
        
        mad_plot=madPlot(predictions_validation)
        fin_out[[5]] <- list()
        fin_out[[5]][["mad_plot"]] <- mad_plot
      }
      ### Model Info
      Dataset <- paste0(data_structure[1] ," Rows & ", data_structure[2], " Columns")
      
      model_train_date=Sys.time()
      input_parameters = list(
        input_dataset = Dataset,
        n_cells = n_cells,
        depth = depth,
        quant.err = quant.err,
        normalize = normalize,
        distance_metric = distance_metric[1],
        error_metric = error_metric[1],
        quant_method = quant_method[1],
        diagnose = diagnose,
        projection.scale = projection.scale,
        hvt_validation = hvt_validation,
        train_validation_split_ratio = train_validation_split_ratio
      )
      
    
  
      if(depth == 1){
        
        validation_result=diag_Suggestion
        fin_out[["model_info"]]=list(model_train_date=model_train_date,
                                     type="hvt_model",
                                     input_parameters = input_parameters,
                                     validation_result = validation_result,
                                     distance_measures = metrics_df)
      }else{
        validation_result=diag_Suggestion
        fin_out[["model_info"]]=list(model_train_date=model_train_date,
                                     type="hvt_model",
                                     input_parameters = input_parameters,
                                     validation_result = validation_result)
      }
      

      # validation_result=diag_Suggestion
      # fin_out[["model_info"]]=list(model_train_date=model_train_date,
      #                              type="hvt_model",
      #                              input_parameters = input_parameters,
      #                              validation_result = validation_result,
      #                              distance_measures = metrics_df)
      
      # generating cell ID using getCellId
      fin_out[[3]]$summary <- getCellId(hvt.results=fin_out)
      fin_out[[3]]$summary <- fin_out[[3]]$summary %>% select(Segment.Level, Segment.Parent, Segment.Child, n, Cell.ID, Quant.Error, colnames(dataset))
      
      return(fin_out)
      
    } else { 
      for (i in 2:nlevel) {
        new_rawdeldata[[i]] <- list()
        par_tile_indices[[i]] <-
          unique(tessdata[[i]][, "Segment.Parent"])
        n_par_tile[[i]] <-
          length(unique(tessdata[[i]][, "Segment.Parent"]))
        par_map[[i - 1]] <- list()
        
        for (tileIndex in 1:n_par_tile[[(i - 1)]]) {
          # print(tileIndex)
          #a chunk of hvqdata which contains the rows corresponding to a particular parent tile
          gi_par_tiles <-
            par_tile_indices[[i]][intersect(which((par_tile_indices[[i]] / n_cells) <= par_tile_indices[[(i - 1)]][tileIndex]),
                                            which((par_tile_indices[[(i - 1)]][tileIndex] - 1) < (par_tile_indices[[i]] / n_cells)))]
          gidata <-
            tessdata[[i]][which(tessdata[[i]][, "Segment.Parent"] %in% gi_par_tiles),]
          par_map[[i - 1]][[par_tile_indices[[(i - 1)]][tileIndex]]] <-
            gi_par_tiles
          
          #datapoints corresponding to a particular parent tile
          rawdeldati <-
            rawdeldata[[i]][intersect(which((par_tile_indices[[i]] / n_cells) <= par_tile_indices[[(i - 1)]][tileIndex]),
                                      which((par_tile_indices[[(i - 1)]][tileIndex] - 1) < (par_tile_indices[[i]] / n_cells)))]
          if (nrow(gidata) != 0) {
            #transformation of rawdeldata such that they are inside the parent tile and output is stored in new_rawdeldata
            transpoints <-
              DelaunayInfo(gidata, polygon_info[[(i - 1)]][[tileIndex]], rawdeldati, n_cells)
            if (all(transpoints != "-1")) {
              new_rawdeldata[[i]] <- append(new_rawdeldata[[i]], transpoints)
            } else{
              deldat1[i] <- 0
              polinfo <<- polygon_info
              #flog.warn("Projection is not scaled enough and the polygon is very small to perform transformation")
              return(deldat1)
              #return("Projection is not scaled enough and the polygon is very small to perform transformation")
            }
            
          }
        }
        # flog.info("Sammon points of level %s are transformed", i)
        
        deldat2 <- list()
        par_tile_polygon <- list()
        #?
        for (tileNo in 1:n_par_tile[[i]]) {
          # print(tileNo)
          #modulus operation for the last index in polygon_info
          if (((par_tile_indices[[i]][tileNo]) %% n_cells)) {
            last_index <- (par_tile_indices[[i]][tileNo] %% n_cells)
          } else{
            last_index <- n_cells
          }
          
          #divide to get the parent tile
          sec_index <-
            Hmisc::ceil(par_tile_indices[[i]][tileNo] / n_cells)
          
          deldat2[[tileNo]] <-
            deldir::deldir(new_rawdeldata[[i]][[tileNo]][, 1],
                           new_rawdeldata[[i]][[tileNo]][, 2],
                           rw = c(
                             range(polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$x) - c(0.5,-0.5),
                             range(polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$y) - c(0.5,-0.5)
                           ))
          
          #CORRECT ROW NAMES
          deldat2[[tileNo]]$rownames.orig <-
            as.numeric(row.names(new_rawdeldata[[i]][[tileNo]]))
          
          #constructing parent polygons
          par_tile_polygon[[tileNo]] <-
            matrix(
              c(polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$x,
                polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$y),
              ncol = 2,
              byrow = FALSE
            )
          #correct the tessellations
          cur_dirsgs <- deldat2[[tileNo]]$dirsgs
          cur_tile <-
            polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]
          cur_polygon <- par_tile_polygon[[tileNo]]
          verify_dirsgs <- Corrected_Tessellations(cur_dirsgs,
                                                   cur_tile,
                                                   cur_polygon)
          
          #polygon is sufficient to calculate next level tessellations inside this
          if (all(verify_dirsgs[, 1:8] != "-1")) {
            deldat2[[tileNo]]$dirsgs <- verify_dirsgs
            # flog.info("Tessellations for level %s is calculated", i)
          } else{
            deldat1[[i]] <- 0
            #flog.warn("Projection is not scaled enough and the polygon is very small to perform transformation")
            polinfo <<- polygon_info
            return(deldat1)
            #return("Projection is not scaled enough to perform tessellations for the next level")
          }
          
          #delete lines with both the points identical
          new_dirsgs <- deldat2[[tileNo]]$dirsgs
          del_rows <- 0
          for (j in 1:nrow(new_dirsgs)) {
            if (round(new_dirsgs[j, "x1"], 6) == round(new_dirsgs[j, "x2"], 6) &&
                round(new_dirsgs[j, "y1"], 6) == round(new_dirsgs[j, "y2"], 6)) {
              del_rows <- c(del_rows, j)
            }
          }
          if (length(del_rows) > 1) {
            new_dirsgs <- new_dirsgs[-del_rows, ]
          }
          deldat2[[tileNo]]$dirsgs <- new_dirsgs
          # flog.info("Line with same endpoints are deleted")
        }
        
        deldat1[[i]] <- deldat2
        
        polygon_info[[i]] <- list()
        #polygon information to correct the polygons
        for (parentIndex in 1:n_par_tile[[i]]) {
          polygon_info[[i]][[parentIndex]] <-
            suppressMessages(deldir::tile.list(deldat1[[i]][[parentIndex]]))
          # Add id corresponding to each segment from rownames.orig information
          polygon_info[[i]][[parentIndex]] <-
            mapply(function(info, id) {
              info$id = as.numeric(id)
              info <-
                append(info, as.list(gdata[row.names(gdata) == id, c('Segment.Level', 'Segment.Parent', 'Segment.Child')]))
              return(info)
            },
            polygon_info[[i]][[parentIndex]],
            as.list(deldat1[[i]][[parentIndex]]$rownames.orig),
            SIMPLIFY = FALSE)
        }
        
        #to delete the points which are outside the parent tile
        
        for (parentIndex in 1:length(polygon_info[[i]])) {
          for (tileIndex in 1:length(polygon_info[[i]][[parentIndex]])) {
            tile <- polygon_info[[i]][[parentIndex]][[tileIndex]]
            check_dirsgs <- deldat1[[i]][[parentIndex]]$dirsgs
            if (nrow(check_dirsgs) != 0 && length(tile$x) != 0) {
              polygon_info[[i]][[parentIndex]][[tileIndex]] <-
                Delete_Outpoints (tile, check_dirsgs)
            }
          }
        }
        # flog.info("Endpoints of tessellation lines which are outside parent polygon are deleted")
        
        for (parentIndex in 1:n_par_tile[[i]]) {
          #modulo operation to obtain the last index
          if (((par_tile_indices[[i]][parentIndex]) %% n_cells)) {
            last_index <- (par_tile_indices[[i]][parentIndex] %% n_cells)
          } else{
            last_index <- n_cells
          }
          #divide to get second index
          sec_index <-
            Hmisc::ceil(par_tile_indices[[i]] / n_cells)[parentIndex]
          
          polygon_info[[i]][[parentIndex]] <-
            Add_boundary_points(polygon_info[[i]][[parentIndex]],
                                polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]],
                                new_rawdeldata[[i]][[parentIndex]])
        }
        # flog.info("Vertices of parent tile are added to appropriate child tile")
      }
      
      polinfo <- polygon_info
      
      # par_map <- reshape2::melt(par_map)
      # colnames(par_map) <- c('Child','Parent','Level')
      # par_map <- par_map %>% mutate(ChildNo = (Parent-1)*n_cells+Child)
      
      fin_out <- list()
      
      fin_out[[1]] <- deldat1
      fin_out[[2]] <- polinfo
      fin_out[[3]] <- hvq_k
      fin_out[[6]] <- hvq_k
      fin_out[[3]][['scale_summary']] <- scale_summary
      # fin_out[[4]] <- par_map
      level_names <- list()
      
      for (level in 1:nlevel) {
        level_names[[level]] <- fin_out[[2]][[level]] %>% purrr::map(~ {
          this <- .
          element <- this[[1]]
          return(element$Segment.Parent)
        }) %>% unlist()
        
        names(fin_out[[2]][[level]]) <- level_names[[level]]
      }
      
      # fin_out <- correctedBoundaries(fin_out,nlevel)
      
      emptyParents  <- depth - length(fin_out[[2]])
      for (i in 1:emptyParents) {
        fin_out[[2]][[length(fin_out[[2]]) + 1]] <- list()
      }
      
      # ###### Adding Code for Diagnosis Plot
      diag_Suggestion =NA
      fin_out[[4]] = NA
      fin_out[[5]] <- NA
      
      if(diagnose){
        fin_out[[4]] = NA
        diag_list = diagPlot(hvt.results = fin_out,
                             data = dataset,
                             level = depth,
                             quant.err = quant.err,
                             distance_metric=distance_metric,
                             error_metric=error_metric
        )
        
        diag_Suggestion = diagSuggestion(hvt.results = fin_out,
                                         data = dataset,
                                         level = depth,
                                         quant.err = quant.err,
                                         distance_metric=distance_metric,
                                         error_metric=error_metric
        )
        fin_out[[4]] = diag_list
        
      }
      
      #  fin_out[[5]] <- NA
      
      ####### Adding Code for MAD Plot
      if(hvt_validation){
        # browser()
        
        ####### MAD Plot ################
        predictions_validation = list()
        predictions_validation <- scoreHVT(
          data = validation_data,
          hvt.results.model=fin_out,
          child.level = depth,
          line.width = c(0.6, 0.4, 0.2),
          color.vec = c("#141B41", "#6369D1", "#D8D2E1"),
          mad.threshold = quant.err,
          distance_metric = distance_metric,
          error_metric = error_metric
        )
        
        mad_plot=madPlot(predictions_validation)
        fin_out[[5]] <- list()
        fin_out[[5]][["mad_plot"]] <- mad_plot
      }
      
      
      ### Model Info
      Dataset <- paste0(data_structure[1] ," Rows & ", data_structure[2], " Columns")
      
      model_train_date=Sys.time()
      input_parameters = list(
        input_dataset = Dataset,
        n_cells = n_cells,
        depth = depth,
        quant.err = quant.err,
        normalize = normalize,
        distance_metric = distance_metric[1],
        error_metric = error_metric[1],
        quant_method = quant_method[1],
        diagnose = diagnose,
        projection.scale = projection.scale,
        hvt_validation = hvt_validation,
        train_validation_split_ratio = train_validation_split_ratio,
        dim_reduction_method = dim_reduction_method[1],
        tsne_theta = tsne_eta[1],
        tsne_eta = tsne_eta[1],
        tsne_perplexity = tsne_perplexity[1],
        tsne_verbose = tsne_verbose[1],
        tsne_max_iter = tsne_max_iter[1],
        umap_n_neighbors = umap_n_neighbors[1],
        umap_n_components = umap_n_components[1],
        umap_min_dist = umap_min_dist[1]
        
      )
      
      validation_result=diag_Suggestion
      fin_out[["model_info"]]=list(model_train_date=model_train_date,
                                   type="hvt_model",
                                   input_parameters=input_parameters,
                                   validation_result=validation_result
      )
      
      # generating cell ID using getCellId
      fin_out[[3]]$summary <- getCellId(hvt.results=fin_out)
      fin_out[[3]]$summary <- fin_out[[3]]$summary %>% select(Segment.Level, Segment.Parent, Segment.Child, n, Cell.ID, Quant.Error, colnames(dataset))
      
      return(fin_out)
      
    }
    
    
  }

