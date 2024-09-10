#' @name clustHVT
#' @title Performing Hierarchical Clustering Analysis
#' @description This is the main function to perform hierarchical clustering
#' analysis which determines optimal number of clusters, perform AGNES clustering
#' and plot the 2D cluster hvt plot.
#' @param data Data frame. A data frame intended for performing hierarchical clustering analysis.
#' @param trainHVT_results List.  A list object which is obtained as a result of trainHVT function.
#' @param scoreHVT_results List. A list object which is obtained as a result of scoreHVT function.
#' @param clusters_k Character.  A parameter that specifies the number of clusters for the provided data. 
#' The options include “champion,” “challenger,” or any integer between 1 and 20. 
#' Selecting “champion” will use the highest number of clusters recommended by the ‘NbClust’ function,
#' while “challenger” will use the second-highest recommendation. If a numerical value from 1 to 20
#' is provided, that exact number will be used as the number of clusters.
#' @param indices Character. The indices used for determining the optimal number of clusters in NbClust function.
#'  By default it uses 20 different indices.
#' @param clustering_method Character. The method used for clustering in both NbClust and hclust function. Defaults to ‘ward.D2’.
#' @return A list object that contains the hierarchical clustering results.
#' \item{[[1]] }{Summary of k suggested by all indices with plots} 
#' \item{[[2]] }{A dendogram plot with the selected number of clusters} 
#' \item{[[3]] }{A 2D Cluster HVT Plotly visualization that colors cells according to clusters derived from AGNES clustering results. 
#' It is interactive, allowing users to view cell contents by hovering over them}
#' @author Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords Clustering_Analysis
#' @include clusterPlot.R
#' @importFrom utils data head tail
#' @examples 
#'data("EuStockMarkets")
#'dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#'                      DAX = EuStockMarkets[, "DAX"],
#'                    SMI = EuStockMarkets[, "SMI"],
#'                      CAC = EuStockMarkets[, "CAC"],
#'                      FTSE = EuStockMarkets[, "FTSE"])
#'rownames(EuStockMarkets) <- dataset$date
#'hvt.results<- trainHVT(dataset,n_cells = 60, depth = 1, quant.err = 0.1,
#'                       distance_metric = "L1_Norm", error_metric = "max",
#'                       normalize = TRUE,quant_method = "kmeans")
#'scoring <- scoreHVT(dataset, hvt.results, analysis.plots = TRUE, names.column = dataset[,1])
#'centroid_data <- scoring$centroidData
#'hclust_data_1 <- centroid_data[,2:3]
#'clust.results <- clustHVT(data = hclust_data_1, 
#'                          trainHVT_results = hvt.results,
#'                          scoreHVT_results = scoring, 
#'                          clusters_k = 'challenger', indices = 'hartigan')
#' @export clustHVT



clustHVT <- function(data, trainHVT_results, scoreHVT_results, clustering_method = 'ward.D2',
                     indices, clusters_k = "champion") {
  requireNamespace('NbClust')
  
  hclust_data <- data
  
  results_df <- c()

  
  # Function to run NbClust and extract results
  print_nbclust_results <- function(hclust_data, indices) {
    get_nbclust_result <- function(index) {
      res <- NbClust::NbClust(hclust_data, method = clustering_method, index = index)$Best.nc
      if (!is.null(res)) {
        return(data.frame(Index = index, 
                          Number_clusters = res["Number_clusters"], 
                          Value_Index = res["Value_Index"]))
      } else {
        return(data.frame(Index = index, 
                          Number_clusters = NA, 
                          Value_Index = NA))
      }
    }
    
    results_list <- lapply(indices, get_nbclust_result)
    results_df <<- do.call(rbind, results_list)
   
    cluster_summary <- table(results_df$Number_clusters)
    cat("** Among all indices:\n")
    lapply(seq_along(cluster_summary), function(i) {
      cat(sprintf("** * %d proposed %d as the best number of clusters\n", 
                  cluster_summary[i], as.numeric(names(cluster_summary)[i])))})
    
    
    best_clusters <- as.numeric(names(cluster_summary)[which.max(cluster_summary)])
    cat("*******************************************************************\n")
    cat("***** Conclusion *****\n")
    cat(sprintf("** * According to the majority rule, the best number of clusters is %d\n", best_clusters))
    cat("*******************************************************************\n")
    cat("                         Index     Number_clusters    Value_Index\n")
    cat("-------------------------------------------------------------------\n")
    apply(results_df, 1, function(row) {
      cat(sprintf("%24s  %15.4f  %15.4f\n", 
                  row["Index"], 
                  as.numeric(row["Number_clusters"]), 
                  as.numeric(row["Value_Index"])))
    })
    cat("*******************************************************************\n")
  }
  
  
  # Run NbClust
  indices <- c("kl", "ch", "hartigan", "cindex", "db", "silhouette", "ratkowsky", "ball","hubert","dindex",
               "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "sdindex", "sdbw")
  print_nbclust_results(hclust_data, indices)

  #to fetch champion and challenger
  cluster_summary <- table(results_df$Number_clusters)
  
  # Determine the number of clusters based on user input
  if (clusters_k == "champion") {
    no_of_clusters <- as.numeric(names(cluster_summary)[which.max(cluster_summary)])
  } else if (clusters_k == "challenger") {
    sorted_summary <- sort(cluster_summary, decreasing = TRUE)
    no_of_clusters <- as.numeric(names(sorted_summary)[2])
  } else if (is.numeric(clusters_k) && clusters_k >= 1 && clusters_k <= 20) {
    no_of_clusters <- clusters_k
  } else {
    stop("Invalid input for clusters_k. Use 'champion', 'challenger', or a numeric value between 1 and 20.")
  }
#browser()  
  # Perform hierarchical clustering
  hc <- stats::hclust(dist(hclust_data), clustering_method)
  clusters <- stats::cutree(hc, k = no_of_clusters)
  

  # Replace the existing plot_dendrogram function with this:
  plot_dendrogram <- function(hc_1, no_of_clusters_1) {
    function() {
      plot(hc_1, xlab = "Clusters", ylab = "Distance", sub ="")
      stats::rect.hclust(hc_1, k = no_of_clusters_1, border = grDevices::rainbow(no_of_clusters_1))
    }
  }
  
  a <- plot_dendrogram(hc_1 = hc, no_of_clusters_1 = no_of_clusters)
  

  
  # Prepare data for clusterPlotly
  cluster_data <- scoreHVT_results$centroidData %>% 
    dplyr::select("Cell.ID", "names.column") %>%
    mutate(clusters =  clusters)
  
  b <- clusterPlot(dataset= cluster_data, hvt.results  = trainHVT_results, domains.column = "clusters" )
    
  output_list <- list(
    hc = hc,
    clusters = clusters,
    cluster_data =cluster_data,
    dendogram = a,
    clusterplot = b
  )
  
  return(output_list)
}
