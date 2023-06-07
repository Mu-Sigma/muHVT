#' removeOutliers
#'
#' Remove identified outlier cell(s) from the dataset
#'
#' This function is used to remove the identified outlier cell(s) from the dataset.
#' It is recommended to run the \code{HVT} function before running this function. It takes input in the form 
#' of cell number of the outlier cell(s) identified using the output of the \code{HVT} function and
#' the compressed map (hvt_mapA) generated using the \code{HVT} function. The output of this function is
#' a list of two items: a new map having the data of removed outlier cell(s) and the subset of dataset without outliers.
#' 
#' @param outlier_cells Vector. A vector with the cell number of the identified outliers
#' @param hvt_results List. A list having the results of the compressed map i.e. output of \code{HVT} function
#' 
#' @return A list of two items: a map having the data of removed outlier cells and the subset of  the dataset without outlier(s) which
#' has to be passed as input argument to \code{HVT} function to generate another map
#' \item{[[1]] }{Dataframe. Information about the removed outlier cell(s)}
#' \item{[[2]] }{Dataframe. Subset of dataset without the outlier cell(s)}
#' 
#' @author Shantanu Vaidya <shantanu.vaidya@@mu-sigma.com>
#' @seealso \code{\link{HVT}} \cr \code{\link{mlayerHVT}}
#' @importFrom magrittr %>%
#' @importFrom plyr rbind.fill
#' @examples
#' data(USArrests)
#' hvt_mapA <- list()
#' hvt_mapA <- HVT(USArrests, min_compression_perc = 70, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE,
#'                    quant_method="kmeans")
#' plotHVT(hvt_mapA, line.width = c(0.8), color.vec = c('#141B41'), 
#'         maxDepth = 1)
#'         
#' identified_outlier_cells <- c(2, 10)
#' output_list <- removeOutliers(identified_outlier_cells, hvt_mapA)
#' hvt_mapB <- output_list[[1]]
#' dataset_without_outliers <- output_list[[2]]
#'
#' @export removeNovelty


removeNovelty <-
  function (outlier_cells, hvt_results)
   {
    #browser()
    remove_outlier_cells <- as.vector(outlier_cells)
    
    if (length(remove_outlier_cells) == 0){
      print("No outlier cells were removed from the dataset.")
      
      hvt_results_cells <- hvt_results[[3]]$clusters
      scaled_data <- data.frame()
      temp_df <- data.frame()
      
      for(i in 1:length(hvt_results_cells)){
        temp_df <-  as.data.frame(hvt_results_cells[[i]])
        temp_df$Row.Number <- row.names(hvt_results_cells[[i]])
        scaled_data <- rbind.fill(scaled_data, temp_df)
      }
      row.names(scaled_data) <- scaled_data$Row.Number
      scaled_data <- select(scaled_data, -c(Row.Number))
      
      return(list(
        hvt_mapB = NA,
        dataset_without_outliers = scaled_data
      ))
      
    }else{
      
    hvt_results_cells <- hvt_results[[3]]$clusters
    scaled_data <- data.frame()
    temp_df <- data.frame()
    
    for(i in 1:length(hvt_results_cells)){
      temp_df <-  as.data.frame(hvt_results_cells[[i]])
      temp_df$Cell.Number <- i
      temp_df$Row.Number <- row.names(hvt_results_cells[[i]])
      scaled_data <- rbind.fill(scaled_data, temp_df)
    }
    row.names(scaled_data) <- scaled_data$Row.Number
    
    Cell.ID.mapping <- hvt_results[[3]]$summary[, c("Segment.Child", "Cell.ID")]
    
    scaled_data <- merge(x = scaled_data, y = Cell.ID.mapping, by.x = c("Cell.Number"), by.y = c("Segment.Child"))
    
    outlier_rows_df <- scaled_data %>% dplyr::filter(Cell.Number %in% identified_Novelty_cells)
    removed_outlier_rows <- as.vector(outlier_rows_df$Row.Number)
    outliers_data_scaled <- select(outlier_rows_df, -c(Row.Number))
    outliers_data_scaled <- outliers_data_scaled[,c(ncol(outliers_data_scaled),1:(ncol(outliers_data_scaled)-1))]
  
    `%notin%` <- Negate(`%in%`)
    dataset_without_outliers_scaled <- scaled_data %>% dplyr::filter(Cell.Number %notin% remove_outlier_cells)
    row.names(dataset_without_outliers_scaled) <- dataset_without_outliers_scaled$Row.Number
    dataset_without_outliers_scaled <- select(dataset_without_outliers_scaled, -c(Row.Number, Cell.Number, Cell.ID))
    dataset_without_outliers_scaled <- dataset_without_outliers_scaled[ order(as.numeric(row.names(dataset_without_outliers_scaled))), ]
    
    print(paste0(c("The following cell(s) have been removed as outliers from the dataset: ", remove_outlier_cells), collapse = " "))

    
    return(list(
      hvt_mapB = outliers_data_scaled,
      dataset_without_outliers = dataset_without_outliers_scaled
      ))
    }
}
