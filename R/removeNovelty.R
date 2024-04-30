#' @name removeNovelty
#' @title Remove identified novelty cell(s) 
#' @description This function is used to remove the identified novelty cells.
#' @param outlier_cells Vector. A vector with the cell number of the identified novelty
#' @param hvt_results List. A list having the results of the compressed map i.e. output of \code{trainHVT} function
#' 
#' @return A list of two items
#' \item{[[1]] }{Dataframe of novelty cell(s)}
#' \item{[[2]] }{Dataframe without the novelty cell(s) from the dataset used in model training}
#' 
#' @author Shantanu Vaidya <shantanu.vaidya@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} \cr \code{\link{scoreLayeredHVT}}
#' @importFrom magrittr %>%
#' @importFrom plyr rbind.fill
#' @examples
#' data("EuStockMarkets")
#' hvt.results <- trainHVT(EuStockMarkets, n_cells = 60, depth = 1, quant.err = 0.1, 
#'                        distance_metric = "L1_Norm", error_metric = "max",
#'                        normalize = TRUE,quant_method="kmeans")
#' identified_Novelty_cells <<- c(2, 10)
#' output_list <- removeNovelty(identified_Novelty_cells, hvt.results) 
#' data_with_novelty <- output_list[[1]]
#' data_without_novelty <- output_list[[2]]                      
#' @keywords Novelty_or_Outliers
#' @export removeNovelty


removeNovelty <- function(outlier_cells, hvt_results) {
  remove_outlier_cells <- as.vector(outlier_cells)

  if (length(remove_outlier_cells) == 0) {
    message("No outlier cells were removed from the dataset.")
    hvt_results_cells <- hvt_results[[3]]$clusters
    scaled_data <- do.call(rbind, lapply(hvt_results_cells, function(x) {
      temp_df <- as.data.frame(x)
      temp_df$Row.Number <- row.names(x)
      temp_df
    }))
    row.names(scaled_data) <- scaled_data$Row.Number
    scaled_data <- select(scaled_data, -c(Row.Number))

    return(list(hvt_mapB = NA, dataset_without_outliers = scaled_data))
  } else {
    hvt_results_cells <- hvt_results[[3]]$clusters
    scaled_data <- do.call(rbind, lapply(seq_along(hvt_results_cells), function(i) {
      temp_df <- as.data.frame(hvt_results_cells[[i]])
      temp_df$Cell.Number <- i
      temp_df$Row.Number <- row.names(hvt_results_cells[[i]])
      temp_df
    }))
    row.names(scaled_data) <- scaled_data$Row.Number
    Cell.ID.mapping <- hvt_results[[3]]$summary[, c("Segment.Child", "Cell.ID")]
    scaled_data <- merge(x = scaled_data, y = Cell.ID.mapping, by.x = c("Cell.Number"), by.y = c("Segment.Child"))
    outlier_rows_df <- scaled_data %>% dplyr::filter(Cell.Number %in% identified_Novelty_cells)
    removed_outlier_rows <- as.vector(outlier_rows_df$Row.Number)
    outliers_data_scaled <- select(outlier_rows_df, -c(Row.Number))
    outliers_data_scaled <- outliers_data_scaled[, c(ncol(outliers_data_scaled), 1:(ncol(outliers_data_scaled) - 1))]
    `%notin%` <- Negate(`%in%`)
    dataset_without_outliers_scaled <- scaled_data %>% dplyr::filter(Cell.Number %notin% remove_outlier_cells)
    row.names(dataset_without_outliers_scaled) <- dataset_without_outliers_scaled$Row.Number
    dataset_without_outliers_scaled <- select(dataset_without_outliers_scaled, -c(Row.Number, Cell.Number, Cell.ID))
    dataset_without_outliers_scaled <- dataset_without_outliers_scaled[order(as.numeric(row.names(dataset_without_outliers_scaled))), ]
    message(paste0(c("The following cell(s) have been removed as novelties from the dataset: ", remove_outlier_cells), collapse = " "))

    return(list(hvt_mapB = outliers_data_scaled, dataset_without_outliers = dataset_without_outliers_scaled))
  }
}