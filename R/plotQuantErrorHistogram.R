#' @name plotQuantErrorHistogram
#' @title Make the diagnostic plots for hierarchical voronoi tessellations model.
#' @description This is the function that produces histograms displaying the distribution of Quantized Error (QE) values 
#' for both train and test datasets, highlighting mean values with dashed lines for quick evaluation.
#' @param hvt.results List. A list of hvt.results obtained from the trainHVT
#' function.
#' @param hvt.scoring List. A list of hvt.scoring obtained from the scoreHVT
#' function.
#' @return Returns the ggplot object containing the Quantized Error distribution plots for the given HVT results and scores.
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords Diagnostics_or_Validation
#' @importFrom magrittr %>%
#' @import ggplot2 
#' @import patchwork
#' @examples
#' data("EuStockMarkets")
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#'                      DAX = EuStockMarkets[, "DAX"],
#'                      SMI = EuStockMarkets[, "SMI"],
#'                      CAC = EuStockMarkets[, "CAC"],
#'                      FTSE = EuStockMarkets[, "FTSE"])
#' #adding this step especially for this function
#' rownames(EuStockMarkets) <- dataset$date
#' #Split in train and test
#' train <- EuStockMarkets[1:1302, ]
#' test <- EuStockMarkets[1303:1860, ]
#' hvt_summary<- trainHVT(train,n_cells = 15, depth = 1, quant.err = 0.2,
#'                       distance_metric = "L1_Norm", error_metric = "mean",
#'                       projection.scale = 10, normalize = TRUE,seed = 123,
#'                       quant_method = "kmeans")
#' scoring <- scoreHVT(test, hvt_summary, child.level = 2, mad.threshold = 0.2)
#' plotQuantErrorHistogram(hvt_summary, scoring)  
#' @export plotQuantErrorHistogram
#' 

plotQuantErrorHistogram <- function(hvt.results, hvt.scoring) {
  # require(patchwork)
  #browser()
  val <- hvt.results[[3]][["max_QE"]] %>%
    unlist() %>%
    as.numeric()
  p1 <- ggplot2::ggplot() +
    ggplot2::aes(x = val) +
    ggplot2::geom_histogram(fill = "midnightblue", colour = "white", alpha = 0.75) +
    ggplot2::theme_minimal() +
    ggplot2::geom_vline(xintercept = mean(val[val > 0.00000001]), linetype = "dashed") +
    ggplot2::geom_vline(xintercept = mean(val), colour = "red", linetype = "dashed") +
    ggplot2::ggtitle("1.Max QE Distribution: Train ") +
    ggplot2::xlab("Max QE values for cells: Train Data")
  # +
  #   ggtitle("Max QE Distribution (Train)")



  pred_val <- hvt.scoring[["scoredPredictedData"]]
  p2 <- ggplot2::ggplot() +
    ggplot2::aes(x = pred_val$Quant.Error) +
    ggplot2::geom_histogram(fill = "midnightblue", colour = "white", alpha = 0.75) +
    ggplot2::theme_minimal() +
    ggplot2::geom_vline(xintercept = mean(pred_val$Quant.Error), colour = "red", linetype = "dashed") +
    ggplot2::ggtitle("2.Max QE Distribution: Test ") +
    ggplot2::xlab("Max QE values for cells: Test Data")

  plot_output <- p1 / p2
  return(plot_output)
}
