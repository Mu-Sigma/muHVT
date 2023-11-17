#' @title plotDiag
#'
#' Make the diagnostic plots for hierarchical voronoi tessellations model.
#'
#' @param hvt.results List. A list of hvt.results obtained from the HVT
#' function.
#' @param hvt.predictions List. A list of hvt.predictions obtained from the Predict
#' function.
#' @return Returns the ggplot object containing the Quantized Error distribution plots for the given HVT results and predictions
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords diagnostics
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export qeHistPlot


qeHistPlot <- function(hvt.results, hvt.predictions) {
  # require(patchwork)
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



  pred_val <- hvt.predictions[["scoredPredictedData"]]
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
