#' @name plotModelDiagnostics
#'
#' @title Make the diagnostic plots for hierarchical voronoi tessellations model.
#'
#' @param model_obj List. A list of model_obj obtained from the trainHVT
#' function or prediction object
#' @returns plot object containing Diagnostics plots for the HVT model or HVT predictions.
#' for HVT model, Minimum Intra-DataPoint Distance Plot, Minimum Intra-Centroid Distance Plot
#' Mean Absolute Deviation Plot,Distribution of Number of Observations in Cells, for Training Data and
#' Mean Absolute Deviation Plot for Validation Data are plotted.
#' for HVT Predictions, "Mean Absolute Deviation Plot for Training Data and Validation Data are plotted
#'
#'
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords Diagnostics / Validation
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export plotModelDiagnostics


plotModelDiagnostics <-
  function(model_obj) {
    # browser()
    # require(ggplot2)
    # require(magrittr)
    # require(patchwork)

    # Model Print

    if (model_obj[["model_info"]][["type"]] == "hvt_model") {
      p1 <- model_obj[[4]]$datapoint_plot + ggplot2::ggtitle("Minimum Intra-DataPoint Distance Plot: Train Data")
      p2 <- model_obj[[4]]$cent_plot + ggplot2::ggtitle("Minimum Intra-Centroid Distance Plot: HVT Model | Train Data")
      p3 <- model_obj[[4]]$mad_plot_train + ggplot2::ggtitle("Mean Absolute Deviation Plot: Calibration: HVT Model | Train Data")
      p4 <- model_obj[[4]]$number_plot + ggplot2::ggtitle("Distribution of Number of Observations in Cells: HVT Model | Train Data")
      p5 <- model_obj[[4]]$singleton_piechart
      if (model_obj[["model_info"]][["input_parameters"]][["hvt_validation"]]) {
        p6 <- hvt.results[[5]][["mad_plot"]] + ggplot2::ggtitle("Mean Absolute Deviation Plot:Validation")

        plotDiag <- (p3 / (p1 | p2) / (p4 | p5) / p6)
      } else {
        plotDiag <- (p3 / (p1 | p2) / (p4 | p5))
      }
    } else if (model_obj[["model_info"]][["type"]] == "hvt_prediction") {
      # browser()
      mtrain <- model_obj[["model_mad_plots"]][["mtrain"]] + ggplot2::ggtitle("Mean Absolute Deviation Plot: Calibration on Train Data")
      mtest <- model_obj[["model_mad_plots"]][["mtest"]] + ggplot2::ggtitle("Mean Absolute Deviation Plot:Validation")
      mpred <- madPlot(model_obj) + ggplot2::ggtitle("Mean Absolute Deviation Plot:Test Data")
      #
      plotDiag <- mtrain / mtest / mpred
    }




    return(plotDiag)
  }
