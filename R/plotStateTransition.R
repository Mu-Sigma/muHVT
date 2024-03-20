#' @name plotStateTransition
#' @title Creating State Transition Plot 
#' @description This is the main function to creating state transition plot from a dataframe.
#' A state transition plot is a type of data visualization used to represent 
#' the changes or transitions in states over time for a given system. 
#' State refers to a particular condition or status of a cell at a specific point in time. 
#' This could be related to various attributes like a phase in a cycle, or any other measurable characteristic.
#' Transition refers to the change of state for a cell from one condition to another over time. 
#' This could represent processes like progression through stages, changes in activity, or other dynamic characteristics.
#' @param df Data frame. Input dataframe should contain two columns of cell ID from scoreHVT function and timestamp.
#' @param sample_size Numeric. An integer indicating the Fraction of the dataframe to sample.
#' Default value is 0.2
#' @param line_plot Logical. A logical value indicating to create a line plot.
#' Default value is NULL.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing timestamps.
#' @return A plotly object representing the state transition plot for the given dataframe.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} \cr \code{\link{scoreHVT}}
#' @keywords internal
#' @importFrom magrittr %>%
#' @examples
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#' DAX = EuStockMarkets[, "DAX"],
#' SMI = EuStockMarkets[, "SMI"],
#' CAC = EuStockMarkets[, "CAC"],
#' FTSE = EuStockMarkets[, "FTSE"])
#' dataset_hvt <- dataset[,-c(1)]
#' hvt.results <- list()
#' hvt.results <- trainHVT(dataset_hvt, n_cells = 15, depth = 3, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE, seed = 123,
#'                    quant_method="kmeans")
#' 
#' predictions <- scoreHVT(dataset_hvt, hvt.results, child.level = 2, mad.threshold = 0.2) 
#' cell_id <- predictions$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$date
#' dataset <- data.frame(cell_id, time_stamp)
#' plotStateTransition(dataset, sample_size = NULL, line_plot = FALSE, cellid_column = "cell_id",
#'                     time_column = "time_stamp")
#' @export plotStateTransition


plotStateTransition <- function(df, sample_size = NULL, line_plot = NULL, cellid_column, time_column) {
  
  # Rename column names for Time and Cell for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"
  
  # Set default values for sample_size and line_plot if they are NULL
  if (is.null(sample_size)) sample_size <- 0.2
  if (is.null(line_plot)) line_plot <- FALSE
  
  # Calculate the number of rows to sample and sample the data based on the specified sample_size
  sampling_percent <- round(sample_size * nrow(df))
  sampled_data <- df[(nrow(df) - sampling_percent + 1):nrow(df), ]
  
  # Group and count frequencies of cell IDs, then arrange by timestamp
  sampled_data <- sampled_data %>%
   dplyr::group_by(Cell.ID) %>%
    dplyr::mutate(Frequency = n()) %>%
    dplyr::arrange(Timestamp)
  
  ### Sampled Plot - Create a heatmap plot for sampled data
  timeseries_plot <- sampled_data %>%
    plotly::plot_ly(x = ~Timestamp, y = ~Cell.ID, z = ~Frequency,
            type = "heatmap", hovertemplate = "Timestamp: %{x}<br> Cell.ID : %{y}<br> Frequency: %{z}") %>%
    plotly::colorbar(title = "Frequency") %>%
    plotly::layout(title = "Time Series Flow Map",
           xaxis = list(title = "Timestamp"),
           yaxis = list(title = "Cell ID"))
  
  ### Plot on the whole dataset with lines
  # Prepare data for state transitions with timestamps and frequencies
  state_transitions <- sampled_data %>%
    dplyr::select(Timestamp, Cell.ID, Frequency)
  
  # Add a column for the next state (next cell ID)
  state_transitions <- state_transitions %>%
    dplyr::mutate(Next_State = lead(Cell.ID))
  
  # Create a lined plot with scatter points and lines connecting them
  lined_plot <- state_transitions %>%
    plotly::plot_ly(x = ~Timestamp, y = ~Cell.ID, z = ~Frequency,
            type = "heatmap", hovertemplate = "Timestamp: %{x}<br> Cell.ID : %{y}<br>Frequency: %{z}") %>%
    plotly::colorbar(title = "Transition Frequency") %>%
    plotly::layout(title = "Flow Map for Sampled Data",
           xaxis = list(title = "Timestamp"),
           yaxis = list(title = "Cell ID")) %>% 
    plotly::add_trace(xend = ~lead(Timestamp), yend = ~Next_State, type = "scatter", mode = "markers",
              line = list(color = "blue", width = 1), marker = list(color = "blue", size = 1))
  
  # Check the sample_size and line_plot parameters and return the appropriate plot
  if (sample_size <= 1) {
    if (line_plot == TRUE) {
      return(lined_plot)
    } else if (line_plot == FALSE) {
      return(timeseries_plot)
    } else {
      stop("Invalid line_plot parameter. Use TRUE or FALSE.")
    }
  } else {
    stop("Invalid sample_size parameter. Use values between 0.1 to 1.")
  }
  
}