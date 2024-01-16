#' @name reconcileTransitionProbability
#' @title Reconciliation of Transition Probability - Model Diagnostics
#' @description This is the main function to creating transition probability heatmaps and reconcilation of the same.
#' The Reconciliation of Transition Probability refers to the process of analyzing and adjusting transition probabilities in a stochastic model like a Markov Chain. 
#' It involves ensuring the probabilities accurately reflect real-world dynamics by normalizing them,
#' removing unlikely transitions, and comparing different models. 
#' The function described creates heatmaps to visually represent these probabilities,
#' aiding in the understanding and analysis of state transitions within the model.
#' 
#' @param df Data frame. Input dataframe should contain two columns of cell ID from scoreHVT function and timestamp.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing timestamps
#' @param  hmap_type Character. Type of heatmap to generate ('with_self_state', 'without_self_state', or 'All')
#' @return A list of plotly heatmap objects representing the transition probability heatmaps.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} \cr \code{\link{scoreHVT}} 
#' @keywords internal
#' @importFrom magrittr %>%
#' @import ggforce 
#' @import markovchain
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
#' predictions <- scoreHVT(dataset_hvt, hvt.results, child.level = 2, mad.threshold = 0.2) 
#' cell_id <- predictions$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$date
#' dataset <- data.frame(cell_id, time_stamp)
#' reconcileTransitionProbability(dataset, hmap_type = 'All', cellid_column = "cell_id",
#'                                time_column = "time_stamp") 
#' @export reconcileTransitionProbability


reconcileTransitionProbability <- function(df, hmap_type = NULL, cellid_column, time_column) {
  
  # Rename columns for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"
  
  # Set default value for hmap_type if it is NULL
  if (is.null(hmap_type)) hmap_type <- "with_self_state"
  
  # Heatmap 1: Transition probability with self-state
  raw_data <- df %>% dplyr::select("Cell.ID", "Timestamp")
 # browser()
  transition_values <- table(raw_data$Cell.ID[-nrow(raw_data)], raw_data$Cell.ID[-1])
  mat <- unclass(transition_values)
  normalized_value <- mat / rowSums(mat)
  melted_matrix <- reshape2::melt(normalized_value)
  a_df <- melted_matrix %>% as.data.frame()
  colnames(a_df) <- c("StateFrom", "StateTo", "Probabilty")
  a_df$StateTo <- as.factor(a_df$StateTo)
  a_df$StateFrom <- as.factor(a_df$StateFrom)
  
  hmap1 <- plotly::plot_ly(
    data = a_df,
    x = a_df$StateFrom,
    y = a_df$StateTo,
    z = a_df$Probabilty,
    type = "heatmap",
    colorscale = "colz",
    showlegend = TRUE,
    hovertemplate = "Cell t: %{x}<br>Cell t+1: %{y}<br>Probability: %{z}"
  ) %>%
    plotly::layout(
      title = "Transition Matrix (With Self-Transitions)",
      xaxis = list(title = "Cell.ID From"),
      yaxis = list(title = "Cell.ID To"),
      autosize = FALSE,
      width = 750,
      height = 600
    )
  
  # Heatmap 2: Transition probability without self-state

  raw_data1 <- df
  transition_values1 <- table(raw_data1$Cell.ID[-nrow(raw_data1)], raw_data1$Cell.ID[-1])
  mat1 <- unclass(transition_values1)
  normalized_value1 <- mat1 / rowSums(mat1)
  

  # browser()  
  # Normalize again after setting self-transitions to 0
  normalized_value1 <- normalized_value1 / rowSums(normalized_value1)
  # Set probability to 0 for transitions to the same state
  for (i in 1:nrow(normalized_value1)) {
    normalized_value1[i, i] <- 0
  }
  melted_matrix1 <- reshape2::melt(normalized_value1)
  a_df1 <- melted_matrix1 %>% as.data.frame()
  colnames(a_df1) <- c("StateFrom", "StateTo", "Probabilty")
  a_df1$StateTo <- as.factor(a_df1$StateTo)
  a_df1$StateFrom <- as.factor(a_df1$StateFrom)
  
  hmap2 <- plotly::plot_ly(
    data = a_df1,
    x = a_df1$StateFrom,
    y = a_df1$StateTo,
    z = a_df1$Probabilty,
    type = "heatmap",
    colorscale = "colz",
    showlegend = TRUE,
    hovertemplate = "Cell t: %{x}<br>Cell t+1: %{y}<br>Probability: %{z}"
  ) %>%
    plotly::layout(
      title = "Transition Matrix (Without Self-Transitions)",
      xaxis = list(title = "Cell ID From"),
      yaxis = list(title = "Cell ID To"),
      autosize = FALSE,
      width = 750,
      height = 600
    )
  
  # Heatmap 3: Markov Chain state transition Probability
  # browser()
  #old code: mc_data  <- df$Cell.ID
  mc_data  <- df$Cell.ID[-c(1, nrow(df))]

  mc <- markovchain::markovchainFit(data = mc_data)
  trans_matrix <- mc$estimate
  trans_plot <- as(trans_matrix, "matrix")
  
  melted_matrix_mc <- reshape2::melt(trans_plot)
  a_df_mc <- melted_matrix_mc %>% as.data.frame()
  colnames(a_df_mc) <- c("State_From", "State_To", "Probability")
  heatmap_data <- a_df_mc
  
  hmap3 <- plotly::plot_ly(
    data = heatmap_data,
    x = ~State_From,
    y = ~State_To,
    z = ~Probability,
    type = "heatmap",
    colors = grDevices::colorRampPalette(c("white", "blue"))(100),
    hovertemplate = "Cell t: %{x}<br>Cell t+1: %{y}<br>Probability: %{z}"
  ) %>%
    plotly::layout(
      title = "Markovchain Transition Matrix (With Self-Transitions)",
      xaxis = list(title = "Cell ID From"),
      yaxis = list(title = "Cell ID To"),
      autosize = FALSE,
      width = 750,
      height = 600
    )

  
  #without self - state
  trans_matrix_no_self <- trans_plot
  diag(trans_matrix_no_self) <- 0  # Set self-transitions to 0
  
  # Create the heatmap data frame
  melted_matrix_mc1 <- reshape2::melt(trans_matrix_no_self)
  a_df_mc1 <- melted_matrix_mc1 %>% as.data.frame()
  colnames(a_df_mc1) <- c("State_From", "State_To", "Probability")
  
  # Create the heatmap without self-state
  hmap4 <- plotly::plot_ly(
    data = a_df_mc1,
    x = ~State_From,
    y = ~State_To,
    z = ~Probability,
    type = "heatmap",
    colors = grDevices::colorRampPalette(c("white", "blue"))(100),
    hovertemplate = "Cell t: %{x}<br>Cell t+1: %{y}<br>Probability: %{z}"
  ) %>%
    plotly::layout(
      title = "Markovchain Transition Matrix (Without Self-Transitions)",
      xaxis = list(title = "Cell ID From"),
      yaxis = list(title = "Cell ID To"),
      autosize = FALSE,
      width = 750,
      height = 600
    )
  
  # Determine which heatmaps to return based on the hmap_type parameter
  if (hmap_type == "without_self_state") {
    return(list(hmap2, hmap4))
  } else if (hmap_type == "with_self_state") {
    return(list(hmap1, hmap3))
  } else if (hmap_type == "All") {
    return(list(hmap1, hmap2, hmap3, hmap4))
  } else {
    stop("Invalid plot_type parameter. Use 'without_self_state', 'with_self_state', or 'both'.")
  }
}
