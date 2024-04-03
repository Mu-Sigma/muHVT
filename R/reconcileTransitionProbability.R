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
#' @param  hmap_type Character. Type of heatmap to generate ('self_state', 'without_self_state', or 'All')
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
  if (is.null(hmap_type)) hmap_type <- "self_state"
  
  # Heatmap 1: Transition probability with self-state
  raw_data <- df %>% dplyr::select("Cell.ID", "Timestamp")
  #browser()
  transition_values <- table(raw_data$Cell.ID[-nrow(raw_data)], raw_data$Cell.ID[-1])
  mat <- unclass(transition_values)
  normalized_value <- mat / rowSums(mat)
  melted_matrix <- reshape2::melt(normalized_value)
  a_df <- melted_matrix %>% as.data.frame()
  colnames(a_df) <- c("StateFrom", "StateTo", "Probabilty")
  a_df$StateTo <- as.integer(a_df$StateTo)
  a_df$StateFrom <- as.integer(a_df$StateFrom)
  a_df$Probabilty <- round(a_df$Probabilty, digits = 4)
  
  hmap1 <- plotly::plot_ly(
    data = a_df,
    x = ~StateFrom,
    y = ~StateTo,
    z = ~Probabilty,
    type = "heatmap",
    colorscale = "colz",
    showlegend = TRUE,
    name = "Probability",
    hovertemplate = "Cell t: %{x}<br>Cell t+1: %{y}<br>Probability: %{z}"
  ) %>%
    plotly::layout(
      title = "Transition Matrix (With Self-Transitions)",
      xaxis = list(title = "Cell.ID From"),
      yaxis = list(title = "Cell.ID To"),
      autosize = FALSE,
      width = 1000,
      height = 600,
      colorbar = list(title = "Probability")
    )
  
  # Heatmap 2: Transition probability without self-state

  raw_data1 <- df
  transition_values1 <- table(raw_data1$Cell.ID[-nrow(raw_data1)], raw_data1$Cell.ID[-1])
  mat1 <- unclass(transition_values1)
  normalized_value1 <- mat1 / rowSums(mat1)
  
  # Normalize again after setting self-transitions to 0
  #normalized_value1 <- normalized_value1 / rowSums(normalized_value1)
  # Set probability to 0 for transitions to the same state
  for (i in 1:nrow(normalized_value1)) {
    normalized_value1[i, i] <- 0
  }
  
  melted_matrix1 <- reshape2::melt(normalized_value1)
  a_df1 <- melted_matrix1 %>% as.data.frame()
  colnames(a_df1) <- c("StateFrom", "StateTo", "Probabilty")
  a_df1$StateTo <- as.integer(a_df1$StateTo)
  a_df1$StateFrom <- as.integer(a_df1$StateFrom)
  a_df1$Probabilty <- round(a_df1$Probabilty,4)
  
  hmap2 <- plotly::plot_ly(
    data = a_df1,
    x = ~StateFrom,
    y = ~StateTo,
    z = ~Probabilty,
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
      width = 1000,
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
  colnames(a_df_mc) <- c("StateFrom", "StateTo", "Probability")
  a_df_mc$Probability <- round(a_df_mc$Probability,4)
  
  hmap3 <- plotly::plot_ly(
    data = a_df_mc,
    x = ~StateFrom,
    y = ~StateTo,
    z = ~Probability,
    type = "heatmap",
    colors = grDevices::colorRampPalette(c("white", "blue"))(100),
    hovertemplate = "Cell t: %{x}<br>Cell t+1: %{y}<br>Probability: %{z}"
  ) %>%
    plotly::layout(
      title = "Probability Reconcilation for Self State Transition",
      xaxis = list(title = "Cell ID From"),
      yaxis = list(title = "Cell ID To"),
      autosize = FALSE,
      width = 1000,
      height = 600
    )

  
  #without self - state
  trans_matrix_no_self <- trans_plot
  diag(trans_matrix_no_self) <- 0  # Set self-transitions to 0
  
  # Create the heatmap data frame
  melted_matrix_mc1 <- reshape2::melt(trans_matrix_no_self)
  a_df_mc1 <- melted_matrix_mc1 %>% as.data.frame()
  colnames(a_df_mc1) <- c("StateFrom", "StateTo", "Probability")
  a_df_mc1$Probability <- round(a_df_mc1$Probability,4)
    
  # Create the heatmap without self-state
  hmap4 <- plotly::plot_ly(
    data = a_df_mc1,
    x = ~StateFrom,
    y = ~StateTo,
    z = ~Probability,
    type = "heatmap",
    colors = grDevices::colorRampPalette(c("white", "blue"))(100),
    hovertemplate = "Cell t: %{x}<br>Cell t+1: %{y}<br>Probability: %{z}"
  ) %>%
    plotly::layout(
      title = "Probability Reconcilation for Non-Self State Transition",
      xaxis = list(title = "Cell ID From"),
      yaxis = list(title = "Cell ID To"),
      autosize = FALSE,
      width = 1000,
      height = 600
    )
  
  #browser()
  # a_df_mc <- a_df_mc[a_df_mc$Probability != 0,]
  # a_df_mc <- a_df_mc[order(a_df_mc$StateTo), ]
  # a_df_mc <- a_df_mc[order(a_df_mc$StateFrom), ]
  # 
  self_state_table <- merge(a_df, a_df_mc, by = c("StateFrom", "StateTo"))
  colnames(self_state_table) <- c("Current_State", "Next_State","Probability_from_manual_calculation","Probability_from_markov_function")
  self_state_table <- self_state_table[self_state_table$Probability_from_manual_calculation != 0, ]
  self_state_table <- self_state_table[self_state_table$Probability_from_markov_function != 0, ]
  
  process_subset <- function(subset_df) {
    max_index <- which.max(subset_df$Probability_from_manual_calculation)
    return(subset_df[max_index, ])
  }
  
  output_list <- lapply(split(self_state_table, self_state_table$Current_State), process_subset)
  self_state_table <- do.call(rbind, output_list)
  self_state_table$diff <- (self_state_table$Probability_from_manual_calculation - self_state_table$Probability_from_markov_function)
  
  non_self_state_table <- merge(a_df1, a_df_mc1, by = c("StateFrom", "StateTo"))
  colnames(non_self_state_table) <- c("Current_State", "Next_State","Probability_from_manual_calculation","Probability_from_markov_function")
  non_self_state_table <- non_self_state_table[non_self_state_table$Probability_from_manual_calculation != 0, ]
  
  process_subset <- function(subset_df) {
    max_index <- which.max(subset_df$Probability_from_manual_calculation)
    return(subset_df[max_index, ])
  }
  
  output_list <- lapply(split(non_self_state_table, non_self_state_table$Current_State), process_subset)
  non_self_state_table <- do.call(rbind, output_list)
  non_self_state_table$diff <- (non_self_state_table$Probability_from_manual_calculation - non_self_state_table$Probability_from_markov_function)
  
  

  self_state_plots <- subplot(
    hmap1, hmap3,
    nrows = 1,
    shareX = TRUE,
    shareY = TRUE,
    widths = c(0.5, 0.5)
  ) 
  

  non_self_state_plots <- subplot(
    hmap2, hmap4,
    nrows = 1,
    shareX = TRUE,
    shareY = TRUE,
    widths = c(0.5, 0.5)
  ) 
  
  
  
  
  
  # Determine which heatmaps to return based on the hmap_type parameter
  if (hmap_type == "without_self_state") {
    return(list(non_self_state_plots, non_self_state_table))
  } else if (hmap_type == "self_state") {
    return(list(self_state_plots, self_state_table))
  } else if (hmap_type == "All") {
    return(list(self_state_plots,non_self_state_table, non_self_state_plots, non_self_state_table))
  } else {
    stop("Invalid plot_type parameter. Use 'without_self_state', 'self_state', or 'both'.")
  }
}

