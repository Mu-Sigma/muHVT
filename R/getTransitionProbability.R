#' @name getTransitionProbability
#' @title Creating Transition Probability table
#' @description This is the main function to create transition probability table.
#' The transition probability table quantifies the likelihood of transitioning from one state to another. 
#' It is derived from historical data and provides a statistical basis for predicting future state changes.
#' States: The table includes the current states and the possible next states.
#' Probabilities: For each current state, it lists the probability of transitioning to each of the next possible states. 
#' Frequency: It often includes the frequency of these transitions, which helps in understanding the commonness of certain state changes.
#' @param df Data frame. Input dataframe should contain two columns of cell ID from scoreHVT function and timestamp.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing timestamps.
#' @return Prints and stores a list of dataframes with transition probabilities.
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
#' predictions <- scoreHVT(dataset_hvt, hvt.results, child.level = 2, mad.threshold = 0.2) 
#' cell_id <- predictions$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$date
#' dataset <- data.frame(cell_id, time_stamp)
#' getTransitionProbability(dataset,cellid_column = "cell_id", time_column = "time_stamp")
#' @export getTransitionProbability


getTransitionProbability <- function(df, cellid_column, time_column) {
 # browser()  
  # Rename columns for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"
  #browser()
  # Get a sorted list of unique Cell.ID values
  cell_id_list <- unique(df$Cell.ID) %>% sort()
  # browser()
  # Initialize an empty list to store probability results for each Cell.ID
  prob_results <- lapply(cell_id_list, function(state) {
    #browser()
    # Find row numbers where the Cell.ID matches the current state
    row_numbers <- which(df$Cell.ID == state) + 1
    # Get the values of the next state (T+1) for the matching rows
    tplus1_states <- df[row_numbers, "Cell.ID"]
    # Create a frequency table of T+1 states
    prob_table <- table(tplus1_states)
    # Calculate the total count of T+1 states
    total_count <- sum(prob_table)
    # Calculate probabilities for each T+1 state
    probabilities <- as.vector(prob_table) / total_count
    # Create a dataframe to store T+1 states, frequencies, and probabilities
    result_df <- data.frame(
      Current_State = state,
      Next_State = names(prob_table),
      Relative_Frequency = as.vector(prob_table),
      Probability_Percentage = round(probabilities, 4)
    )
    
    return(result_df)
  })
  
  # # Set names for the probability results list based on Cell.ID values
  # names(prob_results) <- cell_id_list
  # 
  # trans_prob_df <<- prob_results
  # 
  # if (all_prob_results) {
  #   return(trans_prob_df)
  # } else {
  #   # Print the probability results in R Markdown document
  #   lapply(cell_id_list, function(cell_id_index) {
  #     cat("### Cell ID", cell_id_index, "\n\n")
  #     cat("Probability table for Cell ID", cell_id_index, ":\n\n")
  #     tab <- as.data.frame(prob_results[[cell_id_index]])
  #     print(htmltools::tagList(DT::datatable(tab, rownames = FALSE, width = '70%')))
  #     cat("\n\n")
  #   })
  # 
  # }
}

