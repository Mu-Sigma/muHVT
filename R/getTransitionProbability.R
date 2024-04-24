#' @name getTransitionProbability
#' @title Creating Transition Probability table
#' @description This is the main function to create transition probability table.
#' The transition probability table quantifies the likelihood of transitioning from one state to another. 
#' States: The table includes the current states and the possible next states.
#' Probabilities: For each current state, it lists the probability of transitioning to each of the next possible states. 
#' @param df Data frame. Input dataframe should contain two columns, cell ID from scoreHVT function and timestamp of that dataset.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing timestamps.
#' @return Prints and stores a nested list of dataframes with transition probabilities.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} \cr \code{\link{scoreHVT}} 
#' @keywords Transition_or_Prediction
#' @importFrom magrittr %>%
#' @examples
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#' DAX = EuStockMarkets[, "DAX"],
#' SMI = EuStockMarkets[, "SMI"],
#' CAC = EuStockMarkets[, "CAC"],
#' FTSE = EuStockMarkets[, "FTSE"])
#' rownames(EuStockMarkets) <- dataset$date
#' hvt.results<- trainHVT(dataset,n_cells = 60, depth = 1, quant.err = 0.1,
#'                        distance_metric = "L1_Norm", error_metric = "max",
#'                        normalize = TRUE,quant_method = "kmeans")
#' scoring <- scoreHVT(dataset, hvt.results)
#' cell_id <- scoring$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$date
#' dataset <- data.frame(cell_id, time_stamp)
#' table <- getTransitionProbability(dataset, cellid_column = "cell_id",time_column = "time_stamp")
#' @export getTransitionProbability


getTransitionProbability <- function(df, cellid_column, time_column) {

  # Rename columns for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"

  # Get a sorted list of unique Cell.ID values
  cell_id_list <- unique(df$Cell.ID) %>% sort()

  # Initialize an empty list to store probability results for each Cell.ID
  prob_results <- lapply(cell_id_list, function(state) {

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
      Transition_Probability = round(probabilities, 4)
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

