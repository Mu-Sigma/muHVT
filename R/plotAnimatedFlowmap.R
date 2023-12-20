#' Generating flow maps and animations based on transition probabilities
#'
#' Main function for generating flow maps and animations based on transition probabilities.
#'
#' Flow maps are a type of data visualization used to represent movements or transitions between different locations or states. 
#' They visually connect points to show the direction and volume of movements, such as the transitions in a Hidden Markov Model.
#' These maps help in understanding the dynamics of the system being studied by visually representing the direction and magnitude of flows or transitions. 
#' 
#' @param hvt_model_output List. Output from a trainHVT function.
#' @param transition_probability_df Data frame. Output Dataframe from getTransitionProbability function
#' @param hvt_plot_output List. Base plot for the flow maps.
#' @param animation character. Type of animation ('state_based', 'time_based', or 'All').
#' @param flow_map character. Type of flow map ('with_self_state', 'without_self_state', 'probability', or 'All').
#' @param animation_speed Numeric. An Integer representing the Speed of animation (frames per second).
#' @param threshold Numeric. An Integer representing the Probability threshold for flow map arrows.
#' @param df Data frame. Input dataframe should contain two columns of cell ID from scoreHVT function and timestamp.
#' @param cellid_column character. Name of the column containing cell IDs.
#' @param time_column character. Name of the column containing timestamps
#' @return A list of plot objects representing flow maps and animations.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} \cr \code{\link{scoreHVT}} \cr \code{\link{getTransitionProbability}}
#' @keywords Transition / Outliers
#' @importFrom magrittr %>%
#' @examples
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#' DAX = EuStockMarkets[, "DAX"],
#' SMI = EuStockMarkets[, "SMI"],
#' CAC = EuStockMarkets[, "CAC"],
#' FTSE = EuStockMarkets[, "FTSE"])
#' dataset_hvt <- dataset[,-c(1)]
#' hvt.results <- list()
#' hvt.results <- trainHVT(dataset_hvt, n_cells = 15, depth = 1, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE, seed = 123,
#'                    quant_method="kmeans")
#' predictions <- scoreHVT(dataset_hvt, hvt.results, child.level = 2, mad.threshold = 0.2) 
#' cell_id <- predictions$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$date
#' dataset <- data.frame(cell_id, time_stamp)
#' getTransitionProbability(dataset,cellid_column = "cell_id", time_column = "time_stamp")
#' plot <- plotAnimatedFlowmap(hvt_model_output = hvt.results, transition_probability_df = trans_prob_df,  df = dataset, animation = NULL, flow_map = NULL, animation_speed = NULL, threshold = NULL,  cellid_column = "cell_id", time_column = "time_stamp") 
#' print(plot[[1]])
#' print(plot[[2]])
#' @export plotAnimatedFlowmap



plotAnimatedFlowmap <- function(hvt_model_output, transition_probability_df, hvt_plot_output, df, animation = NULL, flow_map = NULL, animation_speed = NULL, threshold = NULL, cellid_column, time_column) {
  
  # Set default values for animation, flow_map, animation_speed, and threshold if they are NULL
  if (is.null(animation))animation <- "state_based"
  if (is.null(flow_map))flow_map <- "without_self_state"
  if (is.null(animation_speed))animation_speed <- 2
  if (is.null(threshold))threshold <- 0.6
  
  # Rename columns for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"
  # Get the centroid coordinates
  hvt_res1 <- hvt_model_output[[2]][[1]]$`1`
  hvt_res2 <- hvt_model_output[[3]]$summary$Cell.ID
  
  # Prepare cellID_coordinates
  coordinates_value1 <- lapply(1:length(hvt_res1), function(x) {
    centroids1 <- hvt_res1[[x]]
    coordinates1 <- centroids1$pt
  })
  cellID_coordinates <- do.call(rbind.data.frame, coordinates_value1)
  colnames(cellID_coordinates) <- c("x", "y")
  cellID_coordinates$Cell.ID <- hvt_res2
  
  # Function to get highest state and probability excluding self-state
  get_second_highest <- function(df) {
    df$Tplus1_States <- as.integer(df$Tplus1_States)
    max_probability_row <- df[which.max(df$Probability), ]
    max_probability_state <- max_probability_row$Tplus1_States
    
    other_states <- df$Tplus1_States[df$Probability != max(df$Probability)]
    
    if (length(other_states) > 0) {
      sorted_states <- other_states[order(-df$Probability[df$Tplus1_States %in% other_states])]
      if (length(sorted_states) >= 1) {
        next_highest_state <- sorted_states[1]
        next_highest_probability <- df$Probability[df$Tplus1_States == next_highest_state]
      } else {
        next_highest_state <- NA
        next_highest_probability <- NA
      }
    } else {
      next_highest_state <- NA
      next_highest_probability <- NA
    }
    
    return(data.frame(Tplus1_States = next_highest_state, Probability = next_highest_probability))
  }
  
  # Apply the function to each data frame in 'transition_probability_df'
  second_highest_states_list <- lapply(transition_probability_df, get_second_highest)
  
  # Combine the results into a single data frame
  second_state_df <- do.call(rbind, second_highest_states_list)
  
  # Function to get the highest probability state and probability
  get_highest_probability <- function(df) {
    df$Tplus1_States <- as.integer(df$Tplus1_States)
    max_probability_row <- df[which.max(df$Probability), ]
    highest_probability_state <- max_probability_row$Tplus1_States
    highest_probability_probability <- max_probability_row$Probability
    
    return(data.frame(Tplus1_States = highest_probability_state, Probability = highest_probability_probability))
  }
  
  highest_probability_states_list <- lapply(transition_probability_df, get_highest_probability)
  
  # Combine the results into a single data frame
  first_state_df <- do.call(rbind, highest_probability_states_list)
  
  # Subset the arrow starting coordinates based on the order
  current_state_data <- dplyr::arrange(cellID_coordinates, Cell.ID)
  colnames(current_state_data) <- c("x1", "y1", "Cell.ID")
  
  # Dataframe for second_highest state
  merged_df1 <- cbind(current_state_data, second_state_df)
  merged_df1 <- merged_df1 %>%
    left_join(dplyr::select(merged_df1, Cell.ID, x1, y1), by = c("Tplus1_States" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df1) <- c("x1", "y1", "Cell.ID", "Tplus1_States", "Probability", "x2", "y2")
  
  # Dataframe for highest state
  merged_df2 <- cbind(current_state_data, first_state_df)
  merged_df2 <- merged_df2 %>%
    left_join(dplyr::select(merged_df2, Cell.ID, x1, y1), by = c("Tplus1_States" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df2) <- c("x1", "y1", "Cell.ID", "Tplus1_States", "Probability", "x2", "y2")
  merged_df2$Probability <- round(merged_df2$Probability, digits = 3)
  
  # Non-self state PLOT
  merged_df1 <- merged_df1 %>%
    dplyr::mutate(distance = round(sqrt((x2 - x1)^2 + (y2 - y1)^2), 0))
  
  next_state_arrow_plot <- ggplot2::ggplot() +
    geom_segment(data = merged_df1, mapping = aes(x = x1, y = y1, 
                                                  xend = x1 + (x2 - x1) * 0.09 * sqrt((x2 - x1)^2 + (y2 - y1)^2), 
                                                  yend = y1 + (y2 - y1) * 0.09 * sqrt((x2 - x1)^2 + (y2 - y1)^2), 
                                                  color = distance),
                 arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), size = 1) +
    geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
    scale_color_gradient(low = "blue", high = "blue", 
                         name = "Distance",
                         breaks = seq(0, max(merged_df1$distance), by = 1))  +
    labs(title = "Flow map: Distance based on Euclidean Distance") + 
    guides(color = guide_legend(title = "Euclidean\nDistance")) + theme_minimal()
  
  # Self-state Plot
  prob1 <- merged_df2$Probability
  cellID_coordinates$prob1 <- prob1
 #browser()
  min_prob <- min(merged_df2$Probability)
  custom_breaks <- quantile(merged_df2$Probability, probs = seq(0, 1, by = 0.3))
  custom_breaks[1] <- min_prob - 1e-6
  CircleSize <- as.numeric(cut(merged_df2$Probability, breaks = custom_breaks, labels = seq(1, length(custom_breaks) - 1)))
  CircleSize <- ifelse(is.na(CircleSize), 3, CircleSize)
  self_state_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y, color = prob1), size = 1) +
    ggforce::geom_circle(data = merged_df2, aes(x0 = x1, y0 = y1, r = 0.5 * CircleSize)) +
    geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
    scale_color_gradient(low = "blue", high = "blue",
                         name = "Probability",
                         breaks = seq(0, max(cellID_coordinates$prob1), by = 0.005)) +
    guides(color = guide_legend(title = "Transition\nProbability", override.aes=list(shape = 21, size = c(2,3,4,5,6,7,8,9,8,7,4,3,2,1))), fill = guide_legend(title = "Probability")) +
    theme_minimal()
  
  
  # Create a new column "threshold" based on the mean
  
  prob_with_no_selfstate <- function(df) {
    df$Tplus1_States <- as.integer(df$Tplus1_States)
    max_probability_row <- df[which.max(df$Probability), ]
    max_probability_state <- max_probability_row$Tplus1_States
    
    # Exclude the row with the highest probability
    df <- df[df$Tplus1_States != max_probability_state, ]
    
    if (nrow(df) > 0) {
      # Calculate the total count of the remaining probabilities
      total_count <- sum(df$Probability)
      
      # Calculate probabilities based on the remaining rows
      probabilities <- as.vector(df$Probability) / total_count
      
      # Find the state with the highest probability among the remaining rows
      next_highest_state <- df$Tplus1_States[which.max(probabilities)]
      next_highest_probability <- max(probabilities)
    } else {
      next_highest_state <- NA
      next_highest_probability <- NA
    }
    
    return(data.frame(Tplus1_States = next_highest_state, Probability = next_highest_probability))
  }
  
  probability_without_selfstate_list <- lapply(transition_probability_df, prob_with_no_selfstate)
  
  
  # Combine the results into a single data frame
  third_df <- do.call(rbind, probability_without_selfstate_list)
  # Dataframe for second_highest state
  merged_df3 <- cbind(current_state_data, third_df)
  merged_df3 <- merged_df3 %>%
    left_join(dplyr::select(merged_df3, Cell.ID, x1, y1), by = c("Tplus1_States" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df3) <- c("x1", "y1", "Cell.ID", "Tplus1_States", "Probability", "x2", "y2")
  merged_df3 <- merged_df3 %>%
  dplyr::mutate(threshold_label = ifelse(Probability > threshold, "High", "Low"))
  merged_df3$Probability <- round(merged_df3$Probability, digits = 1)
  
  arrow_flow_map <- ggplot2::ggplot() +
    geom_segment(data = merged_df3, mapping = aes(x = x1, y = y1, xend = x1 + (x2 - x1) * 0.8 * Probability * ifelse(threshold_label == "High Probability", 1.3, 0.9), yend = y1 + (y2 - y1) * 0.8 * Probability * ifelse(threshold_label == "High Probability", 1.3, 0.9), color = Probability),
                 arrow = arrow(length = unit(0.2, "cm")), size = 1, linewidth = 1 ) +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), size = 1) +
    geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3)  +
    scale_color_gradient(low = "blue", high = "blue", 
                         name = "Probability",
                         breaks = seq(0, max(merged_df3$Probability), by = 0.2))  + 
       labs(title = "Flow map: Arrow size based on Probability") +
    guides(color = guide_legend(title = "Transition\nProbability")) + theme_minimal()
  
  # Flow map Animation based on Timestamp
  df <- df %>%
    group_by(Cell.ID) %>%
    dplyr::mutate(Frequency = with(rle(Cell.ID), rep(lengths, lengths)))
  state_data <<- df %>%
    group_by(grp = cumsum(c(TRUE, diff(Cell.ID) != 0))) %>%
    slice(n())
  
  anime_data <- merge(state_data, cellID_coordinates, by = "Cell.ID")
  anime_data <- anime_data %>% dplyr::mutate(Timestamp = round(Timestamp, 3)) %>% dplyr::arrange(Timestamp)
  dot_anim <- ggplot2::ggplot(anime_data, aes(x = x, y=y)) +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), size = 1) +
    geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
    ggplot2::geom_point(show.legend = FALSE, alpha = 0.7, color = "red", size = 5) +
    scale_color_manual() + theme_minimal() + 
    labs(x = "x-coordinates", y = "y-coordinates")
  
  dot_anim <- dot_anim + transition_time(Timestamp) +
    labs(title = "Animation showing state transition") +
    shadow_wake(wake_length = 0.03, alpha = FALSE)
  time_animation <- gganimate::animate(dot_anim, fps = animation_speed, duration = 2)
  # time_animation <- animate(dot_anim, fps = animation_speed, duration = 100)
  # anim_save("./source/time_animation.gif", animation = time_animation, width = 800, height = 400)
  
  ### Animation based on next state
  
  order <- unique(state_data$Cell.ID)
  anime_df <- merged_df1[order, ]
  anime_df$order_map <- 1:nrow(anime_df)
  anime_df$label <- rep("State\nTransition", nrow(anime_df))
  
  arrow_anim <- ggplot2::ggplot(anime_df, aes(x = x1, y = y1)) +
    geom_segment(data = anime_df, mapping = aes(x = x1, y = y1, xend = x1 + (x2 - x1) * 0.5, yend = y1 + (y2 - y1) * 0.5, color = label),
                 arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), size = 1) +
    geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
    scale_color_manual(values = c("State\nTransition" = "blue")) + 
    labs(x = "x-coordinates", y = "y-coordinates", color = "Transition\nProbability") + theme_minimal()
  
  
  animation1 <- arrow_anim + gganimate::transition_states(order_map, wrap = FALSE) + shadow_mark() +
    labs(title = "Animation showing state transition excluding self-state")
  
  state_animation <- gganimate::animate(animation1, fps = animation_speed, duration = 2)
   # state_animation <- animate(animation1, fps = animation_speed)
   # anim_save("./next_state_animation.gif", animation = state_animation, width = 800, height = 400)
   # 
  plots <- list()
  
  if (flow_map == "without_self_state" || flow_map == "All") {
    plots$without_self_state <- next_state_arrow_plot
  }
  
  if (flow_map == "self_state" || flow_map == "All") {
    plots$self_state <- self_state_plot
  }
  
  if (flow_map == "probability"|| flow_map == "All") {
    plots$flow_map <- arrow_flow_map
  }
  
  if (animation == "time_based" || animation == "All") {
    plots$time_based <- time_animation
  }
  
  if (animation == "state_based" || animation == "All") {
    plots$state_based <- state_animation
  }
  
  return(plots)
  
}