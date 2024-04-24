#' @name plotAnimatedFlowmap
#' @title Generating flow maps and animations based on transition probabilities
#' @description This is the main function for generating flow maps and animations based on transition probabilities.
#' Flow maps are a type of data visualization used to represent movements or transitions between different locations or states. 
#' They visually connect points to show the direction and volume of movements, such as the transitions in a Hidden Markov Model.
#' These maps help in understanding the dynamics of the system being studied by visually representing the direction 
#' and magnitude of flows or transitions. 
#' @param hvt_model_output List. Output from a trainHVT function.
#' @param transition_probability_df Data frame. Output Dataframe from getTransitionProbability function
#' @param df Data frame. Input dataframe should contain two columns, cell ID from scoreHVT function and timestamp of that dataset.
#' @param animation Character. Type of animation ('state_based', 'time_based', 'All' or  NULL)
#' @param flow_map Character. Type of flow map ('self_state', 'without_self_state', 'All' or NULL)
#' @param fps_time Numeric. A numeric value for the frames per second of the time transition gif.
#' @param fps_state Numeric. A numeric value for the frames per second of the state transition gif.
#' @param time_duration Numeric. A numeric value for the total duration of the time transition gif.
#' @param state_duration Numeric. A numeric value for the total duration of the state transition gif.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing timestamps
#' @return A list of plot and gif objects representing flow maps and animations.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} \cr \code{\link{scoreHVT}} \cr \code{\link{getTransitionProbability}}
#' @keywords Transition_or_Prediction
#' @importFrom magrittr %>%
#' @import gganimate gifski
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
#' plots <- plotAnimatedFlowmap(hvt_model_output = hvt.results, transition_probability_df = table,
#' df = dataset, animation = 'All', flow_map = 'All',fps_time = 1,fps_state =  1,time_duration = 10,
#' state_duration = 10,cellid_column = "cell_id", time_column = "time_stamp")
#' @export plotAnimatedFlowmap

plotAnimatedFlowmap <- function(hvt_model_output, transition_probability_df, df, animation = NULL, flow_map = NULL, fps_time = 1,fps_state = 1,time_duration =2, state_duration = 2, cellid_column, time_column) {
  ##for cran warnings, initializing empty vectors for these variables.
  segment_len <- grp <- colour<-order_map<- Frequency <-Timestamp<-y2 <-x2 <-y1.y<- x1.y<- y1 <-x1<-label<-NULL
  
  
  
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
    df$Next_State <- as.integer(df$Next_State)
    max_probability_row <- df[which.max(df$Transition_Probability), ]
    max_probability_state <- max_probability_row$Next_State
    
    other_states <- df$Next_State[df$Transition_Probability != max(df$Transition_Probability)]
    
    if (length(other_states) > 0) {
      sorted_states <- other_states[order(-df$Transition_Probability[df$Next_State %in% other_states])]
      if (length(sorted_states) >= 1) {
        next_highest_state <- sorted_states[1]
        next_highest_probability <- df$Transition_Probability[df$Next_State == next_highest_state]
      } else {
        next_highest_state <- NA
        next_highest_probability <- NA
      }
    } else {
      next_highest_state <- NA
      next_highest_probability <- NA
    }
    
    return(data.frame(Next_State = next_highest_state, Probability = next_highest_probability))
  }
  
  # Apply the function to each data frame in 'transition_probability_df'
  second_highest_states_list <- lapply(transition_probability_df, get_second_highest)

  # Combine the results into a single data frame
  second_state_df <- do.call(rbind, second_highest_states_list)
  
  # Function to get the highest probability state and probability
  get_highest_probability <- function(df) {
    df$Next_State <- as.integer(df$Next_State)
    max_probability_row <- df[which.max(df$Transition_Probability), ]
    highest_probability_state <- max_probability_row$Next_State
    highest_probability_probability <- max_probability_row$Transition_Probability
    return(data.frame(Next_State = highest_probability_state, Probability = highest_probability_probability))
  }
  
  highest_probability_states_list <- lapply(transition_probability_df, get_highest_probability)
  
  # Combine the results into a single data frame
  first_state_df <- do.call(rbind, highest_probability_states_list)
  
  # Subset the arrow starting coordinates based on the order
  current_state_data <- dplyr::arrange(cellID_coordinates, Cell.ID)
  colnames(current_state_data) <- c("x1", "y1", "Cell.ID")
  
  # Dataframe for second_highest state (without self state)
  merged_df1 <- cbind(current_state_data, second_state_df)
  merged_df1 <- merged_df1 %>%
    left_join(dplyr::select(merged_df1, Cell.ID, x1, y1), by = c("Next_State" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df1) <- c("x1", "y1", "Cell.ID", "Next_State", "Probability", "x2", "y2")
  
  # Dataframe for highest state  (with self state)
  merged_df2 <- cbind(current_state_data, first_state_df)
  merged_df2 <- merged_df2 %>%
    left_join(dplyr::select(merged_df2, Cell.ID, x1, y1), by = c("Next_State" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df2) <- c("x1", "y1", "Cell.ID", "Next_State", "Probability", "x2", "y2")
  merged_df2$Probability <- round(merged_df2$Probability, digits = 3)
  
  # Self-state Plot
  prob1 <- merged_df2$Probability 
  cellID_coordinates$prob1 <- prob1
  
  if (!is.null(flow_map)){
    if(flow_map == 'self_state' || flow_map == 'All'){
    
  min_prob <- min(merged_df2$Probability)
  max_prob <- max(merged_df2$Probability)
  custom_breaks <- stats::quantile(merged_df2$Probability, probs = seq(0, 1, by = 0.3))
  custom_breaks[1] <- min_prob - 0.001
  merged_df2$CircleSize <- as.numeric(cut(merged_df2$Probability, breaks = custom_breaks, labels = seq(1, length(custom_breaks) - 1)))
  max_cirsize <- stats::na.omit(merged_df2$CircleSize)
  max_cirsize<- max(max_cirsize) + 1
  merged_df2$CircleSize <- ifelse(is.na(merged_df2$CircleSize), max_cirsize, merged_df2$CircleSize)
  legend_size <- (2.6 * sort(unique(merged_df2$CircleSize)))
 
  
  breaks <- as.numeric(custom_breaks)
  breaks[1]<- breaks[1]+0.001
  generate_legend_labels <- function(breaks) {
        legend_labels <- lapply(seq_along(breaks)[-length(breaks)], function(i) {
               paste0(format(breaks[i], digits = 4), " to ", format(breaks[i + 1] - 0.0001, digits = 4))
           })
         legend_labels[length(legend_labels) + 1] <- paste0(format(tail(breaks, 1), digits = 4), " to 1")
         return(legend_labels)
     }
  
  legend_labels <- generate_legend_labels(breaks)
  
  self_state_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y, color = prob1), size = 0.9) +
    ggforce::geom_circle(data = merged_df2, aes(x0 = x1, y0 = y1, r = 0.5 * merged_df2$CircleSize), color = "blue") +
    ggplot2::geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
    scale_color_gradient(low = "black", high = "black",
                         name = "Probability",
                         breaks = breaks, 
                         labels = legend_labels) +
    labs(title = "State Transitions: Circle size based on Transition Probability",
         subtitle = "considering self state transitions",
         x = "x-coordinates",  
         y = "y-coordinates") +
    guides(color = guide_legend(title = "Transition\nProbability", 
                                override.aes=list(shape = 21, size = legend_size, color = "blue")), 
           fill = guide_legend(title = "Probability",override.aes = list(color = "blue", size = legend_size))) + 
    theme_minimal() +
   ggplot2::coord_equal()

    } 
    if(flow_map == 'without_self_state' || flow_map == 'All') {
  
  # Create a new column "threshold" based on the mean
  prob_with_no_selfstate <- function(df) {
    df$Next_State <- as.integer(df$Next_State)
    max_probability_row <- df[which.max(df$Transition_Probability), ]
    max_probability_state <- max_probability_row$Next_State
    
    # Exclude the row with the highest probability
    df <- df[df$Next_State != max_probability_state, ]
    
    if (nrow(df) > 0) {
      # Calculate the total count of the remaining probabilities
      total_count <- sum(df$Transition_Probability)
      
      # Calculate probabilities based on the remaining rows
      probabilities <- as.vector(df$Transition_Probability) / total_count
      
      # Find the state with the highest probability among the remaining rows
      next_highest_state <- df$Next_State[which.max(probabilities)]
      next_highest_probability <- max(probabilities)
    } else {
      next_highest_state <- NA
      next_highest_probability <- NA
    }
    
    return(data.frame(Next_State = next_highest_state, Probability = next_highest_probability))
  }
  
  probability_without_selfstate_list <- lapply(transition_probability_df, prob_with_no_selfstate)
  
  
  # Combine the results into a single data frame
  third_df <- do.call(rbind, probability_without_selfstate_list)
  # Dataframe for second_highest state
  merged_df3 <- cbind(current_state_data, third_df)
  merged_df3 <- merged_df3 %>%
    left_join(dplyr::select(merged_df3, Cell.ID, x1, y1), by = c("Next_State" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df3) <- c("x1", "y1", "Cell.ID", "Next_State", "Probability", "x2", "y2")
  merged_df3$Probability <- round(merged_df3$Probability, digits = 1)
  
 
  # NEW CODE FOR PLOT LEGEND
  # Define a function to map probability to segment length
  segment_length <- function(p) {
    segment_len <- ifelse(p <= 0.3, 0.2, 
                          ifelse(p <= 0.6, 0.4, 
                                 ifelse(p <= 0.8, 0.7, 0.9)))
    return(segment_len)
  }
  
  # Calculate segment lengths based on probability
  merged_df3$segment_len <- segment_length(merged_df3$Probability)
  max_x <- max(merged_df3$x1) + 5
  
  #length of segments in legend
  segment_lengths <- sqrt((merged_df3$x2 - merged_df3$x1)^2 + (merged_df3$y2 - merged_df3$y1)^2) * merged_df3$segment_len
  data_length <- data_frame(merged_df3$Probability, segment_lengths, merged_df3$segment_len) 
  data_length <- data_length[order(data_length$`merged_df3$Probability`),] %>% as.data.frame()
  intervals <- cut(data_length$`merged_df3$Probability`, breaks = c(0, 0.3, 0.6, 0.8, 1), include.lowest = TRUE)
  seg_len <- tapply(data_length$segment_lengths, intervals, max)
  seg_len <- as.numeric(seg_len)
  max_seg_len <- stats::na.omit(seg_len)
  max_seg_len  <- max(max_seg_len)
  arr_len <- (max_x +1) + max_seg_len +2
  
  intervals <- cut(data_length$`merged_df3$segment_len`, breaks = c(0, 0.2, 0.4, 0.7, 0.9), include.lowest = TRUE)
  seg_len_arr <- tapply(data_length$`merged_df3$segment_len`, intervals, max)
  len_arrhead <- as.numeric(seg_len_arr)

 
  max_probability <- max(merged_df3$Probability)
  min_probability <- min(merged_df3$Probability)
  annotate_list <- list()
  
  
  y_range <- range(merged_df3$y1)
  y1_l <- y_range[1] + 0.66 * diff(y_range)  
  y2_l <- y_range[1] + 0.60 * diff(y_range) 
  y3_l <- y_range[1] + 0.54 * diff(y_range) 
  y4_l <- y_range[1] + 0.48 * diff(y_range) 
  y5_l <- y_range[1] + 0.42 * diff(y_range) 
  y6_l <- y_range[1] + 0.36 * diff(y_range)
  
  x_range <- range(merged_df3$x1)
  x1_l <- x_range[1] + 1.19 * diff(x_range)  
  x2_l <- x_range[1] + 1.15 * diff(x_range) 
  x3_l <- x_range[1] + 1.3 * diff(x_range) 
  
  
  if (max_probability <= 0.3) {
    annotate_list <- list(
      annotate("text", x = x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = x3_l, y = y3_l, label = "0 to 0.3", size = 3, color = "black"),
      annotate("segment", x = x2_l, xend = x2_l+ seg_len[1], 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[1] * 3, "mm"), type = "open"))
    )
  } else if(min_probability >= 0.4 & max_probability <= 0.6)  {
    annotate_list <- list(
      annotate("text", x =x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = x3_l, y = y3_l, label = "0.4 to 0.6", size = 3, color = "black"),
      annotate("segment", x = x2_l, xend = (x2_l+ seg_len[2]), 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[2] * 3, "mm"), type = "open"))
    )
  } else if(min_probability >= 0.4 & max_probability <= 0.8)  {
    annotate_list <- list(
      annotate("text", x = x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = x3_l, y = y3_l, label = "0.4 to 0.6", size = 3, color = "black"),
      annotate("text", x = x3_l, y = y4_l, label = "0.7 to 0.8", size = 3, color = "black"),
      annotate("segment", x = x2_l, xend = (x2_l+ seg_len[2]), 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[2] * 3, "mm"), type = "open")),
      annotate("segment", x = x2_l, xend =(x2_l+ seg_len[3]), 
               y = y4_l, yend = y4_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[3] * 3, "mm"), type = "open"))
    )
  } else if(min_probability >= 0.4 & max_probability <= 1)  {
    annotate_list <- list(
      annotate("text", x = x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = x3_l, y = y3_l, label = "0.4 to 0.6", size = 3, color = "black"),
      annotate("text", x = x3_l, y = y4_l, label = "0.7 to 0.8", size = 3, color = "black"),
      annotate("text", x = x3_l, y = y5_l, label = "0.9 to 1", size = 3, color = "black"),
      annotate("segment", x = x2_l, xend = (x2_l+ seg_len[2]), 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[2] * 3, "mm"), type = "open")),
      annotate("segment", x = x2_l, xend =(x2_l+ seg_len[3]), 
               y = y4_l, yend = y4_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[3] * 3, "mm"), type = "open")),
      annotate("segment", x = (x2_l), xend =(x2_l+ seg_len[4]), 
               y = y5_l, yend = y5_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[4] * 3, "mm"), type = "open"))
    )
  }else if(min_probability >= 0.7 & max_probability <= 0.8)  {
    annotate_list <- list(
      annotate("text", x = x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = x3_l, y = y3_l, label = "0.7 to 0.8", size = 3, color = "black"),
      annotate("segment", x = x2_l, xend =(x2_l+ seg_len[3]), 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[3] * 3, "mm"), type = "open"))
    )
  }else if(min_probability >= 0.7 & max_probability <= 1)  {
    annotate_list <- list(
      annotate("text", x = x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = x3_l, y = y3_l, label = "0.7 to 0.8", size = 3, color = "black"),
      annotate("text", x = x3_l, y = y4_l, label = "0.9 to 1", size = 3, color = "black"),
      annotate("segment", x = x2_l, xend =(x2_l+ seg_len[3]), 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[3] * 3, "mm"), type = "open")),
      annotate("segment", x = x2_l, xend =(x2_l+ seg_len[4]), 
               y = y4_l, yend = y4_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[4] * 3, "mm"), type = "open"))
    )
  }else if(min_probability >= 0.9 & max_probability <= 1)  {
    annotate_list <- list(
      annotate("text", x = x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = x3_l, y = y3_l, label = "0.9 to 1", size = 3, color = "black"),
      annotate("segment", x = x2_l, xend =(x2_l+ seg_len[4]), 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[4] * 3, "mm"), type = "open"))
    )
  } else {
    annotate_list <- list(
      annotate("text", x = x1_l, y = y1_l, label = "Transition", size = 4, color = "black"),
      annotate("text", x = x1_l, y = y2_l, label = "Probability", size = 4, color = "black"),
      annotate("text", x = (x3_l), y = y3_l, label = "0 to 0.3", size = 3, color = "black"),
      annotate("text", x = (x3_l), y = y4_l, label = "0.4 to 0.6", size = 3, color = "black"),
      annotate("text", x = (x3_l), y = y5_l, label = "0.7 to 0.8", size = 3, color = "black"),
      annotate("text", x = (x3_l), y = y6_l, label = "0.9 to 1", size = 3, color = "black"),
      annotate("segment", x = (x2_l), xend = (x2_l+ seg_len[1]), 
               y = y3_l, yend = y3_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[1] * 3, "mm"), type = "open")),
      annotate("segment", x = x2_l, xend = (x2_l+ seg_len[2]), 
               y = y4_l, yend = y4_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[2] * 3, "mm"), type = "open")),
      annotate("segment", x = x2_l, xend = (x2_l+ seg_len[3]), 
               y = y5_l, yend = y5_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[3] * 3, "mm"), type = "open")),
      annotate("segment", x = (x2_l), xend =(x2_l+ seg_len[4]), 
               y = y6_l, yend = y6_l, color = "blue", 
               arrow = arrow(length = unit(len_arrhead[4] * 3, "mm"), type = "open"))
    )
  }
  
  # Create the plot with the annotations
  arrow_flow_map <- ggplot(merged_df3, aes(x = x1, y = y1)) +
    geom_point(color = "black", size = 0.9) +
    geom_text(aes(label = Cell.ID), vjust = -1, size = 3) +
    geom_segment(aes(xend = x1 + (x2 - x1) * segment_len, 
                     yend = y1 + (y2 - y1) * segment_len), 
                 arrow = arrow(length = unit(merged_df3$segment_len *3, "mm")),
                 color = "blue") +
    labs(title = "State Transitions: Arrow size based on Transition Probability",
         subtitle = "without considering self state transitions",
         x = "x-coordinates",  
         y = "y-coordinates") +
    theme_minimal()
  
  # Add annotations to the plot
  for (annotation in annotate_list) {
    arrow_flow_map <- arrow_flow_map + annotation
  }

    } 
    if(flow_map != 'self_state' && flow_map != 'without_self_state' && flow_map != 'All'){
      print("invalid argument for flow_map")
    }
  }
  
  if(!is.null(animation)){
    if(animation == 'time_based' || animation == 'All' ) {

  sampled_df <- df %>% dplyr::select(Cell.ID,Timestamp)
  anime_data <- merge(sampled_df, cellID_coordinates, by = "Cell.ID", all.x = TRUE)
  anime_data <- anime_data %>% dplyr::arrange(Timestamp) %>% dplyr::select(-prob1)
  anime_data <- anime_data %>% dplyr::group_by(grp = cumsum(Cell.ID != lag(Cell.ID, default = first(Cell.ID)))) %>% 
    dplyr::mutate( colour = 2 - row_number() %% 2 ) %>% dplyr::ungroup() %>% dplyr::select(-grp)
  anime_data <- anime_data %>%
     dplyr::mutate(latency = c(0, difftime(lead(Timestamp), Timestamp, units = "secs")[-length(Timestamp)]))
  anime_data$latency <- formatC(anime_data$latency, format = "f", digits = 5)
  anime_data <- anime_data %>% as.data.frame()
  

  dot_anim <- ggplot2::ggplot(anime_data, aes(x = x, y=y)) +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), color = "black", size = 1) +
    ggplot2::geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
    ggplot2::geom_point(aes(x = x, y = y, color = ifelse(colour == 1, "Active state at t", " ")), alpha = 0.7, size = 5) +
    scale_color_manual(values = c("Active state at t" = "red", " " = "white")) +
    theme_minimal() +
    labs(x = "x-coordinates", y = "y-coordinates", color = "Time Transition")

  dot_anim <- dot_anim +
    transition_time(Timestamp) +
    labs(title = "Animation showing state transitions considering self state transitions",
         subtitle = "\n\ntime(t): {(round(frame_time,3))} seconds\nLatency: {((anime_data$latency[frame]))} seconds") +
  shadow_wake(wake_length = 0.05, alpha = FALSE,wrap = FALSE)+
    theme(plot.subtitle = element_text(margin = margin(t = 20)))
  time_animation <- animate(dot_anim, fps = fps_time, duration = time_duration, renderer = gifski_renderer())
  anim_save("./time_animation.gif", animation = time_animation,width = 800, height = 430)
  
    }
    if(animation == 'state_based' || animation == 'All') {
  
  ### Animation based on next state
  df <- df %>%group_by(Cell.ID) %>%dplyr::mutate(Frequency = with(rle(Cell.ID), rep(lengths, lengths)))
  state_data <- df %>%group_by(grp = cumsum(c(TRUE, diff(Cell.ID) != 0))) %>% slice(n())
  order <- unique(state_data$Cell.ID)
  anime_df <- merged_df1[order, ]
  anime_df$order_map <- 1:nrow(anime_df)
  anime_df$label <- rep("Successive states", nrow(anime_df))
  
  #NEWLY ADDED FOR ARROW HEAD SIZE
  x <- anime_df$x1
  y <- anime_df$y1
  xend <- anime_df$x1 + (anime_df$x2 - anime_df$x1) * 0.5
  yend <- anime_df$y1 + (anime_df$y2 - anime_df$y1) * 0.5
  arrow_head_length <- sqrt((xend - x)^2 + (yend - y)^2)
  
  arrow_anim <- ggplot2::ggplot(anime_df, aes(x = x1, y = y1)) +
    geom_segment(data = anime_df, mapping = aes(x = x1, y = y1, xend = x1 + (x2 - x1) * 0.5, yend = y1 + (y2 - y1) * 0.5, color = label),
                 arrow = arrow(length = unit(arrow_head_length, "mm")), show.legend = TRUE) +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), size = 1, show.legend = FALSE) +
    geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3 ) +
    scale_color_manual(values = c("Successive states" = "blue")) + 
    labs(x = "x-coordinates", y = "y-coordinates", color = "State Transition") + theme_minimal()
  
  animation1 <- arrow_anim + transition_states(order_map, wrap = FALSE) + shadow_mark() +
    labs(title = "Animation showing state transitions",
         subtitle = "without considering self-state transitions")
  
  state_animation <- animate(animation1, fps = fps_state, duration = state_duration, renderer = gifski_renderer())
  anim_save("./next_state_animation.gif", animation = state_animation, width = 800, height = 400)
    } 
    if(animation != 'state_based' && animation != 'time_based' && animation != 'All'){
      print("invalid argument for animation")
    }
  }
  
  plots <- list()
  
 
  if(is.null(flow_map)){
    print("'flowmap' argument is NULL")
  } else{
  if (flow_map == "self_state" || flow_map == "All") {
    plots$self_state <- self_state_plot
  }

  if (flow_map == "without_self_state"|| flow_map == "All") {
    plots$without_self_state <- arrow_flow_map
  }
  }
  if(is.null(animation)){
    print("'animation' argument is NULL")
  }else{
  if (animation == "time_based" || animation == "All") {
    plots$time_based <- time_animation
  }
  
  if (animation == "state_based" || animation == "All") {
    plots$state_based <- state_animation
  }
  }
  return(plots)
  
}

