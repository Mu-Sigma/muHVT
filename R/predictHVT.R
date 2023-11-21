#' @name predictHVT
#'
#' @title Predict which cell and what level each point in the test dataset belongs to
#'
#'
#' @param data List. A dataframe containing test dataset. The dataframe should have atleast one variable used while training. The variables from
#' this dataset can also be used to overlay as heatmap
#' @param hvt.results.model A list of hvt.results.model obtained from HVT function while performing hierarchical vector quantization on train data
#' @param child.level A number indicating the level for which the heat map is to be plotted.(Only used if hmap.cols is not NULL)
#' @param mad.threshold A numeric values indicating the permissible Mean Absolute Deviation
#' @param line.width Vector. A line width vector
#' @param color.vec Vector. A color vector
#' @param normalize Logical. A logical value indicating if the columns in your
#' dataset should be normalized. Default value is TRUE.
#' @param seed Numeric. Random Seed.
#' @param distance_metric character. The distance metric can be 'Euclidean" or "Manhattan". Euclidean is selected by default.
#' @param error_metric character. The error metric can be "mean" or "max". mean is selected by default
#' @param yVar character. Name of the dependent variable(s)
#' @param ...  color.vec and line.width can be passed from here
#' @returns Dataframe containing scored predicted data, prediction plots and mean absolute deviation plots
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>
#' @seealso \code{\link{HVT}} \cr \code{\link{hvtHmap}}
#' @keywords predict
#' @importFrom magrittr %>%
#' @examples
#' data(USArrests)
#' # Split in train and test
#'
#' train <- USArrests[1:40, ]
#' test <- USArrests[41:50, ]
#'
#' hvt.results <- list()
#' hvt.results <- HVT(train,
#'   n_cells = 15, depth = 1, quant.err = 0.2,
#'   distance_metric = "L1_Norm", error_metric = "mean",
#'   projection.scale = 10, normalize = TRUE,
#'   quant_method = "kmeans", diagnose = TRUE
#' )
#'
#' predictions <- predictHVT(test, hvt.results, child.level = 2, mad.threshold = 0.2)
#' print(predictions$scoredPredictedData)
#' @export predictHVT


predictHVT <- function(data,
                       hvt.results.model,
                       child.level = 1,
                       mad.threshold = 0.2,
                       line.width = c(0.6, 0.4, 0.2),
                       color.vec = c("#141B41", "#6369D1", "#D8D2E1"),
                       normalize = TRUE,
                       seed = 300,
                       distance_metric = "L1_Norm",
                       error_metric = "max",
                       yVar = NULL,
                       ...) {
  # browser()

  set.seed(seed)
  requireNamespace("dplyr")
  requireNamespace("purrr")
  # require("plotly")
  # requireNamespace("rjson")
  requireNamespace("data.table")

  # browser()
  if (!("Cell.ID" %in% colnames(hvt.results.model[[3]]$summary))) {
    hvt.results.model[[3]]$summary <- get_cell_id(hvt.results = hvt.results.model)
  }
  hvt.results.model[[3]]$summary <- cbind(hvt.results.model[[3]]$summary, centroidRadius = unlist(hvt.results.model[[3]]$max_QE))


  # distance_metric <-
  #   ifelse(distance_metric == "L1_Norm", "manhattan", "euclidean")

  summary_list <- hvt.results.model[[3]]
  # n_cells <- n_cells.hmap

  train_colnames <- names(summary_list[["nodes.clust"]][[1]][[1]])

  if (!all(train_colnames %in% colnames(data))) {
    stop("Not all training columns are part of test dataset")
  }

  if (!all(is.na(summary_list$scale_summary)) && normalize == TRUE) {
    scaled_test_data <- scale(
      data[, train_colnames],
      center = summary_list$scale_summary$mean_data[train_colnames],
      scale = summary_list$scale_summary$std_data[train_colnames]
    )
  } else {
    scaled_test_data <- data[, train_colnames]
  }

  colnames(scaled_test_data) <- train_colnames
  # level <- length(summary_list$nodes.clust)
  level <- child.level

  # keep_col <- names(summary_list$summary)
  # subsetting df based on multiple dep variables
  if (!is.null(yVar)) {
    yVardf <- data[, yVar]
    if (length(yVar) != 1) {
      colnames(yVardf) <- paste0("Scored.", yVar)
    }
  }

  # dfWithMapping <- summary_list$summary


  find_path <- function(data_vec, centroid_data) {
    # centroidDist <- which.min(sqrt(colSums((centroid_data - data_vec) ^ 2)))
    if (distance_metric == "L1_Norm") {
      centroidDist <- which.min(colSums(abs(centroid_data - data_vec), na.rm = TRUE))
      Quant.Error <- (colSums(abs(centroid_data - data_vec), na.rm = TRUE))[centroidDist]
    } else {
      centroidDist <- which.min(sqrt(colSums((centroid_data - data_vec)^2, na.rm = TRUE)))
      Quant.Error <- sqrt(colSums((centroid_data - data_vec)^2, na.rm = TRUE))[centroidDist]
    }
    return(data.frame("Index" = centroidDist, "Quant.Error" = Quant.Error / length(train_colnames)))
  }

  ## Get a df with Segment level, parent, child info joined with max QE value
  # centroidRadius <- unlist(summary_list$max_QE)
  newdfMapping <- summary_list$summary

  innermostCells2 <- newdfMapping %>%
    dplyr::filter((n > 0 & Segment.Level == level) | (Segment.Level < level & (Quant.Error < mad.threshold | n <= 3)))

  transposedCells <- innermostCells2 %>%
    select(all_of(train_colnames)) %>%
    t()

  cent_dist_df2 <- apply(data.frame(scaled_test_data), 1, find_path, transposedCells) %>%
    bind_rows()

  groupCols2 <- c(paste0("Segment.", c("Level", "Parent", "Child")), yVar)
  if ("Cell.ID" %in% names(innermostCells2)) groupCols2 <- c(groupCols2, "Cell.ID", "centroidRadius")

  predict_test_data2 <-
    cbind(data.frame(scaled_test_data, "n" = 1), cent_dist_df2) %>%
    dplyr::left_join(
      innermostCells2 %>%
        select(all_of(groupCols2)) %>%
        cbind(Index = as.integer(row.names(.))),
      by = "Index"
    ) %>%
    select(-Index) %>%
    select(names(newdfMapping))

  # Considering margin of error
  predict_test_data3 <- predict_test_data2 %>% mutate(diff = centroidRadius - Quant.Error)
  predict_test_data3 <- predict_test_data3 %>% mutate(anomalyFlag = ifelse(Quant.Error < (mad.threshold), 0, 1))

  # Renaming fitted yVar with prefix Fitted
  if (!is.null(yVar)) {
    if (length(yVar) == 3) {
      for (i in 1:length(yVar)) {
        indexScored <- which(colnames(predict_test_data2) == yVar[i])
        colnames(predict_test_data2)[indexScored] <- paste0("Fitted.", yVar[i])
      }
    } else {
      indexScored <- which(colnames(predict_test_data2) == yVar)
      colnames(predict_test_data2)[indexScored] <- paste0("Fitted.", yVar)
    }
  }


  # Adding dep Variables back to scored data
  if (!is.null(yVar)) {
    predict_test_data2 <- merge(predict_test_data2, yVardf, by = 0) %>% select(-"Row.names")
    if (length(yVar) == 1) {
      indexFitted <- which(colnames(predict_test_data2) == "y")
      colnames(predict_test_data2)[indexFitted] <- paste0("Scored.", yVar)
    }
  }

  groupCols2 <- c(paste0("Segment.", c("Level", "Parent", "Child")), "anomalyFlag")
  # Calculating mean for scored data for each centroid
  groupCols3 <- c(paste0("Segment.", c("Level", "Parent", "Child")))
  # filter anomalous values for test dataset

  if (error_metric == "mean") {
    predictQE2 <- predict_test_data3 %>%
      group_by_at(groupCols2) %>%
      dplyr::summarise(
        n = sum(n),
        Quant.Error = mean(Quant.Error)
      )
    predictQE3 <- predict_test_data3 %>%
      group_by_at(groupCols3) %>%
      dplyr::summarise(
        n = sum(n),
        Quant.Error = mean(Quant.Error)
      )
  } else {
    predictQE2 <- predict_test_data3 %>% # with anamoly flags
      group_by_at(groupCols2) %>%
      dplyr::summarise(
        n = sum(n),
        Quant.Error = max(Quant.Error)
      )
    predictQE3 <- predict_test_data3 %>% # wo anamoly flags
      group_by_at(groupCols3) %>%
      dplyr::summarise(
        n = sum(n),
        Quant.Error = max(Quant.Error)
      )
  }

  # Calculating mean for scored data for each centroid
  groupCols2 <- c(paste0("Segment.", c("Level", "Parent", "Child")))
  newdfMapping <- newdfMapping %>% mutate(sumOriginal = Quant.Error * n)
  df_temp <- inner_join(predictQE2,
    newdfMapping %>% select(c(groupCols2, Quant.Error, sumOriginal, n)),
    by = groupCols2
  )
  df_temp2 <- inner_join(predictQE3,
    newdfMapping %>% select(c(groupCols2, Quant.Error, sumOriginal, n)),
    by = groupCols2
  )


  if (error_metric == "mean") {
    df_temp <- df_temp %>% mutate(Scored.Quant.Error = (sumOriginal + (Quant.Error.x * n.x)) / (n.x + n.y)) # sum original is wrong
    df_temp2 <- df_temp2 %>% mutate(Scored.Quant.Error = (sumOriginal + (Quant.Error.x * n.x)) / (n.x + n.y))
  } else {
    df_temp <- df_temp %>% mutate(Scored.Quant.Error = max(Quant.Error.x, Quant.Error.y))
    df_temp2 <- df_temp2 %>% mutate(Scored.Quant.Error = (sumOriginal + (Quant.Error.x * n.x)) / (n.x + n.y)) # should be maxScored
  }

  QECompareDf2 <- df_temp %>%
    mutate(
      Quant.Error.Diff = abs(Scored.Quant.Error - Quant.Error.y),
      `Quant.Error.Diff (%)` = abs(Scored.Quant.Error - Quant.Error.y) / Quant.Error.y * 100
    ) %>%
    dplyr::rename(Fitted.Quant.Error = Quant.Error.y, n = n.x) %>%
    select(-c("Quant.Error.x", "sumOriginal", "n.y"))

  plotList <- hvt.results.model[[2]] %>%
    unlist(., recursive = FALSE) %>%
    unlist(., recursive = FALSE)

  boundaryCoords2 <-
    lapply(plotList, function(x) {
      data.frame(
        "Segment.Level" = x[["Segment.Level"]],
        "Segment.Parent" = x[["Segment.Parent"]],
        "Segment.Child" = x[["Segment.Child"]],
        "x" = x$pt["x"],
        "y" = x$pt["y"],
        "bp.x" = I(x$x),
        "bp.y" = I(x$y)
      )
    }) %>%
    bind_rows(.) %>%
    right_join(.,
      QECompareDf2 %>% dplyr::filter(anomalyFlag == 1),
      by = paste0("Segment.", c("Level", "Parent", "Child"))
    )


  predictPlot <- plotHVT(
    hvt.results.model,
    line.width = line.width,
    color.vec = color.vec,
    centroid.size = 1.5,
    title = paste0(
      "Hierarchical Voronoi Tessellation With Depth = ",
      child.level
    ),
    maxDepth = child.level
  ) + ggtitle(paste(
    "Hierarchical Voronoi Tessellation for Level",
    child.level
  )) +
    theme(
      plot.title = element_text(
        size = 18,
        hjust = 0.5,
        margin = margin(0, 0, 20, 0)
      ),
      # legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))


  colour_scheme <- c(
    "#6E40AA", "#6B44B2", "#6849BA", "#644FC1", "#6054C8", "#5C5ACE", "#5761D3", "#5268D8", "#4C6EDB", "#4776DE", "#417DE0", "#3C84E1", "#368CE1",
    "#3194E0", "#2C9CDF", "#27A3DC", "#23ABD8", "#20B2D4", "#1DBACE", "#1BC1C9", "#1AC7C2", "#19CEBB", "#1AD4B3", "#1BD9AB", "#1DDFA3", "#21E39B",
    "#25E892", "#2AEB8A", "#30EF82", "#38F17B", "#40F373", "#49F56D", "#52F667", "#5DF662", "#67F75E", "#73F65A", "#7FF658", "#8BF457", "#97F357", "#A3F258"
  )

  if (nrow(boundaryCoords2) != 0) {
    hoverText <- paste(
      " Cell ID:",
      boundaryCoords2$Cell.ID,
      "<br>",
      "Segment.Level:",
      boundaryCoords2$Segment.Level,
      "<br>",
      "Segment.Parent:",
      boundaryCoords2$Segment.Parent,
      "<br>",
      "Segment.Child:",
      boundaryCoords2$Segment.Child,
      "<br>",
      "Number of observations:",
      boundaryCoords2$n,
      "<br>"
    )
  } else {
    hoverText <- NULL
  }
  # browser()
  predictPlot <- predictPlot + geom_polygon(
    data = boundaryCoords2,
    aes(
      x = bp.x,
      y = bp.y,
      group = interaction(Segment.Level, Segment.Parent, Segment.Child),
      fill = n,
      text = hoverText
    ),
    color = "red",
    size = 1
  ) +
    geom_point(data = boundaryCoords2 %>% distinct(x, y), aes(x = x, y = y), size = 1.5) +
    scale_fill_gradientn(colours = colour_scheme) +
    guides(colour = "none")

  plotlyPredict <- plotly::ggplotly(predictPlot, tooltip = "text")

  hoverText <- lapply(plotlyPredict$x$data, function(x) {
    if (!is.null(x$text)) {
      return(x$text)
    }
  }) %>% unlist()

  checkCell <- substr(hoverText, 1, 5) %in% " Cell"
  trace_vec <- seq_along(checkCell)[!checkCell]

  plotlyPredict <- plotlyPredict %>%
    plotly::layout(
      hoverlabel = list(bgcolor = "rgba(255,255,0,0.2)"),
      legend = list(
        title = list(text = "Level"),
        itemdoubleclick = FALSE,
        itemclick = "toggleothers",
        traceorder = "reversed"
      )
    ) %>%
    plotly::style(plotlyPredict, hoverinfo = "none", traces = trace_vec) %>%
    plotly::config(displayModeBar = FALSE)

  predict_test_data3 <- predict_test_data3 %>% mutate_if(is.numeric, round, digits = 4) # Rounding decimal columns using dplyr function
  predict_test_dataRaw <- predict_test_data3
  predict_test_dataRaw[, train_colnames] <- data[, train_colnames]

  ################################################# Changes #################



  predicted_result <- hvt.results.model[[3]]$summary
  current_predicted <- colnames(predicted_result)
  new_names <- paste0("pred_", current_predicted)
  colnames(predicted_result) <- new_names
  Cell.ID <- data.frame(predicted_result$pred_Cell.ID)
  predicted_result <- predicted_result %>% select(-c("pred_Segment.Level", "pred_Segment.Parent", "pred_Segment.Child", "pred_n", "pred_Cell.ID", "pred_Quant.Error", "pred_centroidRadius"))
  predicted_result <- cbind(Cell.ID, predicted_result)
  predicted_result <- data.table::setnames(predicted_result, "predicted_result.pred_Cell.ID", "Cell.ID")



  actuals <- predict_test_data3
  current_actual <- colnames(actuals)
  new_names <- paste0("act_", current_actual)
  colnames(actuals) <- new_names

  actuals$Row.No <- row.names(data)
  data_with_row <- data.frame(actuals$Row.No)
  data_with_cell <- data.frame(actuals$act_Cell.ID)
  actuals <- actuals %>% select(-c("act_Segment.Level", "act_Segment.Parent", "act_Segment.Child", "act_n", "act_Cell.ID", "act_Quant.Error", "act_centroidRadius", "act_diff", "act_anomalyFlag", "Row.No"))
  actuals_data <- cbind(data_with_row, actuals, data_with_cell)
  actuals_data <- data.table::setnames(actuals_data, "actuals.act_Cell.ID", "Cell.ID")
  actuals_data <- data.table::setnames(actuals_data, "actuals.Row.No", "Row.No")



  merged_df <- merge(actuals_data, predicted_result, by = "Cell.ID")
  merged_result <- merged_df %>% arrange(as.numeric(merged_df$Row.No))

  subtract_predicted_actual <- function(data, actual_prefix = "act_", predicted_prefix = "pred_") {
    actual_cols <- grep(paste0("^", actual_prefix), names(data), value = TRUE)
    df_new <- data.frame(matrix(ncol = 1, nrow = nrow(data)))
    temp0 <<- data.frame(matrix(nrow = nrow(data)))
    for (col in actual_cols) {
      predicted_col <- gsub(actual_prefix, predicted_prefix, col)

      if (predicted_col %in% names(data)) {
        temp0[[predicted_col]] <<- abs(data[[col]] - data[[predicted_col]])
      }
    }
    temp0 <- temp0 %>% purrr::discard(~ all(is.na(.) | . == ""))
    df_new[, 1] <- rowMeans(temp0)
    return(df_new)
  }



  diff <- subtract_predicted_actual(merged_result)
  merged_result$diff <- diff$matrix.ncol...1..nrow...nrow.data..
  merged_result <- rename(merged_result, c("diff" = "diff"))


  # Define the desired column order
  desired_order <- c("Row.No", grep("^act_", colnames(merged_result), value = TRUE), "Cell.ID", grep("^pred_", colnames(merged_result), value = TRUE), "diff")

  # Reorder the columns in the data frame
  df_reordered <- merged_result[, desired_order]



  #################################################

  prediction_list <- list(
    scoredPredictedData = predict_test_data3,
    actual_predictedTable = df_reordered,
    QECompareDf = QECompareDf2,
    predictPlot = plotlyPredict,
    predictInput = c("depth" = child.level, "quant.err" = mad.threshold),
    model_mad_plots = list(),
    model_info = list(type = "hvt_prediction")
  )
  model_mad_plots <- NA
  # browser()
  if (!all(is.na(hvt.results.model[[4]]))) {
    mtrain <- hvt.results.model[[4]]$mad_plot_train + ggtitle("Mean Absolute Deviation Plot: Calibration on Train Data")
  }
  if (!all(is.na(hvt.results.model[[5]]))) {
    mtest <- hvt.results.model[[5]][["mad_plot"]] + ggtitle("Mean Absolute Deviation Plot:Validation")
  }

  if (!all(is.na(hvt.results.model[[4]])) & !all(is.na(hvt.results.model[[5]]))) {
    model_mad_plots <- list(mtrain = mtrain, mtest = mtest)
  }


  prediction_list$model_mad_plots <- model_mad_plots
  return(prediction_list)
}
