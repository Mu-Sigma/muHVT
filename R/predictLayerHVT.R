#' @name predictLayerHVT
#'
#' @title Predict which cell and what level each point in the test dataset belongs to
#'
#'
#' @param data Data Frame. A dataframe containing test dataset. The dataframe should have atleast one variable used while training. The variables from
#' this dataset can also be used to overlay as heatmap
#' @param hvt_mapA A list of hvt.results.model obtained from HVT function while performing hierarchical vector quantization on train data
#' @param hvt_mapB A list of hvt.results.model obtained from HVT function while performing hierarchical vector quantization on train data with novelty(s)
#' @param hvt_mapC A list of hvt.results.model obtained from HVT function while performing hierarchical vector quantization on train data without novelty(s)
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
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>, Shantanu Vaidya <shantanu.vaidya@@mu-sigma.com>,Somya Shambhawi <somya.shambhawi@@mu-sigma.com>
#' @seealso \code{\link{HVT}} \cr \code{\link{hvtHmap}}
#' @importFrom magrittr %>%
#' @examples
#' data(USArrests)
#' library('dplyr')
#' 
#' #Split in train and test
#' train <- USArrests[1:40,]
#' test <- USArrests[41:50,] 
#' 
#' hvt_mapA <- list()
#' hvt_mapA <- HVT(train, min_compression_perc = 70, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE,
#'                    quant_method="kmeans")
#'
#' 
#' identified_Novelty_cells <<- c(2, 10)
#' output_list <- removeNovelty(identified_Novelty_cells, hvt_mapA)
#' 
#' data_with_novelty <- output_list[[1]] %>% dplyr::select(!c("Cell.ID","Cell.Number"))
#'
#' 
#' hvt_mapB <- HVT(data_with_novelty ,n_cells = 3, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE,
#'                    quant_method="kmeans")
#'                    
#' dataset_without_novelty <- output_list[[2]] 
#' 
#' 
#' mapA_scale_summary = hvt_mapA[[3]]$scale_summary
#' hvt_mapC <- list()
#' hvt_mapC <- HVT(dataset_without_novelty, n_cells = 15, 
#'                  depth = 2, quant.err = 0.2, distance_metric = "L1_Norm",
#'                  error_metric = "max", quant_method = "kmeans",
#'                  projection.scale = 10, normalize = FALSE, scale_summary = mapA_scale_summary)
#' 
#' predictions <- list()
#' predictions <- predictLayerHVT(test, hvt_mapA, hvt_mapB, hvt_mapC)
#'
#' @keywords predictLayer
#' @export predictLayerHVT

predictLayerHVT <- function(data, 
                        hvt_mapA,
                        hvt_mapB,
                        hvt_mapC,
                        mad.threshold = 0.2,
                        normalize = TRUE, 
                        seed = 300,
                        distance_metric="L1_Norm",
                        error_metric="max",
                        child.level = 1, 
                        line.width = c(0.6, 0.4, 0.2),
                        color.vec = c("#141B41", "#6369D1", "#D8D2E1"),
                        yVar= NULL,
                        ...){
  #browser()
  
  set.seed(seed)
  requireNamespace("dplyr")
  requireNamespace("purrr")
  
# Predictions from Map A
  
  mapA_predictions <- predictHVT(data,
                                 hvt_mapA,
                                 child.level = 1,
                                 mad.threshold = 0.2,
                                 line.width = c(0.6),
                                 color.vec = c("#141B41"),
                                 normalize = normalize, 
                                 distance_metric=distance_metric,
                                 error_metric=error_metric,
                                 yVar= NULL,)
  
  mapA_scoredPredictedData <- as.data.frame(mapA_predictions$scoredPredictedData)
  mapA_scoredPredictedData$row_number <- row.names(mapA_scoredPredictedData)
  
  # Predictions  from map B
  mapB_predictions <- predictHVT(data,
                                 hvt_mapB,
                                 child.level = 1,
                                 mad.threshold = 0.2,
                                 line.width = c(0.6),
                                 color.vec = c("#141B41"),
                                 normalize = normalize,
                                 distance_metric=distance_metric,
                                 error_metric=error_metric,
                                 yVar= NULL,)

  mapB_scoredPredictedData <- as.data.frame(mapB_predictions$scoredPredictedData)
  mapB_scoredPredictedData$row_number <- row.names(mapB_scoredPredictedData)
  
  # Predictions from map C
  
  mapC_predictions <- predictHVT(data,
                                 hvt_mapC,
                                 child.level = 1,
                                 mad.threshold = 0.2,
                                 line.width = c(0.6),
                                 color.vec = c("#141B41"),
                                 normalize = normalize, 
                                 distance_metric=distance_metric,
                                 error_metric=error_metric,
                                 yVar= NULL,)
  
  mapC_scoredPredictedData <- as.data.frame(mapC_predictions$scoredPredictedData)
  mapC_scoredPredictedData$row_number <- row.names(mapC_scoredPredictedData)
  mapC_scoredPredictedData$mapC_cell_path <- paste0(mapC_scoredPredictedData$Segment.Level,"-",mapC_scoredPredictedData$Segment.Parent,"-",mapC_scoredPredictedData$Segment.Child)
  
  # Merged map A, map B, map C predictions value.
  
  predictions_set <-merge(merge(mapA_scoredPredictedData,mapB_scoredPredictedData, by = "row_number"), mapC_scoredPredictedData, by = "row_number")
  
  predictions_table <- predictions_set %>% select(row_number, Segment.Level.x, Segment.Parent.x, Segment.Child.x,Cell.ID.x,
                                                  Segment.Level.y, Segment.Parent.y, Segment.Child.y ,Cell.ID.y,mapC_cell_path,Segment.Level,Segment.Parent,Segment.Child,Cell.ID)
                                                  
  
  colnames(predictions_table) <- c('Row.Number','MapA.Segment.Level','MapA.Segment.Parent','MapA.Segment.Child','MapA.Cell.ID',
                                   'MapB.Segment.Level','MapB.Segment.Parent','MapB.Segment.Child','MapB.Cell.ID','Map.Cell.Path','MapC.Segment.Level','MapC.Segment.Parent','MapC.Segment.Child','MapC.Cell.ID')
  
  
  #removed_outlier_cells <- as.vector(unique(map_B$Cell.Number))
  
  predictions_table$MapB.Cell.ID_original <- ifelse(
    (predictions_table$MapA.Segment.Child %in%  identified_Novelty_cells),
    paste0(predictions_table$MapB.Cell.ID),  # if condition is met
    NA   # else put NA
  )
  
  predictions_table$MapC.Cell.ID <- ifelse(
    (is.na(predictions_table$MapB.Cell.ID_original)),
    paste0(predictions_table$MapC.Cell.ID),  # if condition is met
    NA   # else put NA
  )
  
  predictions_table$Row.Number <- as.integer(predictions_table$Row.Number)
  predictions_table <- predictions_table[order(predictions_table$Row.Number, decreasing = FALSE),]
  
  scoredPredictionsData_CellID <- predictions_table %>% select(c('Row.Number','MapA.Cell.ID','MapB.Cell.ID','MapB.Cell.ID_original','MapC.Cell.ID'))
  scoredPredictionsData_CellID <- lapply(scoredPredictionsData_CellID, as.numeric)
  
  scoredPredictionsData_CellID <- as.data.frame(scoredPredictionsData_CellID)
  scoredPredictionsData_CellID$MapB.Cell.ID_original <- unlist(scoredPredictionsData_CellID$MapB.Cell.ID_original)
  
  scoredPredictionsData_CellID <- scoredPredictionsData_CellID %>% dplyr::select("Row.Number","MapA.Cell.ID","MapB.Cell.ID_original","MapC.Cell.ID")
  
  scoredPredictionsData_CellID$MapA.Cell.ID <- ifelse(is.na(scoredPredictionsData_CellID$MapA.Cell.ID), NA, paste0("A",scoredPredictionsData_CellID$MapA.Cell.ID))
  scoredPredictionsData_CellID$MapB.Cell.ID_original <- ifelse(is.na(scoredPredictionsData_CellID$MapB.Cell.ID_original), NA, paste0("B",scoredPredictionsData_CellID$MapB.Cell.ID_original))
  scoredPredictionsData_CellID$MapC.Cell.ID <- ifelse(is.na(scoredPredictionsData_CellID$MapC.Cell.ID), NA, paste0("C",scoredPredictionsData_CellID$MapC.Cell.ID))
  
  scoredPredictionsData_CellID$Combined.Cell.ID <- coalesce(scoredPredictionsData_CellID$MapB.Cell.ID_original,scoredPredictionsData_CellID$MapC.Cell.ID)
  scoredPredictionsData_CellID <- scoredPredictionsData_CellID %>% dplyr::select("Row.Number","MapA.Cell.ID","Combined.Cell.ID")
  colnames(scoredPredictionsData_CellID) <- c("Row.Number","Layer1.Cell.ID","Layer2.Cell.ID")
  
  ##################### Changes ###############
  new_predict <- scoredPredictionsData_CellID
  scaled_test_data <- mapA_predictions[["scoredPredictedData"]]
  current_actual <- colnames(scaled_test_data)
  new_names <- paste0("act_", current_actual)
  colnames(scaled_test_data) <- new_names
  merge <- cbind(new_predict,scaled_test_data) %>% select(-c("act_Segment.Level","act_Segment.Parent","act_Segment.Child","act_n", "act_centroidRadius","act_diff", "act_anomalyFlag","act_Cell.ID"))
  
  data <- merge
  combined <- data %>% 
    mutate(Cell.ID = gsub("[C]", "", Layer2.Cell.ID)) %>%
    merge(hvt_mapC[[3]]$summary, by = "Cell.ID", all.x = TRUE) %>%
    arrange(as.numeric(Row.Number))
  
  data1 <- combined
  combined <- data1 %>%
    mutate(Cell.ID = gsub("[B]", "", Layer2.Cell.ID)) %>%
    merge(hvt_mapB[[3]]$summary, by = "Cell.ID", all.x = TRUE) %>%
    arrange(as.numeric(Row.Number)) %>% select(-c(Cell.ID))
  
  
  combined <- combined %>% select(-c("act_Quant.Error","Segment.Level.x" ,"Segment.Parent.x","Segment.Child.x","n.x","Quant.Error.x","Segment.Level.y" ,"Segment.Parent.y","Segment.Child.y", "n.y","Quant.Error.y" ))
  
  
  df_data <- combined %>%
    select(matches("^act_|Row.Number|Layer1.Cell.ID|Layer2.Cell.ID"))
  
  df <- combined %>%
    select(-c(Row.Number, Layer1.Cell.ID, Layer2.Cell.ID)) %>%
    select(-starts_with("act_"))  
  
  create_predictions <- function(df) {
    pred_cols <- unique(sub("\\..*", "", names(df)))  # Extract unique column names
    pred_values <- vector("list", length(pred_cols))
    
    for (i in seq_along(pred_cols)) {
      col_x <- paste0(pred_cols[i], ".x")
      col_y <- paste0(pred_cols[i], ".y")
      pred_values[[i]] <- coalesce(df[[col_x]], df[[col_y]])
    }
    
    df1 <- as.data.frame(pred_values)
    names(df1) <- paste0("pred_", pred_cols)
    return(df1)
  }
  
  df1 <- create_predictions(df)
  combined_data <- cbind(df_data,df1)
  
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
    temp0 <- temp0 %>% purrr::discard(~all(is.na(.) | . ==""))   
    df_new[,1] <- rowMeans(temp0)
    return(df_new)
  }
  
  diff <- subtract_predicted_actual(combined_data)
  combined_data$diff <- diff$matrix.ncol...1..nrow...nrow.data..
  desired_order <- c("Row.Number", grep("^act_", colnames(combined_data), value = TRUE), "Layer1.Cell.ID","Layer2.Cell.ID", grep("^pred_", colnames(combined_data), value = TRUE),"diff")
  df_reordered <- combined_data[, desired_order]
  
  ####################

  prediction_list = list(
    predictLayer_Output = scoredPredictionsData_CellID,
    actual_predictedTable = df_reordered
  )
  
  return(prediction_list)
  
  
}
  
  