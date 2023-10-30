#' @name mlayerHVT
#'
#' @title Predict which cell and what level each point in the test dataset belongs to
#'
#'
#' @param data Data Frame. A dataframe containing test dataset. The dataframe should have atleast one variable used while training. The variables from
#' this dataset can also be used to overlay as heatmap
#' @param hvt_mapA A list of hvt.results.model obtained from HVT function while performing hierarchical vector quantization on train data
#' @param hvt_mapB A list of removed outlier rows using removedOutliers function
#' @param hvt_mapC A list of hvt.results.model obtained from HVT function while performing hierarchical vector quantization on train data without outlier(s)
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
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>, Shantanu Vaidya <shantanu.vaidya@@mu-sigma.com>
#' @seealso \code{\link{HVT}} \cr \code{\link{hvtHmap}}
#' @importFrom magrittr %>%
#' @examples
#' data(USArrests)
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
#' identified_outlier_cells <- c(2, 10)
#' output_list <- removeOutliers(identified_outlier_cells, hvt_mapA)
#' hvt_mapB <- output_list[[1]]
#' dataset_without_outliers <- output_list[[2]] 
#' 
#' 
#' mapA_scale_summary = hvt_mapA[[3]]$scale_summary
#' hvt_mapC <- list()
#' hvt_mapC <- HVT(dataset_without_outliers, n_cells = 15, 
#'                  depth = 2, quant.err = 0.2, distance_metric = "L1_Norm",
#'                  error_metric = "max", quant_method = "kmeans",
#'                  projection.scale = 10, normalize = FALSE, scale_summary = mapA_scale_summary)
#' 
#' predictions <- list()
#' predictions <- mlayerHVT(test, hvt_mapA, hvt_mapB, hvt_mapC)
#'
#' @keywords mlayer
#' @export mlayerHVT


mlayerHVT <- function(data, 
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
  # browser()
  
  set.seed(seed)
  requireNamespace("dplyr")
  requireNamespace("purrr")
 
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
  
  mapC_predictions <- predictHVT(data,
                                 hvt_mapC,
                                 child.level = length(unique(hvt_mapC[[3]]$summary[, "Segment.Level"])),
                                 line.width = line.width,
                                 color.vec = color.vec,
                                 normalize = normalize, 
                                 distance_metric=distance_metric,
                                 error_metric=error_metric,
                                 yVar= NULL,)
  
  mapC_scoredPredictedData <- as.data.frame(mapC_predictions$scoredPredictedData)
  mapC_scoredPredictedData$row_number <- row.names(mapC_scoredPredictedData)
  mapC_scoredPredictedData$mapC_cell_path <- paste0(mapC_scoredPredictedData$Segment.Level,"-",mapC_scoredPredictedData$Segment.Parent,"-",mapC_scoredPredictedData$Segment.Child)
  
  ##############################
  
  colnames(mapA_scoredPredictedData) <- c("MapA.Segment.Level","MapA.Segment.Parent","MapA.Segment.Child","n","MapA.Cell.ID","Quant.Error","Murder","Assault","UrbanPop","Rape","centroidRadius","diff","anomalyFlag","Row.Number") 
  colnames(mapC_scoredPredictedData) <- c("MapC.Segment.Level","MapC.Segment.Parent","MapC.Segment.Child","n","MapC.Cell.ID","Quant.Error","Murder","Assault","UrbanPop","Rape","centroidRadius","diff","anomalyFlag","Row.Number","MapC.Cell.Path")
  
  predictions_set <- merge(x = mapA_scoredPredictedData, y = mapC_scoredPredictedData, by = c("Row.Number"))
  
  predictions_table <- predictions_set %>% select(Row.Number,MapA.Segment.Level,MapA.Segment.Parent, MapA.Segment.Child,MapA.Cell.ID,
                                                  # Segment.Level.y, Segment.Parent.y, Segment.Child.y, mapB_cell_path,Cell.ID.y,
                                                  MapC.Segment.Level, MapC.Segment.Parent, MapC.Segment.Child, MapC.Cell.Path,MapC.Cell.ID
  )
  
  
  ##############################
  # predictions_set <- merge(x = mapA_scoredPredictedData, y = mapC_scoredPredictedData, by = c("row_number"))
  # 
  # predictions_table <- predictions_set %>% select(row_number, Segment.Level.x, Segment.Parent.x, Segment.Child.x,Cell.ID.x,
  #                                                 # Segment.Level.y, Segment.Parent.y, Segment.Child.y, mapB_cell_path,Cell.ID.y,
  #                                                 Segment.Level.y, Segment.Parent.y, Segment.Child.y, mapC_cell_path,Cell.ID.y
  # )
  # colnames(predictions_table) <- c('Row.Number','MapA.Segment.Level','MapA.Segment.Parent','MapA.Segment.Child','MapA.Cell.ID',
  #                                  'MapC.Segment.Level','MapC.Segment.Parent','MapC.Segment.Child','MapC.Cell.Path','MapC.Cell.ID')
  # 
  removed_outlier_cells <- as.vector(unique(hvt_mapB$Cell.Number))
  
  predictions_table$MapB.Cell.ID <- ifelse(
    (predictions_table$MapA.Segment.Child %in% identified_outlier_cells),
    paste0(predictions_table$MapA.Cell.ID),  # if condition is met
    NA   # else put NA
  )
  
  predictions_table$MapC.Cell.ID <- ifelse(
    (is.na(predictions_table$MapB.Cell.ID)),
    paste0(predictions_table$MapC.Cell.ID),  # if condition is met
    NA   # else put NA
  )

  predictions_table$Row.Number <- as.integer(predictions_table$Row.Number)
  predictions_table <- predictions_table[order(predictions_table$Row.Number, decreasing = FALSE),]
  
  scoredPredictionsData_CellID <- predictions_table %>% select(c('Row.Number','MapA.Cell.ID','MapB.Cell.ID','MapC.Cell.ID'))
  scoredPredictionsData_CellID <- lapply(scoredPredictionsData_CellID, as.numeric)
  
  scoredPredictionsData_CellID <- as.data.frame(scoredPredictionsData_CellID)
  unique.Cell.ID <- as.data.frame(unique(hvt_mapB$Cell.ID))
  colnames(unique.Cell.ID) <- c('MapB.Cell.ID')
  rownames(unique.Cell.ID) <- 1:nrow(unique.Cell.ID) 
  unique.Cell.ID$index <- row.names(unique.Cell.ID)
  unique.Cell.ID <- as.data.frame(unique.Cell.ID)
  
  scoredPredictionsData_CellID <- dplyr::left_join(scoredPredictionsData_CellID, unique.Cell.ID, by = "MapB.Cell.ID")
  scoredPredictionsData_CellID <- scoredPredictionsData_CellID %>% select(c('Row.Number','MapA.Cell.ID','index','MapC.Cell.ID'))
  colnames(scoredPredictionsData_CellID) <- c('Row.Number','MapA.Cell.ID','MapB.Cell.ID','MapC.Cell.ID')
  scoredPredictionsData_CellID <- as.data.frame(lapply(scoredPredictionsData_CellID, as.numeric))
  
  
  return(scoredPredictionsData_CellID)
}
