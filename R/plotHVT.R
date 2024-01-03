#' @name plotHVT
#' @title Plot the hierarchical tesselations.
#'
#' Main plotting function to construct hierarchical voronoi tessellations in 1D or 2D or 3D.
#'
#' @param hvt.results (1D/2D/3D) List. A list containing the ouput of \code{trainHVT} function
#' which has the details of the tessellations to be plotted.
#' @param line.width (1D/2D) Numeric Vector. A vector indicating the line widths of the
#' tessellation boundaries for each level.
#' @param color.vec (1D/2D) Vector. A vector indicating the colors of the boundaries of
#' the tessellations at each level.
#' @param pch1 (1D/2D) Numeric. Symbol type of the centroids of the tessellations
#' (parent levels). Refer \code{\link{points}}.
#' @param centroid.size (1D/2D) Numeric. Size of centroids of first level
#' tessellations. 
#' @param title (1D) String. Set a title for the plot. (default = NULL)
#' @param maxDepth (1D) Numeric. An integer indicating the number of levels. (default = NULL)
#' @param dataset (2D) Data frame. The input data set.
#' @param child.level (2D/3D) Numeric. Indicating the level for which the heat map is
#' to be plotted.
#' @param hmap.cols (2D/3D) Numeric or Character. The column number of column name from
#' the dataset indicating the variables for which the heat map is to be
#' plotted.
#' @param previous_level_heatmap (2D) Logical. If TRUE, the heatmap of previous level
#' will be overlayed on the heatmap of selected level. If #' FALSE, the heatmap
#' of only selected level will be plotted
#' @param show.points (2D) Logical. Indicating if the centroids should
#' be plotted on the tessellations. (default = FALSE)
#' @param asp (2D) Numeric. Indicating the aspect ratio type. For flexible aspect
#' ratio set, asp = NA. (default = 1)
#' @param ask (2D) Logical. If TRUE (and the R session is interactive) the user is
#' asked for input, before a new figure is drawn. (default = TRUE)
#' @param tess.label (2D) Vector. A vector for labelling the tessellations. (default
#' = NULL)
#' @param label.size (2D) Numeric. The size by which the tessellation labels should
#' be scaled. (default = 0.5)
#' @param quant.error.hmap (2D) Numeric. A number indicating the quantization error
#' threshold.
#' @param n_cells.hmap (2D) Numeric. An integer indicating the number of
#' cells/clusters per hierarchy (level)
#' @param sepration_width (3D) Numeric. An integer indicating the width between two Levels
#' @param layer_opacity (3D) Numeric. A vector indicating the opacity of each layer/ level
#' @param dim_size  (3D) Numeric. An integer indicating the dimension size used to create the matrix for the plot
#' @param heatmap String. String indicating the dimensionality of the plot to be rendered in
#' @returns plot object containing the main HVT plot for the given HVT results and heatmap type.
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} 
#' @keywords Tessellation & Heatmap
#' @importFrom magrittr %>%
#' @import ggplot2
#' @examples
#'dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#'                      DAX = EuStockMarkets[, "DAX"],
#'                      SMI = EuStockMarkets[, "SMI"],
#'                      CAC = EuStockMarkets[, "CAC"],
#'                      FTSE = EuStockMarkets[, "FTSE"])
#'dataset_hvt <- dataset[,-c(1)]
#'hvt.results <- list()
#'hvt.results <- trainHVT(dataset_hvt, n_cells = 15, depth = 1, quant.err = 0.2, 
#'                        distance_metric = "L1_Norm", error_metric = "mean",
#'                        projection.scale = 10, normalize = TRUE, seed = 123,
#'                        quant_method="kmeans")
#' #1D - Plot
#' plotHVT(hvt.results, line.width = c(1.2), color.vec = c('#000000'), pch = 21, centroid.size = 1, 
#' maxDepth = 1,heatmap = '1D')
#' #2D - Plot
#' plotHVT(hvt.results, EuStockMarkets, centroid.size = 1,
#' child.level = 1, hmap.cols = "DAX",
#' line.width = c(0.6), color.vec = ('#000000') , 
#' pch1 = 21, heatmap = '2D')
#' #3D - Plot
#' plotHVT( hvt.results, child.level = 1, hmap.cols = "DAX", n_cells.hmap = 15, 
#' layer_opacity = c(0.7, 0.8, 0.99), dim_size = 1000, heatmap = '3D' )
#' @export plotHVT


plotHVT <- function(hvt.results, line.width, color.vec, pch1 = 21, centroid.size = 1.5, title = NULL, maxDepth = NULL, dataset, child.level, hmap.cols, previous_level_heatmap = TRUE, show.points = FALSE, asp = 1, ask = TRUE, tess.label = NULL, quant.error.hmap = NULL, n_cells.hmap = NULL, label.size = 0.5, sepration_width = 7, layer_opacity = c(0.5, 0.75, 0.99), dim_size = 1000, heatmap = '1D') {
  if (is.null(heatmap)) {
    heatmap <- '1D'
  }
  
  if (heatmap == '1D') {
    
    hvt_list <- hvt.results
    
    maxDepth <- min(maxDepth, max(hvt_list[[3]][["summary"]] %>% stats::na.omit() %>% dplyr::select("Segment.Level")))
    
    
    min_x <- 1e9
    min_y <- 1e9
    max_x <- -1e9
    max_y <- -1e9
    depthVal <- c()
    clusterVal <- c()
    childVal <- c()
    value <- c()
    x_pos <- c()
    y_pos <- c()
    x_cor <- c()
    y_cor <- c()
    depthPos <- c()
    clusterPos <- c()
    childPos <- c()
    levelCluster <- c()
    for (clusterNo in 1:length(hvt_list[[2]][[1]][[1]])) {
      bp_x <- hvt_list[[2]][[1]][[1]][[clusterNo]][["x"]]
      bp_y <- hvt_list[[2]][[1]][[1]][[clusterNo]][["y"]]
      
      
      if (min(bp_x) < min_x) {
        min_x <- min(bp_x)
      }
      if (max(bp_x) > max_x) {
        max_x <- max(bp_x)
      }
      if (min(bp_y) < min_y) {
        min_y <- min(bp_y)
      }
      if (max(bp_y) > max_y) {
        max_y <- max(bp_y)
      }
    }
    
    
    for (depth in 1:maxDepth) {
      for (clusterNo in 1:length(hvt_list[[2]][[depth]])) {
        for (childNo in 1:length(hvt_list[[2]][[depth]][[clusterNo]])) {
          current_cluster <- hvt_list[[2]][[depth]][[clusterNo]][[childNo]]
          
          x <- as.numeric(current_cluster[["x"]])
          y <- as.numeric(current_cluster[["y"]])
          x_cor <- c(x_cor, as.numeric(current_cluster[["pt"]][["x"]]))
          y_cor <- c(y_cor, as.numeric(current_cluster[["pt"]][["y"]]))
          depthVal <- c(depthVal, depth)
          clusterVal <- c(clusterVal, clusterNo)
          childVal <- c(childVal, childNo)
          depthPos <- c(depthPos, rep(depth, length(x)))
          clusterPos <- c(clusterPos, rep(clusterNo, length(x)))
          childPos <- c(childPos, rep(childNo, length(x)))
          x_pos <- c(x_pos, x)
          y_pos <- c(y_pos, y)
          levelCluster <- c(levelCluster, depth)
        }
      }
    }
    
    valuesDataframe <- data.frame(
      depth = depthVal,
      cluster = clusterVal,
      child = childVal
    )
    
    
    positionsDataframe <- data.frame(
      depth = depthPos,
      cluster = clusterPos,
      child = childPos,
      x = x_pos,
      y = y_pos
    )
    
    
    centroidDataframe <-
      data.frame(x = x_cor, y = y_cor, lev = levelCluster)
    
    datapoly <-
      merge(valuesDataframe,
            positionsDataframe,
            by = c("depth", "cluster", "child")
      )
    
    
    p <- ggplot2::ggplot()
    
    for (i in maxDepth:1) {
      p <-
        p + ggplot2::geom_polygon(
          data = datapoly[which(datapoly$depth == i), ],
          ggplot2::aes(
            x = x,
            y = y,
            color = factor(depth),
            size = factor(depth),
            group = interaction(depth, cluster, child),
          ),
          fill = NA
        ) +
        ggplot2::scale_colour_manual(values = color.vec) +
        ggplot2::scale_size_manual(values = line.width, guide = "none") +
        ggplot2::labs(color = "Level")
    }
    
    
    for (depth in 1:maxDepth) {
      p <- p + ggplot2::geom_point(
        data = centroidDataframe[centroidDataframe["lev"] == depth, ],
        ggplot2::aes(x = x, y = y),
        size = (centroid.size / (2^(depth - 1))),
        pch = pch1,
        fill = color.vec[depth],
        color = color.vec[depth]
      ) +
        ggplot2::geom_point(
          data = centroidDataframe[centroidDataframe["lev"] == depth, ],
          ggplot2::aes(x = x, y = y),
          size = (centroid.size / (2^(depth - 1))),
          pch = pch1,
          fill = color.vec[depth],
          color = color.vec[depth]
        )
    }
    
    p <- p +
      ggplot2::scale_color_manual(
        name = "Level",
        values = color.vec
      ) +
      ggplot2::theme_bw() + ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        plot.title = element_text(
          size = 20,
          hjust = 0.5,
          margin = margin(0, 0, 20, 0)
        ),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()
      ) + ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::geom_label(
        label = centroidDataframe$outlier_cell,
        nudge_x = 0.45, nudge_y = 0.1,
        check_overlap = TRUE,
        label.padding = unit(0.55, "lines"),
        label.size = 0.4,
        color = "white",
        fill = "#038225"
      ) +
      ggplot2::ggtitle(title)
    
    
    return(suppressMessages(p))
    
    
  } else if (heatmap == '2D') {
    hvt_list <- hvt.results
    # maxDepth <- child.level
    maxDepth <- min(child.level, max(hvt_list[[3]][["summary"]] %>% stats::na.omit() %>% dplyr::select("Segment.Level")))
    summaryDF <- hvt_list[[3]][["summary"]]
    valuesDataframe <- data.frame(
      depth = 0,
      cluster = 0,
      child = 0,
      qe = 0,
      n_cluster = 0,
      value = 0,
      cellID = 0
    )
    positionsDataframe <- data.frame(
      depth = 0,
      cluster = 0,
      child = 0,
      x = 0,
      y = 0
    )
    
    centroidDataframe <- data.frame(x = 0, y = 0, lev = 0)
    #  browser()
    for (depth in 1:maxDepth) {
      if (depth < 3) {
        for (clusterNo in names(hvt_list[[2]][[depth]])) {
          for (childNo in 1:length(hvt_list[[2]][[depth]][[clusterNo]])) {
            if (!is.null(hvt_list[[2]][[depth]][[clusterNo]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster <- hvt_list[[2]][[depth]][[clusterNo]][[childNo]]
              
              
              val <- summaryFilteredDF[, hmap.cols]
              qe <- summaryFilteredDF[, "Quant.Error"]
              ncluster <- summaryFilteredDF[, "n"]
              x <- as.numeric(current_cluster[["x"]])
              y <- as.numeric(current_cluster[["y"]])
              if ("Cell.ID" %in% colnames(summaryFilteredDF)) {
                cell_ID <- summaryFilteredDF[, "Cell.ID"]
              } else {
                cell_ID <- 0
              }
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe = qe,
                    n_cluster = ncluster,
                    value = val,
                    cellID = cell_ID
                  )
                )
              positionsDataframe <-
                rbind(
                  positionsDataframe,
                  data.frame(
                    depth = rep(depth, length(x)),
                    cluster = rep(clusterNo, length(x)),
                    child = rep(childNo, length(x)),
                    x = x,
                    y = y
                  )
                )
              
              centroidDataframe <-
                rbind(
                  centroidDataframe,
                  data.frame(
                    x = as.numeric(current_cluster[["pt"]][["x"]]),
                    y = as.numeric(current_cluster[["pt"]][["y"]]),
                    lev = depth
                  )
                )
            }
          }
        }
      } else {
        for (clusterNo in 1:n_cells.hmap^(child.level - 1)) {
          for (childNo in 1:length(hvt_list[[2]][[depth]][[as.character(clusterNo)]])) {
            if (!is.null(hvt_list[[2]][[depth]][[as.character(clusterNo)]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster <- hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]]
              
              
              val <- summaryFilteredDF[, hmap.cols]
              qe <- summaryFilteredDF[, "Quant.Error"]
              
              x <- as.numeric(current_cluster[["x"]])
              y <- as.numeric(current_cluster[["y"]])
              if ("Cell.ID" %in% colnames(summaryFilteredDF)) {
                cell_ID <- summaryFilteredDF[, "Cell.ID"]
              } else {
                cell_ID <- 0
              }
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe = qe,
                    n_cluster = ncluster,
                    value = val,
                    cellID = cell_ID
                  )
                )
              positionsDataframe <-
                rbind(
                  positionsDataframe,
                  data.frame(
                    depth = rep(depth, length(x)),
                    cluster = rep(clusterNo, length(x)),
                    child = rep(childNo, length(x)),
                    x = x,
                    y = y
                  )
                )
              
              centroidDataframe <-
                rbind(
                  centroidDataframe,
                  data.frame(
                    x = as.numeric(current_cluster[["pt"]][["x"]]),
                    y = as.numeric(current_cluster[["pt"]][["y"]]),
                    lev = depth
                  )
                )
            }
          }
        }
      }
    }
    valuesDataframe <- valuesDataframe[2:nrow(valuesDataframe), ]
    
    positionsDataframe <-
      positionsDataframe[2:nrow(positionsDataframe), ]
    
    centroidDataframe <-
      centroidDataframe[2:nrow(centroidDataframe), ]
    
    
    datapoly <-
      merge(valuesDataframe,
            positionsDataframe,
            by = c("depth", "cluster", "child")
      )
    
    p <- ggplot2::ggplot()
    colour_scheme <- c(
      "#6E40AA", "#6B44B2", "#6849BA", "#644FC1", "#6054C8", "#5C5ACE", "#5761D3", "#5268D8", "#4C6EDB", "#4776DE", "#417DE0", "#3C84E1", "#368CE1",
      "#3194E0", "#2C9CDF", "#27A3DC", "#23ABD8", "#20B2D4", "#1DBACE", "#1BC1C9", "#1AC7C2", "#19CEBB", "#1AD4B3", "#1BD9AB", "#1DDFA3", "#21E39B",
      "#25E892", "#2AEB8A", "#30EF82", "#38F17B", "#40F373", "#49F56D", "#52F667", "#5DF662", "#67F75E", "#73F65A", "#7FF658", "#8BF457", "#97F357", "#A3F258"
    )
    
    data <- datapoly
    if (maxDepth > 1) {
      for (i in 1:(maxDepth - 1)) {
        index_tess <- which(data$depth == i & data$qe > quant.error.hmap & data$n_cluster > 3)
        data <- data[-index_tess, ]
        
        rm(index_tess)
      }
    }
    
    # changing hoverText for torus demo
    if ("Cell.ID" %in% colnames(summaryFilteredDF)) {
      p <-
        p + ggplot2::geom_polygon(
          data = data,
          ggplot2::aes(
            x = x,
            y = y,
            group = interaction(depth, cluster, child),
            fill = value,
            text = paste(
              " Cell.ID:", cellID,
              "<br>", "Segment.Level:", depth,
              "<br>", "Segment.Parent:", cluster,
              "<br>", "Segment.Child:", child
            )
          )
        ) +
        ggplot2::scale_fill_gradientn(colours = colour_scheme) +
        ggplot2::labs(fill = hmap.cols)
    } else {
      p <-
        p + ggplot2::geom_polygon(
          data = data,
          ggplot2::aes(
            x = x,
            y = y,
            group = interaction(depth, cluster, child),
            fill = value,
          )
        ) +
        ggplot2::scale_fill_gradientn(colours = colour_scheme) +
        ggplot2::labs(fill = hmap.cols)
    }
    
    
    for (i in child.level:1) {
      p <-
        p + ggplot2::geom_polygon(
          data = datapoly[which(datapoly$depth == i), ],
          ggplot2::aes(
            x = x,
            y = y,
            color = factor(depth),
            size = factor(depth),
            group = interaction(depth, cluster, child)
          ),
          fill = NA
        ) +
        ggplot2::scale_colour_manual(values = color.vec) +
        ggplot2::scale_size_manual(values = line.width, guide = FALSE) +
        ggplot2::labs(color = "Level")
    }
    
    
    
    
    
    for (depth in 1:maxDepth) {
      p <- p + ggplot2::geom_point(
        data = centroidDataframe[centroidDataframe["lev"] == depth, ],
        ggplot2::aes(x = x, y = y),
        size = (centroid.size / (2^(depth - 1))),
        pch =pch1,
        fill = color.vec[depth],
        color = color.vec[depth]
      ) + ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        plot.title = element_text(
          size = 20,
          hjust = 0.5,
          margin = margin(0, 0, 20, 0)
        ),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()
      ) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0))
    }
    
    return(suppressMessages(p))
    
  } else if (heatmap == '3D') {
    
    maxDepth <- min(child.level, max(hvt.results[[3]][["summary"]]
                                     %>% stats::na.omit()
                                     %>% dplyr::select("Segment.Level")))
    summaryDF <- hvt.results[[3]][["summary"]]
    valuesDataframe <- data.frame(
      depth = 0,
      cluster = 0,
      child = 0,
      qe = 0,
      n_cluster = 0,
      value = 0,
      cellID = 0
    )
    positionsDataframe <- data.frame(
      depth = 0,
      cluster = 0,
      child = 0,
      x = 0,
      y = 0
    )
    
    centroidDataframe <- data.frame(x = 0, y = 0, lev = 0)
    
    for (depth in 1:maxDepth) {
      if (depth < 3) {
        for (clusterNo in names(hvt.results[[2]][[depth]])) {
          for (childNo in 1:length(hvt.results[[2]][[depth]][[clusterNo]])) {
            if (!is.null(hvt.results[[2]][[depth]][[clusterNo]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster <- hvt.results[[2]][[depth]][[clusterNo]][[childNo]]
              
              
              val <- summaryFilteredDF[, hmap.cols]
              qe <- summaryFilteredDF[, "Quant.Error"]
              ncluster <- summaryFilteredDF[, "n"]
              x <- as.numeric(current_cluster[["x"]])
              y <- as.numeric(current_cluster[["y"]])
              if ("Cell.ID" %in% colnames(summaryFilteredDF)) {
                cell_ID <- summaryFilteredDF[, "Cell.ID"]
              } else {
                cell_ID <- 0
              }
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe = qe,
                    n_cluster = ncluster,
                    value = val,
                    cellID = cell_ID
                  )
                )
              positionsDataframe <-
                rbind(
                  positionsDataframe,
                  data.frame(
                    depth = rep(depth, length(x)),
                    cluster = rep(clusterNo, length(x)),
                    child = rep(childNo, length(x)),
                    x = x,
                    y = y
                  )
                )
              
              centroidDataframe <-
                rbind(
                  centroidDataframe,
                  data.frame(
                    x = as.numeric(current_cluster[["pt"]][["x"]]),
                    y = as.numeric(current_cluster[["pt"]][["y"]]),
                    lev = depth
                  )
                )
            }
          }
        }
      } else {
        for (clusterNo in 1:n_cells.hmap^(child.level - 1)) {
          for (childNo in 1:length(hvt.results[[2]][[depth]][[as.character(clusterNo)]])) {
            if (!is.null(hvt.results[[2]][[depth]][[as.character(clusterNo)]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster <- hvt.results[[2]][[depth]][[as.character(clusterNo)]][[childNo]]
              
              
              val <- summaryFilteredDF[, hmap.cols]
              qe <- summaryFilteredDF[, "Quant.Error"]
              
              x <- as.numeric(current_cluster[["x"]])
              y <- as.numeric(current_cluster[["y"]])
              if ("Cell.ID" %in% colnames(summaryFilteredDF)) {
                cell_ID <- summaryFilteredDF[, "Cell.ID"]
              } else {
                cell_ID <- 0
              }
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe = qe,
                    n_cluster = ncluster,
                    value = val,
                    cellID = cell_ID
                  )
                )
              positionsDataframe <-
                rbind(
                  positionsDataframe,
                  data.frame(
                    depth = rep(depth, length(x)),
                    cluster = rep(clusterNo, length(x)),
                    child = rep(childNo, length(x)),
                    x = x,
                    y = y
                  )
                )
              
              centroidDataframe <-
                rbind(
                  centroidDataframe,
                  data.frame(
                    x = as.numeric(current_cluster[["pt"]][["x"]]),
                    y = as.numeric(current_cluster[["pt"]][["y"]]),
                    lev = depth
                  )
                )
            }
          }
        }
      }
    }
    valuesDataframe <- valuesDataframe[2:nrow(valuesDataframe), ]
    
    positionsDataframe <-
      positionsDataframe[2:nrow(positionsDataframe), ]
    
    centroidDataframe <-
      centroidDataframe[2:nrow(centroidDataframe), ]
    
    
    datapoly <-
      merge(valuesDataframe,
            positionsDataframe,
            by = c("depth", "cluster", "child")
      )
    
    
    
    Level_list <- lapply(1:maxDepth, function(x) {
      temp_df <- datapoly[datapoly$depth == x, ]
      min_x <- min(datapoly$x)
      max_x <- max(datapoly$x)
      min_y <- min(datapoly$y)
      max_y <- max(datapoly$y)
      temp_df <- temp_df %>% mutate(name = paste0("Segment", cluster, "-", child))
      # segments_number=unique()
      returnList <- split(temp_df, f = temp_df$name)
      return(list(
        returnList,
        min_x,
        max_x,
        min_y,
        max_y
      ))
    })
    
    
    
    # Level_1_data= data %>% dplyr::filter(depth)
    #####################################################################################
    depth_wise_surface <- function(finalList
                                   # ,
                                   # hmap.cols,
                                   # dim_size
    ) {
      # browser()
      column <- hmap.cols
      min_x <- unlist(finalList[[2]])
      max_x <- unlist(finalList[[3]])
      min_y <- unlist(finalList[[4]])
      max_y <- unlist(finalList[[5]])
      
      # dim_size = 10000
      y <- rep(1:dim_size, times = dim_size)
      x <- rep(1:dim_size, each = dim_size)
      hvtVolcanoMatrix <- rep(0, each = dim_size^2)
      for (k in names(finalList[[1]])) {
        eval(parse(
          text = paste0(
            "polygon_x <- round(scales::rescale(finalList[[1]]$`",
            k,
            "`$x, to=c(0, ", dim_size, "), from  = c(min_x,max_x)))"
          )
        ))
        eval(parse(
          text = paste0(
            "polygon_y <- round(scales::rescale(finalList[[1]]$`",
            k,
            "`$y, to=c(0, ", dim_size, "), from  = c(min_y,max_y)))"
          )
        ))
        present <- sp::point.in.polygon(x, y, polygon_x, polygon_y)
        eval(parse(
          text = paste0(
            "quantError <- finalList[[1]]$`",
            k,
            "`$value[1]"
          )
        ))
        hvtVolcanoMatrix[which(present != 0)] <- quantError
      }
      
      hvtVolcanoMatrix <- matrix(hvtVolcanoMatrix, nrow = dim_size)
      return(hvtVolcanoMatrix)
    }
    
    #####################################################################################
    
    temp_hvtVolcanoMatrix <- lapply(Level_list, depth_wise_surface)
    number_of_layers <- length(temp_hvtVolcanoMatrix)
    p <- plotly::plot_ly(showscale = FALSE)
    
    temp <- lapply(1:number_of_layers, function(i) {
      hvtVolcanoMatrix <- temp_hvtVolcanoMatrix[[i]] - ((i - 1) * sepration_width)
      p <<- p %>%
        plotly::add_surface(z = ~hvtVolcanoMatrix, opacity = layer_opacity[i], name = paste("Layer_", i), showlegend = TRUE)
    })
    
    p <- p %>%
      plotly::config(displaylogo = FALSE) %>%
      plotly::layout(
        zaxis = list(title = hmap.cols),
        title = paste(
          "Hierarchical Voronoi Tessellation 3D Surface plot with",
          hmap.cols,
          "Heatmap Overlay"
        ),
        scene = list(zaxis = list(title = hmap.cols))
      )
    
    
    
    return(suppressMessages(p))
    
  } else {
    stop("Invalid value for 'heatmap'. Expected '1D', '2D', or '3D'.")
  }
}
