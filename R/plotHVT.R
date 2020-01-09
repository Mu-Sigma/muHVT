#' plotHVT
#'
#' Plot the hierarchical tesselations.
#'
#'
#' @param hvt.results List. A list containing the ouput of \code{HVT} function
#' which has the details of the tessellations to be plotted.
#' @param line.width Numeric Vector. A vector indicating the line widths of the
#' tessellation boundaries for each level.
#' @param color.vec Vector. A vector indicating the colors of the boundaries of
#' the tessellations at each level.
#' @param pch1 Numeric. Symbol type of the centroids of the tessellations
#' (parent levels). Refer \code{\link{points}}. (default = 19)
#' @param centroid.size Numeric. Size of centroids of first level
#' tessellations. (default = 3)
#' @param title String. Set a title for the plot. (default = NULL)
#' @author Sangeet Moy Das <sangeet.das@@mu-sigma.com>
#' @seealso \code{\link{HVT}} \cr \code{\link{hvtHmap}}
#' @keywords hplot
#' @importFrom magrittr %>%
#' @examples
#'
#'
#' data("USArrests",package="datasets")
#'
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 3, depth = 3, quant.err = 0.2,
#'                   projection.scale = 10, normalize = TRUE)
#' plotHVT(hvt.results, line.width = c(1.2,0.8,0.4), color.vec = c('#141B41','#0582CA','#8BA0B4'))
#'
#'
#'
#' @export plotHVT
plotHVT <-
  function(hvt.results,
           line.width,
           color.vec,
           pch1 = 21,
           centroid.size = 3,
           title = NULL,
           maxDepth) {
    library(ggplot2)
    
    hvt_list <- hvt.results
    min_x = 1e9
    min_y = 1e9
    max_x = -1e9
    max_y = -1e9
    depthVal = c()
    clusterVal = c()
    childVal = c()
    value = c()
    x_pos = c()
    y_pos = c()
    x_cor = c()
    y_cor = c()
    depthPos = c()
    clusterPos = c()
    childPos = c()
    levelCluster = c()
    for (clusterNo in 1:length(hvt_list[[2]][[1]][[1]])) {
      bp_x = hvt_list[[2]][[1]][[1]][[clusterNo]][["x"]]
      bp_y = hvt_list[[2]][[1]][[1]][[clusterNo]][["y"]]
      if (min(bp_x) < min_x)
        min_x = min(bp_x)
      if (max(bp_x) > max_x)
        max_x = max(bp_x)
      if (min(bp_y) < min_y)
        min_y = min(bp_y)
      if (max(bp_y) > max_y)
        max_y = max(bp_y)
      
    }
    for (depth in 1:maxDepth) {
      for (clusterNo in 1:length(hvt_list[[2]][[depth]])) {
        for (childNo in 1:length(hvt_list[[2]][[depth]][[clusterNo]])) {
          current_cluster = hvt_list[[2]][[depth]][[clusterNo]][[childNo]]
          
          x = as.numeric(current_cluster[["x"]])
          y = as.numeric(current_cluster[["y"]])
          x_cor = c(x_cor, as.numeric(current_cluster[["pt"]][["x"]]))
          y_cor = c(y_cor, as.numeric(current_cluster[["pt"]][["y"]]))
          depthVal = c(depthVal, depth)
          clusterVal = c(clusterVal, clusterNo)
          childVal = c(childVal, childNo)
          depthPos = c(depthPos, rep(depth, length(x)))
          clusterPos = c(clusterPos, rep(clusterNo, length(x)))
          childPos = c(childPos, rep(childNo, length(x)))
          x_pos = c(x_pos, x)
          y_pos = c(y_pos, y)
          levelCluster = c(levelCluster, depth)
        }
      }
    }
    valuesDataframe <- data.frame(depth = depthVal,
                                  cluster = clusterVal,
                                  child = childVal)
    
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
            by = c("depth", "cluster", "child"))
    
    p <- ggplot2::ggplot()
    
    for (i in maxDepth:1) {
      p <-
        p +  ggplot2::geom_polygon(
          data = datapoly[which(datapoly$depth == i), ],
          aes(
            x = x,
            y = y,
            color = factor(depth),
            size = factor(depth),
            group = interaction(depth, cluster, child)
            
          ),
          fill = NA
        ) +
        scale_colour_manual(values = color.vec) +
        scale_size_manual(values = line.width, guide = FALSE) +
        labs(color = "Level")
    }
    for (depth in 1:maxDepth) {
      p <-  p + ggplot2::geom_point(
        data = centroidDataframe[centroidDataframe["lev"] == depth, ],
        aes(x = x, y = y),
        size = (centroid.size / (2 ^ (depth - 1))),
        pch = 21,
        fill = color.vec[depth],
        color = color.vec[depth]
      )
    }
    
    p <- p +
      scale_color_manual(name = "Level",
                         values = color.vec) +
      ggplot2::theme_bw() + ggplot2::theme(
        plot.background = ggplot2::element_blank()
        ,
        plot.title = element_text(
          size = 20,
          hjust = 0.5,
          margin = margin(0, 0, 20, 0)
        )
        ,
        panel.grid = ggplot2::element_blank()
        ,
        panel.border = ggplot2::element_blank()
        ,
        axis.ticks = element_blank()
        ,
        axis.text = element_blank()
        ,
        axis.title = element_blank()
        ,
        panel.background = element_blank()
      ) + ggplot2::ggtitle("Hierarchical Voronoi Tessellation without heatmap overlay") + theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))
    
    return(suppressMessages(p))
    
    
    
  }
