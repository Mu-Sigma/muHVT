#' Heat Map over Hierarchical Voronoi Tessellations
#'
#' Main function to construct heatmap overlay for hierarchical voronoi tessellations.
#'
#' The output of the \code{HVT} function has all the required information about
#' the HVT. Now a heat map is overlayed over this HVT. The user defines the
#' level and also those variables of the data for which the heat map is to be
#' plotted. Now for each variable a separate heat map is plotted. The plot area
#' is divided into 2 screens where the first screen is relatively large and
#' will have the heat map. The second screen is small and contains the gradient
#' scale. To plot the heat map, the data is first normalized. The gradient
#' scale is divided into 'n' regions(500 is the set default). Using the
#' normalized data, the different regions into which the data items fall are
#' found. Each data item is now having a region on the gradient scale. This
#' color is filled in the tile corresponding to the data item. This procedure
#' is done for all the tiles for that level to get the complete heat map. Once
#' the heat map is ready, the higher level tessellations are plotted to
#' represent the hierarchies. The size of the centroids, the thickness of the
#' lines and the color of the tessellation lines can be given as input by the
#' user. Appropriate values for these parameters should be given to identify
#' the hierarchies properly. In the second screen the gradient scale is
#' plotted. The heat maps and hierarchical tessellations are obtained for all
#' the desired variables.
#'
#' @param hvt.results List. A list of hvt.results obtained from the HVT
#' function.
#' @param dataset Data frame. The input data set.
#' @param child.level Numeric. Indicating the level for which the heat map is
#' to be plotted.
#' @param hmap.cols Numeric or Character. The column number of column name from
#' the dataset indicating the variables for which the heat map is to be
#' plotted.
#' @param color.vec Vector. A color vector such that length(color.vec) =
#' (child.level - 1). (default = NULL)
#' @param line.width Vector. A line width vector such that length(line.width) =
#' (child.level - 1). (default = NULL)
#' @param centroid.size Numeric. Indicating the centroid size of the first
#' level. (default = 3)
#' @param pch Numeric. Indicating the centroid's symbol type.
#' (default = 21)
#' @param palette.color Numeric. Indicating the heat map color palette. 1 -
#' rainbow, 2 - heat.colors, 3 - terrain.colors, 4 - topo.colors, 5 -
#' cm.colors, 6 - seas color. (default = 6)
#' @param previous_level_heatmap Logical. If TRUE, the heatmap of previous level will be overlayed on the heatmap of selected level. If #' FALSE, the heatmap of only selected level will be plotted
#' @param show.points Logical. Indicating if the centroids should
#' be plotted on the tesselations. (default = FALSE)
#' @param asp Numeric. Indicating the aspect ratio type. For flexible aspect
#' ratio set, asp = NA. (default = 1)
#' @param ask Logical. If TRUE (and the R session is interactive) the user is
#' asked for input, before a new figure is drawn. (default = TRUE)
#' @param tess.label Vector. A vector for labelling the tessellations. (default
#' = NULL)
#' @param label.size Numeric. The size by which the tessellation labels should
#' be scaled. (default = 0.5)
#' @param quant.error.hmap Numeric. A number indicating the quantization error
#' treshold.
#' @param nclust.hmap Numeric. An integer indicating the number of clusters per
#' hierarchy (level)
#' @param ... The ellipsis is passed to it as additional argument. (Used internally)
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>, Sangeet Moy Das <sangeet.das@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords hplot
#' @importFrom magrittr %>%
#' @import ggplot2
#' @examples
#' data(USArrests)
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 15, depth = 1, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE,
#'                    quant_method="kmeans",diagnose=TRUE)
#' hvtHmap(hvt.results, USArrests, child.level = 1,hmap.cols = 'Murder', 
#'         line.width = c(0.2), color.vec = c('#141B41'),palette.color = 6,
#'         quant.error.hmap = 0.2,nclust.hmap = 6)
#' @export hvtHmap


hvtHmap <-
  function (hvt.results,
            dataset,
            child.level,
            hmap.cols,
            color.vec = NULL,
            line.width = NULL,
            centroid.size = 3,
            pch = 21,
            palette.color = 6,
            previous_level_heatmap = T,
            show.points = F,
            asp = 1,
            ask = T,
            tess.label = NULL,
            quant.error.hmap = NULL,
            nclust.hmap = NULL,
            label.size = .5,
            ...){
    # browser()
    
    hvt_list <- hvt.results
    # maxDepth <- child.level
    maxDepth <- min(child.level,max(hvt_list[[3]][["summary"]] %>% stats::na.omit() %>% dplyr::select("Segment.Level")))
    summaryDF = hvt_list[[3]][["summary"]]
    valuesDataframe <- data.frame(
      depth = 0,
      cluster = 0,
      child = 0,
      qe=0,
      n_cluster=0,
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
      if (depth < 3){
        
        for (clusterNo in names(hvt_list[[2]][[depth]])) {
          for (childNo in 1:length(hvt_list[[2]][[depth]][[clusterNo]])) {
            if (!is.null(hvt_list[[2]][[depth]][[clusterNo]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster = hvt_list[[2]][[depth]][[clusterNo]][[childNo]]
              
              
              val = summaryFilteredDF[, hmap.cols]
              qe = summaryFilteredDF[, "Quant.Error"]
              ncluster = summaryFilteredDF[, "n"]
              x = as.numeric(current_cluster[["x"]])
              y = as.numeric(current_cluster[["y"]])
              if("Cell.ID" %in% colnames(summaryFilteredDF)){
                cell_ID = summaryFilteredDF[, "Cell.ID"]
              } else{
                cell_ID = 0
              }
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe=qe,
                    n_cluster=ncluster,
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
                    child =  rep(childNo, length(x)),
                    x = x,
                    y = y
                  )
                )
              
              centroidDataframe <-
                rbind(centroidDataframe,
                      data.frame(
                        x =  as.numeric(current_cluster[["pt"]][["x"]]),
                        y =  as.numeric(current_cluster[["pt"]][["y"]]),
                        lev = depth
                      ))
              
              
              
            }
            
          }
        }
        
      }else{
        for (clusterNo in 1:nclust.hmap^(child.level-1)) {
          for (childNo in 1:length(hvt_list[[2]][[depth]][[as.character(clusterNo)]])) {
            if (!is.null(hvt_list[[2]][[depth]][[as.character(clusterNo)]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster = hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]]
              
              
              val = summaryFilteredDF[, hmap.cols]
              qe = summaryFilteredDF[, "Quant.Error"]
              
              x = as.numeric(current_cluster[["x"]])
              y = as.numeric(current_cluster[["y"]])
              if("Cell.ID" %in% colnames(summaryFilteredDF)){
                cell_ID = summaryFilteredDF[, "Cell.ID"]
              } else{
                cell_ID = 0
              }
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe=qe,
                    n_cluster=ncluster,
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
                    child =  rep(childNo, length(x)),
                    x = x,
                    y = y
                  )
                )
              
              centroidDataframe <-
                rbind(
                  centroidDataframe,
                  data.frame(
                    x =  as.numeric(current_cluster[["pt"]][["x"]]),
                    y =  as.numeric(current_cluster[["pt"]][["y"]]),
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
            by = c("depth", "cluster", "child"))
    
    p <- ggplot2::ggplot()
    colour_scheme <- c("#6E40AA", "#6B44B2", "#6849BA", "#644FC1", "#6054C8", "#5C5ACE" ,"#5761D3" ,"#5268D8", "#4C6EDB", "#4776DE", "#417DE0", "#3C84E1" ,"#368CE1",
                       "#3194E0", "#2C9CDF", "#27A3DC", "#23ABD8","#20B2D4", "#1DBACE", "#1BC1C9", "#1AC7C2" ,"#19CEBB", "#1AD4B3" ,"#1BD9AB", "#1DDFA3", "#21E39B",
                       "#25E892", "#2AEB8A" ,"#30EF82", "#38F17B" ,"#40F373", "#49F56D", "#52F667", "#5DF662", "#67F75E", "#73F65A", "#7FF658", "#8BF457", "#97F357", "#A3F258")
    
    data <- datapoly
    if(maxDepth>1){
      for(i in 1:(maxDepth-1)){
        index_tess<-which(data$depth==i & data$qe>quant.error.hmap & data$n_cluster>3 )
        data<-data[-index_tess,]
        
        rm(index_tess)
      }
    }
    
    # changing hoverText for torus demo
    if ("Cell.ID" %in% colnames(summaryFilteredDF)) {
      p <-
        p +  ggplot2::geom_polygon(
          data = data,
          ggplot2::aes(
            x = x,
            y = y,
            group = interaction(depth, cluster, child),
            fill = value,
            text = paste(" Cell.ID:", cellID,
                         "<br>", "Segment.Level:", depth,
                         "<br>", "Segment.Parent:", cluster,
                         "<br>", "Segment.Child:", child
            )
          )
          
        ) + 
        ggplot2::scale_fill_gradientn(colours = colour_scheme ) +
        ggplot2::labs(fill = hmap.cols)
      
    } else { p <-
      p +   ggplot2::geom_polygon(
        data = data,
        ggplot2::aes(
          x = x,
          y = y,
          group = interaction(depth, cluster, child),
          fill = value,
        )
      ) + 
      ggplot2::scale_fill_gradientn(colours = colour_scheme ) +
      ggplot2::labs(fill = hmap.cols)}
    
    
    for (i in child.level:1 ) {
      p <-
        p +  ggplot2::geom_polygon(
          data = datapoly[which(datapoly$depth==i),],
          ggplot2::aes(
            x = x,
            y = y,
            color = factor(depth),
            size = factor(depth),
            group = interaction(depth, cluster, child)
            
          ),
          fill = NA
        ) +
        ggplot2::scale_colour_manual(values = color.vec)+
        ggplot2::scale_size_manual(values=line.width,guide=FALSE) +
        ggplot2::labs(color = "Level")}
    
    
    
    
    
    for (depth in 1:maxDepth) {
      p <-  p + ggplot2::geom_point(
        data = centroidDataframe[centroidDataframe["lev"] == depth, ],
        ggplot2::aes(x = x, y = y),
        size = (centroid.size / (2 ^ (depth - 1))),
        fill = color.vec[depth],
        color = color.vec[depth]
      ) + ggplot2::theme(plot.background = ggplot2::element_blank()
                         ,plot.title = element_text(
                           size = 20,
                           hjust = 0.5,
                           margin = margin(0, 0, 20, 0)
                         )
                         ,panel.grid = ggplot2::element_blank()
                         ,panel.border = ggplot2::element_blank()
                         ,axis.ticks = element_blank()
                         ,axis.text = element_blank()
                         ,axis.title = element_blank()
                         ,panel.background = element_blank())+
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0))
    }
    
    return(suppressMessages(p))
  }
