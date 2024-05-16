

plotHVT <- function(hvt.results, line.width = 0.5, color.vec =  'black', pch1 = 21, centroid.size = 1.5, 
                    title = NULL, maxDepth = NULL, child.level, hmap.cols, quant.error.hmap = NULL,cell_id = FALSE,
                    n_cells.hmap = NULL, label.size = 0.5, sepration_width = 7, layer_opacity = c(0.5, 0.75, 0.99), 
                    dim_size = 1000, plot.type = '2Dhvt') {
  
  lev <- NULL
  if (is.null(plot.type)) {
    plot.type <- '2Dhvt'
  }
  if (is.null(cell_id)){
    cell_id <- FALSE
  }
  if (plot.type == '1D') {
   # browser()
    ####hvq output as global vaiable
    generic_col=c("Segment.Level","Segment.Parent","Segment.Child","n","Quant.Error")
    hvq_k <- hvt.results[[6]]  
    temp_summary=hvq_k[["summary"]] %>% dplyr::select(!generic_col) %>% dplyr::mutate(id=row_number())
    cent_val= temp_summary %>% subset(.,complete.cases(.)) 
    set.seed(123)
    sammon_1d_cord <- MASS::sammon(
      d = stats::dist(cent_val %>% dplyr::select(!id),method = "manhattan"),
      niter = 10 ^ 5,
      trace = FALSE,
      k=1
    )$points
    temp_df=data.frame(sammon_1d_cord,id=cent_val$id)%>%dplyr::arrange(sammon_1d_cord) %>% dplyr::mutate(Cell.ID=row_number()) %>% dplyr::select(!sammon_1d_cord)
    temp_summary = dplyr::left_join(temp_summary,temp_df,by="id") %>% select(!"id")
    hvq_k[["summary"]]$Cell.ID=temp_summary$Cell.ID
    
    x <-sammon_1d_cord
    y <- hvq_k[["summary"]][["Cell.ID"]]
    data_plot <- data.frame(x,y)
    
      if(length(y) <= 100) {dot_size <- 5} else if(length(y) <= 500) {dot_size <- 2.5} 
      else if(length(y) <= 1000) {dot_size <- 1.5} else {dot_size <- 1}
    
    plotly_obj <-  plotly::plot_ly(data_plot, x = ~y, y = ~x, type = 'scatter', mode = 'markers',
            marker = list(size = dot_size, color = 'blue', symbol = 'circle'),
            text = ~paste('Cell ID:', y, '<br>1D point:', round(x,4)), 
            hoverinfo = 'text') %>% 
            plotly::layout(title = 'Sammons 1D x Cell ID',
             xaxis = list(title = 'Cell ID', zeroline = FALSE),
             yaxis = list(title = '1D points',zeroline = FALSE))
    
     return(suppressMessages(plotly_obj))  
      
  } else if (plot.type == '2Dproj'){
   
     hvt_centroids_list <- hvt.results
      
    hvt_coordinates<- hvt_centroids_list[[2]][[1]][["1"]]
    centroids <- list()
    coordinates_value <- lapply(1:length(hvt_coordinates), function(x){
      centroids <-hvt_coordinates[[x]]
      coordinates <- centroids$pt
    })
    centroid_coordinates<<- do.call(rbind.data.frame, coordinates_value)  
    colnames(centroid_coordinates) <- c("x_coord","y_coord")
    centroid_coordinates$Row.No <- as.numeric(row.names(centroid_coordinates)) 
    centroid_coordinates <- centroid_coordinates %>% dplyr::select(Row.No,x_coord,y_coord)
    centroid_coordinates1 <- centroid_coordinates %>% data.frame() %>% round(4)

    gg_proj <- ggplot(centroid_coordinates1, aes(x_coord, y_coord)) +
      geom_point(color = "blue") +
      labs(x = "X", y = "Y")
    
    
    return(suppressMessages(gg_proj))
    
     } else if (plot.type == '2Dhvt') {
      
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
   # browser()
    if(cell_id == TRUE){
    hvt_res1 <- hvt_list[[2]][[1]]$`1`
    hvt_res2 <- hvt_list[[3]]$summary$Cell.ID
    a <- 1: length(hvt_res1)
    b <- a[hvt_res2]
    b <-  as.vector(b)
    hvt_res2 <- stats::na.omit(b)
    
    coordinates_value1 <- lapply(1:length(hvt_res1), function(x) {
      centroids1 <- hvt_res1[[x]]
      coordinates1 <- centroids1$pt})
    cellID_coordinates <- do.call(rbind.data.frame, coordinates_value1)
    colnames(cellID_coordinates) <- c("x", "y")
    cellID_coordinates$Cell.ID <- hvt_res2
    centroidDataframe_2 <- merge(cellID_coordinates, centroidDataframe, by = c("x" ,"y"))
    
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
    
   # browser()
    #for (depth in 1:maxDepth) {
    # for (centroidDataframe$lev == 1) {
    #   p <- p + ggplot2::geom_text(
    #     data = centroidDataframe[centroidDataframe["lev"] == depth, ],
    #     ggplot2::aes(x = x, y = y, label = centroidDataframe$Cell.ID ),
    #     size = 3,
    #     color = color.vec[depth], vjust = -1
    #   ) +
    #    ggplot2::geom_text(
    #       data = centroidDataframe[centroidDataframe["lev"] == depth, ],
    #       ggplot2::aes(x = x, y = y, label = centroidDataframe$Cell.ID ),
    #       size = 3,
    #       color = color.vec[depth], vjust = -1
    #     )
    # }
    subset_data <- subset(centroidDataframe_2, lev == 1)  # Filter the dataframe
   # for (depth in 1:maxDepth) {
      p <- p + ggplot2::geom_text(
        data = subset_data,
        ggplot2::aes(x = x, y = y, label = Cell.ID),
        size = 3,
        color = "black",
        vjust = -1
      )
    #}
    
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
    } else{
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
      
      
    }

    return(suppressMessages(p))
    
    
  } else if (plot.type == '2Dheatmap') {
  # browser()
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
    # browser()
    data <- datapoly
    if (maxDepth > 1) {
      for (i in 1:(maxDepth - 1)) {
        #index_tess <- which(data$depth == i & data$qe > quant.error.hmap & data$n_cluster > 3)
         index_tess <- which( data$qe > quant.error.hmap )
        
        if (length(index_tess) >0){
          data <- data[-index_tess, ] 
        } else {
            data = data
          }
         
        #rm(index_tess)
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
        #browser()
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
# browser()
    return(suppressMessages(p))
    
  } else if (plot.type == 'surface_plot') {
    
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
          "Hierarchical Voronoi Tessellation Interactive surface plot with",
          hmap.cols,
          "Heatmap Overlay"
        ),
        scene = list(zaxis = list(title = hmap.cols))
      )
    
    
    
    return(suppressMessages(p))
    
  } else {
    stop("Invalid value for 'plot.type'. Expected '1D','2Dproj', '2Dhvt','2Dheatmap', or 'surface_plot'.")
  }
}
