clusterPlot <- function(dataset, hvt.results, domains.column)
  suppressWarnings({
    
    hoverText = NULL
    lev = NULL
    
    domain.color <- c(
      "#FF4136", "#FF851B", "#FFDC00", "#2ECC40", "#0074D9",
      "#B10DC9", "#F012BE", "#85144b", "#3D9970", "#39CCCC",
      "#01FF70", "#DDDDDD", "#AAAAAA", "#FF6F61", "#6B5B95",
      "#88B04B", "#F7CAC9", "#92A8D1", "#955251", "#B565A7"
    )
    hvt_list <- hvt.results
    
    hvt_res1 <- hvt_list[[2]][[1]]$`1`
    hvt_res2 <- hvt_list[[3]]$summary$Cell.ID
    a <- 1:length(hvt_res1)
    b <- a[hvt_res2]
    b <- as.vector(b)
    hvt_res2 <- stats::na.omit(b)
    
    coordinates_value1 <- lapply(1:length(hvt_res1), function(x) {
      centroids1 <- hvt_res1[[x]]
      coordinates1 <- centroids1$pt
    })
    cellID_coordinates <- do.call(rbind.data.frame, coordinates_value1)
    colnames(cellID_coordinates) <- c("x", "y")
    cellID_coordinates$Cell.ID <- hvt_res2
    
    maxDepth <- 1
    
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
    cell_ids <- c()
    x_coords <- c()
    y_coords <- c()
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
          cell_ids[[length(cell_ids) + 1]] <- hvt_res2[childNo]
          x_coords[[length(x_coords) + 1]] <- x
          y_coords[[length(y_coords) + 1]] <- y
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
    
    valuesDataframe <- data.frame(depth = depthVal, cluster = clusterVal, child = childVal, cellid = unlist(cell_ids))
    positionsDataframe <- data.frame(depth = depthPos, cluster = clusterPos, child = childPos, x = x_pos, y = y_pos)
    centroidDataframe <- data.frame(x = x_cor, y = y_cor, lev = levelCluster)
    datapoly <- merge(valuesDataframe, positionsDataframe, by = c("depth", "cluster", "child"))
    centroidDataframe_2 <- merge(cellID_coordinates, centroidDataframe, by = c("x", "y"))
    
    colnames(datapoly) <- c("depth", "cluster", "child", "Cell.ID", "x", "y")
    datapoly <- merge(datapoly, dataset, by = c("Cell.ID"))
    column_color <- datapoly[, domains.column]
    
    centroidDataframe <- centroidDataframe %>% cbind(cellID_coordinates$Cell.ID)
    names(centroidDataframe) <- c("x", "y", "lev", "Cell.ID")
    dataset_new <- dataset %>% dplyr::select('Cell.ID', 'names.column')
    centroidDataframe <- merge(centroidDataframe, dataset_new, by = ('Cell.ID'))
    centroidDataframe_2 <- centroidDataframe_2 %>% cbind(centroidDataframe$names.column)
    colnames(centroidDataframe_2) <- c("x", "y", "Cell.ID", "lev", "names.column")
    
    wrap_and_limit_text <- function(text, line_length = 100, max_chars = 500) {
      # First, limit the total text to max_chars
      if (nchar(text) > max_chars) {
        text <- substr(text, 1, max_chars - 3)
        text <- paste0(text, "...")
      }
      
      # Then wrap the text
      wrapped <- sapply(seq(1, nchar(text), line_length), function(i) {
        substr(text, i, min(i + line_length - 1, nchar(text)))
      })
      
      # Join the wrapped lines with <br> for HTML line breaks
      paste(wrapped, collapse = "<br>")
    }
    
    if (nrow(datapoly) != 0) {
      datapoly$hoverText <- apply(datapoly, 1, function(row) {
        full_names <- row["names.column"]
        formatted_names <- wrap_and_limit_text(full_names, line_length = 100, max_chars = 500)
        
        paste(
          formatted_names
        )
      })
    } else {
      datapoly$hoverText <- NULL
    }
    
    domains_plot <- ggplot2::ggplot()
    for (i in maxDepth:1) {
      domains_plot <- domains_plot + ggplot2::geom_polygon(
        data = datapoly[which(datapoly$depth == i), ],
        ggplot2::aes(
          x = x,
          y = y,
          color = "black",
          size = factor(depth),
          group = interaction(depth, cluster, child),
          fill = factor(column_color),
          text = hoverText
        ),
        colour = "black",
        tooltip = "text",
        show.legend = F
      ) +
        ggplot2::scale_size_manual(values = 0.5, guide = "none") +
        ggplot2::labs(color = "Level") 
    }
    
    if (nrow(centroidDataframe) != 0) {
      centroidDataframe$hoverText <- apply(centroidDataframe, 1, function(row) {
        full_names <- row["names.column"]
        formatted_names <- wrap_and_limit_text(full_names, line_length = 100, max_chars = 500)
        
        paste(
          formatted_names
        )
      })
    } else {
      centroidDataframe$hoverText <- NULL
    }
    
    for (depth in 1:maxDepth) {
      domains_plot <- domains_plot + ggplot2::geom_point(
        data = centroidDataframe[centroidDataframe["lev"] == depth, ],
        ggplot2::aes(
          x = x,
          y = y,
          text = hoverText
        ),
        size = (1.5 / (2^(depth - 1))),
        pch = 21,
        fill = "black",
        color = "black",
        tooltip = "text"
      )
    }
    subset_data <- subset(centroidDataframe_2, lev == 1)
    
    domains_plot <- domains_plot +
      ggplot2::scale_fill_manual(
        name = "Domains",
        values = domain.color
      ) +
      ggplot2::theme_bw() + ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 20,
          hjust = 0.5,
          margin = ggplot2::margin(0, 0, 20, 0)
        ),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0))
    
    
    # Convert to plotly
    domainsPlot <- plotly::ggplotly(domains_plot, tooltip = "text")
    
    # Add annotations for Cell IDs in the plotly version
    domainsPlot <- domainsPlot %>%
      plotly::add_annotations(
        x = subset_data$x,
        y = subset_data$y,
        text = subset_data$Cell.ID,
        showarrow = FALSE,
        font = list(size = 10),
        yshift = -10  
      )
    
    # Function to clean up legend labels
    clean_label <- function(label) {
      gsub("^\\(1,|\\)$", "", label)
    }
    
    # Modify the plotly object to fix legend issues
    for (i in seq_along(domainsPlot$x$data)) {
      if (!is.null(domainsPlot$x$data[[i]]$name)) {
        # Remove the prefix from legend labels
        domainsPlot$x$data[[i]]$name <- clean_label(domainsPlot$x$data[[i]]$name)
      }
      if (!is.null(domainsPlot$x$data[[i]]$marker)) {
        # Remove the border from legend markers
        domainsPlot$x$data[[i]]$marker$line$width  <- 0
      }
    }
    
    
    
    # Continue with your existing layout modifications
    domainsPlot <- domainsPlot %>%
      plotly::layout(
        hoverlabel = list(bgcolor = "rgba(255,255,0,0.2)"),
        legend = list(
          title = list(text = "Domains"),
          itemsizing = "constant",
          itemdoubleclick = FALSE,
          itemclick = "toggleothers",
          traceorder = "reversed"
        )
      ) %>%
      plotly::config(displayModeBar = T)
    
    
    for (i in seq_along(domainsPlot$x$data)) {
      if (!is.null(domainsPlot$x$data[[i]]$marker) && 
          !is.null(domainsPlot$x$data[[i]]$showlegend) &&
          domainsPlot$x$data[[i]]$showlegend) {
        domainsPlot$x$data[[i]]$marker$line$width <- 0
        domainsPlot$x$data[[i]]$marker$line$color <- "rgba(0,0,0,0)"
      }
    }
    return(domainsPlot)
  })
