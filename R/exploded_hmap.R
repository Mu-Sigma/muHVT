#' @name exploded_hmap
#' 
#' @title Function to construct an interactive 3D heatmap overlay for each HVT Level
#'
#' @param hvt.results List. A list of hvt.results obtained from the HVT
#' function.
#' @param child.level Numeric. Indicating the level for which the heat map is
#' to be plotted.
#' @param hmap.cols Numeric or Character. The column number of column name from
#' the dataset indicating the variables for which the heat map is to be
#' plotted.
#' @param n_cells.hmap Numeric. An integer indicating the number of clusters per
#' hierarchy (level)
#' @param sepration_width Numeric. An integer indicating the width between two Levels
#' @param layer_opacity Numeric. A vector indicating the opacity of each layer/ level
#' @param dim_size Numeric. An integer indicating the dimension size used to create the matrix for the plot
#' @param ... color.vec and line.width can be passed from here
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @export exploded_hmap
#' 
#' 
exploded_hmap <-
  function (hvt.results,
            child.level=NULL, # Numeric
            hmap.cols= NULL, # Character
            n_cells.hmap = NULL, # Numeric
            sepration_width=7,
            layer_opacity=c(0.5,0.75,0.99),
            dim_size=1000,
            ...){

    # library(tidyverse)
    # library(plotly)
    maxDepth <- min(child.level,max(hvt.results[[3]][["summary"]] %>% stats::na.omit() %>% dplyr::select("Segment.Level")))
    summaryDF = hvt.results[[3]][["summary"]]
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
        
        for (clusterNo in names(hvt.results[[2]][[depth]])) {
          for (childNo in 1:length(hvt.results[[2]][[depth]][[clusterNo]])) {
            if (!is.null(hvt.results[[2]][[depth]][[clusterNo]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster = hvt.results[[2]][[depth]][[clusterNo]][[childNo]]
              
              
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
        for (clusterNo in 1:n_cells.hmap^(child.level-1)) {
          for (childNo in 1:length(hvt.results[[2]][[depth]][[as.character(clusterNo)]])) {
            if (!is.null(hvt.results[[2]][[depth]][[as.character(clusterNo)]])) {
              summaryFilteredDF <-
                summaryDF %>% dplyr::filter(
                  Segment.Level == depth,
                  Segment.Parent == clusterNo,
                  Segment.Child == childNo
                )
              
              current_cluster = hvt.results[[2]][[depth]][[as.character(clusterNo)]][[childNo]]
              
              
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
    
    

    Level_list = lapply(1:maxDepth, function(x) {
      temp_df=datapoly[datapoly$depth==x,] 
      min_x=min(datapoly$x)
      max_x=max(datapoly$x)
      min_y=min(datapoly$y)
      max_y=max(datapoly$y)
      temp_df = temp_df %>% mutate(name=paste0("Segment",cluster,"-",child))
      # segments_number=unique()
      returnList=split(temp_df, f =temp_df$name )  
      return(list(returnList,
                  min_x,
                  max_x,
                  min_y,
                  max_y))
    })



# Level_1_data= data %>% dplyr::filter(depth)    
 #####################################################################################   
    depth_wise_surface =function(finalList
                                 # ,
                                 # hmap.cols,
                                 # dim_size
                                 ){
      # browser()
      column <- hmap.cols
      min_x = unlist(finalList[[2]])
      max_x = unlist(finalList[[3]])
      min_y = unlist(finalList[[4]])
      max_y = unlist(finalList[[5]])
      
      # dim_size = 10000
      y <- rep(1:dim_size, times = dim_size)
      x <- rep(1:dim_size, each = dim_size)
      hvtVolcanoMatrix <- rep(0, each = dim_size^2)
      for (k in names(finalList[[1]])) {
        eval(parse(
          text = paste0(
            'polygon_x <- round(scales::rescale(finalList[[1]]$`',
            k,
            '`$x, to=c(0, ', dim_size,'), from  = c(min_x,max_x)))'
          )
        ))
        eval(parse(
          text = paste0(
            'polygon_y <- round(scales::rescale(finalList[[1]]$`',
            k,
            '`$y, to=c(0, ', dim_size,'), from  = c(min_y,max_y)))'
          )
        ))
        present <- sp::point.in.polygon(x, y, polygon_x, polygon_y)
        eval(parse(
          text = paste0(
            'quantError <- finalList[[1]]$`',
            k,
            '`$value[1]'
          )
        ))
        hvtVolcanoMatrix[which(present != 0)] <- quantError
      }
      
      hvtVolcanoMatrix <- matrix(hvtVolcanoMatrix, nrow = dim_size)
      return(hvtVolcanoMatrix)
    }  
    
    #####################################################################################      
    
   temp_hvtVolcanoMatrix= lapply(Level_list, depth_wise_surface) 
    number_of_layers = length(temp_hvtVolcanoMatrix)
    p = plotly::plot_ly(showscale=F)
    
    temp=lapply(1: number_of_layers, function(i){
      hvtVolcanoMatrix=temp_hvtVolcanoMatrix[[i]] - ((i-1) * sepration_width)
      p <<- p %>%
        plotly::add_surface(z = ~ hvtVolcanoMatrix,opacity = layer_opacity[i] ,name=paste("Layer_",i), showlegend = TRUE) 
    })

    p<-p %>%
      plotly::config( displaylogo = FALSE) %>%
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

  }













