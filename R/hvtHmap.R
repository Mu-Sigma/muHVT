#' hvtHmap
#'
#' Heat Map over Hierarchical Voronoi Tessellations
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
#' @param ... The ellipsis is passed to it as additional argument. (Used internally)
#' @author Sangeet Moy Das <sangeet.das@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords hplot
#' @importFrom magrittr %>%
#' @examples
#' data(USArrests)
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 6, depth = 1, quant.err = 0.2,
#'                   projection.scale = 10, normalize = TRUE)
#' hvtHmap(hvt.results, USArrests, child.level = 1,hmap.cols = 'Murder', line.width = c(0.2),
#' color.vec = c('#141B41'),palette.color = 6)
#'
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 3, depth = 3, quant.err = 0.2,
#'                   projection.scale = 10, normalize = TRUE)
#' hvtHmap(hvt.results, train_computers, child.level = 3,hmap.cols = 'quant_error',
#' line.width = c(1.2,0.8,0.4),color.vec = c('#141B41','#0582CA','#8BA0B4'),palette.color = 6)
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
            quant.err_hmap = NULL,
            nclust_hmap = NULL,
            label.size = .5,
            ...)
  {
    hvt_list <- hvt.results
    maxDepth <- child.level
    
    
   #  maxDepth =2
   # hmap.cols ="Quant.Error" 
   #  centroid.size=3
   #  color.vec <- c("red","green")
   #  line.width <- c(1,2)
   #  hvt_list <- readRDS("/home/sanjay/hvt.RDS")
    # level_names <- list()
    # 
    # for (level in 1:maxDepth) {
    #   level_names[[level]] <- hvt_list[[2]][[level]] %>% map( ~ {
    #     this <- .
    #     element <- this[[1]]
    #     return(element$Segment.Parent)
    #   }) %>% unlist()
    #   
    #   names(hvt_list[[2]][[level]]) <- level_names[[level]]
    # }
    
    summaryDF = hvt_list[[3]][["summary"]]
    # min_x = 1e9
    # min_y = 1e9
    # max_x = -1e9
    # max_y = -1e9
    # 
    # 
    # for (clusterNo in 1:length(hvt_list[[2]][[1]][[1]])) {
    #   bp_x = hvt_list[[2]][[1]][["1"]][[clusterNo]][["x"]]
    #   bp_y = hvt_list[[2]][[1]][["1"]][[clusterNo]][["y"]]
    #   
    #   
    #   if (min(bp_x) < min_x)
    #     min_x = min(bp_x)
    #   if (max(bp_x) > max_x)
    #     max_x = max(bp_x)
    #   if (min(bp_y) < min_y)
    #     min_y = min(bp_y)
    #   if (max(bp_y) > max_y)
    #     max_y = max(bp_y)
    #   
    # }
    
    
    valuesDataframe <- data.frame(
      depth = 0,
      cluster = 0,
      child = 0,
      qe=0,
      n_cluster=0,
      value = 0
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
      if (depth < 3)
        
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
              
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe=qe,
                    n_cluster=ncluster,
                    value = val
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
      
      else{
        for (clusterNo in 1:nclust_hmap^(child.level-1)) {
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
              
              valuesDataframe <-
                rbind(
                  valuesDataframe,
                  data.frame(
                    depth = depth,
                    cluster = clusterNo,
                    child = childNo,
                    qe=qe,
                    n_cluster=ncluster,
                    value = val
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
    
    p <- ggplot()
    # crp6 <- "#aff05b"
    # crp5 <- "#19d0b8"
    # crp4 <- "#368ee1"
    # crp3 <- "#3a88e1"
    # crp2 <-'#5069d9'
    # crp1 <- '#6e40aa'.
    crp1<-"#A3F258"
    crp2<-"#97F357"
    crp3<-"#8BF457"
    crp4<-"#7FF658"
    crp5<-"#73F65A"
    crp6<-"#67F75E"
    crp7<-"#5DF662"
    crp8<-'#52F667'
    crp9<-'#49F56D'
    crp10<-'#40F373'
    crp11<-'#38F17B'
    crp12<-'#30EF82'
    crp13<-'#2AEB8A'
    crp14<-'#25E892'
    crp15<-'#21E39B'
    crp16<-'#1DDFA3'
    crp17<-'#1BD9AB'
    crp18<-'#1AD4B3'
    crp19<-'#19CEBB'
    crp20<-'#1AC7C2'
    crp21<-'#1BC1C9'
    crp22<-'#1DBACE'
    crp23<-'#20B2D4'
    crp24<-'#23ABD8'
    crp25<-'#27A3DC'
    crp26<-'#2C9CDF'
    crp27<-'#3194E0'
    crp28<-'#368CE1'
    crp29<-'#3C84E1'
    crp30<-'#417DE0'
    crp31<-'#4776DE'
    crp32<-'#4C6EDB'
    crp33<-'#5268D8'
    crp34<-'#5761D3'
    crp35<-'#5C5ACE'
    crp36<-'#6054C8'
    crp37<-'#644FC1'
    crp38<-'#6849BA'
    crp39<-'#6B44B2'
    crp40<-'#6E40AA'
    
  
      # val <- unique(datapoly["value"])
      # val <- val[order(val$value),]
      # colorGrad <- colorRampPalette(c(crp1,crp2,crp3,crp4,crp5,crp6,crp7))(length(val))
      #colorGrad <- viridis::viridis_pal(option = "A")(length(val))
      data <- datapoly
          if(child.level>1){
            for(i in 1:(child.level-1)){
            index_tess<-which(data$depth==i & data$qe>quant.err_hmap & data$n_cluster>nclust_hmap )
            data<-data[-index_tess,]
    
            rm(index_tess)
            }
          }
      p <-
        p +  geom_polygon(
          data = data,
          aes(
            x = x,
            y = y,
            group = interaction(depth, cluster, child),
            fill = value
          )
          
        ) + 
        scale_fill_gradientn(colours = c(crp40,crp39,crp38,crp37,crp36,crp35,crp34,crp33,crp32,crp31,
                                         crp30,crp29,crp28,crp27,crp26,crp25,crp24,crp23,crp22,crp21,crp20,
                                         crp19,crp18,crp17,crp16,crp15,crp14,crp13,crp12,crp11,crp10,
                                         crp9,crp8,crp7,crp6,crp5,crp4,crp3,crp2,crp1
                                         )) +
        labs(fill = hmap.cols)
      
      for (i in child.level:1 ) {
      p <-
        p +  geom_polygon(
          data = datapoly[which(datapoly$depth==i),],
          aes(
            x = x,
            y = y,
            color = factor(depth),
            size = factor(depth),
            group = interaction(depth, cluster, child)
            
          ),
          #color = color.vec[depth],
          fill = NA
          # ,
          # size = line.width[depth]
        ) +
        scale_colour_manual(values = color.vec)+
        scale_size_manual(values=line.width,guide=FALSE) +
        labs(color = "Level")}
      
    
      
      
      
    for (depth in 1:maxDepth) {
      p <-  p + geom_point(
        data = centroidDataframe[centroidDataframe["lev"] == depth, ],
        aes(x = x, y = y),
        size = (centroid.size / (2 ^ (depth - 1))),
        pch = 21,
        # fill= "black",
        # color="black"
        fill = color.vec[depth],
        color = color.vec[depth]
      )
      
    }
     
    return(suppressMessages(p))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #   requireNamespace("MASS")
    #   requireNamespace("deldir")
    #   requireNamespace("dplyr")
    #   #require(gtools)
    #   #require(seas)
    # #  require(futile.logger)
    #   options(warn = -1)
    #
    #
    #
    #   #select child level data
    #   if(child.level > 1){
    #     parlevel = child.level - 1
    #   }else{
    #     parlevel = 1
    #   }
    #
    #   tess_results <- hvt.results[[1]]
    #   polinfo <- hvt.results[[2]]
    #   hvq_k <- hvt.results[[3]]
    #   hvqdata <- hvq_k$summary
    #
    #   if(length(color.vec) == length(line.width) && (length(line.width)) == child.level){
    #
    #     # flog.info("Lengths of color vector and line width vector is equal to the number of parent levels")
    #     #select the data only for the user-defined level
    #     # hvqztab <- hvqdata[which(hvqdata[, 1] == child.level), ]
    #     # ncolumns <- ncol(hvqdata)
    #     #
    #     #
    #     #
    #     # # select only the input columns
    #     # if (class(hmap.cols) == "character") {
    #     #
    #     #   if(length(list(...))){
    #     #
    #     #     gradient_data <- hvqztab[,hmap.cols,drop=F]
    #     #     get_indices_for_NA <- is.na(gradient_data)
    #     #     if (any(get_indices_for_NA)) {
    #     #       gradient_data[get_indices_for_NA, 1] <- 0
    #     #     }
    #     #
    #     #   }
    #     #
    #     #
    #     #   else{
    #     #
    #     #   if (hmap.cols == "quant_error") {
    #     #     gradient_data <- hvqztab[, 5, drop = F]
    #     #     gradient_data <- gradient_data[complete.cases(hvqztab),,drop=F]
    #     #     # get_indices_for_NA <- is.na(gradient_data)
    #     #     # if (any(get_indices_for_NA)) {
    #     #     #   gradient_data[get_indices_for_NA, 1] <- 0
    #     #     # }
    #     #   }
    #     #
    #     #   else if (hmap.cols == "no_of_points") {
    #     #     gradient_data <- hvqztab[, 4, drop = F]
    #     #     gradient_data <- gradient_data[complete.cases(hvqztab),,drop=F]
    #     #     # get_indices_for_NA <- is.na(gradient_data)
    #     #     # if (any(get_indices_for_NA)) {
    #     #     #   gradient_data[get_indices_for_NA, 1] <- 0
    #     #     # }
    #     #   }
    #     #
    #     #
    #     #
    #     #   else{
    #     #     hmap.cols = which(colnames(dataset) == hmap.cols)
    #     #     if (length(hmap.cols) == 0) {
    #     #       stop("Column name for plotting heatmap incorrect")
    #     #       }
    #     #
    #     #   }
    #     #   }
    #     # }
    #     #
    #     # if (is.numeric(hmap.cols)) {
    #     #   column_no_for_hmap = hmap.cols
    #     #
    #     #   if (length(column_no_for_hmap) == 0) {
    #     #     stop("Column name for plotting heatmap incorrect")
    #     #   }
    #     #
    #     #   ## Get row index for all clusters in the asked child level
    #     #   row_index_clusters = hvq_k$idnodes[[child.level]]
    #     #   # Remove NULL
    #     #   row_index_clusters <- Filter(Negate(is.null),row_index_clusters)
    #     #
    #     #   depth <- 2
    #     #
    #     #   if(child.level==1){
    #     #     depth <- 1
    #     #   }
    #     #
    #     #   gradient_data <-
    #     #     data.frame(unlist(
    #     #       purrr::modify_depth(row_index_clusters, depth,  ~ colMeans(dataset[as.vector(.x[, 1]), column_no_for_hmap,drop=F]))
    #     #     ))
    #     #   colnames(gradient_data) <-
    #     #     colnames(dataset[, column_no_for_hmap, drop = F])
    #     # }
    #     #
    #     #
    #     # #store the column names
    #     # grad_scale <- gradient_data
    #     gtitles <- 1
    #     #different color palette
    #     pal.col <- c("rainbow(500, start = .7, end = .1)", "heat.colors(500)",
    #                  "terrain.colors(500)", "topo.colors(500)", "cm.colors(500)",
    #                  "colorRampPalette(c(crp1,crp2,crp3,crp4,crp5))(500)","RColorBrewer::brewer.pal(n,name)")
    #
    #
    #     # #select the five colors for two color gradient heat map
    #     # crp1 <- "#0000FF"
    #     # crp2 <- "#00FFFF"
    #     # crp3 <- "#00FF00"
    #     # crp4 <- "#FFFF00"
    #     # crp5 <- "#FF0000"
    #
    #
    #     #for each variable in the hvqdata
    #     for(i in 1: length(gtitles)){
    #       #graphics::close.screen(all = T)
    #       #tiles information of user-defined level. It is the output of tile.list.
    #       #pdat <- polinfo[[child.level]]
    #       #gradient of colors is divided into n colors
    #       n <- 500
    #
    #       plot_gg <- ggplot2::ggplot() + ggplot2::theme_bw() +  ggplot2::theme(
    #                                  plot.background = ggplot2::element_blank()
    #                                 ,panel.grid.major = ggplot2::element_blank()
    #                                 ,panel.grid.minor = ggplot2::element_blank())
    #       #gradient_values = data.frame(grad_scale[,i,drop=FALSE])
    #       gradient_palette = pal.col[palette.color]
    #
    #       #call the function which plots the heat map for the user-defined level
    #       if(previous_level_heatmap==T){
    #         start.level = 1
    #       }
    #       else{
    #         start.level = child.level
    #       }
    #
    #       for(current.child.level in start.level:child.level){
    #
    #
    #         hvqztab <- hvqdata[which(hvqdata[, 1] == current.child.level), ]
    #         ncolumns <- ncol(hvqdata)
    #
    #
    #
    #         # select only the input columns
    #         if (class(hmap.cols) == "character") {
    #
    #           if(hmap.cols %in% colnames(hvqztab)){
    #
    #             gradient_data <- hvqztab[,hmap.cols,drop=F]
    #             get_indices_for_NA <- is.na(gradient_data)
    #             if (any(get_indices_for_NA)) {
    #               gradient_data[get_indices_for_NA, 1] <- 0
    #             }
    #
    #           }
    #
    #
    #           else{
    #
    #             if (hmap.cols == "quant_error") {
    #               # gradient_data <- hvqztab[, 5, drop = F]
    #               # gradient_data <- gradient_data[complete.cases(gradient_data),,drop=F]
    #               gradient_data <- hvqztab[,5,drop=F]
    #               get_indices_for_NA <- is.na(gradient_data)
    #               if (any(get_indices_for_NA)) {
    #                 gradient_data[get_indices_for_NA, 1] <- 0
    #               }
    #               # get_indices_for_NA <- is.na(gradient_data)
    #               # if (any(get_indices_for_NA)) {
    #               #   gradient_data[get_indices_for_NA, 1] <- 0
    #               # }
    #             }
    #
    #             else if (hmap.cols == "no_of_points") {
    #               # gradient_data <- hvqztab[, 4, drop = F]
    #               # gradient_data <- gradient_data[complete.cases(gradient_data),,drop=F]
    #               gradient_data <- hvqztab[,4,drop=F]
    #               get_indices_for_NA <- is.na(gradient_data)
    #               if (any(get_indices_for_NA)) {
    #                 gradient_data[get_indices_for_NA, 1] <- 0
    #               }
    #               # get_indices_for_NA <- is.na(gradient_data)
    #               # if (any(get_indices_for_NA)) {
    #               #   gradient_data[get_indices_for_NA, 1] <- 0
    #               # }
    #             }
    #
    #
    #
    #             else{
    #               hmap.cols = which(colnames(dataset) == hmap.cols)
    #               if (length(hmap.cols) == 0) {
    #                 stop("Column name for plotting heatmap incorrect")
    #               }
    #
    #             }
    #           }
    #         }
    #
    #         if (is.numeric(hmap.cols)) {
    #           column_no_for_hmap = hmap.cols
    #
    #           if (length(column_no_for_hmap) == 0) {
    #             stop("Column name for plotting heatmap incorrect")
    #           }
    #
    #           ## Get row index for all clusters in the asked child level
    #           row_index_clusters = hvq_k$idnodes[[current.child.level]]
    #           # Remove NULL
    #           row_index_clusters <- Filter(Negate(is.null),row_index_clusters)
    #
    #           depth <- 2
    #
    #           if(current.child.level==1){
    #             depth <- 1
    #           }
    #
    #           gradient_data <-
    #             data.frame(unlist(
    #               purrr::modify_depth(row_index_clusters, depth,  ~ colMeans(dataset[as.vector(.x[, 1]), column_no_for_hmap,drop=F]))
    #             ))
    #           colnames(gradient_data) <-
    #             colnames(dataset[, column_no_for_hmap, drop = F])
    #         }
    #
    #
    #         #store the column names
    #         grad_scale <- gradient_data
    #
    #       pdat <- polinfo[[current.child.level]]
    #
    #       plot_gg <- ggplotTileHmap(plot_gg,pdat,grad_scale,ptext = tess.label, polycol = gradient_palette,
    #                     close = T, showpoints = show.points,
    #                     lnwid = (line.width[1] / child.level),
    #                     frame.plot = F, xlab = "", ylab = "",
    #                     asp = asp, label.size = label.size,
    #                     pointmag = (centroid.size / child.level),pch=pch)
    #       }
    #
    #       #plot the centroids for parent levels
    #       plot_gg <- ggplotTessHmap(plot_gg,hvt.results, line.width = line.width, color.vec = color.vec,pch=pch,child.level=child.level,show.points = show.points,centroid.size = centroid.size)
    #
    #       #plot the polygons of the parent levels
    #       # for(lev in parlevel: 1){
    #       #   len <- length(polinfo[[lev]])
    #       #   for(ind1 in 1: len){
    #       #     for(ind2 in 1: length(polinfo[[lev]][[ind1]])){
    #       #      # graphics::polygon(polinfo[[lev]][[ind1]][[ind2]]$x, polinfo[[lev]][[ind1]][[ind2]]$y,
    #       #               #lwd = line.width[lev], border = color.vec[lev])
    #       #       df_pol <- data.frame(x=polinfo[[lev]][[ind1]][[ind2]]$x,y=polinfo[[lev]][[ind1]][[ind2]]$y)
    #       #       plot_gg <- plot_gg + ggplot2::geom_polygon(data = df_pol,mapping = ggplot2::aes_string(x="x",y="y"),size=line.width[lev],colour = color.vec[lev],fill=NA)
    #       #     }
    #       #   }
    #       #   # flog.debug("Polygons for Level %s are drawn", lev)
    #       # }
    #
    #       return(suppressMessages(plot_gg))
    #     }
    #   }else{
    #     return("Length of color vector and line width vector should be equal to child level")
    #   }
  }
