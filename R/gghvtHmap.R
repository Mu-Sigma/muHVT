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
#' @param hmap.cols Vector. A vector with column names (or) column indices of
#' the dataset indicating the variables for which the heat map is to be
#' plotted.
#' @param color.vec Vector. A color vector such that length(color.vec) =
#' (child.level - 1). (default = NULL)
#' @param line.width Vector. A line width vector such that length(line.width) =
#' (child.level - 1). (default = NULL)
#' @param centroid.size Numeric. Indicating the centroid size of the first
#' level. (default = 3)
#' @param pch1 Numeric. Indicating the parent level centroid's symbol type.
#' (default = 19)
#' @param pch2 Numeric. Indicating the child level centroid's symbol type.
#' (default = 1)
#' @param palette.color Numeric. Indicating the heat map color palette. 1 -
#' rainbow, 2 - heat.colors, 3 - terrain.colors, 4 - topo.colors, 5 -
#' cm.colors, 6 - seas color. (default = 1)
#' @param show.points Logical. Indicating if the size of the centroids should
#' be relative to the number of data points in that cluster. (default = FALSE)
#' @param asp Numeric. Indicating the aspect ratio type. For flexible aspect
#' ratio set, asp = NA. (default = 1)
#' @param ask Logical. If TRUE (and the R session is interactive) the user is
#' asked for input, before a new figure is drawn. (default = TRUE)
#' @param tess.label Vector. A vector for labelling the tessellations. (default
#' = NULL)
#' @param label.size Numeric. The size by which the tessellation labels should
#' be scaled. (default = 0.5)
#' @author Meet K. Dave <dave.kirankumar@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords hplot
#' @examples
#' 
#' 
#' 
#' 
#' data("iris",package="datasets")
#' iris <- iris[,1:2]
#' hvt.results <- HVT(iris, nclust = 6, depth = 1, quant.err = 0.2, 
#' projection.scale = 10, normalize = TRUE)
#' 
#' hvtHmap(hvt.results, iris, child.level = 1,hmap.cols =c(1:2), show.points=TRUE)
#' 
#' 
#' 
#' 
#' @export gghvtHmap
gghvtHmap <-
function (hvt.results, dataset, child.level, hmap.cols, color.vec = NULL, line.width = NULL, centroid.size = 3, pch1 = 19, pch2 = 1, palette.color = 1, show.points = F, asp = 1, ask = T, tess.label = NULL, label.size = .5)
  {
    requireNamespace("MASS")
    requireNamespace("deldir")
    requireNamespace("dplyr")
    #require(gtools)
    #require(seas)
  #  require(futile.logger)
    
    #select child level data
    if(child.level > 1){
      parlevel = child.level - 1
    }else{
      parlevel = 1
    }
    
    tess_results <- hvt.results[[1]]
    polinfo <- hvt.results[[2]]
    hvq_k <- hvt.results[[3]]
    hvqdata <- hvq_k$summary
    
    if(length(color.vec) == length(line.width) && (length(line.width)) == child.level){
      
      # flog.info("Lengths of color vector and line width vector is equal to the number of parent levels")
      #select the data only for the user-defined level
      hvqztab <- hvqdata[which(hvqdata[, 1] == child.level), ]
      ncolumns <- ncol(hvqdata)
      

      
      # select only the input columns
      if (class(hmap.cols) == "character") {
        if (hmap.cols == "quant_error") {
          gradient_data <- hvqztab[, 5, drop = F]
          get_indices_for_NA <- is.na(gradient_data)
          if (any(get_indices_for_NA)) {
            gradient_data[get_indices_for_NA, 1] <- 0
          }
        }
        
        else if (hmap.cols == "no_of_points") {
          gradient_data <- hvqztab[, 4, drop = F]
          get_indices_for_NA <- is.na(gradient_data)
          if (any(get_indices_for_NA)) {
            gradient_data[get_indices_for_NA, 1] <- 0
          }
        }
        
        else{
          hmap.cols = which(colnames(dataset) == hmap.cols)
          if (length(hmap.cols) == 0) {
            stop("Column name for plotting heatmap incorrect")
            }

        }
      }
      
      if (is.numeric(hmap.cols)) {
        column_no_for_hmap = hmap.cols
        
        if (length(column_no_for_hmap) == 0) {
          stop("Column name for plotting heatmap incorrect")
        }
        
        ## Get row index for all clusters in the asked child level
        row_index_clusters = hvq_k$idnodes[[child.level]]
        
        depth <- 2
        
        if(child.level==1){
          depth <- 1
        }
        
        gradient_data <-
          data.frame(unlist(
            purrr::modify_depth(row_index_clusters, depth,  ~ mean(dataset[as.vector(.x[, 1]), column_no_for_hmap]))
          ))
        colnames(gradient_data) <-
          colnames(dataset[, column_no_for_hmap, drop = F])
      }
      
      
      #store the column names
      grad_scale <- gradient_data
      gtitles <- 1
      #different color palette
      pal.col <- c("rainbow(500, start = .7, end = .1)", "heat.colors(500)", 
                   "terrain.colors(500)", "topo.colors(500)", "cm.colors(500)", 
                   "colorRampPalette(c(crp1, crp2))(500)")
      #select the two colors for two color gradient heat map
      crp1 <- "#DADDD8"
      crp2 <- "#8FB339"
      
      #for each variable in the hvqdata
      for(i in 1: length(gtitles)){
        #graphics::close.screen(all = T)
        #tiles information of user-defined level. It is the output of tile.list.
        pdat <- polinfo[[child.level]]
        #gradient of colors is divided into n colors
        n <- 500

        plot_gg <- ggplot2::ggplot() + ggplot2::theme_bw() +  ggplot2::theme(
                                   plot.background = ggplot2::element_blank()
                                  ,panel.grid.major = ggplot2::element_blank()
                                  ,panel.grid.minor = ggplot2::element_blank())
        #gradient_values = data.frame(grad_scale[,i,drop=FALSE])
        gradient_palette = pal.col[palette.color]
        
        #call the function which plots the heat map for the user-defined level
        plot_gg <- ggplotTileHmap(plot_gg,pdat,grad_scale,ptext = tess.label, polycol = gradient_palette, 
                      close = T, showpoints = show.points,  
                      lnwid = (line.width[1] / child.level),
                      frame.plot = F, xlab = "", ylab = "", 
                      asp = asp, label.size = label.size, 
                      pointmag = (centroid.size / child.level),pch2=pch2)
        
        #plot the centroids for parent levels
        plot_gg <- ggplotTessHmap(plot_gg,hvt.results, line.width = line.width, color.vec = color.vec,pch1=pch1,child.level=child.level)
        
        #plot the polygons of the parent levels
        for(lev in parlevel: 1){   
          len <- length(polinfo[[lev]])
          for(ind1 in 1: len){
            for(ind2 in 1: length(polinfo[[lev]][[ind1]])){
             # graphics::polygon(polinfo[[lev]][[ind1]][[ind2]]$x, polinfo[[lev]][[ind1]][[ind2]]$y, 
                      #lwd = line.width[lev], border = color.vec[lev])
              df_pol <- data.frame(x=polinfo[[lev]][[ind1]][[ind2]]$x,y=polinfo[[lev]][[ind1]][[ind2]]$y)
              plot_gg <- plot_gg + ggplot2::geom_polygon(data = df_pol,mapping = ggplot2::aes_string(x="x",y="y"),size=line.width[lev],colour = color.vec[lev],fill=NA)
            }
          }
          # flog.debug("Polygons for Level %s are drawn", lev)
        }
  
        return(plot_gg)
      }
    }else{
      return("Length of color vector and line width vector should be equal to child level")
    }
  }
