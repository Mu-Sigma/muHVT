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
#' @author Meet K. Dave <dave.kirankumar@@mu-sigma.com>
#' @seealso \code{\link{HVT}} \cr \code{\link{hvtHmap}}
#' @keywords hplot
#' @examples
#' 
#' 
#' data("iris",package="datasets")
#' iris <- iris[,1:2]
#' hvt.results <- list()
#' hvt.results <- HVT(iris, nclust = 6, depth = 1, quant.err = 0.2, 
#' projection.scale = 10, normalize = TRUE)
#' 
#' plotHVT(hvt.results, line.width = c(3), color.vec = c("blue"))
#' 
#' hvt.results <- list()
#' hvt.results <- HVT(iris, nclust = 6, depth = 3, quant.err = 0.2, 
#' projection.scale = 10, normalize = TRUE)
#' 
#' 
#' plotHVT(hvt.results, line.width = c(4,3), color.vec = c("red", "green"))
#' 
#' 
#' @export plotHVT
plotHVT <-
function(hvt.results, line.width, color.vec, pch1 = 19, centroid.size = 3,title=NULL){
  requireNamespace("ggplot2")
  
  # select the plot area

  del_results <- hvt.results[[1]]
  parlevel <- length(del_results)
  
  if(length(line.width) == parlevel && length(color.vec) == parlevel){
    
  plot_gg <- ggplot2::ggplot()

  for(lev in length(del_results):1 ){
    for(lev1 in 1: length(del_results[[lev]])){


          df = data.frame(del_results[[lev]][[lev1]]$summary$x,del_results[[lev]][[lev1]]$summary$y)
          colnames(df) <- c("x","y")
          
          seg_df <- cbind(del_results[[lev]][[lev1]]$dirsgs,Legend= paste("Level",lev))
          
          plot_gg <- plot_gg + ggplot2::geom_segment(ggplot2::aes_string(x="x1",y="y1",xend="x2",yend="y2",color="Legend"),
                                                     size = line.width[lev],
                                                     data = seg_df,
                                                     linetype = 1
                                                     )  + ggplot2::scale_color_manual(values = color.vec) +
            ggplot2::geom_point(data = df,
                                ggplot2::aes_string(x="x",y="y"),
                                pch=21,
                                size = (centroid.size/lev),
                                fill = color.vec[lev]) + ggplot2::theme_bw() +  ggplot2::theme(
                                  plot.background = ggplot2::element_blank()
                                  ,panel.grid.major = ggplot2::element_blank()
                                  ,panel.grid.minor = ggplot2::element_blank()
                                ) 

    }
  }
  return(plot_gg)
  } else {
    return("Length of color vector and line width vector should be 1 less than child level")
  }
  
}

