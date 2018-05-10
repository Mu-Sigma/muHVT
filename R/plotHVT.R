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
  
  # Split the screen for plot area and legend area
  mymat <- rbind(c(0, 0.8, 0, 1), c(0.8, 1, 0.2, 0.8))
  graphics::split.screen(mymat)
  
  # select the plot area
  graphics::screen(1)
  
  del_results <- hvt.results[[1]]
  parlevel <- length(del_results)
  
  if(length(line.width) == parlevel && length(color.vec) == parlevel){
    
  deldir::plot.deldir(del_results[[1]][[1]], wlines = "tess", lty = 1, lwd = line.width[1], xlab = " ", ylab = " ",main=title)
  
  for(lev in 1: length(del_results)){
    for(lev1 in 1: length(del_results[[lev]])){
      graphics::par(new=TRUE)
      deldir::plot.deldir(del_results[[lev]][[lev1]], wlines = "tess", add = T, lty = 1, col = color.vec[lev], 
                  lwd = line.width[lev], pch = pch1, cex = (centroid.size / lev))
    }
  }
  
  # select the legend area
  graphics::screen(2)
  
  if(parlevel > 1){
    for(j in 1: parlevel){
      graphics::text(0.3,(0.8-(0.1*j)), paste("Level ", j))
      graphics::segments(0.6,(0.8-(0.1*j)),0.8,(0.8-(0.1*j)),col=color.vec[j],lty=1,lwd=line.width[j])
    }
  } else {
    graphics::text(0.3,0.7, paste("Level ", parlevel))
    graphics::segments(0.6,0.7,0.8,0.7,col=color.vec[1],lty=1,lwd=line.width[1])
  }
  } else {
    return("Length of color vector and line width vector should be 1 less than child level")
  }
  
}
