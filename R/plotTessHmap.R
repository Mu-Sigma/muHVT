plotTessHmap <-
function(hvt.results, line.width, color.vec, pch1 = 19, centroid.size = 3){
  
  del_results <- hvt.results[[1]]
  #plot(del_results[[1]][[1]], wlines = "tess", lty = 1, lwd = line.width[1], xlab = "", ylab = "") 
  
  for(lev in 1: length(del_results)){
    for(lev1 in 1: length(del_results[[lev]])){
      deldir::plot.deldir(del_results[[lev]][[lev1]], wlines = "tess", add = T, lty = 1, col = color.vec[lev], 
                  lwd = line.width[lev], pch = pch1, cex = (centroid.size / lev))
    }
  }
}
