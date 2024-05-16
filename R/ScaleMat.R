ScaleMat <-
function(samRectangle, parRectangle){
  
  xrange_sam <- range(samRectangle[, 1])
  yrange_sam <- range(samRectangle[, 2])
  
  xrange_par <- range(parRectangle[, 1])
  yrange_par <- range(parRectangle[, 2])
  
  xlength_sam <- xrange_sam[2] - xrange_sam[1]
  ylength_sam <- yrange_sam[2] - yrange_sam[1]
  
  xlength_par <- xrange_par[2] - xrange_par[1]
  ylength_par <- yrange_par[2] - yrange_par[1]
  
  x_scalefactor <- xlength_par / xlength_sam
  y_scalefactor <- ylength_par / ylength_sam
  
  scale_mat <- matrix(c(x_scalefactor, 0, 0, y_scalefactor), ncol = 2)
  return(scale_mat)
}
