Transform_Coordinates <- function(sam_rectangle, par_rectangle, scale_mat){  
  
  xrange_sam <- range(sam_rectangle[, 1])
  yrange_sam <- range(sam_rectangle[, 2])
  
  xrange_par <- range(par_rectangle[, 1])
  yrange_par <- range(par_rectangle[, 2])
  
  xlength_sam <- xrange_sam[2] - xrange_sam[1]
  ylength_sam <- yrange_sam[2] - yrange_sam[1]
  
  xlength_par <- xrange_par[2] - xrange_par[1]
  ylength_par <- yrange_par[2] - yrange_par[1]
  
  # Use lapply to transform coordinates, then combine the results into a matrix
  transformed_coord_list <- lapply(1:4, function(i) {
    t(scale_mat %*% matrix(c(sam_rectangle[i, 1], sam_rectangle[i, 2]), ncol = 1))
  })
  
  transformed_coord_int <- do.call(rbind, transformed_coord_list)
  
  translate_mat <- matrix(c(((sum(xrange_par) / 2) - (sum(range(transformed_coord_int[, 1])) / 2)),
                            ((sum(yrange_par) / 2) - (sum(range(transformed_coord_int[, 2])) / 2))), 
                          ncol = 1)
  
  transformed_data <- c(scale_mat[1, 1], scale_mat[2, 2], translate_mat[1], translate_mat[2])
  
  return(transformed_data)
}
