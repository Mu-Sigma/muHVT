Delete_Outpoints <- function(tile, check_dirsgs){
  check_dirsgs[, c("x1","x2","y1", "y2")] <- round(check_dirsgs[, c("x1","x2","y1", "y2")], 6)
  tile$x <- round(tile$x, 6)
  tile$y <- round(tile$y, 6)
  
  # Use lapply to iterate over each tile vertex
  tile_vert_info <- lapply(1:length(tile$x), function(tile_index){
    if(length(intersect(which(check_dirsgs[, c("y1", "y2")] == tile$y[tile_index]),
                        which(check_dirsgs[, c("x1", "x2")] == tile$x[tile_index])))){
      TRUE
    } else {
      FALSE
    }
  })
  
  # Convert list to logical vector
  tile_vert_info <- unlist(tile_vert_info)
  
  # Delete the vertices which are outside the parent tile
  del_ind <- which(tile_vert_info == FALSE)
  if(length(del_ind) > 0){
    tile$x <- tile$x[-del_ind]
    tile$y <- tile$y[-del_ind]
    tile$bp <- tile$bp[-del_ind]
  }
  
  return(tile)
}
