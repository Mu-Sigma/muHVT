Delete_Outpoints <-
function(tile, check_dirsgs){
  
  tile_vert_info <- list()
  for(tile_index in 1: length(tile$x)){
    #get the index of the vertices which are outside the parent tile; if a vertex is outside
    #the parent tile then the coordinate of that vertex will not be present in dirsgs
    if(length(intersect(which(round(check_dirsgs[, c("y1", "y2")], 6) == round(tile$y[tile_index], 6)),
                        which(round(check_dirsgs[, c("x1", "x2")], 6) == round(tile$x[tile_index], 6))))){
      tile_vert_info[[tile_index]] <- T
    }else{
      tile_vert_info[[tile_index]] <- F
    }
    
    # tile_vert_info[[tile_index]] <- is.na(which(round(check_dirsgs[, c("y1", "y2")], 6) == round(tile$y[tile_index], 6)) &&
    #                                        which(round(check_dirsgs[, c("x1", "x2")], 6) == round(tile$x[tile_index], 6)))
  }
  #delete the vertices which are outside the parent tile
  del_ind <- which(tile_vert_info == F)
  if(length(del_ind)){
    tile$x <- tile$x[-del_ind]
    tile$y <- tile$y[-del_ind]
    tile$bp <- tile$bp[-del_ind]
  }
  return(tile)
}
