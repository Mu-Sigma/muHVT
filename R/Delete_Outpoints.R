Delete_Outpoints <-
function(tile, check_dirsgs){
  check_dirsgs[, c("x1","x2","y1", "y2")]<-round(check_dirsgs[, c("x1","x2","y1", "y2")], 6)
  tile$x <- round(tile$x,6)
  tile$y <- round(tile$y,6)
  tile_vert_info <- list()
  for(tile_index in 1: length(tile$x)){
    #get the index of the vertices which are outside the parent tile; if a vertex is outside
    #the parent tile then the coordinate of that vertex will not be present in dirsgs
    if(length(intersect(which(check_dirsgs[, c("y1", "y2")] == tile$y[tile_index]),
                        which(check_dirsgs[, c("x1", "x2")] == tile$x[tile_index])))){
      tile_vert_info[[tile_index]] <- TRUE
    }else{
      tile_vert_info[[tile_index]] <- FALSE
    }

    # tile_vert_info[[tile_index]] <- is.na(which(round(check_dirsgs[, c("y1", "y2")], 6) == round(tile$y[tile_index], 6)) &&
    #                                        which(round(check_dirsgs[, c("x1", "x2")], 6) == round(tile$x[tile_index], 6)))
  }
  #delete the vertices which are outside the parent tile
  del_ind <- which(tile_vert_info == FALSE)
  if(length(del_ind)){
    tile$x <- tile$x[-del_ind]
    tile$y <- tile$y[-del_ind]
    tile$bp <- tile$bp[-del_ind]
  }
  return(tile)
}