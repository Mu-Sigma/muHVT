Add_boundary_points <-
  function (tile.child, tile.parent, data.points) {
    desired_index <- c()
    dist_to_tile_endpoints <- list()
    #for all the vertices in the parent tile
    for (m in 1: length(tile.parent$x)) {
      #for all the centroids of the child tile.
      for (n in 1: length(data.points[, 1])) {
        #find out the distance of each vertex from the all the centroids of the child tile
        dist_to_tile_endpoints[[n]] <- sum(((tile.parent$x[m] - data.points[n, 1]) ^ 2),
                                           ((tile.parent$y[m] - data.points[n, 2]) ^ 2))
      }
      # index of the child tile to which the vertex of the parent tile belongs is the one to which
      # the vertex of the parent tile is closest
      desired_index <- which(dist_to_tile_endpoints == min(as.numeric(dist_to_tile_endpoints)))
      #if more than one centroid of the child is close to a given vertex then it should be
      #added to all those child tiles.
      for(i in 1: length(desired_index)){
        tile.x <- c(tile.child[[desired_index[i]]]$x, tile.parent$x[m])
        tile.y <- c(tile.child[[desired_index[i]]]$y, tile.parent$y[m])
        tile.bp <- c(tile.child[[desired_index[i]]]$bp, tile.parent$bp[m])
        #use only those points which form a convex hull
        var1 <- grDevices::chull(tile.x,tile.y)
        tile.child[[desired_index[i]]]$x <- tile.x[var1]
        tile.child[[desired_index[i]]]$y <- tile.y[var1]
        tile.child[[desired_index[i]]]$bp <- tile.bp[var1]
      }
    }
    return (tile.child)
  }

# Add_boundary_points <- function(tile.child, tile.parent, data.points) {
#   desired_index <- c()
#   dist_to_tile_endpoints <- list()
#   
#   lapply(tile.parent$x, function(px) {
#     dist_to_tile_endpoints <<- lapply(data.points[,1], function(dx) sum((px - dx)^2))
#     
#     desired_index <<- which(dist_to_tile_endpoints == min(as.numeric(dist_to_tile_endpoints)))
#     
#     lapply(desired_index, function(di) {
#       tile.x <- c(tile.child[[di]]$x, px)
#       tile.y <- c(tile.child[[di]]$y, tile.parent$y[which(tile.parent$x == px)])
#       tile.bp <- c(tile.child[[di]]$bp, tile.parent$bp[which(tile.parent$x == px)])
#       
#       var1 <- grDevices::chull(tile.x, tile.y)
#       tile.child[[di]]$x <- tile.x[var1]
#       tile.child[[di]]$y <- tile.y[var1]
#       tile.child[[di]]$bp <- tile.bp[var1]
#     })
#   })
#   
#   return(tile.child)
# }
