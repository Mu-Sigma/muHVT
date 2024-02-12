Add_boundary_points <-
  function (tile.child, tile.parent, data.points) {
    desired_index <- c()
    dist_to_tile_endpoints <- list()
    # For all the vertices in the parent tile
    lapply(1:length(tile.parent$x), function(m) {
      # For all the centroids of the child tile, find out the distance of each vertex from all the centroids of the child tile
      dist_to_tile_endpoints <- sapply(1:length(data.points[, 1]), function(n) {
        sum(((tile.parent$x[m] - data.points[n, 1]) ^ 2) + ((tile.parent$y[m] - data.points[n, 2]) ^ 2))
      })
      # Index of the child tile to which the vertex of the parent tile belongs is the one to which the vertex of the parent tile is closest
      desired_index <- which(dist_to_tile_endpoints == min(as.numeric(dist_to_tile_endpoints)))
      # If more than one centroid of the child is close to a given vertex then it should be added to all those child tiles.
      lapply(1:length(desired_index), function(i) {
        tile.x <- c(tile.child[[desired_index[i]]]$x, tile.parent$x[m])
        tile.y <- c(tile.child[[desired_index[i]]]$y, tile.parent$y[m])
        tile.bp <- c(tile.child[[desired_index[i]]]$bp, tile.parent$bp[m])
        # Use only those points which form a convex hull
        var1 <- grDevices::chull(tile.x, tile.y)
        tile.child[[desired_index[i]]]$x <- tile.x[var1]
        tile.child[[desired_index[i]]]$y <- tile.y[var1]
        tile.child[[desired_index[i]]]$bp <- tile.bp[var1]
      })
    })
    return (tile.child)
  }
