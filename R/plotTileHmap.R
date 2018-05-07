plotTileHmap <-
function (tilesinfo, ptext = NULL, verbose = FALSE, lnwid = 1, close = FALSE, pch2 = 1, polycol = NA, 
                           showpoints = TRUE, asp = 1, label.size = 0.5, pointmag = pointmag, ...) 
{
  object <- tilesinfo
  ntiles <- length(object)
  x.all <- y.all <- x.pts <- y.pts <- NULL
  for(j in 1: ntiles){
    #vector containing vertices as well as centroids for x co-ordinates
    x.all <- c(x.all, unlist(lapply(object[[j]], function(w) {
      c(w $ pt[1], w $ x)
    })))
    #vector containing vertices as well as centroids for y co-ordinates
    y.all <- c(y.all, unlist(lapply(object[[j]], function(w) {
      c(w $ pt[2], w $ y)
    })))
    #vector containing x co-ordinates of centroids
    x.pts <- c(x.pts, unlist(lapply(object[[j]], function(w) {
      w $ pt[1]
    })))
    #vector containing y co-ordinates of centroids
    y.pts <- c(y.pts, unlist(lapply(object[[j]], function(w) {
      w $ pt[2]
    })))
  }
  #range of all the points of this level
  rx <- range(x.all)
  ry <- range(y.all)
  
  polycol <- apply(grDevices:: col2rgb(polycol, TRUE), 2, function(x) {
    do.call(grDevices::rgb, as.list(x / 255))
  })
  
  hexbla <- do.call(grDevices::rgb, as.list(grDevices:: col2rgb("black", TRUE) / 255))
  hexwhi <- do.call(grDevices::rgb, as.list(grDevices:: col2rgb("white", TRUE) / 255))
  ptcol <- ifelse(polycol == hexbla, hexwhi, hexbla)
  for(j in 1: ntiles){
    inner <- !any(object[[j]]$bp)
    small_tiles <- length(object[[j]])
    ind <- (j - 1) * small_tiles
    for(k in 1: small_tiles){
      if (close | inner){
        #draw the polygon with appropriate colors representative of its value for that variable
        graphics::polygon(object[[j]][[k]], col = polycol[ind + k], 
                border = ptcol[ind + k], lwd = lnwid, lty = 2)
      }
      else{
        #if the polygon is not closed, then draw the line segments
        x <- object[[j]][[k]]$x
        y <- object[[j]][[k]]$y
        bp <- object[[j]][[k]]$bp
        ni <- length(x)
        for (l in 1: ni) {
          lnext <- if (l < ni) 
            l + 1
          else 1
          #verify that the point is not outside the plot range
          do.it <- deldir::mid.in(x[c(l, lnext)], y[c(l, lnext)], rx, ry)
          if (do.it)
            #draw the line graphics::segments
            graphics::segments(x[l], y[l], x[lnext], y[lnext], col = ptcol[ind + k], 
                     lwd = lnwid)
        }
      }
      
      if(verbose & showpoints)
        graphics::points(object[[j]][[k]]$pt[1], object[[j]][[k]]$pt[2], pch = pch2, col = ptcol[ind + k])
      
      if(verbose & (j < small_tiles))
        readline("Go? ")
    }
    
    #making the points appear in proportion to the size of the cluster
    
    if(showpoints){
      for(m in 1: small_tiles)
        graphics::points(x.pts[ind + m], y.pts[ind + m], pch = pch2, cex = pointmag, col = ptcol[ind + m])
      if(!is.null(ptext)){
        graphics::text(x.pts, y.pts, ptext[1: ntiles], cex = label.size, adj = 1)
      }
    }
  }
  invisible()
}
