
#' @importFrom magrittr %>%

ggplotTileHmap <-
  function (plot_gg,tilesinfo,gradient_values,ptext = NULL, verbose = FALSE, lnwid = 0.2, close = FALSE, pch = 21, polycol = NA, 
            showpoints = TRUE, asp = 1, label.size = 0.5, pointmag = pointmag,...) 
  {
    object <- tilesinfo
    ntiles <- length(object)
    x.all <- y.all <- x.pts <- y.pts <- NULL
    gradient_values <- gradient_values
    gradient_palette <- polycol
    gradient_col_name <- colnames(gradient_values)
    
    
    
    
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
    
    # polycol <- apply(grDevices:: col2rgb(polycol, TRUE), 2, function(x) {
    #   do.call(grDevices::rgb, as.list(x / 255))
    # })
    # 
    hexbla <- do.call(grDevices::rgb, as.list(grDevices:: col2rgb("black", TRUE) / 255))
    # hexwhi <- do.call(grDevices::rgb, as.list(grDevices:: col2rgb("white", TRUE) / 255))
    # ptcol <- ifelse(polycol == hexbla, hexwhi, hexbla)
    ptcol <- hexbla
    
    #select the five colors for two color gradient heat map
    crp1 <- "#0000FF"
    crp2 <- "#00FFFF"
    crp3 <- "#00FF00"
    crp4 <- "#FFFF00"
    crp5 <- "#FF0000"
    
    n <- nrow(gradient_values)
    name <- "GnBu"
    
    
    for(j in 1: ntiles){
      inner <- !any(object[[j]]$bp)
      small_tiles <- length(object[[j]])
      ind <- (j - 1) * small_tiles
      
      if (close | inner){
        #draw the polygon with appropriate colors representative of its value for that variable
        #graphics::polygon(object[[j]][[k]], col = polycol[ind + k], 
        #border = ptcol[ind + k], lwd = lnwid, lty = 2)
        
        
        #df_pol <- data.frame(x=object[[j]][[k]]$x,y=object[[j]][[k]]$y) %>% dplyr::mutate(!!gradient_col_name:= gradient_values[ind+k,])
        
        #df_pol <- purrr::map_df(object[[j]],.f = function(obj) data.frame(x=obj$x,y=obj$y)) %>% dplyr::mutate(!!gradient_col_name:= gradient_values[ind+1:ind+1+small_tiles,])
        
        grad_val <- gradient_values[ind+1:(ind+small_tiles),,drop=F]
        
        df_pol <- do.call(rbind,purrr::map2(object[[j]],c(1:small_tiles),.f = function(obj,row) data.frame(x=obj$x,y=obj$y,group=row,so = grad_val[row,])))
        
        colnames(df_pol) <- c("x","y","group",gradient_col_name)
        
        plot_gg <- plot_gg + ggplot2::geom_polygon(data = df_pol,mapping = ggplot2::aes_string(x="x",y="y",group="group",fill=as.name(gradient_col_name)),size=lnwid) + ggplot2::scale_fill_gradientn(colours = eval(parse(text = gradient_palette)))
        
  
        # plot_gg <- plot_gg + ggplot2::geom_polygon(data = df_pol,mapping = ggplot2::aes_string(x="x",y="y",color=gradient_values),fill=polycol[ind+k],size=lnwid,colour = ptcol[ind + k])
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
      
      # if(verbose & showpoints)
      #   graphics::points(object[[j]][[k]]$pt[1], object[[j]][[k]]$pt[2], pch = pch2, col = ptcol[ind + k])
      
      if(verbose & (j < small_tiles))
        readline("Go? ")
      
      
      #making the points appear in proportion to the size of the cluster
      
      # if(showpoints){
      #   
      #   df_points <- data.frame(x=x.pts,y=y.pts,col=ptcol,pch=pch,cex=pointmag)
      #   
      #   plot_gg <- plot_gg + ggplot2::geom_point(data=df_points, mapping = ggplot2::aes(x=x,y=y),
      #                                                                               size=pointmag,
      #                                                                               fill=ptcol,
      #                                                                               pch=pch,
      #                                                                               color = ptcol)
      #   #for(m in 1: small_tiles)
      #     #graphics::points(x.pts[ind + m], y.pts[ind + m], pch = pch2, cex = pointmag, col = ptcol[ind + m])
      #   if(!is.null(ptext)){
      #     graphics::text(x.pts, y.pts, ptext[1: ntiles], cex = label.size, adj = 1)
      #   }
      # }
    }
    invisible()
    return(plot_gg)
  }
