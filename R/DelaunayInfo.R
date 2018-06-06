DelaunayInfo <-
function(gidata, poly_info, rawdeldati, nclust){
  
  requireNamespace("deldir")       #deldir function 
  requireNamespace("grDevices")    #chull function
  requireNamespace("splancs")      #csr function 
  requireNamespace("sp")           #point.in.polygon function
  requireNamespace("conf.design")  #factorize function
  
  new_pt <-  matrix(0, ncol = 2, nrow = 1)
  #function to perform the transformation to get the new points which will be inside the parent polygon
  New_Points <- function(scale_translate_info){
    mat_scale <- matrix(c(scale_translate_info[1], 0, 0, scale_translate_info[2]), ncol = 2)
    mat_tran <- matrix(c(scale_translate_info[3], scale_translate_info[4]), ncol = 1)
    new_pt[1, ] <- t((mat_scale %*% matrix(c(rawdeldati[[ind1]][j, 1],
                                             rawdeldati[[ind1]][j, 2]), ncol = 1)) + mat_tran)
    return(new_pt)
  }
  
  #function to chop parent tile into smaller boxes
  Parent_Tile_Boxes <- function(output_polygon){
    
    xrange_parent_poly <- range(output_polygon[, 1])
    yrange_parent_poly <- range(output_polygon[, 2])
    #divide the range into segments
    xseq_parent <- seq(xrange_parent_poly[1], xrange_parent_poly[2], length.out = 15)
    yseq_parent <- seq(yrange_parent_poly[1], yrange_parent_poly[2], length.out = 15)
    
    Points_on_ylines <- Points_on_xlines <- y_points_inside_polygon <- x_points_inside_polygon <- list()
    
    for(i in 1: length(xseq_parent)){
      #points on line with constant 'y'
      Points_on_ylines[[i]] <- matrix(c(xseq_parent, 
                                        rep(yseq_parent[i], length(xseq_parent))),
                                      ncol = 2, byrow = F)
      #points on line with constant 'x'
      Points_on_xlines[[i]] <- matrix(c(rep(xseq_parent[i], length(yseq_parent)), 
                                        yseq_parent), ncol = 2, byrow = F)
    }
    
    for(i in 1: length(xseq_parent)){
      #points on line with constant 'x' and those that are inside the polygon
      x_points_inside_polygon[[i]] <- Points_on_xlines[[i]][c(which(sp::point.in.polygon(Points_on_xlines[[i]][, 1], 
                                                                                     Points_on_xlines[[i]][, 2],
                                                                                     output_polygon[, 1], 
                                                                                     output_polygon[, 2]) != 0)), , 
                                                            drop = F]
      #lines(x_points_inside_polygon[[i]][, 1], x_points_inside_polygon[[i]][, 2])  
    }
    
    #constructing smaller polygons inside parent polygon
    smaller_polygons_within_parent <- list()
    #npoly <-0
    no_of_small_poly <- 1
    for(i in 1: (length(x_points_inside_polygon) - 1)){
      
      #if there are 2 or more points on a line
      if(length(x_points_inside_polygon[[i]][, 1]) > 1){
        
        for(j in 1: (length(x_points_inside_polygon[[i]][, 1]) - 1)){
          
          #if the consecutive y co-ordinates are present in the next line
          if(x_points_inside_polygon[[i]][j, 2] %in% x_points_inside_polygon[[(i + 1)]][, 2] && 
               x_points_inside_polygon[[i]][(j + 1), 2] %in% x_points_inside_polygon[[(i + 1)]][, 2]){
            
            smaller_polygons_within_parent[[no_of_small_poly]] <- matrix(c(x_points_inside_polygon[[i]][j, ],
                                                                           x_points_inside_polygon[[(i + 1)]][1, 1],
                                                                           x_points_inside_polygon[[i]][j, 2],
                                                                           x_points_inside_polygon[[(i + 1)]][1, 1],
                                                                           x_points_inside_polygon[[i]][(j + 1), 2],
                                                                           x_points_inside_polygon[[i]][j, 1],
                                                                           x_points_inside_polygon[[i]][(j + 1), 2]), 
                                                                         ncol = 2, byrow = T)
            no_of_small_poly <- no_of_small_poly + 1
          }
        }  
      }
    }
    return(smaller_polygons_within_parent)
  }
  
  #function to chop sammon space into smaller boxes
  Sammon_Space_Boxes <- function(input_polygon, nbox){
    
    requireNamespace("grDevices")    #chull function
    requireNamespace("splancs")      #csr function 
    requireNamespace("sp")           #point.in.polygon function
    requireNamespace("conf.design")  #factorize function
    
    xrange_sammon_poly <- range(input_polygon[, 1])
    yrange_sammon_poly <- range(input_polygon[, 2])
    #create same number of boxes as that in parent polygon
    nbox_factors <- conf.design:: factorize(nbox)
    if(length(nbox_factors) == 1){
      nbox <- nbox - 1
      nbox_factors <- conf.design:: factorize(nbox)
    }
    xseq_sammon <- seq(xrange_sammon_poly[1], 
                       xrange_sammon_poly[2],
                       length.out = nbox_factors[length(nbox_factors)] + 1)
    yseq_sammon <- seq(yrange_sammon_poly[1], 
                       yrange_sammon_poly[2],
                       length.out = nbox / nbox_factors[length(nbox_factors)] + 1)
    
    samPoints_on_ylines <- samPoints_on_xlines <-  list()
    
    for(i in 1: length(xseq_sammon)){
      #points on line with constant 'x'
      samPoints_on_xlines[[i]] <- matrix(c(rep(xseq_sammon[i], length(yseq_sammon)), 
                                           yseq_sammon), 
                                         ncol = 2, byrow = F)
    }
    
    #constructing polygons inside sammon polygon
    smaller_polygons_within_sammon <- list()
    #npoly <-0
    no_of_small_samPoly <- 1
    for(l in 1: (length(samPoints_on_xlines) - 1)){
      #if there are 2 or more points on a line
      if(length(samPoints_on_xlines[[l]][, 1]) > 1){
        for(m in 1: (length(samPoints_on_xlines[[l]][, 1]) - 1)){
          #if the consecutive y co-ordinates are present in the next line
          if(samPoints_on_xlines[[l]][m, 2] %in% samPoints_on_xlines[[(l + 1)]][, 2] && 
               samPoints_on_xlines[[l]][(m + 1), 2] %in% samPoints_on_xlines[[(l + 1)]][, 2]){
            
            smaller_polygons_within_sammon[[no_of_small_samPoly]] <- matrix(c(samPoints_on_xlines[[l]][m, ],
                                                                              samPoints_on_xlines[[(l + 1)]][1, 1],
                                                                              samPoints_on_xlines[[l]][m, 2],
                                                                              samPoints_on_xlines[[(l + 1)]][1, 1],
                                                                              samPoints_on_xlines[[l]][(m + 1), 2],
                                                                              samPoints_on_xlines[[l]][m, 1],
                                                                              samPoints_on_xlines[[l]][(m + 1), 2]),
                                                                            ncol = 2, byrow = T)
            no_of_small_samPoly <- no_of_small_samPoly + 1
          }
        }  
      }
    }
    return(smaller_polygons_within_sammon)
  }
  
  #construct the polygons in parent tile
  par_tile_polygon <- list()
  #variable storing appropriate parent tile index
  lindex <- c()
  #for constructing sammon space polygon
  xrange <- yrange <- sammon_space_polygon <- list()
  #to store the chopped rectangles in both parent tile and sammon space
  boxes_in_sammon_space <- boxes_in_parent_tile <- list()
  #to store the scale-factors and translate matrix information
  transformation_matrices_info <- int_list <- list()
  #for the indices of the smaller rectangles containing sammon points in the sammon space polygon
  rect_indices_containing_points <- inter_indices <- list()
  #to store the transformed points
  new_rawdeldati <- list()
  parentTiles <- unique(gidata[, "Segment.Parent"])
  
  for(ind1 in 1: length(parentTiles)){
    if(((parentTiles[ind1]) %% nclust)){
      lindex[ind1] <- (parentTiles[ind1] %% nclust)
    }else{
      lindex[ind1] <- nclust
    }
    par_tile_polygon[[ind1]] <- matrix(c(poly_info[[lindex[ind1]]]$x,
                                         poly_info[[lindex[ind1]]]$y),
                                       ncol = 2, byrow = F)
    
    #construct the sammon space polygon using xrange and yrange of sammon
    xrange[[ind1]] <- range(rawdeldati[[ind1]][, 1])
    yrange[[ind1]] <- range(rawdeldati[[ind1]][, 2])
    sammon_space_polygon[[ind1]] <- matrix(c(xrange[[ind1]][1], yrange[[ind1]][1], 
                                             xrange[[ind1]][2], yrange[[ind1]][1],
                                             xrange[[ind1]][2], yrange[[ind1]][2], 
                                             xrange[[ind1]][1], yrange[[ind1]][2]), 
                                           ncol = 2, byrow = T)
    
    #represent each small box in the bigger polygon as a polygon
    
    boxes_in_parent_tile[[ind1]] <- Parent_Tile_Boxes(par_tile_polygon[[ind1]])
    nbox <- length(boxes_in_parent_tile[[ind1]])
    if(nbox == 0){
      return("-1")
    }
    boxes_in_sammon_space[[ind1]] <- Sammon_Space_Boxes(sammon_space_polygon[[ind1]], nbox)
    
    #transformation_matrices_info will have scale matrix and translate matrix values for each
    #rectangle-rectangle transformation from sammon space to parent polygon
    
    #obtain the scale matrix
    scaleMat <- ScaleMat(boxes_in_sammon_space[[ind1]][[1]],
                         boxes_in_parent_tile[[ind1]][[1]])
    for(j in 1: length(boxes_in_sammon_space[[ind1]])){
      #get the translate matrix values by giving scale matrix also as input
      int_list[[j]] <- Transform_Coordinates(boxes_in_sammon_space[[ind1]][[j]],
                                             boxes_in_parent_tile[[ind1]][[j]], 
                                             scaleMat)
    }
    transformation_matrices_info[[ind1]] <- int_list
    int_list <- list()
    
    #find the indices of the smaller boxes in sammon space in which the input sammon points exist
    
    for(j in 1: length(rawdeldati[[ind1]][, 1])){
      for(k in 1: length(boxes_in_sammon_space[[ind1]])){
        if(sp::point.in.polygon(rawdeldati[[ind1]][j, 1], 
                            rawdeldati[[ind1]][j, 2],
                            boxes_in_sammon_space[[ind1]][[k]][, 1], 
                            boxes_in_sammon_space[[ind1]][[k]][, 2]) != 0){
          inter_indices[[j]] <- k 
        }
      }
    }
    rect_indices_containing_points[[ind1]] <- inter_indices
    
    #using the indices obtained above and the scale and translate matrices to 
    #get the transformed coordinates for the input sammon datapoints
    int_matrix <- matrix(0, ncol = 2, nrow = length(rawdeldati[[ind1]][, 1]))
    for(j in 1: length(rawdeldati[[ind1]][, 1])){
      int_matrix[j, ] <- New_Points(transformation_matrices_info[[ind1]][[rect_indices_containing_points[[ind1]][[j]]]])
    }
    new_rawdeldati[[ind1]] <- int_matrix
  }
  return(new_rawdeldati)
}
