Corrected_Tessellations <-
function(current_dirsgs, current_tile, current_polygon){
  
  requireNamespace("splancs")      #csr function 
  requireNamespace("sp")           #point.in.polygon function
    
  initial_dirsgs <- current_dirsgs
  #vert_outside1 and vert_outside2 check for the points which are outside the parent polygon
  vert_outside1 <- which(sp::point.in.polygon(current_dirsgs[, "x1"], 
                                          current_dirsgs[, "y1"], 
                                          current_tile$x,
                                          current_tile$y) == 0)
  
  vert_outside2 <- which(sp::point.in.polygon(current_dirsgs[, "x2"], 
                                          current_dirsgs[, "y2"],
                                          current_tile$x,
                                          current_tile$y) == 0)
  
  #if the same index is present in both vert_outside1 and vert_outside2, it means the line 
  #joining these points has both the end-points outside the parent polygon
  vert_outside <- intersect(vert_outside1, vert_outside2)
  
  #if the line joining the points p1 and p2 is outside the parent polygon and
  #p1 is boundary point and p2 is not, then make p2 the boundary point or vice versa
  if (length(vert_outside) > 0 ) {
    for (j in 1: length(vert_outside)) {
      #obtain the index of the line for which the point is outside
      x_coord_index1 <- which((current_dirsgs[vert_outside[j], "x1"] == current_dirsgs[, c("x1", "x2")]) == T)
      y_coord_index1 <- which((current_dirsgs[vert_outside[j], "y1"] == current_dirsgs[, c("y1", "y2")]) == T)      
      x_coord_index2 <- which((current_dirsgs[vert_outside[j], "x2"] == current_dirsgs[, c("x1", "x2")]) == T)
      y_coord_index2 <- which((current_dirsgs[vert_outside[j], "y2"] == current_dirsgs[, c("y1", "y2")]) == T)
      x_coord_index <- c(x_coord_index1, x_coord_index2)
      y_coord_index <- c(y_coord_index1, y_coord_index2)
      xy_index <- intersect(x_coord_index, y_coord_index)
      ind_len <- length(xy_index)
      #change the boundary points of those indices to TRUE
      for (i in 1: ind_len) {
        if (xy_index[i] > nrow(current_dirsgs)) {
          current_dirsgs[(xy_index[i] - nrow(current_dirsgs)), "bp2"] <- T
        } else {
          current_dirsgs[xy_index[i], "bp1"] <- T
        } 
      }      
    }
  }
  
  #boundary points in the first index
  firstpt <- current_dirsgs[which(current_dirsgs[, "bp1"] == T), ]
  #boundary points in the second index
  secondpt <- current_dirsgs[which(current_dirsgs[, "bp2"] == T), ]
  
  #use all these points together and remove the common rows
  pts <- rbind(firstpt, secondpt)
  if(any(duplicated(pts) == T)){
    ptsout <- pts[-which(duplicated(pts) == T), ]
  }else{
    ptsout <- pts
  }
  
  #2 matrices, 'm' to store slope and 'c' to store constants of the lines in the polygon
  m_parent <-  c_parent <- matrix(ncol = 1, nrow = nrow(current_polygon))
  #mat1 is the current polygon
  mat1 <-  current_polygon
  #closing the polygon by making the first and last points same
  mat1 <- rbind(mat1, mat1[1, ])
  
  #slopes and intercepts for lines in the parent polygon
  for (i in 1: (nrow(mat1) - 1)) {  
    m_parent[i] <- ((mat1[(i + 1), 2] - mat1[i, 2]) / 
                      (mat1[(i + 1), 1] - mat1[i, 1]))
    c_parent[i] <- mat1[i, 2] - (m_parent[i] * mat1[i, 1])
  }
  
  #polygon is too small to calculate child level tessellations
  if(any(is.nan(m_parent) == T)){
    #flog.warn("Projection is not scaled enough")
    return("-1")
  }
  
  #slopes and intercepts for lines with endpoints outside the polgyon 
  if(nrow (ptsout) != 0){
    m_ptsout <- c_ptsout <- matrix(ncol = 1, nrow = nrow(ptsout))
    #for all lines in the child tessellations for which the points are outside
    for (i in 1: nrow(ptsout)){
      m_ptsout[i] <- ((ptsout[i, "y2"] - ptsout[i, "y1"]) / 
                        (ptsout[i, "x2"] - ptsout[i, "x1"]))
      c_ptsout[i] <- ptsout[i, "y1"] - (m_ptsout[i] * ptsout[i, "x1"])
      
      #for all lines in the child tessellation for which the points are outside,, 
      #calculate the point of intersection of each line
      #with all the boundary lines of the parent polygon
      x <- y <- c()
      for (j in 1: (nrow(mat1) - 1)) {
        if ((abs(m_parent[j]) == Inf) || (m_parent[j] == m_ptsout[i])) {
          x[j] <- mat1[j, 1]
          if((abs(m_ptsout[i]) == Inf) || (m_parent[j] == m_ptsout[i])){
            #both are vertical lines or both are parallel
            y[j] <- 500
          }else{
            y[j] <- (m_ptsout[i] * x[j]) + c_ptsout[i]
          }
        } else {
          if (abs(m_ptsout[i]) == Inf) {
            x[j] <- ptsout[i, "x1"]
            y[j] <- (m_parent[j] * x[j]) + c_parent[j]
          } else {
            x[j] <- (c_ptsout[i] - c_parent[j]) / (m_parent[j] - m_ptsout[i])
            y[j] <- (m_ptsout[i] * x[j]) + c_ptsout[i]
          }
        }
      }
      
      #check which points among all the points of intersection lies on the boundary of parent polygon
      pt_intersect <- matrix(NA, ncol = 2, nrow = (nrow(mat1) - 1))
      xy.poly <- splancs::as.points(mat1)
      for(j in 1: (nrow(mat1) - 1)){
        xy <- splancs::as.points(x[j], y[j])
        if((nrow(splancs::pip(xy, xy.poly, out = F, bound = T)) != 0) ||
             (sp::point.in.polygon(x[j], y[j], mat1[, 1], mat1[, 2]) !=0) ||
             (sp::point.in.polygon(as.character(x[j]), as.character(y[j]), mat1[, 1], mat1[, 2]) != 0)){
          pt_intersect[j, ] <- xy
        }
      }
      #discard the points which are not on the boundary.
      rows_del <- NULL
      for(k in 1: (nrow(mat1) - 1)){
        if(length(all(is.na(pt_intersect[k,])))){
          rows_del[k] <- all(is.na(pt_intersect[k,]))
        }
      }
      
      if(any(rows_del == T)){
        pt_intersect <- pt_intersect[-which(rows_del == T), , drop = F]
      }
      
      #calculate the distance between the boundary point of the second level tessellation with the 
      #points of intersection which are on the boundary of the parent tile.      
      if(nrow(pt_intersect) != 0){
        distmat <- matrix(0, ncol = 2, nrow = (nrow(pt_intersect) + 1))
        distmat[2: nrow(distmat), ] <- pt_intersect
        #choose the point which is a boundary point
        if(ptsout[i, "bp1"] == T){
          distmat[1, ] <- as.matrix(ptsout[i, c("x1", "y1")])
          desired_pt <- distmat[which(stats::dist(distmat) == min(as.matrix(stats::dist(distmat))[2: nrow(distmat),1])) + 1,]   # +1 to get the correct index from the output of dist function
          ptsout[i, c("x1", "y1")] <- desired_pt
        }
        if(ptsout[i, "bp2"] == T){
          distmat[1, ] <- as.matrix(ptsout[i, c("x2", "y2")])
          desired_pt <- distmat[which(stats::dist(distmat) == min(as.matrix(stats::dist(distmat))[2: nrow(distmat),1])) + 1,]   # +1 to get the correct index from the output of dist function
          ptsout[i, c("x2", "y2")] <- desired_pt
        }
      }
    }
  }
  
  #assign the new points to the output of the tessellation
  current_dirsgs[rownames(ptsout), ] <- ptsout
  
  #return the result
  return (current_dirsgs)
}
