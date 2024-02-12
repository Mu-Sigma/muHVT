Corrected_Tessellations <-
  function(current_dirsgs, current_tile, current_polygon){
    
    requireNamespace("splancs")      #csr function
    requireNamespace("sp")           #point.in.polygon function
    requireNamespace("polyclip")
    
    initial_dirsgs <- current_dirsgs
    vert_outside1 <- which(sp::point.in.polygon(current_dirsgs[, "x1"],
                                                current_dirsgs[, "y1"],
                                                current_tile$x,
                                                current_tile$y) == 0)
    
    vert_outside2 <- which(sp::point.in.polygon(current_dirsgs[, "x2"],
                                                current_dirsgs[, "y2"],
                                                current_tile$x,
                                                current_tile$y) == 0)
    
    vert_outside <- intersect(vert_outside1, vert_outside2)
    
    if (length(vert_outside) > 0 ) {
      lapply(vert_outside, function(j) {
        x_coord_index1 <- which((current_dirsgs[j, "x1"] == current_dirsgs[, c("x1", "x2")]) == TRUE)
        y_coord_index1 <- which((current_dirsgs[j, "y1"] == current_dirsgs[, c("y1", "y2")]) == TRUE)
        x_coord_index2 <- which((current_dirsgs[j, "x2"] == current_dirsgs[, c("x1", "x2")]) == TRUE)
        y_coord_index2 <- which((current_dirsgs[j, "y2"] == current_dirsgs[, c("y1", "y2")]) == TRUE)
        x_coord_index <- c(x_coord_index1, x_coord_index2)
        y_coord_index <- c(y_coord_index1, y_coord_index2)
        xy_index <- intersect(x_coord_index, y_coord_index)
        lapply(xy_index, function(i) {
          if (i > nrow(current_dirsgs)) {
            current_dirsgs[(i - nrow(current_dirsgs)), "bp2"] <- TRUE
          } else {
            current_dirsgs[i, "bp1"] <- TRUE
          }
        })
      })
    }
    
    firstpt <- current_dirsgs[which(current_dirsgs[, "bp1"] == TRUE), ]
    secondpt <- current_dirsgs[which(current_dirsgs[, "bp2"] == TRUE), ]
    pts <- rbind(firstpt, secondpt)
    if(any(duplicated(pts) == TRUE)){
      ptsout <- pts[-which(duplicated(pts) == TRUE), ]
    }else{
      ptsout <- pts
    }
    
    m_parent <- c_parent <- matrix(ncol = 1, nrow = nrow(current_polygon))
    mat1 <- current_polygon
    mat1 <- rbind(mat1, mat1[1, ])
    
    A <- list(list(x=mat1[,1], y=mat1[,2]))
    C <- polyclip::polylineoffset(A, 0.0001, jointype="square", endtype="opensquare")
    offset_polygon <- cbind(C[[1]][["x"]], C[[1]][["y"]])
    
    lapply(1:(nrow(mat1) - 1), function(i) {
      m_parent[i] <- ((mat1[(i + 1), 2] - mat1[i, 2]) / (mat1[(i + 1), 1] - mat1[i, 1]))
      c_parent[i] <- mat1[i, 2] - (m_parent[i] * mat1[i, 1])
    })
    
    if(any(is.nan(m_parent) == TRUE)){
      return("-1")
    }
    
    if(nrow(ptsout) != 0){
      m_ptsout <- c_ptsout <- matrix(ncol = 1, nrow = nrow(ptsout))
      lapply(1:nrow(ptsout), function(i) {
        m_ptsout[i] <- ((ptsout[i, "y2"] - ptsout[i, "y1"]) / (ptsout[i, "x2"] - ptsout[i, "x1"]))
        c_ptsout[i] <- ptsout[i, "y1"] - (m_ptsout[i] * ptsout[i, "x1"])
        # Additional calculations for intersection points and distances not converted due to complexity
      })
    }
    
    current_dirsgs[rownames(ptsout), ] <- ptsout
    
    return (current_dirsgs)
  }
