# Function to take care of wacky tessellations in the Brick i.e; boundaries which are outside a given regions
getIntersectionPoints <-
  function(child_x, child_y, parent_x, parent_y,min_x,min_y,centroid_x,centroid_y) {
    
    
    requireNamespace("rgeos")
    requireNamespace("sp")
    requireNamespace("dplyr")
    
    value <- sp::point.in.polygon(child_x, child_y, parent_x, parent_y)
    if (length(child_x) < 3 | length(child_y) < 3 | length(parent_x) < 3 | length(parent_y) < 3)
    {
      return(list(
        "child_x" = rep(0, length(child_x)),
        "child_y" = rep(0, length(child_y)),
        "x" = min_x,
        "y"=min_y
        
      ))
    }
    polygon_child <- "POLYGON(("
    for (j in 1:length(child_x)) {
      polygon_child <-
        paste0(polygon_child, child_x[[j]], " ", child_y[[j]], ",")
    }
    polygon_child <-
      paste0(polygon_child, child_x[[1]], " ", child_y[[1]], "))")
    polygon_parent <- "POLYGON(("
    for (j in 1:length(parent_x)) {
      polygon_parent <-
        paste0(polygon_parent, parent_x[[j]], " ", parent_y[[j]], ",")
    }
    polygon_parent <-
      paste0(polygon_parent, parent_x[[1]], " ", parent_y[[1]], "))")
    
    polygon_parent <- rgeos::readWKT(polygon_parent)
    polygon_child <- rgeos::readWKT(polygon_child)
    if (!is.null(rgeos::gIntersection(polygon_parent, polygon_child))) {
      intersection_matrix <-
        rgeos::gIntersection(polygon_parent, polygon_child)@polygons[[1]]@Polygons[[1]]@coords
      
      child_x <- intersection_matrix[, 1][-1]
      child_y <- intersection_matrix[, 2][-1]
      return(list("child_x" = child_x, 
                  "child_y" = child_y,
                  "x" = centroid_x,
                  "y"=centroid_y))
      
    }
    else{
      return(list(
        "child_x" = rep(0, length(child_x)),
        "child_y" = rep(0, length(child_y)),
        "x" = min_x,
        "y"=min_y
      ))
    }
    
  }



correctedBoundaries <- function(hvt_list, maxDepth) {
  min_x = 1e9
  min_y = 1e9
 
  
  
  for (clusterNo in 1:length(hvt_list[[2]][[1]][[1]])) {
    bp_x = hvt_list[[2]][[1]][["1"]][[clusterNo]][["x"]]
    bp_y = hvt_list[[2]][[1]][["1"]][[clusterNo]][["y"]]
    
    
    if (min(bp_x) < min_x)
      min_x = min(bp_x)
    
    if (min(bp_y) < min_y)
      min_y = min(bp_y)
   
    
  }
  
  for (depth in 2:maxDepth) {
    if (depth == 2) {
      for (clusterNo in names(hvt_list[[2]][[depth]])) {
        for (childNo in 1:length(hvt_list[[2]][[depth]][[clusterNo]])) {
          if (!is.null(hvt_list[[2]][[depth]][[clusterNo]])) {
            child_x <-
              hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["x"]]
            child_y <-
              hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["y"]]
            parent_x <-
              hvt_list[[2]][[1]][["1"]][[as.numeric(clusterNo)]][["x"]]
            parent_y <-
              hvt_list[[2]][[1]][["1"]][[as.numeric(clusterNo)]][["y"]]
            centroid_x <-
              hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["pt"]][["x"]]
            centroid_y <-
              hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["pt"]][["y"]]
            updatedList <-
              getIntersectionPoints(child_x, child_y, parent_x, parent_y,min_x,min_y,centroid_x,centroid_y)
            
            hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["x"]] <-
              updatedList[["child_x"]]
            hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["y"]] <-
              updatedList[["child_y"]]
            hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["pt"]][["x"]] <-
              updatedList[["x"]]
            hvt_list[[2]][[depth]][[clusterNo]][[childNo]][["pt"]][["y"]] <-
              updatedList[["y"]]
          }
        }
      }
    }
    else {
      for (clusterNo in 1:length(unlist(hvt_list[[2]][[depth - 1]], recursive = F, use.names = F))) {
        for (childNo in 1:length(hvt_list[[2]][[depth]][[as.character(clusterNo)]])) {
          if (!is.null(hvt_list[[2]][[depth]][[as.character(clusterNo)]])) {
            temp_list <-
              unlist(hvt_list[[2]][[depth - 1]],
                     recursive = F,
                     use.names = F)
            
            child_x <-
              hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["x"]]
            child_y <-
              hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["y"]]
            parent_x <- temp_list[[clusterNo]][["x"]]
            parent_y <- temp_list[[clusterNo]][["y"]]
            centroid_x <-
              hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["pt"]][["x"]]
            centroid_y <-
              hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["pt"]][["y"]]
            updatedList <-
              getIntersectionPoints(child_x, child_y, parent_x, parent_y,min_x,min_y,centroid_x,centroid_y)
            
            hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["x"]] <-
              updatedList[["child_x"]]
            hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["y"]] <-
              updatedList[["child_y"]]
            hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["pt"]][["x"]] <-
              updatedList[["x"]]
            hvt_list[[2]][[depth]][[as.character(clusterNo)]][[childNo]][["pt"]][["y"]] <-
              updatedList[["y"]]
            
          }
        }
      }
    }
    
  }
  return(hvt_list)
}
