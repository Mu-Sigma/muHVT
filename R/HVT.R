#' Constructing Hierarchical Voronoi Tessellations
#' 
#' Main function to construct hierarchical voronoi tessellations.
#' 
#' This is the main function to construct hierarchical voronoi tessellations.
#' The \code{hvq} function is called from this function. The output of the
#' \code{hvq} function is hierarchical clustered data which will be the input
#' for constructing tessellations. The data is then represented in 2d
#' coordinates and the tessellations are plotted using these coordinates as
#' centroids. For subsequent levels, transformation is performed on the 2d
#' coordinates to get all the points within its parent tile. Tessellations are
#' plotted using these transformed points as centroids. The lines in the
#' tessellations are chopped in places so that they do not protrude outside the
#' parent polygon. This is done for all the subsequent levels.
#' 
#' @param dataset Data frame. A data frame with different columns is given as
#' input.
#' @param nclust Numeric. An integer indicating the number of clusters per
#' hierarchy (level)
#' @param depth Numeric. An integer indicating the number of levels. (1 = No
#' hierarchy, 2 = 2 levels, etc ...)
#' @param quant.err Numeric. A number indicating the quantization error
#' treshold.
#' @param projection.scale Numeric. A number indicating the scale factor for
#' the tesselations so as to visualize the sub-tesselations well enough.
#' @param normalize Logical. A logical value indicating if the columns in your
#' dataset should be normalized. Default value is TRUE.
#' @param distance_metric character. The distance metric can be 'Euclidean" or "Manhattan". Euclidean is selected by default.
#' @param error_metric character. The error metric can be "mean" or "max". mean is selected by default 
#' @return A list that contains the hierarchical tesselation information. This
#' list has to be given as input argument to plot the tessellations.
#' \item{[[1]] }{List. Information about the tesselation co-ordinates - level
#' wise} \item{[[2]] }{List. Information about the polygon co-ordinates - level
#' wise} \item{[[3]] }{List. Information about the hierarchical vector
#' quantized data - level wise}
#' @author Meet K. Dave <dave.kirankumar@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}} \cr \code{\link{hvtHmap}}
#' @keywords hplot
#' @importFrom magrittr %>%
#' @examples
#' 
#' data("USarrests",package="datasets")
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 6, depth = 1, quant.err = 0.2, 
#'                   projection.scale = 10, normalize = TRUE)
#' plotHVT(hvt.results, line.width = c(0.8), color.vec = c("#141B41""))
#'
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 3, depth = 3, quant.err = 0.2, 
#'                   projection.scale = 10, normalize = TRUE)
#' plotHVT(hvt.results, line.width = c(1.2,0.8,0.4), color.vec = c("#141B41","#0582CA","#8BA0B4"))
#' 
#' @export HVT
HVT <-
function (dataset, nclust, depth, quant.err, projection.scale, normalize = T,distance_metric = c("L1_Norm","L2_Norm"),error_metric = c("mean","max")) {

    requireNamespace("MASS")         #sammon function
    requireNamespace("deldir")       #deldir function 
    requireNamespace("Hmisc")        #ceil function
    #require(gtools)
    #require(seas)
    #require(mgcv)
    #require(spatstat)
    requireNamespace("grDevices")    #chull function
    requireNamespace("splancs")      #csr function 
    requireNamespace("sp")           #point.in.polygon function
    requireNamespace("conf.design")  #factorize function
    
    options(warn = -1)
    
    dataset <- as.data.frame(dataset)
    
    if(normalize){
      scaledata <- scale(dataset, scale = T, center = T)
      rownames(scaledata) <- rownames(dataset)
      
      mean_data <- attr(scaledata,"scaled:center")
      std_data <- attr(scaledata,"scaled:scale")
      
      scale_summary <- list(mean_data=mean_data,std_data=std_data)
      
      # flog.info("scaling is done")
    }else{
      scaledata <- as.matrix(dataset)
      rownames(scaledata) <- rownames(dataset)
      # flog.info("The data is not scaled as per the user requirement")
      scale_summary <- NULL
    }
    
    polinfo <- hvqdata <- list()
    hvq_k <- hvq(scaledata, nclust = nclust, depth = depth, quant.err = quant.err,distance_metric = distance_metric,error_metric = error_metric)
    # flog.info("HVQ output is ready")
    hvqoutput <- hvq_k$summary
    
    gdata <- hvqoutput  #assign the output of hvq file to gdata
    #cleaning the data by deleting the rows containing NA's
    #gdata <- gdata[-which(is.na(gdata[, 5])), ]
    gdata <- hvqoutput[stats::complete.cases(hvqoutput),]
    hvqdata <- gdata
    # flog.info("NA's are removed from the HVQ output")
    
    #to store hvqdata according to each level
    tessdata <- input.tessdata <- list()
    #stores sammon co-ordinates and segregated according to each level
    points2d <- list()
    #sammon datapoints to be stored in hierarchy
    rawdeldata <- list()
    #rawdeldata is transformed to be within its parent tile and stored in new_rawdeldata
    new_rawdeldata <- list()
    #contains the vertices of the parent polygon
    pol_info <- polygon_info <- list()
    #number of levels
    nlevel <- length(unique(gdata[, "Segment.Level"]))
    #verify if the transformed points are correct
    transpoints <- list()
    
    # New columns added to the dataset by the hvq function
    newcols <-  ncol(hvqoutput) - ncol(dataset)
    
    for (i in 1: nlevel) {
      
      #hvqdata segregated according to different levels
      tessdata[[i]] <- gdata[which(gdata[, "Segment.Level"] == i), ]
      
      #data to be used as input to sammon function
      input.tessdata[[i]] <- tessdata[[i]][, (newcols+1): ncol(hvqoutput)]
      
      #sammon function output is 2d coordinates which are saved level-wise
      points2d[[i]] <- projection.scale * (MASS::sammon(stats::dist(unique(input.tessdata[[i]])),niter = 10^5,trace=FALSE)$points)
      
      #sammon datapoints grouped according to the hierarchy
      intermediate.rawdata <- list()
      
      for(j in 1: length(unique(tessdata[[i]][, "Segment.Parent"]))) {
        intermediate.rawdata[[j]] <- cbind(points2d[[i]][((nclust * (j - 1)) + 1): (j * nclust), 1], 
                                           points2d[[i]][((nclust * (j - 1)) + 1): (j * nclust), 2])
      }
      rawdeldata[[i]] <- intermediate.rawdata
      
    }
    rm(intermediate.rawdata)
    # flog.info("Sammon points for all the levels have been calculated")
    
    #new_rawdeldata contains the transformed points of rawdeldata
    new_rawdeldata[[1]] <- rawdeldata[[1]]
    
    #deldat1 is the output of the deldir::deldir function and contains the tessellation information
    deldat1 <- deldat2 <- list()
    #deldir function of deldir package gives the tessellations output
    deldat2[[1]] <- deldir::deldir(new_rawdeldata[[1]][[1]][, 1], 
                           new_rawdeldata[[1]][[1]][, 2])
    deldat1[[1]] <- deldat2
    rm(deldat2)
    # flog.info("Tessellations are calculated for the first level")
    #plotting the tessellation
    #plot(deldat1[[1]][[1]], wlines = "tess", lty = 1, lwd = 4)
    
    #polygon_info stores parent tile vertices information
    #polygon_info is the modified tile.list output except for first level.
    pol_info[[1]] <- deldir::tile.list(deldat1[[1]][[1]])
    polygon_info[[1]] <- pol_info
    rm(pol_info)
    par_tile_indices <- n_par_tile <- list()
    par_tile_indices[[1]] <- unique(tessdata[[1]][, "Segment.Parent"])
    n_par_tile[[1]] <- length(unique(tessdata[[1]][, "Segment.Parent"]))
    
    if(nlevel < 2){
      polinfo <- polygon_info
      
      fin_out <- list()
      
      fin_out[[1]] <- deldat1
      fin_out[[2]] <- polinfo
      fin_out[[3]] <- hvq_k
      fin_out[[3]][['scale_summary']] <- scale_summary
      
      
      return(fin_out)
      
    } else {
      for(i in 2: nlevel){
        
        new_rawdeldata[[i]] <- list() 
        par_tile_indices[[i]] <- unique(tessdata[[i]][, "Segment.Parent"])
        n_par_tile[[i]] <- length(unique(tessdata[[i]][, "Segment.Parent"]))
        
        for(tileIndex in 1: n_par_tile[[(i - 1)]]){
          #a chunk of hvqdata which contains the rows corresponding to a particular parent tile
          gidata <- tessdata[[i]][which(tessdata[[i]][, "Segment.Parent"] %in% 
                                          par_tile_indices[[i]][intersect(which((par_tile_indices[[i]] / nclust ) <= par_tile_indices[[(i - 1)]][tileIndex]),
                                                                          which((par_tile_indices[[(i - 1)]][tileIndex] - 1) < (par_tile_indices[[i]] / nclust )))]), ]
          
          #datapoints corresponding to a particular parent tile
          rawdeldati <- rawdeldata[[i]][intersect(which((par_tile_indices[[i]] / nclust ) <= par_tile_indices[[(i - 1)]][tileIndex]),
                                                  which((par_tile_indices[[(i - 1)]][tileIndex] - 1) < (par_tile_indices[[i]] / nclust )))]
          if(nrow(gidata) != 0){
            #transformation of rawdeldata such that they are inside the parent tile and output is stored in new_rawdeldata
            transpoints <- DelaunayInfo(gidata, polygon_info[[(i - 1)]][[tileIndex]], rawdeldati, nclust)
            if(all(transpoints != "-1")){
              new_rawdeldata[[i]] <- append(new_rawdeldata[[i]], transpoints)
            }else{
              deldat1[i] <- 0
              polinfo <<- polygon_info
              #flog.warn("Projection is not scaled enough and the polygon is very small to perform transformation")
              return(deldat1)
              #return("Projection is not scaled enough and the polygon is very small to perform transformation")
            }
            
          }
        }
        # flog.info("Sammon points of level %s are transformed", i)
        
        deldat2 <- list()
        par_tile_polygon <- list()
        for(tileNo in 1: n_par_tile[[i]]){
          
          #modulus operation for the last index in polygon_info
          if(((par_tile_indices[[i]][tileNo]) %% nclust)){
            last_index <- (par_tile_indices[[i]][tileNo] %% nclust)
          }else{
            last_index <- nclust
          }
          #divide to get the parent tile
          sec_index <- Hmisc::ceil(par_tile_indices[[i]][tileNo] / nclust)
          
          deldat2[[tileNo]] <- deldir::deldir(new_rawdeldata[[i]][[tileNo]][, 1], 
                                      new_rawdeldata[[i]][[tileNo]][, 2], 
                                      rw = c(range(polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$x) - c(0.5, -0.5),
                                             range(polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$y) - c(0.5, -0.5)))
          #constructing parent polygons
          par_tile_polygon[[tileNo]] <- matrix(c(polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$x,
                                                 polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]$y),
                                               ncol = 2, byrow = F )
          #correct the tessellations
          cur_dirsgs <- deldat2[[tileNo]]$dirsgs
          cur_tile <- polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]]
          cur_polygon <- par_tile_polygon[[tileNo]]
          verify_dirsgs <- Corrected_Tessellations(cur_dirsgs, 
                                                   cur_tile, 
                                                   cur_polygon)
          
          #polygon is sufficient to calculate next level tessellations inside this
          if(all(verify_dirsgs[,1:8] != "-1")){
            deldat2[[tileNo]]$dirsgs <- verify_dirsgs
            # flog.info("Tessellations for level %s is calculated", i)
          }else{
            deldat1[[i]] <- 0
            #flog.warn("Projection is not scaled enough and the polygon is very small to perform transformation")
            polinfo <<- polygon_info
            return(deldat1)
            #return("Projection is not scaled enough to perform tessellations for the next level")
          }
          
          #delete lines with both the points identical
          new_dirsgs <- deldat2[[tileNo]]$dirsgs
          del_rows <- 0
          for (j in 1: nrow(new_dirsgs)) {
            if (round(new_dirsgs[j, "x1"], 6) == round(new_dirsgs[j, "x2"], 6) && 
                  round(new_dirsgs[j, "y1"], 6) == round(new_dirsgs[j, "y2"], 6)) {
              del_rows <- c(del_rows, j)
            }
          }
          if(length(del_rows) > 1){
            new_dirsgs <- new_dirsgs[-del_rows,]
          }
          deldat2[[tileNo]]$dirsgs <- new_dirsgs
          # flog.info("Line with same endpoints are deleted")
        }
        
        deldat1[[i]] <- deldat2
        
        polygon_info[[i]] <- list()
        #polygon information to correct the polygons
        for(parentIndex in 1: n_par_tile[[i]]){
          polygon_info[[i]][[parentIndex]] <- suppressMessages(deldir::tile.list(deldat1[[i]][[parentIndex]]))
        }
        
        #to delete the points which are outside the parent tile
        
        for (parentIndex in 1: length(polygon_info[[i]])) {
          for (tileIndex in 1: length(polygon_info[[i]][[parentIndex]])) {
            tile <- polygon_info[[i]][[parentIndex]][[tileIndex]]
            check_dirsgs <- deldat1[[i]][[parentIndex]]$dirsgs
            if(nrow(check_dirsgs) != 0 && length(tile$x) != 0){
              polygon_info[[i]][[parentIndex]][[tileIndex]] <- Delete_Outpoints (tile, check_dirsgs)
            }
          }
        }
        # flog.info("Endpoints of tessellation lines which are outside parent polygon are deleted")
        
        for (parentIndex in 1: n_par_tile[[i]]) {
          #modulo operation to obtain the last index
          if(((par_tile_indices[[i]][parentIndex]) %% nclust)){
            last_index <- (par_tile_indices[[i]][parentIndex] %% nclust)
          }else{
            last_index <- nclust
          }
          #divide to get second index
          sec_index <- Hmisc::ceil(par_tile_indices[[i]] / nclust)[parentIndex]
          
          polygon_info[[i]][[parentIndex]] <- Add_boundary_points(polygon_info[[i]][[parentIndex]], 
                                                                  polygon_info[[(i - 1)]][[which(par_tile_indices[[(i - 1)]] == sec_index)]][[last_index]],
                                                                  new_rawdeldata[[i]][[parentIndex]])
        }
        # flog.info("Vertices of parent tile are added to appropriate child tile")
      }
      
      polinfo <- polygon_info
      
      fin_out <- list()
      
      fin_out[[1]] <- deldat1
      fin_out[[2]] <- polinfo
      fin_out[[3]] <- hvq_k
      fin_out[[3]][['scale_summary']] <- scale_summary
      
      return(fin_out)
    }
  }
