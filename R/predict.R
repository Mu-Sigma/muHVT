#' @export predict
predict <- function(data,hvt.results){
  
  requireNamespace("dplyr")
  requireNamespace("purrr")
  requireNamespace("gtools")
  requireNamespace("data.tree")
  
  
  summary_list <- hvt.results[[3]]
  nclust <- length(summary_list[['plt.clust']][[2]])
  
  train_colnames <- names(summary_list$scale_summary$mean_data)
  
  if(!identical(train_colnames,colnames(data))){
    stop("Colnames of train and test data don't match")
  }
  
  if(!is.null(summary_list$scale_summary)){
    data <- scale(data,center = summary_list$scale_summary$mean_data,scale = summary_list$scale_summary$std_data)
  }
  
  level <- length(summary_list$nodes.clust)
  
  hierachy_structure <- c(1:level) %>% purrr::map(~data.frame(gtools::permutations(n = nclust,r = .x,v = seq(1:nclust),repeats.allowed = T))) %>% purrr::map(~apply(.,1,function(x) paste(x,collapse=''))) %>% unlist()
  
  pathString <- hierachy_structure %>% purrr::map_chr(~paste(c('cluster',unlist(strsplit(.,''))),collapse=' -> '))
  
  summary_table_with_hierarchy <- cbind(summary_list$summary,pathString,index=as.numeric(rownames(summary_list$summary)),stringsAsFactors = FALSE)
  
  #tree_summary <- data.tree::as.Node(summary_table_with_hierarchy)
  path_list <- list()
  
  find_path <- function(summary,data,nclust,final_level,init_level,init_row,path_list){
    
    if(init_level > final_level){
      return(path_list)
    }
    else{
      intermediate_df <- summary %>% filter(Segment.Level==init_level & index>=init_row & index<=init_row+nclust-1)
      min_dist <- apply(intermediate_df[,train_colnames],1,FUN = function(x,y) dist(rbind(x,y)),data)
      if(all(is.na(min_dist))){
        return(path_list)
      }
      index_min_row <- as.numeric(intermediate_df[which.min(min_dist),"index"])
      path_list <- summary[index_min_row,"pathString"]
      next_row_no <- index_min_row*nclust + 1
      find_path(summary,data,nclust,final_level,init_level +1,init_row = next_row_no,path_list)
    }
  }
  
  path_list <- apply(data, 1,FUN = function(data) find_path(summary_table_with_hierarchy,data = data,nclust = nclust ,final_level = level,init_level = 1,init_row = 1,path_list = path_list))
  
  return(data.frame(path_list))

}