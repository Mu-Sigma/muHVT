#' predictHVT
#' 
#' Predict which cell and what level each point in the test dataset belongs to
#' 
#' 
#' @param data List. A dataframe containing test dataset. The dataframe should have atleast one variable used while training. The variables from
#' this dataset can also be used to overlay as heatmap
#' @param hvt.results A list of hvt.results obtained from HVT function while performing hierarchical vector quantization on train data
#' @param hmap.cols - The column number of column name from the dataset indicating the variables for which the heat map is to be plotted.(Default = #' NULL). A heatmap won’t be plotted if NULL is passed
#' @param child.level A number indicating the level for which the heat map is to be plotted.(Only used if hmap.cols is not NULL)
#' @param ...  color.vec and line.width can be passed from here
#' @author Meet K. Dave <dave.kirankumar@@mu-sigma.com>
#' @seealso \code{\link{HVT}} \cr \code{\link{hvtHmap}}
#' @keywords predict
#' @importFrom magrittr %>%
#' @examples
#' data(USArrests)
#' #Split in train and test
#' 
#' train <- USArrests[1:40,]
#' test <- USArrests[41:50,]
#' 
#' hvt.results <- list()
#' hvt.results <- HVT(train, nclust = 3, depth = 2, quant.err = 0.2, 
#'                   projection.scale = 10, normalize = TRUE)
#' 
#' predictions <- predictHVT(test,hvt.results,hmap.cols = NULL, child.level=2)
#' print(predictions$predictions)
#' @export predictHVT
predictHVT <- function(data,hvt.results,hmap.cols= NULL,child.level=1,...){
  
  requireNamespace("dplyr")
  requireNamespace("purrr")

  options(warn = -1)
  
  summary_list <- hvt.results[[3]]
  nclust <- length(summary_list[['plt.clust']][[2]])
  
  train_colnames <- names(summary_list$scale_summary$mean_data)
  
  if(!all(train_colnames %in% colnames(data))){
    if(any(train_colnames %in% colnames(data))){
      train_colnames <- train_colnames[train_colnames %in% colnames(data)]
    }
    else{
      stop('test columns not part of train dataset')
    }
  }
  
  if(!is.null(summary_list$scale_summary)){
    scaled_test_data <- scale(data[,train_colnames],center = summary_list$scale_summary$mean_data[train_colnames],scale = summary_list$scale_summary$std_data[train_colnames])
  }
  
  colnames(scaled_test_data) <- train_colnames
  
  level <- length(summary_list$nodes.clust)
  
  # hierachy_structure <- c(1:level) %>% purrr::map(~data.frame(gtools::permutations(n = nclust,r = .x,v = seq(1:nclust),repeats.allowed = T))) %>% purrr::map(~apply(.,1,function(x) paste(x,collapse=''))) %>% unlist()
  # 
  # pathString <- hierachy_structure %>% purrr::map_chr(~paste(c('cluster',unlist(strsplit(.,''))),collapse=' -> '))
  
  
  hierarchy_structure <- function(nclust,init,depth,temp_list,final_list,sep = "->"){

    empty_list = c()
    
    if(init>depth){
      return(final_list)
    }
    
    if(length(final_list)==0){
      empty_list <- seq(1:nclust)
      final_list <- empty_list
    }
    else{
      for(i in temp_list){
        for(j in 1:nclust){
          final_list <- c(final_list,paste(i,j,sep = sep))
          empty_list <- c(empty_list,paste(i,j,sep = sep))
        }
      }
    }
    hierarchy_structure(nclust= nclust, init+1,depth= depth,temp_list = empty_list,final_list = final_list ,sep = sep)
    
  }
  
  pathString <- hierarchy_structure(nclust = nclust,init = 1,depth = level,temp_list = list(),final_list  = list(),sep = "->")
  
  summary_table_with_hierarchy <- cbind(summary_list$summary,pathString,index=as.numeric(rownames(summary_list$summary)),stringsAsFactors = FALSE)
  
  #tree_summary <- data.tree::as.Node(summary_table_with_hierarchy)
  path_list <- list()
  
  find_path <- function(summary,data,nclust,final_level,init_level,init_row,path_list){
    
    if(init_level > final_level){
      return(path_list)
    }
    else{
      intermediate_df <- summary %>% dplyr::filter(Segment.Level==init_level & index>=init_row & index<=init_row+nclust-1)
      min_dist <- apply(intermediate_df[,train_colnames,drop=F],1,FUN = function(x,y) stats::dist(rbind(x,y)),data)
      if(all(is.na(min_dist))){
        return(path_list)
      }
      index_min_row <- as.numeric(intermediate_df[which.min(min_dist),"index"])
      path_list <- summary[index_min_row,"pathString"]
      next_row_no <- index_min_row*nclust + 1
      find_path(summary,data,nclust,final_level,init_level +1,init_row = next_row_no,path_list)
    }
  }
  
  path_list <- apply(scaled_test_data, 1,FUN = function(data) find_path(summary_table_with_hierarchy,data = data,nclust = nclust ,final_level = level,init_level = 1,init_row = 1,path_list = path_list))
  
  path_df <- data.frame(path_list)
  
  output_path_with_data <- cbind(data[,train_colnames],path_df)
  colnames(output_path_with_data) <- c(train_colnames,"cell_path")
  
  ### Heatmap
  if (!is.null(hmap.cols)) {
    if (class(hmap.cols) == "character") {
      column_no_for_hmap = which(colnames(data) == hmap.cols)
      if (length(hmap.cols) == 0) {
        stop("Column name for plotting heatmap incorrect")
      }
    }
    else if (class(hmap.cols) == "numeric") {
      column_no_for_hmap = hmap.cols
      
      if (length(column_no_for_hmap) == 0 ||
          column_no_for_hmap > length(colnames(data))) {
        stop("Column number for plotting heatmap incorrect")
      }
      
    }
  }
  else{
    return(list(predictions = output_path_with_data, predictPlot = NULL))
  }
  
  df_with_hmapcol_path <- cbind(data[,column_no_for_hmap,drop=F],path_df)
  
  hmap_colname <- colnames(df_with_hmapcol_path)[1]
  
  test_hmap_colname <- paste0(hmap_colname,"_test")
  
  colnames(df_with_hmapcol_path)[1] <- test_hmap_colname
  
  mean_of_hmap_per_cluster <- df_with_hmapcol_path %>% dplyr::group_by(path_list) %>% dplyr::summarise_at(test_hmap_colname,dplyr::funs(mean))
  
  summary_table_with_hierarchy <- dplyr::left_join(summary_table_with_hierarchy,mean_of_hmap_per_cluster,by=c("pathString"="path_list"))
  
  hvt.results[[3]][['summary']] <- summary_table_with_hierarchy
  
  line_color_func <- function(child.level) {
    switch (
      child.level,
      list(
        line.width = c(0.8),
        color.vec = c('#326273')
      ),
      list(
        line.width = c(0.8, 0.5),
        color.vec = c('#326273', '#ADB2D3')
      ),
      list(
        line.width = c(0.8, 0.5, 0.3),
        color.vec = c("#326273", "#ADB2D3", "#BFA89E")
      ),
      list(
        line.width = c(0.8, 0.5, 0.3, 0.1),
        color.vec = c("#326273", "#ADB2D3", "#BFA89E", "#CFC7D2")
      ),
    )
  }
  
  if(length(list(...))){
    input_col_line <- list(...)
    color.vec <- input_col_line$color.vec
    line.width <-  input_col_line$line.width
  }
  else{
    input_col_line <- line_color_func(child.level)
    if(is.null(input_col_line)){
      stop('Please pass color.vec and line.width to the predict function')
    }
     color.vec <- input_col_line$color.vec
     line.width <-  input_col_line$line.width
  }
  
plot_gg_test <- hvtHmap(hvt.results,data,hmap.cols = test_hmap_colname,child.level = child.level,line.width = line.width,color.vec = color.vec,palette.color = 6,test=T)
  
  print(plot_gg_test)
  
  return(list(predictions = output_path_with_data, predictPlot = plot_gg_test))
  
  

}