

diagSuggestion <- function(hvt.results,
                  data,
                  level,
                  quant.err,
                  distance_metric="L1_Norm",
                  error_metric="max",
                  ...){
  
  requireNamespace("reshape2")     
  requireNamespace("dplyr")     

  ##### Min Inter-Centroid distance plot
  # browser()
  newdfMapping <- hvt.results[[3]][["summary"]]
  
  x <- newdfMapping %>% 
    dplyr::filter((n > 0 & Segment.Level == level) | (Segment.Level < level & (Quant.Error < quant.err | n <= 3)))
  x <- x[x%>%complete.cases(),]
  
  singleton_count=sum(x$Quant.Error< 0.0001)
  num_cells=length(x$Quant.Error)
  
  # x <- hvt.results[[3]][["summary"]]
  d  <- stats::dist(x,method = "manhattan")
  df <- reshape2::melt(as.matrix(d), varnames = c("row", "col"))
  df <- df[df$value!=0,]
  df$value <- df$value/ncol(x)
  df_cent <- df %>% dplyr::group_by(row) %>% dplyr::summarise(min_dist = min(value, na.rm = T))
  
  
  mean_cent_train=mean(df_cent$min_dist)
  
  ##### Min Inter-Point distance plot
  
  # browser()
  # x=data
  d = stats::dist(data,method = "manhattan")
  df <- reshape2::melt(as.matrix(d), varnames = c("row", "col"))
  df=df[df$value!=0,]
  df$value=df$value/ncol(data)
  df_data = df %>% dplyr::group_by(row) %>% dplyr::summarise(min_dist = min(value, na.rm = T))
  
  mean_dist_train=mean(df_data$min_dist)

  
  diag_list=list(
    mean_centroid_train = mean_cent_train,
    mean_distance_train = mean_dist_train
  )
  return(diag_list)
  
} 











