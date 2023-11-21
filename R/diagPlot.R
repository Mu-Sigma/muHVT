#' Diagnosis Plot
#'
#' Function to construct Diagnosis Plots for HVT Model
#'
#' This function creates Diagnosis Plots for HVT Model. The output of the functions contains a Minimum Intra-Centroid distance plot, a Minimum Intra-DataPoint Distance Plot,
#' Distribution of number of cells,  a Minimum Intra-DataPoint Distance Plot,
#' Distribution of number of cells and count of singletons(segments with single observation)
#' 
#' @param hvt.results A list of hvt.results obtained from HVT function while performing hierarchical vector quantization on train data
#' @param data Data frame. A data frame with different columns is given as input. The dataframe should be the same dataframe used to train the HVT Model
#' @param level Numeric. Indicating the level for which the heat map is
#' to be plotted.
#' @param quant.err Numeric. A number indicating the quantization error threshold.
#' @param ... The ellipsis is passed to it as additional argument. (Used internally)
#' @return A list that contains  a Minimum Inter-Centroid distance plot, a Minimum Intra-DataPoint Distance Plot,
#' Distribution of number of cells and count of singletons(segments with single observation)
#' \item{cent_plot }{Plot. a Minimum Intra-Centroid distance plot}
#'  \item{datapoint_plot }{Plot. a Minimum Intra-Datapoints distance plot} 
#'  \item{number_plot }{Plot. a Distribution of number of cells} 
#' \item{singleton_count}{Numeric. Count of singletons(segments with single observation)}

#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @seealso \code{\link{predictHVT}}
#' @keywords hplot
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import dplyr
#' @examples
#' data(USArrests)
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, n_cells = 15, depth = 1, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE,
#'                    quant_method="kmeans",diagnose=TRUE)
#'diagPlot(hvt.results = hvt.results,
#'data = USArrests,
#'level = 1,
#'quant.err = 0.2)
#'
#' @export diagPlot
#' @keywords internal

diagPlot <- function(hvt.results,
                  data,
                  level,
                  quant.err,
                  distance_metric="L1_Norm",
                  error_metric="max",
                  ...){
  # browser()
  
  requireNamespace("reshape2")     
  requireNamespace("ggplot2")     
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
  df_cent <- df %>% dplyr::group_by(row) %>% dplyr::summarise(min_dist = min(value, na.rm = TRUE))
  
  
  # p_cent <- ggplot() +
  #               aes(x = df_cent$min_dist) +
  #               geom_histogram(fill = "#112446") +
  #               theme_minimal()+
  #               geom_vline(xintercept = mean(df_cent$min_dist), colour="red",linetype="dashed")+
  #               ggtitle("Minimum Intra-Centroid Distance Plot")+
  #   ggplot2::xlab("Distance between Centroids")+
  #   ggplot2::ylab("Number of Occurrences")+
  #   annotate("text",x = mean(df_cent$min_dist),
  #            y = Inf,vjust=1,hjust=-0.5,
  #            label = mean(df_cent$min_dist)%>%round(2),fontface = "bold") 
  
  p_cent <- ggplot2::ggplot(df_cent,aes(x = min_dist)) +
    ggplot2::geom_histogram(ggplot2::aes(y = stat(density)),fill = "midnightblue",colour="white",alpha=0.75) +
    
    ggplot2::stat_function(ggplot2::aes(color = "Normal"), fun = stats::dnorm,args = list(mean = mean(df_cent$min_dist),sd = stats::sd(df_cent$min_dist))) +
    ggplot2::stat_density(ggplot2::aes(color = "Density"), geom = "line", linetype = "dashed")  +
    ggplot2::scale_colour_manual("", values = c("dodgerblue", "#EE7600")) +
    ggplot2::scale_linetype_manual("", values = c("Normal" = 2, "Density" = 1)) +
    
    ggplot2::theme_minimal()+
    ggplot2::geom_vline(xintercept = mean(df_cent$min_dist), colour="#EE7600",linetype="dashed")+
    ggplot2::ggtitle("Minimum Intra-Centroid Distance Plot")+
    ggplot2::xlab("Distance between Centroids")+
    ggplot2::ylab("Density")+
    ggplot2::annotate("text",x = mean(df_cent$min_dist),
             y = Inf,vjust=1,hjust=-0.5,
             label = mean(df_cent$min_dist)%>%round(2),fontface = "bold")
  
  
  ##### Min Inter-Point distance plot
  
  # browser()
  # x=data
  d = stats::dist(data,method = "manhattan")
  
  df <- reshape2::melt(as.matrix(d), varnames = c("row", "col"))
  df=df[df$value!=0,]
  df$value=df$value/ncol(data)
  df_data = df %>% dplyr::group_by(row) %>% dplyr::summarise(min_dist = min(value, na.rm = TRUE))
  
  # p_datapoint = ggplot() +
  #               aes(x = df_data$min_dist) +
  #               geom_histogram(fill = "#112446") +
  #               theme_minimal()+
  #               geom_vline(xintercept = mean(df_data$min_dist), colour="red",linetype="dashed")+
  #               ggtitle("Minimum Intra-DataPoint Distance Plot")+
  #               ggplot2::xlab("Distance between DataPoints")+
  #               ggplot2::ylab("Number of Occurrences")+
  #               annotate("text",x = mean(df_data$min_dist),
  #                        y = Inf,vjust=1,hjust=-0.5,
  #                        label = mean(df_data$min_dist)%>%round(2),fontface = "bold") 
  # 
  
  
  p_datapoint = ggplot2::ggplot(df_data,aes(x = min_dist)) +
    
    ggplot2::geom_histogram(ggplot2::aes(y = stat(density)),fill = "midnightblue",colour="white",alpha=0.75) +
    
    ggplot2::stat_function(ggplot2::aes(color = "Normal"), fun = stats::dnorm,args = list(mean = mean(df_data$min_dist),sd = stats::sd(df_data$min_dist))) +
    ggplot2::stat_density(ggplot2::aes(color = "Density"), geom = "line", linetype = "dashed")  +
    ggplot2::scale_colour_manual("", values = c("dodgerblue", "#EE7600")) +
    ggplot2::scale_linetype_manual("", values = c("Normal" = 2, "Density" = 1)) +
    
    ggplot2::theme_minimal()+
    ggplot2::geom_vline(xintercept = mean(df_data$min_dist), colour="#EE7600",linetype="dashed")+
    ggtitle("Minimum Intra-DataPoint Distance Plot")+
    ggplot2::xlab("Distance between DataPoints")+
    ggplot2::ylab("Density")+
    ggplot2::annotate("text",x = mean(df_data$min_dist),
             y = Inf,vjust=1,hjust=-0.5,
             label = mean(df_data$min_dist)%>%round(2),fontface = "bold") 
  
  # p_datapoint
  
  ####### Number of datapoints
  
  n_cells=x%>% dplyr::select("n") %>% unlist()
  p_obs=ggplot2::ggplot() +
    ggplot2::aes(x = n_cells) +
    ggplot2::geom_histogram(fill = "midnightblue") +
    ggplot2::theme_minimal()+
    ggplot2::geom_vline(xintercept = round(mean(n_cells),0), colour="red",linetype="dashed")+
    ggplot2::ggtitle("Distribution of number of observation in cells")+
    ggplot2::xlab("Number of Observation in Cells")+
    ggplot2::ylab("Number of Occurrences")+
    ggplot2::annotate("text",x =round(mean(n_cells),0),
             y = Inf,vjust=1,hjust=-0.5,
             label = round(mean(n_cells),0),fontface = "bold") 
  
  # MAD Calibration
  # browser()
  ####### MAD Plot ################
  predictions_train = list()
  predictions_train <- predictHVT(
    data = data,
    hvt.results.model=hvt.results,
    child.level = level,
    line.width = c(0.6, 0.4, 0.2),
    color.vec = c("#141B41", "#6369D1", "#D8D2E1"),
    mad.threshold = quant.err,
    distance_metric = distance_metric,
    error_metric = error_metric
  )
  
  mad_plot_train=madPlot(predictions_train)
  
  df_sing=data.frame(
    "Centroid_Type"=c("Singletons","Non-Singletons"),
    n=c(singleton_count,(num_cells-singleton_count))
  )%>% 
    dplyr::mutate(perc = `n` / sum(`n`)) %>% 
    dplyr::arrange(perc) %>%
    dplyr::mutate(labels = scales::percent(perc))
  
  
  sing_pie=ggplot2::ggplot(df_sing, aes(x = "", y = perc, fill = Centroid_Type)) +
    ggplot2::geom_col(color = "white") +
    ggplot2::geom_label(aes(label = labels),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE,colour="white",fontface = "bold") +
    ggplot2::guides(fill = guide_legend(title = "Centroid_Type")) +
    # scale_fill_viridis_d() +
    ggplot2::coord_polar(theta = "y") + 
    ggplot2::scale_fill_manual(values=c("#2C83B5","#EE6A42"))+
    ggplot2::theme_void()+ggplot2::labs(title = "Singletons Pie Chart: HVT Model | Train Data",
                      caption = paste("Total number of centroids:",num_cells,",Singletons:",singleton_count,",Non-Singletons:",(num_cells-singleton_count)))
  sing_pie

  
  plot_list=list(
    cent_plot = p_cent,
    datapoint_plot = p_datapoint,
    number_plot = p_obs,
    mad_plot_train=mad_plot_train,
    singleton_piechart=sing_pie,
    singleton_count=list(singleton_count=singleton_count,
                         num_cells=num_cells,
                         singleton_count_percentage=(singleton_count/num_cells)*100)
   

  )
  return(plot_list)
  
} 











