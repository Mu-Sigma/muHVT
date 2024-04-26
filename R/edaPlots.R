#' @name edaPlots
#' @title plots for understanding dataset
#' @description main function that gives all the eda plots 
#' @param df Dataframe. The input dataset, can be a time series too.
#' @param time_series Logical. A value to indicate whether the dataset is time_series or not.
#' @param time_column Character. The name of the time column in the dataset.
#' @return Five objects which includes time series plots, data distribution plots, 
#' box plots, correlation matrix plot and a descriptive statistics table.
#' @author Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords EDA
#' @examples
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#' DAX = EuStockMarkets[, "DAX"],
#' SMI = EuStockMarkets[, "SMI"],
#' CAC = EuStockMarkets[, "CAC"],
#' FTSE = EuStockMarkets[, "FTSE"])
#' edaPlots(dataset, time_series = TRUE, time_column = 'date')
#' @export edaPlots



edaPlots <- function(df, time_series = FALSE, time_column) {

###########Summary EDA Function
  summary_eda <- function(df) {
    variable  <-  `1st Quartile`<-median<- sd<-`3rd Quartile`<-  hist<- n_row<- n_missing<- NULL
    type <-complete_rate <-p0 <-p25 <-p50 <-p75<-p100<-NULL
    numeric_columns <- sapply(df, is.numeric)
    missing_counts <- sapply(seq_along(numeric_columns)[numeric_columns], function(i) sum(is.na(df[[i]])))
    missing_counts_vector <- unlist(missing_counts)
    
    result <- df %>%
      select(where(is.numeric)) %>%
      skimr::skim() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      rename_with(~sub("^(skim_|numeric\\.)", "", .)) %>%
      select(-type, -n_missing, -complete_rate) %>%
      mutate(n_row = format(nrow(df), scientific = FALSE), n_missing = missing_counts_vector) %>%
      dplyr::rename(min = p0, `1st Quartile` = p25, median = p50, `3rd Quartile` = p75, max = p100) %>%
      dplyr::select(variable, min, `1st Quartile`, median, mean, sd, `3rd Quartile`, max, hist,n_row, n_missing)
    
    
    # Generate the kable table with options using knitr's kable and kableExtra's styling functions
    eda_format <- knitr::kable(result, "html", escape = FALSE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"))
        # caption = "EDA summary Table", font_size = 14, position = "center") 

    
    return(eda_format)
 }  
 
#############Histogram   
  generateDistributionPlot <- function(data, column) {
    name  <-  NULL
   
    p1<- ggplot2::ggplot(data, ggplot2::aes(x = data[[column]])) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..count..), fill = "midnightblue", size = 0.2, alpha=0.7) +
      ggplot2::xlab(column) + ggplot2::ylab("Count") +
      ggplot2::labs(column) +ggplot2::theme_bw() + 
      ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),legend.position = c(0.8, 0.8))
    
    p2<-ggplot2::ggplot(data, ggplot2::aes(x = data[[column]])) +
      ggplot2::stat_function(ggplot2::aes(color = "Normal"), 
      fun = stats::dnorm,args = list(mean = mean(data[[column]]),sd = stats::sd(data[[column]]))) +
      ggplot2::stat_density(ggplot2::aes(color = "Density"), geom = "line", linetype = "dashed")  +
      ggplot2::scale_colour_manual("", values = c("black", "orange")) +
      ggplot2::scale_linetype_manual("", values = c("Normal" = 2, "Density" = 1))  +
      ggplot2::guides(
        fill = ggplot2::guide_legend(keywidth = 1, keyheight = 1),
        linetype = ggplot2::guide_legend(keywidth = 3, keyheight = 1),
        colour = ggplot2::guide_legend(keywidth = 3, keyheight = 1)) + 
      ggplot2::ylab("Density") +
      ggplot2::xlab("Density") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(colour="black", size=8),
        axis.text.x = ggplot2::element_text(size = 14),
        axis.ticks = ggplot2::element_line(colour = 'gray50'),
        axis.ticks.x = ggplot2::element_line(colour = "black"))
    
    g1 <- ggplot2::ggplotGrob(p1)
    g2 <- ggplot2::ggplotGrob(p2)
    
   # pp <- c(subset(g1$layout, name == "panel", se = t:r))
    pp <- c(subset(g1$layout, name == "panel"))
    
    # superimpose p2 (the panel) on p1
    g <- gtable::gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                                 pp$l, pp$b, pp$l)
    
    # extract the y-axis of p2
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    
    # flip it horizontally
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    
    # add the flipped y-axis to the right
    g <- gtable::gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable::gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    distHistogram <- g
    return(distHistogram)
  }
  
################Box Plots Function
  quantile_outlier_plots_fn <- function(data, outlier_check_var, data_cat = data[, cat_cols], numeric_cols = numeric_cols){
    CheckColumnType <- function(data) {
      if (is.numeric(data) || is.integer(data)) {
      #if (class(data) == "integer" || class(data) == "numeric") {
        columnType <- "numeric"
      } else { columnType <- "character" }
      return(columnType)}
    cat_cols <-
      colnames(data)[unlist(sapply(
        data,FUN = function(x) {CheckColumnType(x) == "character" ||CheckColumnType(x) == "factor" }))]
    
    lower_threshold <- stats::quantile(data[, outlier_check_var], .25,na.rm = TRUE) - 1.5*(stats::IQR(data[, outlier_check_var], na.rm = TRUE))
    upper_threshold <- stats::quantile(data[,outlier_check_var],.75,na.rm = TRUE) +
      1.5*(stats::IQR(data[,outlier_check_var],na.rm = TRUE))
    data$QuantileOutlier <- data[,outlier_check_var] > upper_threshold | data[,outlier_check_var] < lower_threshold
    
    quantile_outlier_plot <- ggplot2::ggplot(data, ggplot2::aes(x="", y = data[,outlier_check_var])) +
      ggplot2::geom_boxplot(fill = 'midnightblue',alpha=0.7) + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),
                     panel.grid.minor.x=ggplot2::element_blank(),
                     panel.grid.major.x=ggplot2::element_blank(),legend.position = "bottom")                          +ggplot2::ylab(outlier_check_var) + ggplot2::xlab("")
    data <- cbind(data[, !names(data) %in% c("QuantileOutlier")] %>% round(2), outlier = data[, c("QuantileOutlier")])
    data <- cbind(data, data_cat)  
    return(list(quantile_outlier_plot, data, lower_threshold, upper_threshold))
  }

############Correlation Plot Function
  correlation_plot <- function(df, title = "Feature Correlation Analysis") {
    corrplot::corrplot(stats::cor(df, use = "complete.obs"), main = list(title , font = 1, cex = 1),method = "color", 
                       outline = TRUE, addgrid.col = "darkgray", addrect = 4,
                       rect.col = "black", rect.lwd = 5, cl.pos = "b", tl.col = "black", tl.cex = 1, 
                       cl.cex = 1,addCoef.col = "black", number.digits = 2, number.cex = 1.5, 
                       type = "lower",col = grDevices::colorRampPalette(c("maroon", "white", "midnightblue"))(200), mar=c(0,0,2,0))  
  }

#####################timeseries plots
  generateTimeseriesPlot <- function(df, time_column) {
          if (!is.data.frame(df)) {
                 stop("Input dataset must be a data frame")
             }
        
           # Check if the time_column_name exists in the dataset
           if (!(time_column %in% names(df))) {
                 stop("Specified time column does not exist in the dataset")
             }
         
           # Order the dataset based on the time column
           ordered_dataset <- df[order(df[[time_column]]), ]
           
           # Extract variable names
            variable_names <- setdiff(names(df), time_column)
             
          # Create plots using lapply
               plot_list <- lapply(variable_names, function(variable_name) {
                     ggplot(data = ordered_dataset, aes_string(x = time_column, y = variable_name)) +
                           geom_line(color = "midnightblue") +
                           labs(x = time_column, y = variable_name) 
                        
                   })
              
                 # Add a separate plot for the time variable
                time_plot <- ggplot(data = ordered_dataset, aes_string(x = time_column, y = time_column)) +
                  geom_line(color = "midnightblue") +
                       labs(x = time_column, y = time_column) 
               
                   plot_list <- c(plot_list, list(time_plot))  # Add the time plot to the list of plots
                   
                     # Arrange plots using grid.arrange
                   gridExtra::grid.arrange(grobs = plot_list, ncol = 2, top = "Time series plots of Features")
  }
  
  
if (time_series == TRUE && (!is.null(time_column)) ){

    df <- df[order(df[[time_column]]), ]
    
    # Execute Time series Plots
    generateTimeseriesPlot(df,time_column)
    
  
    # Execute Distribution Plots
    eda_cols <- names(df)[sapply(df, is.numeric)]
    dist_list <- lapply(eda_cols, function(column) {
      generateDistributionPlot(df, column)
    }) 
    do.call(gridExtra::grid.arrange, args = list(grobs = dist_list, ncol = 2, top = "Distribution of Features"))
    # Execute Box Plots for Outliers
    box_plots <- lapply(eda_cols, function(column) {
      quantile_outlier_plots_fn(df, outlier_check_var = column)[[1]]
    })
    do.call(gridExtra::grid.arrange, c(grobs = box_plots, ncol = 3, top = "Boxplot Visualization"))
    
    # Execute Correlation Plot
    correlation_plot(df,title = "Features Correlation Visualization")
    
    # Execute Summary EDA
    eda_table <- summary_eda(df)
    return(eda_table)
    
    
  }  else {
  
    # Execute Distribution Plots
  eda_cols <- names(df)[sapply(df, is.numeric)]
  dist_list <- lapply(eda_cols, function(column) {
    generateDistributionPlot(df, column)
  }) 
  do.call(gridExtra::grid.arrange, args = list(grobs = dist_list, ncol = 2, top = "Distribution of Features"))
  # Execute Box Plots for Outliers
  box_plots <- lapply(eda_cols, function(column) {
    quantile_outlier_plots_fn(df, outlier_check_var = column)[[1]]
  })
  do.call(gridExtra::grid.arrange, c(grobs = box_plots, ncol = 3, top = "Boxplot Visualization"))

  # Execute Correlation Plot
   correlation_plot(df,title = "Features Correlation Visualization")
  
   # Execute Summary EDA
   eda_table <- summary_eda(df)
   return(eda_table)
  } 
   
}

