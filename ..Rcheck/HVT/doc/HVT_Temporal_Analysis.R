## ---- warning=FALSE,message=FALSE,include = FALSE-----------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "672px",
  out.height = "480px",
  fig.width = 7.5,
  fig.height = 5,
  fig.align = "center",
  fig.retina = 1,
  dpi = 150
)

# installing all required packages
list.of.packages <- c("dplyr", "kableExtra", "plotly", "purrr", "sp", "data.table", "gridExtra","plyr", "grid", "ggforce", "reactable", "markovchain", "reshape", "gganimate", "gapminder", "tidyr", "stringr", "DT", "knitr", "feather")

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE, verbose = FALSE, repos='https://cloud.r-project.org/')

# Loading the required libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
options(expressions = 10000)
global_var <- nzchar(Sys.getenv("RUN_VIGNETTE"))
global_var <- TRUE
scrolLimit <- function(noOfRows){
  if(noOfRows<10){
    swe = paste(as.character(noOfRows*50),"px")
  }
  else{
    swe = "400px"
   }
  return(swe)
}
Table <- function(data,scroll = F, limit = NULL){
  if(!is.null(limit)){
    data <- head(data,limit)}
kable_table <- data %>% kable(escape = F,align = "c") %>% kable_styling(bootstrap_options = c("striped", "hover", "responsive"))
if(scroll == T){
kable_table <- kable_table %>% scroll_box(width = "100%", height = scrolLimit(nrow(data)))
  }
return(kable_table)
}

set.seed(240)

## -----------------------------------------------------------------------------
###direct installation###
#install.packages("HVT")

#or

###git repo installation###
#library(devtools)
#devtools::install_github(repo = "Mu-Sigma/HVT")


## ---- loading all the script files of the package, message=FALSE, warning=FALSE, include = TRUE----
# Sourcing required code scripts for HVT
script_dir <- "../R"
r_files <- list.files(script_dir, pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, function(file) { source(file, echo = FALSE); }))

## ----warning=FALSE,message=FALSE----------------------------------------------
file_path <- system.file("extdata", "lorenz_attractor.feather", package = "HVT")
dataset <- read_feather(file_path) %>% as.data.frame()
dataset <- dataset %>% select(X,Y,Z,U,t)
dataset$t <- round(dataset$t, 5)
Table(dataset, limit = 10)

## ---- eval=TRUE,fig.cap='Figure 1: Lorenz attractor in 3D space'--------------
data_3d <- dataset[sample(1:nrow(dataset), 1000), ]
plot_ly(data_3d, x= ~X, y= ~Y, z = ~Z) %>% add_markers( marker = list(
                          size = 2,
                          symbol = "circle",
                          color = ~Z,
                          colorscale = "Bluered",
                          colorbar = (list(title = 'Z'))))

## ---- message=FALSE, warning=FALSE--------------------------------------------
str(dataset)

## ---- warning=FALSE,message=FALSE---------------------------------------------
edaPlots(dataset, time_series = TRUE, time_column = "t")

## -----------------------------------------------------------------------------
noOfPoints <- dim(dataset)[1]
trainLength <- as.integer(noOfPoints * 0.8)
trainDataset <- dataset[1:trainLength,]
testDataset <- dataset[(trainLength+1):noOfPoints,]
rownames(testDataset) <- NULL

## ----warning=FALSE,message=FALSE----------------------------------------------
Table(trainDataset, limit = 10)

## ----train structure, warning=FALSE, eval = global_var------------------------
str(trainDataset)

## ---- train distribution, warning=FALSE,message=FALSE-------------------------
edaPlots(trainDataset, time_series = TRUE, time_column = 't')

## ---- warning=FALSE,message=FALSE---------------------------------------------
Table(testDataset, limit = 10)

## ----test structure, warning=FALSE, eval = global_var-------------------------
str(testDataset)

## ---- test distribution, warning=FALSE,message=FALSE--------------------------
edaPlots(testDataset, time_series = TRUE, time_column = 't')

## ----warning=FALSE,fig.show='hold',results='hide',message=FALSE,eval = global_var----
set.seed(240)
hvt.results <- trainHVT(
  trainDataset[,-c(4:5)],
  n_cells = 100,
  depth = 1,
  quant.err = 0.1,
  normalize = TRUE,
  distance_metric = "L1_Norm",
  error_metric = "max",
  quant_method = "kmeans"
)


## ----compression summary torus first,warning=FALSE,eval = global_var----------
displayTable(data = hvt.results[[3]]$compression_summary,
             columnName = 'percentOfCellsBelowQuantizationErrorThreshold', 
             value = 0.8, tableType = "compression")

## -----------------------------------------------------------------------------
hvt.results$model_info$input_parameters

## ---- warning=FALSE,message=FALSE,fig.cap='Figure 2: The Voronoi tessellation for layer 1 shown for the 100 cells in the dataset ’Lorenz attractor’', fig.align = "center", fig.show ='hold', include=TRUE, out.width='90%', results='asis'----
plotHVT(
  hvt.results,
  centroid.size = c(0.6),
  plot.type = '2Dhvt',
  cell_id = FALSE)


## ---- warning=FALSE,message=FALSE,fig.cap='Figure 3: The Voronoi tessellation for layer 1 shown for the 100 cells in the dataset ’Lorenz attractor’ with Cell ID', fig.align = "center", fig.show ='hold', include=TRUE, out.width='90%', results='asis'----
plotHVT(
  hvt.results,
  centroid.size = c(0.6),
  plot.type = '2Dhvt',
  cell_id = TRUE)


## ----scoreHVT function,warning=FALSE,message=FALSE,eval = global_var----------
set.seed(240)
scoring <- scoreHVT(dataset,
                    hvt.results,
                    child.level = 1)

## ----scoreHVT,warning=FALSE,eval = global_var---------------------------------
displayTable(data =scoring$scoredPredictedData, columnName= 'Quant.Error', 
             value = 0.1, tableType = "summary", limit =100)

## -----------------------------------------------------------------------------
scoring$model_info$scored_model_summary

## -----------------------------------------------------------------------------
temporal_data <- cbind(scoring$scoredPredictedData, dataset[,c(4,5)]) %>% select(Cell.ID,t)

## ----plotStateTransition function, echo=TRUE, eval=FALSE----------------------
#  plotStateTransition(
#         df,
#         sample_size,
#         line_plot,
#         cellid_column,
#         time_column)

## ----state_plot1, results='asis', warning=FALSE,message=FALSE-----------------
plotStateTransition(df = temporal_data, 
                    cellid_column = "Cell.ID", 
                    time_column = "t",
                    sample_size = 0.2)

## ----state_plot2, results='asis', warning=FALSE,message=FALSE-----------------
plotStateTransition(df = temporal_data, 
                    cellid_column = "Cell.ID", 
                    time_column = "t",
                    sample_size = 1)

## ----getTransitionProbability,eval=FALSE--------------------------------------
#  getTransitionProbability(
#          df,
#          cellid_column,
#          time_column)
#  

## ---- message=FALSE-----------------------------------------------------------
trans_table <- getTransitionProbability(df = temporal_data, 
                                        cellid_column = "Cell.ID", 
                                        time_column = "t")

## -----------------------------------------------------------------------------
combined_df <- do.call(rbind, trans_table)
Table(combined_df, limit = 10)

## ----reconcileTransitionProbability, eval=FALSE-------------------------------
#  reconcileTransitionProbability(
#                  df,
#                  hmap_type = "All",
#                  cellid_column,
#                  time_column)

## ---- warning=FALSE, message=FALSE--------------------------------------------
reconcile_plots <- reconcileTransitionProbability(df = temporal_data, 
                                                  hmap_type = "All", 
                                                  cellid_column = "Cell.ID",
                                                  time_column = "t")

## ----hmap1, fig.align = "center", fig.show ='hold', message=FALSE, out.height='100%', out.width='100%', results='asis', warning=FALSE----
reconcile_plots[[1]]

## ----hmap2, fig.align = "center", fig.show ='hold', message=FALSE, out.width='70%', results='asis', warning=FALSE----
Table(reconcile_plots[[2]], scroll = TRUE)

## ----hmap3, fig.align = "center", fig.show ='hold', message=FALSE, out.width='70%', results='asis', warning=FALSE----
reconcile_plots[[3]]

## ----hmap4, fig.align = "center", fig.show ='hold', message=FALSE, out.width='70%', results='asis', warning=FALSE----
Table(reconcile_plots[[4]], scroll = TRUE)

## ----plotAnimatedFlowmap, eval=FALSE------------------------------------------
#  plotAnimatedFlowmap(
#           hvt_model_output,
#           transition_probability_df,
#           df,
#           animation = "All",
#           flow_map = "All",
#           fps_state,
#           fps_time,
#           time_duration,
#           state_duration,
#           cellid_column,
#           time_column )
#  

## ----flow_map_fn, warning=FALSE,message=FALSE---------------------------------
flowmap_plots <- plotAnimatedFlowmap(hvt_model_output = hvt.results,
                                     transition_probability_df =trans_table,
                                     df = temporal_data, 
                                     animation ='All' , flow_map = 'All',
                                     fps_time = 30, fps_state =  5, 
                                     time_duration = 180,state_duration = 20,
                                     cellid_column = "Cell.ID", time_column = "t")

## ----dot_flow_map, fig.align = "center", fig.show ='hold', message=FALSE, out.width='100%', out.height='70%', results='asis', warning=FALSE----
flowmap_plots[[1]]

## ----arrow_map, fig.align = "center", fig.show ='hold', message=FALSE, out.width='100%',out.height='70%', results='asis', warning=FALSE----
flowmap_plots[[2]]

## ----dot_anime, fig.align = "center", fig.show ='hold', message=FALSE, out.width='100%',out.height='70%', results='asis', warning=FALSE----
flowmap_plots[[3]] 

## ----arrow_anime, fig.align = "center", fig.show ='hold', message=FALSE, out.width='100%',out.height='70%', results='asis', warning=FALSE----
flowmap_plots[[4]]

