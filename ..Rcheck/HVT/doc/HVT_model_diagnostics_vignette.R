## ---- warning=FALSE, message=FALSE, include=FALSE, error=FALSE----------------

options=list(paging = TRUE,escape = FALSE,scrollX = TRUE,searching = FALSE,lengthChange = FALSE,dom = 'prlti',
             initComplete = DT::JS(
               "function(settings, json) {",
               "$(this.api().table().header()).css({'font-size': '14px','background-color': '#8b9194', 'color': '#fff'});","}"),
              columnDefs = list(list(
              className = 'dt-center', targets = "_all" )),
              autowidth = TRUE)
other_option <- list(
  escape = FALSE,
  scrollX = TRUE,
  paging = TRUE,
  pageLength = 10,
  searching = TRUE)
options(scipen=999)
table_no <- 0
fig_no <- 0
eq_no <- 0
ref_no <- 0
if(!require('devtools')){
  install.packages("devtools", repos = 'https://cloud.r-project.org/')
  library(devtools)
}else{
  library(devtools)
}
# for windows user
if (Sys.info()['sysname'] == 'Windows' && find_rtools() == FALSE){
  if(!require('installr')){
    install.packages("installr", repos = 'https://cloud.r-project.org/')
  }
  installr::install.URL(exe_URL = 'https://cloud.r-project.org/bin/windows/Rtools/Rtools35.exe')
}
library(knitr)
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      comment = NA)
row_keep=c("mean","std.dev","var","median","range","min","max","sum")

## ----lib, warning=FALSE, message=FALSE, include=FALSE, error=FALSE------------
list.of.packages <<- c("DT",'plotly', "magrittr", "data.table", "tidyverse", "crosstalk", "kableExtra", "gganimate","gdata", "data.table", "jmuOutlier", "viridis", "deldir", "conf.design", "splancs","Hmisc","xfun","sp",
"polyclip","devtools","deldir","gdata","tidyverse","skimr","patchwork", "crosstalk", "ggforce",
"SmartEDA", "scales", "ggplot2", "htmlwidgets", "gridExtra","gtable","tibble")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)){install.packages(new.packages, repos='https://cloud.r-project.org/')}
invisible(lapply(list.of.packages, library, character.only = TRUE))


## ----setup, warning = FALSE, include = FALSE,echo=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE,
                      collapse = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      comment = NA)


options=list(paging = TRUE,       # Paging of the data table
escape = FALSE, 
scrollX = TRUE,      # Horizontal scroll for the tables
searching = FALSE, # Search option at each column
lengthChange = FALSE,
      dom = 'prlti',
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'font-size': '14px',
                                                        'background-color': '#8b9194', 'color': '#fff'});",
        "}"
      ),
      columnDefs = list(list(
        className = 'dt-center', targets = "_all"
      )),
      autowidth = TRUE
    )


# installing all required packages
list.of.packages <- c("dplyr", "kableExtra", "geozoo", "plotly", "purrr", "sp", "HVT")

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE, repos='https://cloud.r-project.org/')

install.packages('https://cran.r-project.org/src/contrib/Archive/dummies/dummies_1.5.6.tar.gz', type="source", repos=NULL)
# Loading the required libraries
lapply(list.of.packages, library, character.only = TRUE)

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

Table <- function(data,scroll = FALSE, limit = NULL){
  if(!is.null(limit)){
    data <- head(data,limit)
  }
  kable_table <- data %>% kable(escape = FALSE,align = "c") %>% kable_styling(bootstrap_options = c("striped", "hover", "responsive"))
  scroll <- scroll
  if(scroll == TRUE){
  kable_table <- kable_table %>% scroll_box(width = "100%", height = scrolLimit(nrow(data)))
  }
  return(kable_table)
}



set.seed(240)

## ----Quantization Error,echo=FALSE,warning=FALSE,fig.show='hold',message=FALSE,fig.cap='Figure 1: The Voronoi tessellation for level 1 shown for the 5 cells with the points overlayed'----
knitr::include_graphics('quant_explainer.png')

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

## ----loading csv, message=FALSE, warning=FALSE--------------------------------
import_data_from_local = FALSE # expects logical input

  file_name <- " " #enter the name of the local file
  file_path <- " " #enter the path of the local file

if(import_data_from_local){
  file_load <- paste0(file_path, file_name)
  dataset_updated <- as.data.frame(fread(file_load))
  if(nrow(dataset_updated) > 0){
    paste0("File ", file_name, " having ", nrow(dataset_updated), " row(s) and ", ncol(dataset_updated), " column(s)",  " imported successfully. ") %>% cat("\n")
    dataset_updated <- dataset_updated %>% mutate_if(is.numeric, round, digits = 4)
    paste0("Code chunk executed successfully. Below table showing first 10 row(s) of the dataset.") %>% cat("\n")
    dataset_updated %>% head(10) %>%as.data.frame() %>%DT::datatable(options = options, rownames = TRUE)
  }
  
} 

## -----------------------------------------------------------------------------
simulate_dataset= TRUE

if(simulate_dataset == TRUE){
  
set.seed(257)
##torus data generation
torus <- geozoo::torus(p = 3,n = 12000)
dataset_updated <- data.frame(torus$points)
colnames(dataset_updated) <- c("x","y","z")


  if(nrow(dataset_updated) > 0){
    paste0( "Dataset having ", nrow(dataset_updated), " row(s) and ", ncol(dataset_updated), " column(s)",  "simulated successfully. ") %>% cat("\n")  
    dataset_updated <- dataset_updated %>% mutate_if(is.numeric, round, digits = 4) 
    paste0("Code chunk executed successfully. The table below is showing first 10 row(s) of the dataset.") %>% cat("\n")
    dataset_updated %>% head(10) %>%as.data.frame() %>%Table(limit = 10)
  }
}

## ----struc_test3,  warning=FALSE, message=FALSE, error=FALSE------------------
dataset_updated %>% str()

## -----------------------------------------------------------------------------
cat_cols <-
  colnames(dataset_updated)[unlist(sapply(
    dataset_updated,
    FUN = function(x) {
      CheckColumnType(x) == "character" ||
        CheckColumnType(x) == "factor"
    }
  ))]

apply(dataset_updated[cat_cols], 2, function(x) {
  length(unique(x))
})

## ----dummify,purl = TRUE------------------------------------------------------
########################################################################################
################################## User Input Needed ###################################
########################################################################################
# Do you want to dummify the categorical variables?

dummify_cat <- FALSE ## TRUE,FALSE

# Select the columns on which dummification is to be performed
dum_cols <- c(" "," ") #enter column name in smalls


## -----------------------------------------------------------------------------
numeric_cols=as.vector(sapply(dataset_updated, is.numeric))
dataset_updated=dataset_updated[,numeric_cols]
colnames(dataset_updated)

## ----train-test dataset_updated,warning=FALSE,message=FALSE,eval = global_var----
## 80% of the sample size
smp_size <- floor(0.80 * nrow(dataset_updated))

## set the seed to make your partition reproducible
set.seed(279)
train_ind <- sample(seq_len(nrow(dataset_updated)), size = smp_size)

dataset_updated_train <- dataset_updated[train_ind, ]
dataset_updated_test <- dataset_updated[-train_ind, ]

## ----trainhvt list,echo=FALSE,warning=FALSE,fig.show='hold',message=FALSE,fig.cap='Figure 3: The Output list generated by trainHVT function.', out.width="50%", out.height=  "20%", fig.align='center'----
knitr::include_graphics('hvt_result_diag.png')

## ----level one sample data,warning=FALSE,message=FALSE,eval = global_var,echo=TRUE,class.source = 'fold-show'----
set.seed(240)
hvt.results <- list()
hvt.results <-trainHVT(dataset_updated_train,
                          n_cells = 1035,
                          depth = 1,
                          quant.err = 0.1,
                          normalize = FALSE,
                          distance_metric = "L1_Norm",
                          error_metric = "max",
                          quant_method = "kmeans",
                          diagnose = TRUE,
                          hvt_validation = TRUE,
                          train_validation_split_ratio=0.8)

## ----fig.height=4, fig.width=6,fig.align='center'-----------------------------
plotHVT(hvt.results,
        line.width = c(0.6 ),
        color.vec = c("navy blue"),
        centroid.size = 1,
        maxDepth = 1, 
        plot.type = '2Dhvt')

## -----------------------------------------------------------------------------
displayTable(data = hvt.results[[3]]$compression_summary,columnName = 'percentOfCellsBelowQuantizationErrorThreshold', value = 0.8, tableType = "compression")

## -----------------------------------------------------------------------------
displayTable(data =hvt.results[[3]][['summary']], columnName= 'Quant.Error', value = 0.1, tableType = "summary")


## ----diagplot function1,echo = TRUE, eval= TRUE, fig.height=12, fig.width=14,class.source = 'fold-show'----
plotModelDiagnostics(hvt.results)

## ----fig.height=4, fig.width=6,fig.align='center'-----------------------------
p3=hvt.results[[4]]$mad_plot_train+ggtitle("Mean Absolute Deviation Plot: Calibration: HVT Model | Train Data")
p3

## ----fig.height=4, fig.width=6,fig.align='center'-----------------------------
p1=hvt.results[[4]]$datapoint_plot+ggtitle("Minimum Intra-DataPoint Distance Plot: Train Data")
p1

## ----fig.height=4, fig.width=6,fig.align='center'-----------------------------
p2=hvt.results[[4]]$cent_plot+ggtitle("Minimum Intra-Centroid Distance Plot: HVT Model | Train Data")
p2

## ----fig.height=4, fig.width=6,fig.align='center'-----------------------------
p4=hvt.results[[4]]$number_plot+ggtitle("Distribution of Number of Observations in Cells: HVT Model | Train Data")
p4

## ----fig.height=4, fig.width=6,fig.align='center'-----------------------------
p5=hvt.results[[4]]$singleton_piechart
p5

## ----fig.height=4, fig.width=6,fig.align='center'-----------------------------
m1=hvt.results[[5]][["mad_plot"]]+ggtitle("Mean Absolute Deviation Plot:Validation")
m1

## ----loading csv test, message=FALSE, warning=FALSE,class.source = 'fold-show'----
load_test_data=FALSE
if(load_test_data){
  file_name <- " " #enter the name of the local file for validation
  file_path <- " " #enter the path of the local file for validation
  file_load <- paste0(file_path, file_name)
  dataset_updated_test <- as.data.frame(fread(file_load))
  
  if(nrow(dataset_updated_test) > 0){


    paste0("File ", file_name, " having ", nrow(dataset_updated_test), " row(s) and ",
ncol(dataset_updated_test), " column(s)",  " imported successfully. ") %>% cat("\n")
dataset_updated_test <- dataset_updated_test %>% mutate_if(is.numeric, round, digits = 4)
    paste0("Code chunk executed successfully. Below table showing first 10 row(s) of the dataset.") %>% cat("\n")
    dataset_updated_test %>% head(10) %>%as.data.frame() %>%DT::datatable(options = options, rownames = TRUE)
  }
  
  colnames( dataset_updated_test) <- colnames( dataset_updated_test) %>% casefold()
 dataset_updated_test <- spaceless( dataset_updated_test)
}

## ----class.source = 'fold-show'-----------------------------------------------
hvt.score = list()
mad_threshold= 0.7 #Mean of Minimum Intra-Centroid Distance
hvt.score <- scoreHVT(
  data = dataset_updated_test,
  hvt.results.model=hvt.results,
  child.level = 1,
  mad.threshold = mad_threshold,
  line.width = c(0.6, 0.4, 0.2),
  color.vec = c("navyblue", "slateblue", "lavender"),
  distance_metric = "L1_Norm",
  error_metric = "max"
)

## ---- fig.width=8,fig.height=10,class.source = 'fold-show',fig.align='center'----
plotModelDiagnostics(hvt.score)

## ----scoreHVT2 hotel,warning=FALSE,message=FALSE,eval=global_var--------------

QEdata=hvt.results[[3]]$summary

Quant.Error.Actual <- QEdata %>% 
      mutate(Quant.Error =  Quant.Error * n) %>% 
      select(Quant.Error, n) %>% 
      summarise_all(sum) %>% 
      transmute(Quant.Error = Quant.Error/ n) %>% 
      unlist()%>%round(4)


## ----out.width="100%"---------------------------------------------------------
QECompareDf <- hvt.score$QECompareDf %>% filter(anomalyFlag == 1)
percentageAnomalies = formatC((sum(QECompareDf$n) / nrow(dataset_updated_test)) *
                                100, digits = 2, format = "f") %>% paste0("%")

## ----fig.height=7.5, fig.width=6,fig.align='center'---------------------------
plotQuantErrorHistogram(hvt.results,hvt.score)

## ----echo=TRUE,eval=TRUE------------------------------------------------------

QECompareDf <- hvt.score$QECompareDf %>% filter(anomalyFlag == 1)
# adding cell ID to table
QECompareDf1 <-
  left_join(
    QECompareDf,
    hvt.results[[3]]$summary,
    by = c("Segment.Level", "Segment.Parent", "Segment.Child")
  )
QECompareDf1 <-
  QECompareDf1 %>% select(
    "Segment.Level",
    "Segment.Parent",
    "Segment.Child",
    # "Cell.ID",
    "anomalyFlag",
    "n.x" ,
    "Fitted.Quant.Error" ,
    "Scored.Quant.Error" ,
    "Quant.Error.Diff",
    "Quant.Error.Diff (%)"
  )
colnames(QECompareDf1) <- c(
    "Segment.Level",
    "Segment.Parent",
    "Segment.Child",
    # "Cell.ID",
    "anomalyFlag",
    "No.Of.Points" ,
    "Fitted.Quant.Error" ,
    "Scored.Quant.Error" ,
    "Quant.Error.Diff",
    "Quant.Error.Diff (%)"
  )
QECompareDf1$flag <-
  ifelse(QECompareDf1$anomalyFlag == 1, 1, 0)
DT::datatable(
  QECompareDf1 %>%
    mutate_if(is.numeric, ~ round(., 4)) %>%
    select(-c(`Quant.Error.Diff (%)`)),
  class = 'cell-border stripe',
  rownames = FALSE,
  filter = "top",
  escape = FALSE,
  selection = "none",
  options = options,
  callback = htmlwidgets::JS(
    "var tips = ['Segment Level based on the hierarchical structure of the HVT model output',
                        'Segment Parent based on the hierarchical structure of the HVT model output',
                        'Segment Child based on the hierarchical structure of the HVT model output',
                        'Cell ID based on the hierarchical structure of the HVT model output',
                        'Flag indicating whether data points are anomalous or not',
                        'Number of anomalous scored data points for each centroid in the HVT map',
                        'Quantization Error for the highlighted cell built on the fitted data',
                        'Quantization Error for the highlighted cell built on the scored data',
                        'Change in Quantization Error between scored and the fitted model'],
                            header = table.columns().header();
                        for (var i = 0; i < tips.length; i++) {
                          $(header[i]).attr('title', tips[i]);
                        }"
  )
) %>%
  formatStyle('flag',
              target = 'row',
              backgroundColor = styleEqual(c(1), c('lightcoral')))


## ---- echo=FALSE--------------------------------------------------------------
# library(knitr)
# library(kableExtra)
# 
# kable(
#   QECompareDf1 %>%
#     mutate_if(is.numeric, ~ round(., 4)) %>%
#     select(-c(`Quant.Error.Diff (%)`)) %>%
#     rename(No.Of.Points = n),
#   format = "html",
#   table.attr = 'class="cell-border stripe"',
#   row.names = FALSE
# ) %>%
#   kable_styling(bootstrap_options = c("striped"))

#print(
df1 <-  QECompareDf1 %>%
    mutate_if(is.numeric, ~ round(., 4)) %>%
    select(-c(`Quant.Error.Diff (%)`)) 
    #rename(No.Of.Points = n)
Table(df1,scroll = TRUE, limit = 10)
#)

## -----------------------------------------------------------------------------
predictClusterData <- hvt.score[["scoredPredictedData"]]%>%as.data.frame()
 predictClusterData  %>% head(100) %>% round(2)%>%
      as.data.frame() %>%
      Table(scroll = TRUE, limit = 100)

