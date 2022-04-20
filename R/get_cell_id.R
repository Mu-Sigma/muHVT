#' Cell ID
#'
#' @title Function to generate cell ID based on 1D sammons projection
#'
#' @description To generate cell id for the multivariate data, the data is being projected from n-dimensions to 1-dimension 
#' and the cell id is being assigned by ordering these values and finding the corresponding indexes. The ouput CellID gets
#' appended to the HVT model. 
#'
#' @param hvt.results List. A list of hvt.results obtained from the HVT
#' function.
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @importFrom magrittr %>%
#' @examples
#' data(USArrests)
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 15, depth = 1, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE,
#'                    quant_method="kmeans",diagnose=TRUE)
#' plotHVT(hvt.results, line.width = c(0.8), color.vec = c('#141B41'), 
#'         maxDepth = 1)
#'get_cell_id (hvt.results)
#' @export get_cell_id


get_cell_id <-  function (hvt.results){
# browser()
  generic_col=c("Segment.Level","Segment.Parent","Segment.Child","n","Quant.Error")
  temp_summary=hvt.results[[3]][["summary"]] %>% dplyr::select(!generic_col) %>% dplyr::mutate(id=row_number())
  cent_val= temp_summary %>% subset(.,complete.cases(.)) 
  sammon_1d_cord <-MASS::sammon(
    d = stats::dist(cent_val %>% dplyr::select(!id),method = "manhattan"),
    niter = 10 ^ 5,
    trace = FALSE,
    k=1
  )$points
  temp_df=data.frame(sammon_1d_cord,id=cent_val$id)%>%dplyr::arrange(sammon_1d_cord) %>% dplyr::mutate(Cell.ID=row_number()) %>% dplyr::select(!sammon_1d_cord)
  temp_summary = dplyr::left_join(temp_summary,temp_df,by="id") %>% select(!"id")
  hvt.results[[3]][["summary"]]$Cell.ID=temp_summary$Cell.ID

    return(hvt.results[[3]][["summary"]])
  }




