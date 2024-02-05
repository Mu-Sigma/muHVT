#' Cell ID
#'
#' Function to generate cell ID based on 1D sammons projection
#'
#' To generate cell id for the multivariate data, the data is being projected from n-dimensions to 1-dimension 
#' and the cell id is being assigned by ordering these values and finding the corresponding indexes. The output Cell id gets
#' appended to the HVT model. 
#'
#' @param hvt.results List. A list of hvt.results obtained from the trainHVT function.
#' @param seed Numeric. Random Seed
#' @returns Object containing Cell.ID mappings for the given hvt.results list.
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @importFrom magrittr %>%
#' @examples
#' data("EuStockMarkets")
#' hvt.results <- list()
#' hvt.results <- trainHVT(EuStockMarkets, n_cells = 15, depth = 1, quant.err = 0.2, 
#'                    distance_metric = "L1_Norm", error_metric = "mean",
#'                    projection.scale = 10, normalize = TRUE, seed = 123,
#'                    quant_method="kmeans")
#' plotHVT(hvt.results, line.width = c(0.8), color.vec = c('#141B41'), 
#'         maxDepth = 1, heatmap ='2DHVT')
#'getCellId (hvt.results)
#' @export getCellId
#' @keywords internal


getCellId <-  function(hvt.results, seed = 123) {
# browser()
  generic_col=c("Segment.Level","Segment.Parent","Segment.Child","n","Quant.Error")
  temp_summary=hvt.results[[3]][["summary"]] %>% dplyr::select(!generic_col) %>% dplyr::mutate(id=row_number())
  cent_val= temp_summary %>% subset(.,complete.cases(.)) 
  set.seed(seed)
  sammon_1d_cord <- MASS::sammon(
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




