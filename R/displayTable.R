#' @name displayTable
#' @title Table for displaying summary
#' @description This is the main function for displaying summary from model training and scoring
#' @param data List. A listed object from trainHVT or scoreHVT
#' @param columnName Character. Name of the column that needs highlighting.
#' @param value Numeric. The value above will be highlighted in red or green.
#' @param tableType Character. Type of table to generate ('summary', 'compression' and 'metrics')
#' @param scroll Logical. A value to have a scroll or not in the table.
#' @param limit Numeric. A value to indicate how many rows to display.
#' Applicable for summary tableType.
#' @return A consolidated table of results from trainHVT and scoreHVT.
#' @author Vishwavani <vishwavani@@mu-sigma.com>, Alimpan Dey <alimpan.dey@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} 
#' @importFrom rlang sym
#' @importFrom dplyr mutate across where
#' @keywords EDA
#' @examples
#' data <- datasets::EuStockMarkets
#' dataset <- as.data.frame(data)
#' #model training
#' hvt.results <- trainHVT(dataset, n_cells = 60, depth = 1, quant.err = 0.1,
#'                       distance_metric = "L1_Norm", error_metric = "max",
#'                       normalize = TRUE, quant_method = "kmeans", dim_reduction_method = 'sammon')
#' displayTable(data =  hvt.results$model_info$distance_measures, tableType = "metrics")
#' displayTable(data = hvt.results[[3]]$compression_summary,
#' columnName = 'percentOfCellsBelowQuantizationErrorThreshold', 
#' value = 0.8, tableType = "compression")
#' displayTable(data =hvt.results[[3]][['summary']], columnName= 'Quant.Error',
#' value = 0.1, tableType = "summary", scroll = TRUE)
#' @export displayTable

displayTable <- function(data, columnName = NULL, value = NULL, tableType = "summary", scroll = FALSE, limit= 100) {
  # Check that columnName is a column in data
  if (is.null(columnName) && is.null(value) && tableType == "metrics") {
    kable_table <- knitr::kable(data, "html", escape = FALSE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive", "bordered")) %>%
      kableExtra::collapse_rows(columns = 1, valign = "middle")
    # Return the final table
    return(kable_table)
  }
  else{
    # Check that columnName is not NULL and valid before using it
    if (!is.null(columnName)) {
      if (!columnName %in% names(data)) {
        stop("columnName is not a valid column in the provided data frame.")
      }
    }
      
      # Check that value is numeric
    # Check that value is numeric if it's provided
    if (!is.null(value)) {
      if (!is.numeric(value)) {
        stop("value should be a numeric value.")
      }
    }
      
      # Check that tableType is valid
      valid_table_types <- c("summary", "compression","measures")
      if (!tableType %in% valid_table_types) {
        stop("tableType should be one of the following: ", paste(valid_table_types, collapse = ", "), ".")
      }
    
      # Limit the data
      data <- head(data, limit)
      
      # Ensure all numeric columns are rounded to 2 decimal places using dplyr's mutate and across
      data <- data %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 2)))
      
      # Apply conditional coloring based on tableType and value
      # Apply conditional coloring based on tableType and value if columnName is provided
      if (!is.null(columnName)){
        data <- data %>%
          dplyr::mutate(!!sym(columnName) := dplyr::case_when(
            tableType == "summary" & .data[[columnName]] > value ~ kableExtra::cell_spec(.data[[columnName]], "html", color = "red"),
            tableType == "compression" & .data[[columnName]] > value ~ kableExtra::cell_spec(.data[[columnName]], "html", color = "green"),
            TRUE ~ kableExtra::cell_spec(.data[[columnName]], "html", color = "black")
          ))
      }
      # Generate the kable table with options using knitr's kable and kableExtra's styling functions
      kable_table <- knitr::kable(data, "html", escape = FALSE, align = "c") %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"))
      
      # Set scroll box height based on tableType
      scroll_height <- if(tableType == "summary") {
        "500px"
      } else  {
        "250px"
      }
      
      # Optionally add a scroll box with dynamic height using kableExtra
      if(scroll) {
        kable_table <- kable_table %>% kableExtra::scroll_box(width = "100%", height = scroll_height)
      }
      
      return(kable_table)
    }
    
}
