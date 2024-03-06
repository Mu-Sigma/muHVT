#' @importFrom rlang sym

displayTable <- function(data, columnName, value, tableType = "summary", scroll = TRUE) {
  # Limit the data
  data <- head(data, 100)
  
  # Ensure all numeric columns are rounded to 2 decimal places using dplyr's mutate and across
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 2)))
  
  # Apply conditional coloring based on tableType and value
  data <- data %>%
    dplyr::mutate(!!sym(columnName) := dplyr::case_when(
      tableType == "summary" & .data[[columnName]] > value ~ kableExtra::cell_spec(.data[[columnName]], "html", color = "red"),
      tableType == "compression" & .data[[columnName]] > value ~ kableExtra::cell_spec(.data[[columnName]], "html", color = "green"),
      TRUE ~ kableExtra::cell_spec(.data[[columnName]], "html", color = "black")
    ))
  
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
