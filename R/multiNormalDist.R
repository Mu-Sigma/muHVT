#' @keywords internal
multiNormalDist <-
  function(sample.size = 2500, ncol = 5) {
    if (sample.size < 1) stop("sample.size is less than 1")
    df <- matrix(ncol = ncol, nrow = sample.size) %>% as.data.frame()
    for (i in 1:ncol) {
      df[, i] <- stats::rnorm(n = sample.size, mean = 0, sd = 1)
    }
    return(df)
  }
