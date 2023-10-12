#' Multivariate normal distribution
#'
#' Function to generate multivariate normal distribution where each variable has a standard normal distribution N(0,1)
#'
#' The multivariate normal distribution, multivariate Gaussian distribution, or 
#' joint normal distribution is a generalization of the one-dimensional (univariate) 
#' normal distribution to higher dimensions. It is a vector in multiple normally distributed variables, 
#' such that any linear combination of the variables is also normally distributed.
#'
#' @param sample.size Numeric. Indicating the sample size for distribution
#' @param ncol Numeric. Indicating the number of columns
#' @author Shubhra Prakash <shubhra.prakash@@mu-sigma.com>
#' @importFrom magrittr %>%
#' @examples
#' multiNormalDist(2500,2) 
#' x=multiNormalDist(2500,2)
#' hist(x[,1])

#' @export multiNormalDist


multiNormalDist <-
  function (sample.size=2500,ncol=5){
    if(sample.size<1) stop("sample.size is less than 1")
    df = matrix(ncol = ncol, nrow = sample.size) %>% as.data.frame()
    for (i in 1:ncol) {
      df[,i] = stats::rnorm(n = sample.size ,mean = 0 ,sd = 1)
    }
    return(df)
  }





