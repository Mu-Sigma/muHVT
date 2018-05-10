#' sammonsProjection
#' 
#' This is a wrapper for the sammon function of the MASS package for non-metric
#' multidimensional scaling
#' 
#' This chooses a two-dimensional configuration to minimize the stress, the sum
#' of squared differences between the input distances and those of the
#' configuration, weighted by the distances, the whole sum being divided by the
#' sum of input distances to make the stress scale-free.
#' 
#' An iterative algorithm is used, which will usually converge in around 50
#' iterations. As this is necessarily an O(n^2) calculation, it is slow for
#' large datasets. Further, since the configuration is only determined up to
#' rotations and reflections (by convention the centroid is at the origin), the
#' result can vary considerably from machine to machine. In this release the
#' algorithm has been modified by adding a step-length search (magic) to ensure
#' that it always goes downhill.
#' 
#' @param d distance structure of the form returned by dist, or a full,
#' symmetric matrix. Data are assumed to be dissimilarities or relative
#' distances, but must be positive except for self-distance. This can contain
#' missing values.
#' @param y An initial configuration. If none is supplied, cmdscale is used to
#' provide the classical solution. (If there are missing values in d, an
#' initial configuration must be provided.) This must not have duplicates.
#' @param k The dimension of the configuration.
#' @param niter The maximum number of iterations.
#' @param trace Logical for tracing optimization. Default TRUE.
#' @param magic initial value of the step size constant in diagonal Newton
#' method.
#' @param tol Tolerance for stopping, in units of stress.
#' @return \item{points}{ A two-column vector of the fitted configuration. }
#' \item{stress}{ The final stress achieved. }
#' @examples
#' 
#' require(MASS)
#' swiss.x <- as.matrix(swiss[, -1])
#' swiss.sam <- sammonsProjection(dist(swiss.x))
#' 
#' 
#' 
#' 
#' @export sammonsProjection
sammonsProjection <- function(d, y = stats::cmdscale(d, k), k = 2, niter = 100, trace = TRUE,
       magic = 0.2, tol = 1e-4){
	   
    return(MASS::sammon(d, y = stats::cmdscale(d, k), k = 2, niter = 100, trace = TRUE,
       magic = 0.2, tol = 1e-4))
}
