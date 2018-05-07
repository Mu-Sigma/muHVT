# Jensen-Shannon-Bregman distance d(x, y) between vectors x and y
# is defined as:
# d(x, y) = sqrt(jsbd(x, y))
# d(x, y) is a metric.
# x, y: probability distributions (i.e., \sum xi = \sum yi = 1)
# jsbd: Jensen-Shannon-Bregman divergence, a symmetrized Bregman Divergence
# jsbd(x, y): JSB divergence between non-neg vectors x and y.
# jsbd(x, y) = 0.5 * (id(x, m) + id(y, m))
# where:
# I-divergence is defined as:
# id(x, y) = \sum xi \log (xi / yi) - sum_i(xi) + sum_i(yi)
# mi = 0.5 * (xi + yi)

# inm is the input matrix such that every row represents a non-neg vector
# Assumed: the matrix does not have any zero value.

# id <- function(x, y) sum(x*log(x/y)) - sum(x) + sum(y)
# jsd <- function(x, y) sqrt(0.5 * (id(x, (x+y)/2) + id(y, (x+y)/2)))

id <- function(x, y) {
    newVec <- (x + y)/2
    vec1 <- colSums(x*log(x/newVec)) - sum(x) + colSums(newVec)
    vec2 <- colSums(y*log(y/newVec)) - colSums(y) + colSums(newVec)
    return(vec1 + vec2)
}

jsd_pair <- function(x, y){
    sqrt(0.5 * id(x, y))  
} 







#' Jensen-Shannon-Bregman distance
#' 
#' Jensen-Shannon-Bregman distance d(x, y) between vectors x and y
#' 
#' Jensen-Shannon-Bregman distance \code{d(x, y)} between vectors \code{x} and
#' \code{y} is defined as: \cr \code{d(x, y) = sqrt(jsbd(x, y))} \cr \cr where:
#' \cr 1. \code{d(x, y)} is a metric. \cr 2. \code{x, y}: probability
#' distributions (i.e., \code{sum(x) = sum(y) = 1}) \cr 3. jsbd:
#' Jensen-Shannon-Bregman divergence, a symmetrized Bregman Divergence \cr 4.
#' \code{jsbd(x, y)}: JSB divergence between non-neg vectors x and y. \cr 5.
#' \code{jsbd(x, y) = 0.5 * (id(x, m) + id(y, m))} \cr \cr I-divergence is
#' defined as: \cr \code{id(x, y) = sum(x * log(x/y)) - sum(x) + sum(y)} \cr
#' \code{m = 0.5 * (x + y)} \cr
#' 
#' \code{inm} is the input matrix such that every row represents a non-neg
#' vector \cr Assumed: The matrix does not have any zero value. \cr
#' 
#' \code{id <- function(x, y) sum(x*log(x/y)) - sum(x) + sum(y)} \cr \code{jsd
#' <- function(x, y) sqrt(0.5 * (id(x, (x+y)/2) + id(y, (x+y)/2)))}
#' 
#' @param inm a numeric matrix or a data frame that comprises of non zero
#' positive entries.
#' @param pc Numeric. A small non-zero value to replace zero entries in the
#' vectors
#' @return A distance matrix
#' @note The limitation with this metric is that the divergence can be computed
#' only between positive vectors with non-zero components.
#' @author Meet K. Dave <dave.kirankumar@@mu-sigma.com>
#' @examples
#' 
#' 
#' data(sampledata)
#' 
#' dMat = jsd(sampledata)
#' 
#' 
#' @export jsd
jsd <- function(inm, pc=0.000001) {
    Cols <- ncol(inm)
    Rows <- nrow(inm)
    names <- rownames(inm)
    
# res is the resulting matrix of distances
    res <- matrix(0, Rows, Rows)

# check if the input matrix has any zero entry; if yes, replace it with
# pc, a very small value
    inm = apply(inm, 1:2, function(x) ifelse (x == 0, pc, x))
    res <- apply(inm, 2, jsd_pair, inm)
    
    # colnames(res) <- names
    # rownames(res) <- names
    # res <- as.dist(res)
    # attr(res, "method") <- "dist"
    return(res)
}
