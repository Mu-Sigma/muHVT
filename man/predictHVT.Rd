% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictHVT.R
\name{predictHVT}
\alias{predictHVT}
\title{Predict which cell and what level each point in the test dataset belongs to}
\usage{
predictHVT(
  data,
  hvt.results,
  hmap.cols = NULL,
  child.level = 1,
  quant.error.hmap = NULL,
  nclust.hmap = NULL,
  line.width = NULL,
  color.vec = NULL,
  ...
)
}
\arguments{
\item{data}{List. A dataframe containing test dataset. The dataframe should have atleast one variable used while training. The variables from
this dataset can also be used to overlay as heatmap}

\item{hvt.results}{A list of hvt.results obtained from HVT function while performing hierarchical vector quantization on train data}

\item{hmap.cols}{- The column number of column name from the dataset indicating the variables for which the heat map is to be plotted.(Default = #' NULL). A heatmap won’t be plotted if NULL is passed}

\item{child.level}{A number indicating the level for which the heat map is to be plotted.(Only used if hmap.cols is not NULL)}

\item{quant.error.hmap}{Numeric. A number indicating the quantization error threshold.}

\item{nclust.hmap}{Numeric. An integer indicating the number of clusters per hierarchy}

\item{line.width}{Vector. A line width vector such that length(line.width) =
(child.level - 1). (default = NULL)}

\item{color.vec}{Vector. A color vector such that length(color.vec) =
(child.level - 1). (default = NULL)}

\item{...}{color.vec and line.width can be passed from here}
}
\description{
Main function to predict cell path of new datapoints
}
\examples{
data(USArrests)
#Split in train and test

train <- USArrests[1:40,]
test <- USArrests[41:50,]

hvt.results <- list()
hvt.results <- HVT(train, nclust = 3, depth = 2, quant.err = 0.2, 
                   distance_metric = "L1_Norm", error_metric = "mean",
                   projection.scale = 10, normalize = TRUE)

predictions <- predictHVT(test,hvt.results,hmap.cols = "Quant.Error", child.level=2,
                          quant.error.hmap = 0.2,nclust.hmap = 3,line.width = c(1.2,0.8,0.4),
                          color.vec = c('#141B41','#0582CA','#8BA0B4'))
print(predictions$predictions)
}
\seealso{
\code{\link{HVT}} \cr \code{\link{hvtHmap}}
}
\author{
Sangeet Moy Das <sangeet.das@mu-sigma.com>
}
\keyword{predict}
