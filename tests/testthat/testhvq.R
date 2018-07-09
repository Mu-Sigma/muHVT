context("hvq package")


## Load data 

USArrests <- datasets::USArrests
USArrests <- scale(USArrests,center = T,scale = T)
set.seed(420)

test_that("getCentroids give correct results for L1_Norm and mean",{
  skip_on_cran()
  
  set.seed(420)
  hvt.results <- muHVT::hvq(USArrests,nclust = 3,depth = 1,quant.err = 0.2,distance_metric = "L1_Norm",error_metric = "mean")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results$summary[,"Quant.Error"],c(1.679058,2.106304,1.937505),1e-5)
  
  expect_equal(hvt.results$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)
  
})

test_that("getCentroids give correct results for L2_Norm and mean",{
  skip_on_cran()
  
  set.seed(420)
  
  hvt.results <- muHVT::hvq(USArrests,nclust = 3,depth = 1,quant.err = 0.2,distance_metric = "L2_Norm",error_metric = "mean")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results$summary[,"Quant.Error"],c(0.9628496,1.2567811,1.1542245),1e-5)
  
  expect_equal(hvt.results$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)  
})

test_that("getCentroids give correct results for L1_Norm and max",{
  skip_on_cran()
  
  set.seed(420)
  
  hvt.results <- muHVT::hvq(USArrests,nclust = 3,depth = 1,quant.err = 0.2,distance_metric = "L1_Norm",error_metric = "max")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results$summary[,"Quant.Error"],c(2.507375,4.059012,3.396253),1e-5)
  
  expect_equal(hvt.results$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)  
})

test_that("getCentroids give correct results for L2_Norm and max",{
  skip_on_cran()
  
  set.seed(420)
  hvt.results <- muHVT::hvq(USArrests,nclust = 3,depth = 1,quant.err = 0.2,distance_metric = "L2_Norm",error_metric = "max")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results$summary[,"Quant.Error"],c(1.410263,2.467309,2.288882),1e-5)
  
  expect_equal(hvt.results$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)  
})