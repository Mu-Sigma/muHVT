context("hvq package")
# require(plotly)

## Load data 

USArrests <- datasets::USArrests
USArrests <- scale(USArrests,center = T,scale = T)
set.seed(420)

test_that("getCentroids give correct results for L1_Norm and mean",{
  skip_on_cran()
  
  set.seed(420)
  hvt.results <- HVT::HVT(USArrests,n_cells = 3,depth = 1,quant.err = 0.2,distance_metric = "L1_Norm",error_metric = "mean", quant_method = "kmeans")
  
  expect_equal(length(hvt.results),6)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(0.5265759, 0.4197644, 0.4843762),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)
  
})

test_that("getCentroids give correct results for L2_Norm and mean",{
  skip_on_cran()
  
  set.seed(420)
  
  hvt.results <- HVT::HVT(USArrests,n_cells = 3,depth = 1,quant.err = 0.2,distance_metric = "L2_Norm",error_metric = "mean", quant_method = "kmeans")
  
  expect_equal(length(hvt.results),6)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(0.3141953, 0.2407124, 0.2885561),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)  
})

test_that("getCentroids give correct results for L1_Norm and max",{
  skip_on_cran()
  
  set.seed(420)
  
  hvt.results <- HVT::HVT(USArrests,n_cells = 3,depth = 1,quant.err = 0.2,distance_metric = "L1_Norm",error_metric = "max", quant_method = "kmeans")
  
  expect_equal(length(hvt.results),6)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(1.0147530, 0.6268437, 0.8490633),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)  
})

test_that("getCentroids give correct results for L2_Norm and max",{
  skip_on_cran()
  
  set.seed(420)
  hvt.results <- HVT::HVT(USArrests,n_cells = 3,depth = 1,quant.err = 0.2,distance_metric = "L2_Norm",error_metric = "max", quant_method = "kmeans")
  
  expect_equal(length(hvt.results),6)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(0.6168272, 0.3525659, 0.5722204),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0)  
})
