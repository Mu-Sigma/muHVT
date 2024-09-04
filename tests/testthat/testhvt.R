context("hvq package")
# require(plotly)

## Load data 

USArrests <- datasets::USArrests
USArrests <- scale(USArrests,center = TRUE,scale = TRUE)
set.seed(420)

test_that("getCentroids give correct results for L1_Norm and mean",{
  skip_on_cran()
  
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests,n_cells = 10,depth = 1,quant.err = 0.2,distance_metric = "L1_Norm",error_metric = "mean", quant_method = "kmeans", dim_reduction_method = "sammon")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(0.3609466 ,0.2791825, 0.2990230 ,0.3566383 ,0.2383200 ,0.3392648, 0.2637289, 0.0000000 ,0.3795258, 0.2487440),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0.1)
  
})

test_that("getCentroids give correct results for L2_Norm and mean",{
  skip_on_cran()
  
  set.seed(420)
  
  hvt.results <- HVT::trainHVT(USArrests,n_cells = 10,depth = 1,quant.err = 0.2,distance_metric = "L2_Norm",error_metric = "mean", quant_method = "kmeans", dim_reduction_method = "sammon")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(0.2027530, 0.1638378 ,0.1638530, 0.2100104, 0.1350629, 0.1868272 ,0.1477676, 0.0000000, 0.2131553, 0.1453146),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0.7)  
})

test_that("getCentroids give correct results for L1_Norm and max",{
  skip_on_cran()
  
  set.seed(420)
  
  hvt.results <- HVT::trainHVT(USArrests,n_cells = 10,depth = 1,quant.err = 0.2,distance_metric = "L1_Norm",error_metric = "max", quant_method = "kmeans",dim_reduction_method = "sammon")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(0.5282025 ,0.3198631 ,0.4280770 ,0.5259606, 0.2936061 ,0.3972785, 0.2977948 ,0.0000000, 0.4775840, 0.2994434),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0.1)  
})

test_that("getCentroids give correct results for L2_Norm and max",{
  skip_on_cran()
  
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests,n_cells = 10,depth = 1,quant.err = 0.2,distance_metric = "L2_Norm",error_metric = "max", quant_method = "kmeans", dim_reduction_method = "sammon")
  
  expect_equal(length(hvt.results),7)
  
  expect_equal(hvt.results[[3]]$summary[,"Quant.Error"],c(0.3098097 ,0.1954690, 0.2424398 ,0.3029314, 0.1719962 ,0.2006439, 0.1599492 ,0.0000000, 0.2916686, 0.1742819),1e-5)
  
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]],0.5)  
})
