# muHVT: Collection of functions used for vector quantization and construction of hierarchical voronoi tessellations for data analysis

#### Zubin Dowlaty, Shubhra Prakash, Sangeet Moy Das, Shantanu Vaidya, Praditi Shah

#### 2023-06-07

<div id="TOC">

*   [<span class="toc-section-number">1</span> Abstract](#abstract)
*   [<span class="toc-section-number">2</span> muHVT 4.0.0 | What’s New?](#muhvt-4.0.0-whats-new)
    *   [<span class="toc-section-number">2.1</span> Installation of muHVT 4.0.0](#installation-of-muhvt-4.0.0)
*   [<span class="toc-section-number">3</span> muHVT 3.0.0](#muhvt-3.0.0)
*   [<span class="toc-section-number">4</span> Vignettes](#vignettes)
    *   [<span class="toc-section-number">4.1</span> muHVT Vignette](#muhvt-vignette)
    *   [<span class="toc-section-number">4.2</span> muHVT Model Diagnostics Vignette](#muhvt-model-diagnostics-vignette)
    *   [<span class="toc-section-number">4.3</span> muHVT : Predicting Cells and Layers using predictLayerHVT to monitor entities over time](#muhvt---predicting-cells-and-layers-using-predictLayerHVT-to-monitor-entities-over-time)

</div>

<div id="abstract" class="section level1" number="1">

# <span class="header-section-number">1</span> Abstract

The muHVT package is a collection of R functions to facilitate building [topology preserving maps](https://link.springer.com/chapter/10.1007/1-84628-118-0_7) for rich multivariate data. Tending towards a big data preponderance, a large number of rows. A collection of R functions for this typical workflow is organized below :

1.  **Data Compression**: Vector quantization (VQ), HVQ (hierarchical vector quantization) using means or medians. This step compresses the rows (long data frame) using a compression objective

2.  **Data Projection**: Dimension projection of the compressed cells to 1D,2D and 3D with the Sammons Non-linear Algorithm. This step creates topology preserving map (also called as [embedding](https://en.wikipedia.org/wiki/Embedding)) coordinates into the desired output dimension . 

3.  **Tessellation**: Create cells required for object visualization using the Voronoi Tessellation method, package includes heatmap plots for hierarchical Voronoi tessellations (HVT). This step enables data insights, visualization, and interaction with the topology preserving map. Useful for semi-supervised tasks

4.  **Prediction**: Scoring new data sets and recording their assignment using the map objects from the above steps, in a sequence of maps if required

This package additionally provides functions for computing Sammon’s projection and plotting the heat map of the variables on the tiles of the tessellations.

The muHVT process involves three steps:

1.  **Compress:** Using a quantization threshold
2.  **Project:** Using a dimension projection algorithm
3.  **Tessellate:** Using a Voronoi Tessellation

</div>

<div id="muhvt-4.0.0| What’s New?" class="section level1" number="2">

# <span class="header-section-number">2</span> muHVT 4.0.0 | What’s New? 

07th June, 2023

This package now additionally provides functionality to predict based on a set of maps to monitor entities over time.

The creation of a predictive set of maps involves five steps -

1.  **Compress:** Compress the dataset using a percentage compression rate and a quantization threshold using the HVT() function to generate map A
2.  **Remove novelty cells:** Manually identify and remove the novelty cells from the dataset using the removeNovelty() function
3.  **Compress the dataset with novelty:** compress the dataset with novelty records using n_cells, depth and a quantization threshold     using the HVT() function to generate map B
4.  **Compress the dataset without novelty:** Again, compress the dataset without novelty(s) using n_cells, depth and a quantization threshold using the HVT() function to generate map C
5.  **Predict based on a predictive set of maps:** Using the predictLayerHVT function

Let us try to understand the steps with the help of the diagram below -

<img src="https://github.com/Somya545/muHVT/blob/master/vignettes/predictLayerHVT_function.png" width="672px" height="480px" />
<p class="caption">
Figure 1: Flow diagram for predicting based on a set of maps using predictLayerHVT()
</p>

Initially, the raw data is passed, and a highly compressed map A is constructed using the __`HVT`__ function. The output of this function will be hierarchically arranged vector quantized data that is used to identify the novelty cells in the dataset using the number of data points within each cell and the z-scores for each cell.

The identified novelty cell(s) is then passed to the __`removeNovelty`__ function along with map A. This function removes the identified novelty cell(s) from the dataset and stores them separately. The final output of this function is a list of two items - a dataset with novelty records, and a subset of the dataset without novelty cell(s).

The __`plotCells`__ function plots the Voronoi tessellations for the compressed map (map A) and highlights the identified novelty cell(s) in red on the plot. The function requires the identified novelty cell(s) number and the compressed map (map A) as input in order to plot the tessellations map and highlight those novelty cells on it.

The dataset with novelty records gotten as an output from the __`removeNovelty`__ function is then passed as an argument to the __`HVT`__ function with other parameters such as n_cells, quant.error, depth, etc. to construct another map (map B).

The dataset without novelty gotten as an output from the __`removeNovelty`__ function is then passed as an argument to the __`HVT`__ function with other parameters such as n_cells, quant.error, depth, etc. to construct another map (map C).

Finally, all the constructed maps are passed to the __`predictLayerHVT`__ function along with the test dataset on which the function will predict/score for finding which map and what cell each test record gets assigned to.


<div id="installation-of-muhvt-4.0.0" class="section level2" number="2.1">

## <span class="header-section-number">2.1</span> Installation of muHVT 4.0.0

<div class="sourceCode" id="cb1">

    library(devtools)
    devtools::install_github(repo = "Mu-Sigma/muHVT")

</div>

</div>




</div>

<div id="muhvt-3.0.0" class="section level1" number="3">

# <span class="header-section-number">3</span> muHVT 3.0.0 

06th December, 2022

This package now additionally provides functionality to predict based on a set of maps to monitor entities over time.

The creation of a predictive set of maps involves four steps -

1.  **Compress:** Compress the dataset using a percentage compression rate and a quantization threshold using the HVT() function (Map A)
2.  **Remove outlier cells:** Manually identify and remove the outlier cells from the dataset using the removeOutliers() function (Map B)
3.  **Compress the dataset without outliers:** Again, compress the dataset without outlier(s) using n_cells, depth and a quantization threshold using the HVT() function (Map C)
4.  **Predict based on a predictive set of maps:** Using the mlayerHVT() function

Let us try to understand the steps with the help of the diagram below -

<img src="https://github.com/Somya545/muHVT/blob/master/vignettes/mlayer1.png" width="672px" height="480px" />
<p class="caption">
Figure 2: Flow diagram for predicting based on a set of maps using mlayerHVT()
</p>

Initially, the raw data is passed, and a highly compressed Map A is constructed using the **`HVT`** function. The output of this function will be hierarchically arranged vector quantized data that is used to identify the outlier cells in the dataset using the number of data points within each cell and the z-scores for each cell.

The identified outlier cell(s) is then passed to the **`removeOutliers`** function along with Map A. This function removes the identified outlier cell(s) from the dataset and stores them in Map B as shown in the diagram. The final output of this function is a list of two items - a newly constructed map (Map B), and a subset of the dataset without outlier cell(s).

The **`plotCells`** function plots the Voronoi tessellations for the compressed map (Map A) and highlights the identified outlier cell(s) in red on the plot. The function requires the identified outlier cell(s) number and the compressed map (Map A) as input in order to plot the tessellations map and highlight those outlier cells on it.

The dataset without outlier(s) gotten as an output from the removeOutliers function is then passed as an argument to the **`HVT`** function with other parameters such as n_cells, quant.error, depth, etc. to construct another map (Map C).

Finally, all the constructed maps are passed to the **`mlayerHVT`** function along with the test dataset on which the function will predict/score for finding which map and what cell each test record gets assigned to.

**For detailed information on the above functions, refer the vignette.**


</div>

<div id="vignettes" class="section level1" number="4">

# <span class="header-section-number">4</span> Vignettes

Following are the links to the vignettes for the muHVT package:

<div id="muhvt-vignette" class="section level2" number="4.1">

## <span class="header-section-number">4.1</span> muHVT Vignette

[**muHVT Vignette:**](https://htmlpreview.github.io/?https://github.com/Somya545/muHVT/blob/master/vignettes/muHVT_vignette.html) Contains descriptions of the functions used for vector quantization and construction of hierarchical voronoi tessellations for data analysis.

</div>

<div id="muhvt-model-diagnostics-vignette" class="section level2" number="4.2">

## <span class="header-section-number">4.2</span> muHVT Model Diagnostics Vignette

[**muHVT Model Diagnostics Vignette:**](https://htmlpreview.github.io/?https://github.com/Somya545/muHVT/blob/master/vignettes/muHVT_model_diagnostics_vignette.html) Contains descriptions of functions used to perform model diagnostics and validation for muHVT model.

</div>

<div id="muhvt---predicting-cells-and-layers-using-predictLayerHVT-to-monitor-entities-over" class="section level2" number="4.3">

## <span class="header-section-number">4.3</span> muHVT - Predicting Cells and Layers using predictLayerHVT to monitor entities over Time

[**muHVT : Predicting Cells and Layers using predictLayerHVT to monitor entities over Time:**](https://htmlpreview.github.io/?https://github.com/Somya545/muHVT/blob/master/vignettes/Predicting_Cells_and_Layers_using_predictLayerHVT.html) Contains descriptions of the functions used for monitoring entities over time using a predictive set of HVT maps.
