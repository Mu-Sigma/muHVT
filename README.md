# HVT: Collection of functions used to build hierarchical topology preserving maps

#### Zubin Dowlaty

#### 2023-11-17

<div id="TOC">

*   [<span class="toc-section-number">1</span> Abstract](#abstract)
*   [<span class="toc-section-number">2</span> Version History](#version-history)
    *   [<span class="toc-section-number">2.1</span> HVT (v23.11.02) | What’s New?](#hvt-(v23.11.02)-whats-new)
    *   [<span class="toc-section-number">2.2</span> HVT (v22.12.06)](#hvt-(v22.12.06))
*   [<span class="toc-section-number">3</span> Installation of HVT (v23.11.02)](#installation-of-hvt-(v23.11.02))
*   [<span class="toc-section-number">4</span> Vignettes](#vignettes)
    *   [<span class="toc-section-number">4.1</span> HVT Vignette](#hvt-vignette)
    *   [<span class="toc-section-number">4.2</span> HVT Model Diagnostics Vignette](#hvt-model-diagnostics-vignette)
    *   [<span class="toc-section-number">4.3</span> HVT : Predicting Cells with Layers using predictLayerHVT ](#hvt---predicting-cells-with-layers-using-predictLayerHVT)
    *   [<span class="toc-section-number">4.4</span> HVT - Time Series data visualization using Flowmap ](#hvt---timeseries-data-visualization-using-flowmap)

</div>

<div id="abstract" class="section level1" number="1">

# <span class="header-section-number">1</span> Abstract

The HVT package is a collection of R functions to facilitate building [topology preserving maps](https://users.ics.aalto.fi/jhollmen/dippa/node9.html#:~:text=The%20property%20of%20topology%20preserving,tool%20of%20high%2Ddimensional%20data) for rich multivariate data analysis, see `Figure 1` as an example of a 2D torus map generated from the package. Tending towards a big data preponderance, a large number of rows. A collection of R functions for this typical workflow is organized below:

1.  **Data Compression**: Vector quantization (VQ), HVQ (hierarchical vector quantization) using means or medians. This step compresses the rows (long data frame) using a compression objective.

2.  **Data Projection**: Dimension projection of the compressed cells to 1D,2D or 3D with the Sammons Non-linear Algorithm. This step creates topology preserving map (also called an [embedding](https://en.wikipedia.org/wiki/Embedding)) coordinates into the desired output dimension. 

3.  **Tessellation**: Create cells required for object visualization using the Voronoi Tessellation method, package includes heatmap plots for hierarchical Voronoi tessellations (HVT). This step enables data insights, visualization, and interaction with the topology preserving map useful for semi-supervised tasks.

4.  **Prediction**: Scoring new data sets and recording their assignment using the map objects from the above steps, in a sequence of maps if required.


The HVT package allows creation of visually stunning tessellations, showcasing the power of topology preserving maps. Below is an image depicting a captivating tessellation of a torus, see [vignette](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/HVT.html) for more details.

<img src="https://github.com/Mu-Sigma/HVT/blob/master/vignettes/torus.png" width="642px" height="440px" />
<p class="caption">
Figure 1: The Voronoi tessellation for layer 1 and number of cells 900 with the heat map overlaid for variable z.
</p>


</div>

<div id="version-history" class="section level1" number="2">

# <span class="header-section-number">2</span> Version History 

<div id="hvt-(v23.11.02)-whats-new" class="section level2" number="2.1">

## HVT (v23.11.02) | What’s New? 

17th November, 2023

In this version of HVT package, the following new features have been introduced:

This package provides  functionality to predict cells with layers based on a sequence of maps using `predictLayerHVT`. 
</div>

<div id="hvt-(v22.12.06)" class="section level2" number="2.2">

## HVT (v22.12.06) 

06th December, 2022

This package provides functionality to predict based on a sequence of maps.

The creation of a predictive set of maps involves three steps -

1.  **Compress:** Compress the dataset using a percentage compression rate and a quantization threshold using the HVT() function (Map A).
2.  **Remove novelty cells:** Manually identify and remove the novelty cells from the dataset using the removeNovelty() function (Map B).
3.  **Compress the dataset without novelty:** Again, compress the dataset without novelty using n_cells, depth and a quantization threshold using the HVT() function (Map C).


Let us try to understand the steps with the help of the diagram below -

<img src="https://github.com/Mu-Sigma/HVT/blob/master/vignettes/predictLayerHVT_function.png" width="672px" height="480px" />
<p class="caption">
Figure 2: Flow diagram for predicting based on a sequence of maps using predictLayerHVT()
</p>



<div id="installation-of-hvt-(v23.11.02)" class="section level2" number="3">

# <span class="header-section-number">3</span> Installation of HVT (v23.11.02)

<div class="sourceCode" id="cb1">

    library(devtools)
    devtools::install_github(repo = "Mu-Sigma/HVT")

</div>

</div>


</div>

<div id="vignettes" class="section level1" number="4">

# <span class="header-section-number">4</span> Vignettes

Following are the links to the vignettes for the HVT package:

<div id="hvt-vignette" class="section level2" number="4.1">

## <span class="header-section-number">4.1</span> HVT Vignette

[**HVT Vignette:**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/HVT.html) Contains descriptions of the functions used for vector quantization and construction of hierarchical voronoi tessellations for data analysis.

</div>

<div id="hvt-model-diagnostics-vignette" class="section level2" number="4.2">

## <span class="header-section-number">4.2</span> HVT Model Diagnostics Vignette

[**HVT Model Diagnostics Vignette:**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/HVT_model_diagnostics_vignette.html) Contains descriptions of functions used to perform model diagnostics and validation for HVT model.

</div>

<div id="hvt---predicting-cells-with-layers-using-predictLayerHVT" class="section level2" number="4.3">

## <span class="header-section-number">4.3</span> HVT - Predicting Cells with Layers using predictLayerHVT

[**HVT : Predicting Cells with Layers using predictLayerHVT :**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/Predicting_Cells_with_Layers_using_predictLayerHVT.html) Contains descriptions of the functions used for predicting cells with layers based on a sequence of maps using predictLayerHVT.


<div id="hvt---timeseries-data-visualization-using-flowmap" class="section level2" number="4.4">

## <span class="header-section-number">4.4</span> HVT - Time Series data visualization using Flowmap

[**HVT : Time Series data visualization using Flowmap :**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/flowmap/vignettes/Flowmap_Vignette.html) Contains descriptions of the functions used for analysing timeseries data using the flowmap functions.
