# muHVT: Collection of functions used to build hierarchical topology preserving maps

#### Zubin Dowlaty, Shubhra Prakash, Sangeet Moy Das, Shantanu Vaidya, Praditi Shah, Srinivasan Sudarsanam, Somya Shambhawi

#### 2023-06-07

<div id="TOC">

*   [<span class="toc-section-number">1</span> Abstract](#abstract)
*   [<span class="toc-section-number">2</span> Version History](#version-history)
    *   [<span class="toc-section-number">2.1</span> muHVT (v23.06.07) | What’s New?](#muhvt-(v23.06.07)-whats-new)
    *   [<span class="toc-section-number">2.2</span> muHVT (v22.12.06)](#muhvt-(v22.12.06))
*   [<span class="toc-section-number">3</span> Installation of muHVT (v23.06.07)](#installation-of-muhvt-(v23.06.07))
*   [<span class="toc-section-number">4</span> Vignettes](#vignettes)
    *   [<span class="toc-section-number">4.1</span> muHVT Vignette](#muhvt-vignette)
    *   [<span class="toc-section-number">4.2</span> muHVT Model Diagnostics Vignette](#muhvt-model-diagnostics-vignette)
    *   [<span class="toc-section-number">4.3</span> muHVT : Predicting Cells with Layers using predictLayerHVT ](#muhvt---predicting-cells-with-layers-using-predictLayerHVT)

</div>

<div id="abstract" class="section level1" number="1">

# <span class="header-section-number">1</span> Abstract

The muHVT package is a collection of R functions to facilitate building [topology preserving maps](https://users.ics.aalto.fi/jhollmen/dippa/node9.html#:~:text=The%20property%20of%20topology%20preserving,tool%20of%20high%2Ddimensional%20data) for rich multivariate data analysis, see `Figure 1` as an example of a 2D torus map generated from the package. Tending towards a big data preponderance, a large number of rows. A collection of R functions for this typical workflow is organized below:

1.  **Data Compression**: Vector quantization (VQ), HVQ (hierarchical vector quantization) using means or medians. This step compresses the rows (long data frame) using a compression objective.

2.  **Data Projection**: Dimension projection of the compressed cells to 1D,2D or 3D with the Sammons Non-linear Algorithm. This step creates topology preserving map (also called as [embedding](https://en.wikipedia.org/wiki/Embedding)) coordinates into the desired output dimension. 

3.  **Tessellation**: Create cells required for object visualization using the Voronoi Tessellation method, package includes heatmap plots for hierarchical Voronoi tessellations (HVT). This step enables data insights, visualization, and interaction with the topology preserving map useful for semi-supervised tasks.

4.  **Prediction**: Scoring new data sets and recording their assignment using the map objects from the above steps, in a sequence of maps if required.


The muHVT package allows creation of visually stunning tessellations, showcasing the power of topology preserving maps. below is an image depicting a captivating tessellation of a torus-

<img src="https://github.com/Somya545/muHVT/blob/master/vignettes/torus2.png" width="642px" height="440px" />
<p class="caption">
Figure 1:  Heatmap Visualization of a Torus with 900 Cells
</p>


</div>

<div id="Version History" class="section level1" number="2">

# <span class="header-section-number">2</span> Version History 

<div id="muHVT (v23.06.07)| What’s New?" class="section level1" number="2.1">

## <span class="header-section-number">2.1</span> muHVT (v23.06.07) | What’s New? 

07th June, 2023

In this version of muHVT package, the following new features have been introduced:

This package provides  functionality to predict cells with layers based on a sequence of maps using `predictLayerHVT`. 
</div>

<div id="muhvt-(v22.12.06)" class="section level1" number="2.2">

## <span class="header-section-number">2.2</span> muHVT (v22.12.06) 

06th December, 2022

This package provides functionality to predict based on a set of maps to monitor entities over time.

The creation of a predictive set of maps involves three steps -

1.  **Compress:** Compress the dataset using a percentage compression rate and a quantization threshold using the HVT() function (Map A).
2.  **Remove novelty cells:** Manually identify and remove the novelty cells from the dataset using the removeNovelty() function (Map B).
3.  **Compress the dataset without novelty:** Again, compress the dataset without novelty using n_cells, depth and a quantization threshold using the HVT() function (Map C).


Let us try to understand the steps with the help of the diagram below -

<img src="https://github.com/Somya545/muHVT/blob/master/vignettes/predictLayerHVT_function.png" width="672px" height="480px" />
<p class="caption">
Figure 2: Flow diagram for predicting based on a sequence of maps using predictLayerHVT()
</p>



<div id="installation-of-muhvt-(v23.06.07)" class="section level2" number="3">

# <span class="header-section-number">3</span> Installation of muHVT (v23.06.07)

<div class="sourceCode" id="cb1">

    library(devtools)
    devtools::install_github(repo = "Mu-Sigma/muHVT")

</div>

</div>


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

<div id="muhvt---predicting-cells-with-layers-using-predictLayerHVT" class="section level2" number="4.3">

## <span class="header-section-number">4.3</span> muHVT - Predicting Cells with Layers using predictLayerHVT

[**muHVT : Predicting Cells with Layers using predictLayerHVT :**](https://htmlpreview.github.io/?https://github.com/Somya545/muHVT/blob/master/vignettes/Predicting_Cells_with_Layers_using_predictLayerHVT.html) Contains descriptions of the functions used for predicting cells with layers based on a sequence of maps using predictLayerHVT.
