# HVT: Collection of functions used to build hierarchical topology preserving maps

#### Zubin Dowlaty

#### 2024-01-31

<div id="TOC">

*   [<span class="toc-section-number">1</span> Abstract](#abstract)
*   [<span class="toc-section-number">2</span> Vignettes](#vignettes)
    *   [<span class="toc-section-number">2.1</span> HVT Vignette](#hvt-vignette)
    *   [<span class="toc-section-number">2.2</span> HVT Model Diagnostics Vignette](#hvt-model-diagnostics-vignette)
    *   [<span class="toc-section-number">2.3</span> HVT Scoring Cells with Layers using scoreLayeredHVT ](#hvt-scoring-cells-with-layers-using-scoreLayeredHVT)
*   [<span class="toc-section-number">3</span> Version History](#version-history)
    *   [<span class="toc-section-number">3.3</span> HVT (v24.1.1) | What’s New?](#hvt-(v24.1.1)-whats-new)
    *   [<span class="toc-section-number">3.2</span> HVT (v23.11.02)](#hvt-(v23.11.02))
    *   [<span class="toc-section-number">3.1</span> HVT (v22.12.06)](#hvt-(v22.12.06))
*   [<span class="toc-section-number">4</span> Installation of HVT (v24.1.1)](#installation-of-hvt-(v24.1.1))


</div>

<div id="abstract" class="section level1" number="1">

# <span class="header-section-number">1</span> Abstract

The HVT package is a collection of R functions to facilitate building [topology preserving maps](https://users.ics.aalto.fi/jhollmen/dippa/node9.html#:~:text=The%20property%20of%20topology%20preserving,tool%20of%20high%2Ddimensional%20data) for rich multivariate data analysis, see `Figure 1` as an example of a 2D torus map generated from the package. Tending towards a big data preponderance, a large number of rows. A collection of R functions for this typical workflow is organized below:

1.  **Data Compression**: Vector quantization (VQ), HVQ (hierarchical vector quantization) using means or medians. This step compresses the rows (long data frame) using a compression objective.

2.  **Data Projection**: Dimension projection of the compressed cells to 1D,2D or Interactive Surface plot with the Sammons Non-linear Algorithm. This step creates topology preserving map (also called an [embedding](https://en.wikipedia.org/wiki/Embedding)) coordinates into the desired output dimension. 

3.  **Tessellation**: Create cells required for object visualization using the Voronoi Tessellation method, package includes heatmap plots for hierarchical Voronoi tessellations (HVT). This step enables data insights, visualization, and interaction with the topology preserving map useful for semi-supervised tasks.

4.  **Scoring**: Scoring new data sets and recording their assignment using the map objects from the above steps, in a sequence of maps if required.


The HVT package allows creation of visually stunning tessellations, showcasing the power of topology preserving maps. Below is an image depicting a captivating tessellation of a torus, see [vignette](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/HVT.html) for more details.

<img src="https://github.com/Mu-Sigma/HVT/blob/master/vignettes/torus.png" width="642px" height="440px" />
<p class="caption">
Figure 1: The Voronoi tessellation for layer 1 and number of cells 900 with the heat map overlaid for variable z.
</p>


</div>

<div id="vignettes" class="section level1" number="2">

# <span class="header-section-number">2</span> Vignettes

Following are the links to the vignettes for the HVT package:

<div id="hvt-vignette" class="section level2" number="2.1">

## <span class="header-section-number">2.1</span> HVT Vignette

[**HVT Vignette:**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/HVT.html) Contains descriptions of the functions used for vector quantization and construction of hierarchical voronoi tessellations for data analysis.

</div>

<div id="hvt-model-diagnostics-vignette" class="section level2" number="2.2">

## <span class="header-section-number">2.2</span> HVT Model Diagnostics Vignette

[**HVT Model Diagnostics Vignette:**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/HVT_model_diagnostics_vignette.html) Contains descriptions of functions used to perform model diagnostics and validation for HVT model.

</div>

<div id="hvt-scoring-cells-with-layers-using-scoreLayeredHVT" class="section level2" number="2.3">

## <span class="header-section-number">2.3</span> HVT Scoring Cells with Layers using scoreLayeredHVT

[**HVT Scoring Cells with Layers using scoreLayeredHVT :**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/Scoring_Cells_with_Layers_using_scoreLayeredHVT.html) Contains descriptions of the functions used for scoring cells with layers based on a sequence of maps using scoreLayeredHVT.



<div id="version-history" class="section level1" number="3">

# <span class="header-section-number">3</span> Version History 


<div id="hvt-(v24.1.1)-whats-new" class="section level2" number="3.1">

## HVT (v24.1.1) 

31st January, 2024

In this version of HVT package, the following new features have been introduced:

1. **Rename:** Renamed the functions.
2. **Reorganise:** Reorganised the functions into new sections.
3. **Modification:** Merged the HVT plots and Heatmap generation functions for 1D,2D and Interactive Surface plot.

</div>



<div id="hvt-(v23.11.02)" class="section level2" number="3.2">

## HVT (v23.11.02) 

17th November, 2023

In this version of HVT package, the following new features have been introduced:

This package provides  functionality to score cells with layers based on a sequence of maps using `scoreLayeredHVT`. 
</div>

<div id="hvt-(v22.12.06)" class="section level2" number="3.3">

## HVT (v22.12.06) 

06th December, 2022

This package provides functionality to score based on a sequence of maps.

The creation of a scored set of maps involves three steps -

1.  **Compress:** Compress the dataset using a percentage compression rate and a quantization threshold using the trainHVT() function (Map A).
2.  **Remove novelty cells:** Manually identify and remove the novelty cells from the dataset using the removeNovelty() function (Map B).
3.  **Compress the dataset without novelty:** Again, compress the dataset without novelty using n_cells, depth and a quantization threshold using the trainHVT() function (Map C).


Let us try to understand the steps with the help of the diagram below -

<img src="https://github.com/Mu-Sigma/HVT/blob/master/vignettes/predictLayerHVT_function.png" width="672px" height="480px" />
<p class="caption">
Figure 2: Flow diagram for scoring based on a sequence of maps using scoreLayeredHVT()
</p>



<div id="installation-of-hvt-(v24.1.1)" class="section level2" number="4">

# <span class="header-section-number">3</span> Installation of HVT (v24.1.1)

<div class="sourceCode" id="cb1">

    library(devtools)
    devtools::install_github(repo = "Mu-Sigma/HVT")

</div>

</div>


</div>
