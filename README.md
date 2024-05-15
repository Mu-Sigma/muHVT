# HVT: Collection of functions used to build hierarchical topology preserving maps

#### Zubin Dowlaty

#### 2024-05-02

<div id="TOC">

*   [<span class="toc-section-number">1.</span> Abstract](#abstract)
*   [<span class="toc-section-number">2.</span> Vignettes](#vignettes)
    *   [<span class="toc-section-number">2.1</span> HVT Vignette](#hvt-vignette)
    *   [<span class="toc-section-number">2.2</span> HVT Model Diagnostics Vignette](#hvt-model-diagnostics-vignette)
    *   [<span class="toc-section-number">2.3</span> HVT Scoring Cells with Layers using scoreLayeredHVT ](#hvt-scoring-cells-with-layers-using-scoreLayeredHVT)
    *   [<span class="toc-section-number">2.4</span> Temporal Analysis and Visualization: Leveraging Time Series Capabilities in HVT](#temporal-analysis-and-visualization-leveraging-time-series-capabilities-in-hvt)
*   [<span class="toc-section-number">3.</span> Version History](#version-history)
    *   [<span class="toc-section-number">3.3</span> HVT (v24.5.2) | What’s New?](#hvt-(v24.5.2)-whats-new)
    *   [<span class="toc-section-number">3.2</span> HVT (v23.11.02)](#hvt-(v23.11.02))
    *   [<span class="toc-section-number">3.1</span> HVT (v22.12.06)](#hvt-(v22.12.06))
*   [<span class="toc-section-number">4.</span> Installation of HVT (v24.5.2)](#installation-of-hvt-(v24.5.2))


</div>

<div id="abstract" class="section level1" number="1">

# <span class="header-section-number">1.</span> Abstract

The HVT package is a collection of R functions to facilitate building [topology preserving maps](https://users.ics.aalto.fi/jhollmen/dippa/node9.html#:~:text=The%20property%20of%20topology%20preserving,tool%20of%20high%2Ddimensional%20data) for rich multivariate data analysis, see `Figure 1` as an example of a 2D torus map generated from the package. Tending towards a big data preponderance, a large number of rows. A collection of R functions for this typical workflow is organized below:

1.  **Data Compression**: Vector quantization (VQ), HVQ (hierarchical vector quantization) using means or medians. This step compresses the rows (long data frame) using a compression objective.

2.  **Data Projection**: Dimension projection of the compressed cells to 1D,2D or Interactive surface plot with the Sammons Non-linear Algorithm. This step creates topology preserving map (also called an [embedding](https://en.wikipedia.org/wiki/Embedding)) coordinates into the desired output dimension. 

3.  **Tessellation**: Create cells required for object visualization using the Voronoi Tessellation method, package includes heatmap plots for hierarchical Voronoi tessellations (HVT). This step enables data insights, visualization, and interaction with the topology preserving map useful for semi-supervised tasks.

4.  **Scoring**: Scoring new data sets and recording their assignment using the map objects from the above steps, in a sequence of maps if required.

5. **Temporal Analysis and Visualization**: A Collection of new functions that leverages the capacity of the HVT package by analyzing time series data for its underlying patterns, calculation of transitioning probabilities and the visualizations for the flow of data over time.

The HVT package allows creation of visually stunning tessellations, showcasing the power of topology preserving maps. Below is an image depicting a captivating tessellation of a torus, see [vignette](https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/HVT_vignette.html) for more details.

<img src="https://raw.githubusercontent.com/Mu-Sigma/HVT/master/vignettes/torus2.png" width="642px" height="440px" />
<p class="caption">
Figure 1: The Voronoi tessellation for layer 1 and number of cells 900 with the heat map overlaid for variable z.
</p>


</div>

<div id="vignettes" class="section level1" number="2">

# <span class="header-section-number">2.</span> Vignettes

Following are the links to the vignettes for the HVT package:

<div id="hvt-vignette" class="section level2" number="2.1">

## <span class="header-section-number">2.1</span> HVT Vignette

[**HVT Vignette:**](https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/HVT_vignette.html) Contains descriptions of the functions used for vector quantization and construction of hierarchical voronoi tessellations for data analysis.

</div>

<div id="hvt-model-diagnostics-vignette" class="section level2" number="2.2">

## <span class="header-section-number">2.2</span> HVT Model Diagnostics Vignette

[**HVT Model Diagnostics Vignette:**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/HVT_model_diagnostics_vignette.html) Contains descriptions of functions used to perform model diagnostics and validation for HVT model.

</div>

<div id="hvt-scoring-cells-with-layers-using-scoreLayeredHVT" class="section level2" number="2.3">

## <span class="header-section-number">2.3</span> HVT Scoring Cells with Layers using scoreLayeredHVT

[**HVT Scoring Cells with Layers using scoreLayeredHVT :**](https://htmlpreview.github.io/?https://github.com/Mu-Sigma/HVT/blob/master/vignettes/Scoring_Cells_with_Layers_using_scoreLayeredHVT.html) Contains descriptions of the functions used for scoring cells with layers based on a sequence of maps using scoreLayeredHVT.

</div>

<div id="temporal-analysis-and-visualization-leveraging-time-series-capabilities-in-hvt" class="section level2" number="2.4">

## <span class="header-section-number">2.4</span> Temporal Analysis and Visualization: Leveraging Time Series Capabilities in HVT

[**Temporal Analysis and Visualization: Leveraging Time Series Capabilities in HVT :**](https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/HVT_Temporal_Analysis.html) Contains descriptions of the functions used for analyzing time series data and its flow maps.

</div>


<div id="version-history" class="section level1" number="3">

# <span class="header-section-number">3.</span> Version History 


<div id="hvt-(v24.5.2)-whats-new" class="section level2" number="3.1">

## HVT (v24.5.2) 

2nd May, 2024

In this version of HVT package, the following new features have been introduced:

1. **Updated Nomenclature:** To make the function names more consistent and understandable/intuitive, we have renamed the functions throughout the package. Given below are the few instances.

* `HVT` to `trainHVT`
* `predictHVT` to `scoreHVT`
* `predictLayerHVT` to `scoreLayeredHVT`

2. **Restructured Functions:** The functions have been rearranged and grouped into new sections which are highlighted on the index page of package’s PDF documentation. Given below are the few instances.

* `trainHVT` function now resides within the `Training_or_Compression` section.
* `plotHVT` function now resides within the `Tessellation_and_Heatmap` section.
* `scoreHVT` function now resides within the `Scoring` section.

3. **Enhancements:** The pre-existed functions, `hvtHmap` and `exploded_hmap`, have been combined and incorporated into the `plotHVT` function. Additionally, `plotHVT` now includes the ability to perform 1D plotting.

4. **Temporal Analysis** 
- The new update focuses on the integration of time series capabilities into the HVT package by extending its foundational operations to time series data which is emphasized in this vignette.
- The new functionalities are introduced to analyze underlying patterns and trends within the data, providing insights into its evolution over time and also offering the capability to analyze the movement of the data by calculating its transitioning probability and creates elegant plots and GIFs.

Below are the new functions and its brief descriptions:

- `plotStateTransition`: Provides the time series flowmap plot.
- `getTransitionProbability`: Provides a list of transition probabilities.
- `reconcileTransitionProbability`: Provides plots and tables for comparing transition probabilities calculated manually and from markovchain function.
- `plotAnimatedFlowmap`: Creates flowmaps and animations for both self state and without self state scenarios.

</div>



<div id="hvt-(v23.11.02)" class="section level2" number="3.2">

## HVT (v23.11.02) 

17th November, 2023

This version of HVT package offers functionality to score cells with layers based on a sequence of maps created using `scoreLayeredHVT`. Given below are the steps to created the successive set of maps.

1. **Map A** - The output of `trainHVT` function which is trained on parent data.

2. **Map B** - The output of `trainHVT` function which is trained on the 'data with novelty' created from `removeNovelty` function.

3. **Map C** - The output of `trainHVT` function which is trained on the 'data without novelty' created from `removeNovelty` function.

The `scoreLayeredHVT` function uses these three maps to score the test datapoints.

Let us try to understand the steps with the help of the diagram below

<img src="https://raw.githubusercontent.com/Mu-Sigma/HVT/master/vignettes/scoreLayeredHVT_function.png" width="672px" height="480px" />
<p class="caption">
Figure 2: Data Segregation for scoring based on a sequence of maps using scoreLayeredHVT()</p>



</div>

<div id="hvt-(v22.12.06)" class="section level2" number="3.3">

## HVT (v22.12.06) 

06th December, 2022

This version of HVT package offers features for both training an HVT model and eliminating outlier cells from the trained model.

1. **Training or Compression:** The initial step entails training the parent data using the `trainHVT` function, specifying the desired compression percentage and quantization error.

2. **Remove novelty cells:** Following the training process, outlier cells can be identified manually from the 2D hvt plot. These outlier cells can then be inputted into the `removeNovelty` function, which subsequently produces two datasets in its output: one containing 'data with novelty' and the other containing 'data without novelty'.




<div id="installation-of-hvt-(v24.5.2)" class="section level2" number="4">

# <span class="header-section-number">4.</span> Installation of HVT (v24.5.2)

<div class="sourceCode" id="cb1">

    library(devtools)
    devtools::install_github(repo = "Mu-Sigma/HVT")

</div>

</div>


</div>
