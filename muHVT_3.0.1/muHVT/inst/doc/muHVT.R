## ----setup, warning = FALSE, include = FALSE----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "672px",
  out.height = "480px",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  fig.retina = 1,
  dpi = 150
)

## ----predictlayer_flow,echo=FALSE,warning=FALSE,fig.show='hold',message=FALSE,out.width='90%',fig.height=8,fig.cap='Figure 1:  Heatmap Visualization of a Torus with 900 Cells'----
knitr::include_graphics('./torus2.png')

## ----mlayer_flow,echo=FALSE,warning=FALSE,fig.show='hold',message=FALSE,out.width='90%',fig.height=8,fig.cap='Figure 2: Flow diagram for predicting based on a sequence of maps using predictLayerHVT()'----
#knitr::include_graphics('./mlayer1.png')
knitr::include_graphics('./predictLayerHVT_function.png')

