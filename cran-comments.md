## R CMD check results
Duration: 1m 28.1s

❯ checking installed package size ... NOTE
    installed size is 12.3Mb
    sub-directories of 1Mb or more:
      doc  11.9Mb

❯ checking top-level files ... NOTE
  File
    LICENSE
  is not mentioned in the DESCRIPTION file.
  Non-standard file/directory found at top level:
    ‘README.html’

❯ checking files in ‘vignettes’ ... NOTE
  Files named as vignettes but with no recognized vignette engine:
     ‘vignettes/Predicting_Cells_with_Layers_using_predictLayerHVT.Rmd’
  (Is a VignetteBuilder field missing?)

❯ checking for unstated dependencies in vignettes ... NOTE
  '::' or ':::' imports not declared from:
    ‘DT’ ‘gridExtra’ ‘gtable’ ‘htmlwidgets’ ‘installr’ ‘skimr’ ‘tibble’
  'library' or 'require' calls not declared from:
    ‘devtools’ ‘installr’

0 errors ✔ | 0 warnings ✔ | 4 notes ✖