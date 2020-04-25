# Tools for prior sensitivity analysis with Bayesian MCMC models

The package BayeSens provides functions for non-parametric calculation of the Hellinger distance, parametric calculation of the Hellinger distance; data cloning ; and calculating posterior shrinkage . Refer to the vignette for more help. (type once the package is loaded, type vignette('BayeSens') in R).  

To install this package open R and type:
`install.packages("devtools")`  
`devtools::install_github("cbrown5/BayeSens")`  

Or if you want the vignettes:

`devtools::install_github("cbrown5/BayeSens", build_vignettes = TRUE)`  
(this option requires running a couple of mixing models, but they are short runs)

To get started check out the vignette: 
`vignette("BayeSens")`

If you want to apply this tool for isotope mixing models, then see
`vignette("isotope-mixing-example")`

To cite this package please cite the paper ["Quantifying learning in biotracer studies" (Brown,et al. Oecologia 2018)](https://link.springer.com/article/10.1007/s00442-018-4138-y), or see `citation("BayeSens"). 