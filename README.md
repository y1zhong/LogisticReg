# LogisticReg
Re-implement glm() in stats package only focus on logistic regression in lrm()
<!-- badges: start -->
  [![R-CMD-check](https://github.com/y1zhong/LogisticReg/workflows/R-CMD-check/badge.svg)](https://github.com/y1zhong/LogisticReg/actions)

[![codecov](https://codecov.io/gh/y1zhong/LogisticReg/branch/main/graph/badge.svg?token=E6YPRIXBPO)](https://codecov.io/gh/y1zhong/LogisticReg)  
<!-- badges: end -->

## Installation
To install and use the package `LogisticReg`, download from GitHub and load in your environment
```r
devtools::install_github("y1zhong/LogisticReg", build_vignettes = T)
library(LogisticReg)
```

## Overview

The package contains three functions that help you fit a logistic regression model, evaluate model fitting and predict new data. It mimics the function of `glm()` but focus on the logistic model.
1. `lrm()`: main function to fit the model
2. `lrm.rsqr()`: calculate Cox & Snell and Nagelkerke's r-square
3. `lrm.predict()`: predict new outcome with fitted model  

For more detailed information on how to use function, check:
```r
browseVignettes("LogisticReg")
```

## Side Note
You may notice the commented out Rcpp code in the src file. It works perfectly fine on local machine and passed all build check with no warnings. This implementation improves the runtime of `lrm()`. But it cannot pass continuous integration because of gfortan settings on GitHub. For usage and installation, check https://github.com/y1zhong/LogisticRegRcpp for more information. 