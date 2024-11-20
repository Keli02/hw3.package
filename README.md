# hw3.625 Package

## Overview

`hw3.625` is an R package designed to fit linear regression models and
provide detailed summaries of the model’s coefficients, residuals, and
R-squared values. This package aims to offer a user-friendly interface
for performing linear regression analysis, similar to the `lm()`
function.

## Installation

    devtools::install_github("Keli02/hw3.package", build_vignettes = T)

    ## Using GitHub PAT from the git credential store.

    ## Skipping install of 'hw3.625' from a github remote, the SHA1 (3ee0dfec) has not changed since last install.
    ##   Use `force = TRUE` to force installation

## Usage

    library(hw3.625)
    model <- hw3_lm(mpg ~ wt + hp * cyl, data = mtcars)
    print(model)

    ## Call:
    ## hw3_lm(formula = mpg ~ wt + hp * cyl, data = mtcars)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -3.3440391 -1.4143552 -0.6166422  1.2159983  4.2815252 
    ## 
    ## Coefficients:
    ##              Estimate   Std.Err t value Pr(>|t|)    
    ## (Intercept) 52.017520  4.916935  10.579 4.18e-11 ***
    ## wt          -3.119815  0.661322  -4.718 6.51e-05 ***
    ## hp          -0.163594  0.052122  -3.139  0.00408 ** 
    ## cyl         -2.742125  0.800228  -3.427  0.00197 ** 
    ## hp:cyl       0.018954  0.006645   2.852  0.00823 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.242 on 27 degrees of freedom
    ## Multiple R-squared:  0.8795 ,  Adjusted R-squared:  0.8616
    ## F-statistic: 49.25 on 3 and 27 DF,  p-value: 5.065e-12
