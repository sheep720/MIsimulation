
<!-- README.md is generated from README.Rmd. Please edit that file -->
MIsimulation
============

The goal of MIsimulation is to replicate and extend the simulation study in Kontopantelis and Evangelos (2017)

Installation
------------

You can install the source package MIsimulation by changing the path to file with:

``` r
install.packages("/Users/mingyang/MIsimulation_0.1.0.tar.gz", repos = NULL, type="source")
```

Example
-------

This is a basic example on how to replicate the results in table 2 in Kontopantelis and Evangelos (2017):

``` r
library(MIsimulation)
## 1000 samples generated in each iteration with missing rate 0.2 for each variable under MCAR assumption 
## with 10 iterations
sim_MCAR(n = 1000, NSIM = 10, missRate = 0.2, trueValue = log(2), cores = 3)
#>                         MethodA  MethodB  MethodC  MethodD
#> trueValue               0.69315  0.69315  0.69315  0.69315
#> # sims                 10.00000 10.00000 10.00000 10.00000
#> # invalid               0.00000  0.00000  0.00000  0.00000
#> meanBeta                0.63083  0.68204  0.84081  0.74217
#> meanBias               -0.06231 -0.01111  0.14766  0.04902
#> meanError               0.24803  0.16606  0.20225  0.24458
#> relativeBias           -0.08990 -0.01603  0.21303  0.07072
#> coverage                1.00000  1.00000  1.00000  1.00000
#> stdError                0.26487  0.23658  0.23856  0.26621
#> MC standard deviation   0.08682  0.04725  0.03761  0.09413
#> MC MSE                  0.01142  0.00236  0.02322  0.01126
#> MC relative efficiency  1.00000  4.84763  0.49186  1.01398
## 1000 samples generated in each iteration with missing rate 0.2 for each variable under MAR assumption 
## with 10 iterations
sim_MAR(n = 1000, NSIM = 10, missRate = 0.4, trueValue = log(2), cores = 3)
#>                         MethodA  MethodB  MethodC  MethodD
#> trueValue               0.69315  0.69315  0.69315  0.69315
#> # sims                 10.00000 10.00000 10.00000 10.00000
#> # invalid               0.00000  0.00000  0.00000  0.00000
#> meanBeta                0.86438  0.95434  0.54452  0.72757
#> meanBias                0.17123  0.26119 -0.14863  0.03442
#> meanError               0.29023  0.27666  0.25723  0.27069
#> relativeBias            0.24703  0.37682 -0.21443  0.04966
#> coverage                1.00000  1.00000  1.00000  0.80000
#> stdError                0.35179  0.26985  0.30251  0.29128
#> MC standard deviation   0.08770  0.04055  0.08183  0.13328
#> MC MSE                  0.03701  0.06986  0.02879  0.01895
#> MC relative efficiency  1.00000  0.52975  1.28569  1.95330
```
