
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
#> meanBeta                0.66179  0.77400  0.58864  0.60690
#> meanBias               -0.03136  0.08086 -0.10451 -0.08625
#> meanError               0.15436  0.19719  0.17106  0.14577
#> relativeBias           -0.04524  0.11665 -0.15077 -0.12443
#> coverage                1.00000  1.00000  1.00000  1.00000
#> stdError                0.26337  0.23588  0.23379  0.25115
#> MC standard deviation   0.04087  0.04436  0.03274  0.04167
#> MC MSE                  0.00265  0.00851  0.01199  0.00918
#> MC relative efficiency  1.00000  0.31205  0.22130  0.28928
## 1000 samples generated in each iteration with missing rate 0.2 for each variable under MAR assumption 
## with 10 iterations
sim_MAR(n = 1000, NSIM = 10, missRate = 0.4, trueValue = log(2), cores = 3)
#>                         MethodA  MethodB  MethodC  MethodD
#> trueValue               0.69315  0.69315  0.69315  0.69315
#> # sims                 10.00000 10.00000 10.00000 10.00000
#> # invalid               0.00000  0.00000  0.00000  0.00000
#> meanBeta                0.58138  0.84987  0.62327  0.67667
#> meanBias               -0.11177  0.15672 -0.06988 -0.01648
#> meanError               0.29015  0.26366  0.29840  0.23886
#> relativeBias           -0.16125  0.22610 -0.10082 -0.02377
#> coverage                0.90000  0.90000  0.80000  1.00000
#> stdError                0.35719  0.27400  0.29346  0.32418
#> MC standard deviation   0.13767  0.10389  0.16819  0.08701
#> MC MSE                  0.03145  0.03535  0.03317  0.00784
#> MC relative efficiency  1.00000  0.88944  0.94794  4.00994
```
