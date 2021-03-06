---
title: "Rspc user guide"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Rspc quick guide}
  %\VignetteEncoding{UTF-8}
---




## SPC quick guide

Rspc is an implementation of nelson rules for control charts in R. The RSPC package implements some Statistical Process Control methods, namely Levey-Jennings type of I (individuals) chart, Shewhart C (count) chart and Nelson rules. Typical workflow is taking the time series, specify the control limits, and list of Nelson rules you want to evaluate. There are several options how to modify the rules (one sided limits, numerical parameters of rules, etc.). Package is also capable of calculating the control limits from the data (so far only for i-chart and c-chart are implemented).


## Instalation

So far the package is not at CRAN. Only way to install it is from the archive file:


```r
path = "path_to_the_archive/Rspc_x.x.x.tar.gz" # set the path to the archive
install.packages(path, repos = NULL, type="source") # install package
```

## Functions

* `EvaluateRules()`

* `SetParameters()`

* `CalculateLimits()`

* `CalculateZoneBorders()`

* `NelsonRules()`

* `Rule1()-Rule8()`

There are two main functions: EvaluateRules() and SetParameters(). The first one starts the evaluation, the second one could be used for customizing the rules 


### EvaluateRules()

This function evaluates the Nelson rules (1 through 8) selected by the user. It uses output from the CalculateLimits function. 


```r
#load the package
library(Rspc)
# generate some data
data <- rnorm(10) 
# evaluate all the Nelson rules, 
# calculate control limits from data (lcl, cl, ucl are not provided) using formulas for i-chart, 
# don't modify any rule parameters (parRules = NULL)
EvaluateRules(x = data, type = 'i', whichRules = 1:8, lcl = NA, cl = NA, ucl = NA)
```

```
##              x Rule1 Rule2 Rule3 Rule4 Rule5 Rule6 Rule7 Rule8
## 1   0.62311609     0     0     0     0     0     0     0     0
## 2  -0.34180284     0     0     0     0     0     0     0     0
## 3   1.42707928     0     0     0     0     0     0     0     0
## 4   0.70475183     0     0     0     0     0     0     0     0
## 5  -0.99742255     0     0     0     0     0     0     0     0
## 6   0.94982101     0     0     0     0     0     0     0     0
## 7  -0.80192158     0     0     0     0     0     0     0     0
## 8   1.24992489     0     0     0     0     0     0     0     0
## 9  -0.01080232     0     0     0     0     0     0     0     0
## 10 -0.51299975     0     0     0     0     0     0     0     0
```

The vector 'x' is the only compulsory parameter, default values for the rest is as above. The code above is equivalent to data = rnorm(100); EvaluateRules(x = data).

### SetParameters()

Optional parameters are generated using SetParameters() function.
Function provides flexibility to change some default settings in the Rules 1-8 functions, such as the number of data points matching a non-random pattern for identification of run rules violations, or whether to use ‘jmp’ or ‘minitab’ convention for rules 3 and 4. 


```r
pars = SetParameters()
print(pars)
```

```
## $Rule1
## $Rule1$sides
## [1] "two-sided"
## 
## 
## $Rule2
## $Rule2$nPoints
## [1] 9
## 
## 
## $Rule3
## $Rule3$nPoints
## [1] 6
## 
## $Rule3$convention
## [1] "jmp"
## 
## $Rule3$equalBreaksSeries
## [1] TRUE
## 
## 
## $Rule4
## $Rule4$nPoints
## [1] 14
## 
## $Rule4$convention
## [1] "jmp"
## 
## 
## $Rule5
## $Rule5$minNPoints
## [1] 2
## 
## $Rule5$nPoints
## [1] 3
## 
## 
## $Rule6
## $Rule6$minNPoints
## [1] 4
## 
## $Rule6$nPoints
## [1] 5
## 
## 
## $Rule7
## $Rule7$nPoints
## [1] 15
## 
## 
## $Rule8
## $Rule8$nPoints
## [1] 8
```

Detailed information about optional pamateres are written in package documentation. If you want to change the number of consecutive points for this rule3 from 6 to 5 ...



```r
pars$Rule3$nPoints = 5
```

Modified **pars** is then plugged to **EvaluateRules(…,parRules = pars)** function call.

### CalculateLimits()

If no predefined limits are entered, this function calculates k sigma limits from the center line, where k can take the values 2 or 3 (3 by default) as defined by user, the sigma is calculated as the standard deviation of the numeric data values, and the center line is calculated as the average of the numeric data values. Providing a center line as input overrides calculation of the mean from the input data. Providing any of the limits as input overrides calculation of the standard deviation from the input data.


```r
CalculateLimits(x = rnorm(10), lcl = NA, cl = 100, ucl = NA, type = 'i')
```

```
## $ucl
## [1] 101.991
## 
## $cl
## [1] 100
## 
## $lcl
## [1] 98.00895
```


### CalculateZoneBorders()

This function determines the values at ±1 sigma, ±2 sigma and ±3 sigma that define the zones required for the evaluation of some Nelson Rules. It requires the input from the CalculateLimits function.


```r
limits = CalculateLimits(x = rnorm(10), lcl = NA, cl = NA, ucl = NA, type = 'i')
CalculateZoneBorders(limits, controlLimitDistance = 3)
```

```
##         -3         -2         -1          0          1          2 
## -1.7783225 -0.9752511 -0.1721798  0.6308916  1.4339629  2.2370343 
##          3 
##  3.0401056
```
Limits is object created by CalculateLimits() function.

### NelsonRules()

Wrapper function to evaluate the Rule 1-Rule 8 functions

### Rule1()-Rule8()

These functions detect and flag violation of Nelson rules test 1 through 8 on numeric data values, relative to the center line and limits as passed by the CalculateLimits function, and delimited zones as passed by the CalculateZoneBorders function.  For definition of the Nelson Rules, see Appendix 1. These functions are all called within the NelsonRules function.

### For more information about parameters read the package manual. 











