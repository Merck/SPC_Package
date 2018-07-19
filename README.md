# Rspc

## Overview

Rspc is an implementation of nelson rules for control charts in R. The RSPC package implements some Statistical Process Control methods, namely Levey-Jennings type of I (individuals) chart, Shewhart C (count) chart and Nelson rules. Typical workflow is taking the time series, specify the control limits, and list of Nelson rules you want to evaluate. There are several options how to modify the rules (one sided limits, numerical parameters of rules, etc.). Package is also capable of calculating the control limits from the data (so far only for i-chart and c-chart are implemented).

* `EvaluateRules()`
This function evaluates the Nelson rules (1 through 8) selected by the user. It uses output from the CalculateLimits function. 

* `SetParameters()`
Provides flexibility to change some default settings in the Rules 1-8 functions, such as the number of data points matching a non-random pattern for identification of run rules violations, or whether to use ‘jmp’ or ‘minitab’ convention for rules 3 and 4. 

* `CalculateLimits()`
If no predefined limits are entered, this function calculates k sigma limits from the center line, where k can take the values 2 or 3 (3 by default) as defined by user, the sigma is calculated as the standard deviation of the numeric data values, and the center line is calculated as the average of the numeric data values. Providing a center line as input overrides calculation of the mean from the input data. Providing any of the limits as input overrides calculation of the standard deviation from the input data.

* `CalculateZoneBorders()`
This function determines the values at ±1 sigma, ±2 sigma and ±3 sigma that define the zones required for the evaluation of some Nelson Rules. It requires the input from the CalculateLimits function.

* `NelsonRules()`
Wrapper function to evaluate the Rule 1-Rule 8 functions

* `Rule1()-Rule8()`
These functions detect and flag violation of Nelson rules test 1 through 8 on numeric data values, relative to the center line and limits as passed by the CalculateLimits function, and delimited zones as passed by the CalculateZoneBorders function.  For definition of the Nelson Rules, see Appendix 1. These functions are all called within the NelsonRules function.

## Installation

```r
# The easiest way to get Rspc is to install it from CRAN repository:
install.packages("Rspc")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("MSD/Rspc")
```
## Usage

```r
#load the package
library(Rspc)
# generate some data
data <- rnorm(10) 
# evaluate all the Nelson rules, 
# calculate control limits from data (lcl, cl, ucl are not provided) using formulas for i-chart, 
# don't modify any rule parameters (parRules = NULL)
EvaluateRules(x = data, type = 'i', whichRules = 1:8, lcl = NA, cl = NA, ucl = NA)
#The vector 'x' is the only compulsory parameter, default values for the rest is as above. 
#The code above is equivalent to data = rnorm(100); EvaluateRules(x = data).
```
