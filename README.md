# Rnalytica [![Build Status](https://travis-ci.org/software-analytics/Rnalytica.svg?branch=master)](https://travis-ci.org/software-analytics/Rnalytica)

An R package of the Miscellaneous Functions for Data Analytics Research

### Install
To prepare execution enrionment, please run the command below in terminal.

```
apt-get install r-base r-base-dev libcurl4-gnutls-dev libcurl4-openssl-dev libssl-dev
```

Then, install `Rnalytica` with the `devtools` R package:
```r
install.packages('devtools')
devtools::install_github('software-analytics/Rnalytica')
```

### Examples

To load a defect dataset from a collection of publicly-available defect datasets:
```r
library('Rnalytica')
Data = loadDefectDataset('eclipse-2.0')
```

To visualize pair-wise correlations among input metrics and presents using a visualization of the hierarchical cluster analysis:
```r
library('Rnalytica')
Data = loadDefectDataset('eclipse-2.0')
plotVarClus(dataset = Data$data, metrics = Data$indep, correlation = 'spearman', correlation.threshold = 0.7)
```

To automatically mitigate correlated metrics with AutoSpearman:
```r
library('Rnalytica')
Data = loadDefectDataset('eclipse-2.0')
AutoSpearman(dataset = Data$data, metrics = Data$indep, spearman.threshold = 0.7, vif.threshold = 5)
```