# Rnalytica [![Build Status](https://travis-ci.org/software-analytics/Rnalytica.svg?branch=master)](https://travis-ci.org/software-analytics/Rnalytica)

An R package of JIRA defect datasets and tool suites for explainable software analytics.

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

### Usage

To load the library:
```r
library(Rnalytica)
```

To list all 131 defect datasets:
```r
listDataset
```

To load a defect dataset from the Rnalytica R package:
```r
Data = loadDefectDataset('groovy-1_5_7','jira')
```

To visualize pair-wise correlations among input metrics and presents using a visualization of the hierarchical cluster analysis:
```r
plotVarClus(dataset = Data$data, metrics = Data$indep)
```

To automatically remove irrelevant metrics and mitigate correlated metrics with AutoSpearman:
```r
AutoSpearman(dataset = Data$data, metrics = Data$indep)
```