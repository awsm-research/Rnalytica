# Rnalytica [![Build Status](https://travis-ci.org/awsm-research/Rnalytica.svg?branch=master)](https://travis-ci.org/awsm-research/Rnalytica)

An R package of JIRA defect datasets and tool suites for explainable software analytics.

### Download and Reference for the JIRA defect datasets

[JIRA defect datasets](https://github.com/awsm-research/Rnalytica/blob/master/jira-defect-datasets.zip?raw=true)

the JIRA defect datasets can be referenced as:

```tex
@inproceedings{yatish2019icse,
    Author={Yatish, Suraj and Jiarpakdee, Jirayus and Thongtanunam, Patanamon and Tantithamthavorn, Chakkrit},
    Title = {Mining Software Defects: Should We Consider Affected Releases?},
    Booktitle = {The International Conference on Software Engineering (ICSE)},
    Year = {2019}
}
```

### Install an Rnalytica R package
To prepare execution enrionment, please run the command below in terminal.

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
