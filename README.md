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

### Example R Usage

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

### Example Python Usage (by calling R package)

```python
import rpy2
from rpy2.robjects.packages import importr
from rpy2.robjects import r, pandas2ri, StrVector
pandas2ri.activate()
import pandas as pd
Rnalytica = importr('Rnalytica')

features_names = ["CountDeclMethodPrivate","AvgLineCode","CountLine","MaxCyclomatic","CountDeclMethodDefault",
                  "AvgEssential","CountDeclClassVariable","SumCyclomaticStrict","AvgCyclomatic","AvgLine",
                  "CountDeclClassMethod","AvgLineComment","AvgCyclomaticModified","CountDeclFunction",
                  "CountLineComment","CountDeclClass","CountDeclMethod","SumCyclomaticModified",
                  "CountLineCodeDecl","CountDeclMethodProtected","CountDeclInstanceVariable",
                  "MaxCyclomaticStrict","CountDeclMethodPublic","CountLineCodeExe","SumCyclomatic",
                  "SumEssential","CountStmtDecl","CountLineCode","CountStmtExe","RatioCommentToCode",
                  "CountLineBlank","CountStmt","MaxCyclomaticModified","CountSemicolon","AvgLineBlank",
                  "CountDeclInstanceMethod","AvgCyclomaticStrict","PercentLackOfCohesion","MaxInheritanceTree",
                  "CountClassDerived","CountClassCoupled","CountClassBase","CountInput_Max","CountInput_Mean",
                  "CountInput_Min","CountOutput_Max","CountOutput_Mean","CountOutput_Min","CountPath_Max",
                  "CountPath_Mean","CountPath_Min","MaxNesting_Max","MaxNesting_Mean","MaxNesting_Min",
                  "COMM","ADEV","DDEV","Added_lines","Del_lines","OWN_LINE","OWN_COMMIT","MINOR_COMMIT",
                  "MINOR_LINE","MAJOR_COMMIT","MAJOR_LINE"]
    
data_train = pd.read_csv("datasets/activemq-5.0.0.csv")
X_train = data_train[features_names]

results = Rnalytica.AutoSpearman(dataset = X_train, metrics = rpy2.robjects.StrVector(features_names))
print(results)
```
```
['CountDeclMethodPrivate' 'CountDeclMethodDefault' 'AvgEssential'
 'CountDeclClassVariable' 'CountDeclClassMethod' 'AvgLineComment'
 'AvgCyclomaticModified' 'CountDeclClass' 'CountDeclMethodProtected'
 'CountDeclInstanceVariable' 'CountDeclMethodPublic' 'RatioCommentToCode'
 'AvgLineBlank' 'PercentLackOfCohesion' 'MaxInheritanceTree'
 'CountClassDerived' 'CountClassCoupled' 'CountClassBase'
 'CountInput_Mean' 'CountInput_Min' 'CountOutput_Min' 'MaxNesting_Min'
 'ADEV' 'OWN_LINE' 'OWN_COMMIT' 'MINOR_COMMIT' 'MAJOR_LINE']
 ```

