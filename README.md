# ![AEDA](https://github.com/ptl93/AEDA/blob/tle_vignette/man/images/AEDA_logo.png) Automated Exploratory Data Analysis 
***

[![Build Status](https://travis-ci.org/ptl93/AEDA.svg?branch=master)](https://travis-ci.org/ptl93/AEDA)
## Description

Writing exploratory data analysis (EDA) scripts helps in extracting valuable information from the data but can be very time consuming. Often people are producing the same tables and figures again and again which could be automatized with EDA scripts. <br>
<br>
This package should help automating the process of creating an EDA report by providing functions which, one would normally script for each data type of dataset. Therefore we provide following functionalites:

1. **Basic Data Summary**
2. **Categorical Data Summary**
3. **Numeric Data Summary**
4. **Cluster Analysis**
5. **Principal Component Analysis**
6. **Multidimensional Scaling Analysis**
7. **Exploratory Factor Analysis**

## Installation
```R
# Install the development version from GitHub
devtools::install_github("ptl93/AEDA")
```

## Examples

```R
#load library
library(AEDA)
data("survey", package = "MASS")

#get data types
data.types = getDataType(data = survey, target = "Exer")
print(data.types)
#there are integer, numerical and categorical columns in the dataset.

######### AEDA Pipeline: long version #########
##### This pipeline should be conducted, if the user wants to modify parameters for the analysis reports

###A - Basic Report
#create task
basic.report.task = makeBasicReportTask(id = "students.survey", data = survey, target = "Exer")
#create report
basic.report = makeReport(basic.report.task)


###B - Categorical Data Summary
#create task
cat.sum.task = makeCatSumTask(id = "students.survey", data = survey, target = "Exer",
  position = "stack")
#compute analysis
cat.sum = makeCatSum(cat.sum.task)
#create report
cat.sum.report = makeReport(cat.sum)


###C - Numeric Data Summary
#create task
num.sum.task = makeNumSumTask(id = "students.survey", data = survey, target = "Exer",
  geom.hist.args = list(bins = 20L, alpha = 0.8))
#compute analysis
num.sum = makeNumSum(num.sum.task)
#create report
num.sum.report = makeReport(num.sum)


###D - Cluster Analysis
#create task
cluster.task = makeClusterTask(id = "students.survey", data = survey,
  method = "cluster.pam")
#compute analysis
cluster.analysis = makeClusterAnalysis(cluster.task)
#create the report
cluster.report = makeReport(cluster.analysis)


###E - Principal Component Analysis
#create task
pca.task = makePCATask(id = "students.survey", data = survey,
  center = TRUE)
#compute analysis
pca.result = makePCA(pca.task)
#create report
pca.report = makeReport(pca.result)


###F - Multidimensional Scaling Analysis
#create task
mds.task = makeMDSTask(id = "students.survey", data = survey,
  method = "isoMDS", par.vals = list(maxit = 100L))
#compute analysis
mds.result = makeMDSAnalysis(mds.task)
#create report
mds.report = makeReport(mds.result)


###G - Exploratory Factor Analysis
#create task
fa.task = makeFATask(id = "students.survey", data = survey,
  rotate = "varimax", par.vals = list(max.iter = 20L))
#compute analysis
fa.result = makeFA(fa.task)
#create report
fa.report = makeReport(fa.result)

###H - create the HTML-report
finishReport(basic.report, cat.sum.report, num.sum.report, cluster.report,
  pca.report, mds.report, fa.report)
``` 
