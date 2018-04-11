# ![AEDA](https://github.com/ptl93/AEDA/blob/tle_vignette/man/images/AEDA_logo.png) Automated Exploratory Data Analysis in R
***

[![Build Status](https://travis-ci.org/ptl93/AEDA.svg?branch=master)](https://travis-ci.org/ptl93/AEDA)
## Description

Writing exploratory data analysis (EDA) scripts helps in extracting valuable information from the data but can be very time consuming. Often people are producing the same tables and figures again and again which could be automatized with EDA scripts. <br>
<br>
This package should help automating the process of creating an EDA report by providing functions which, one would normally script for each data type of dataset. Therefore we provide following functionalites:

1. **Basic Data Summary**
2. **Categorical Data Summary**
3. **Numeric Data Summary**
4. **Corellation Analysis**
5. **Cluster Analysis**
6. **Principal Component Analysis**
7. **Multidimensional Scaling Analysis**
8. **Exploratory Factor Analysis**

## Installation
```R
# Install the development version from GitHub
devtools::install_github("ptl93/AEDA", build_vignettes = TRUE)
```

## Examples
In the following 2 sub sections we will show how you can simply conduct a exploratory data analysis.  
In general we provide the functionalites/subreports 1-8 listed above, which the "fast"-version calls with its default parameter arguments provided by us.  
The second version gives you, as user of the package, the freedom to choose between different methods, e.g in cluster analysis, instead of the default k-means algorithm, you might want to choose a hierarchical clustering for your data set, etc.

### Create a fast report

#### `fastReport()`
With the `fastReport()` function you can create a full EDA report for a data set stored in your current R environment with two lines of code.  
```r 
#load library
library(AEDA)
data("survey", package = "MASS")
fastReport(data = survey, target = "Exer")
```
After executing the last line, you should see a **MainReport.rmd** file in your current directory and a subdirectory **Data_Report/** which has all subreport rmd files and analysis result stored as .rds files :
<img src="https://github.com/ptl93/AEDA/blob/master/man/tutorial/0_childRMDs.PNG" width="500" height="400" />

In order to render the final EDA HTML-report simply run `rmarkdown::render("MainReport.rmd")` or open the MainReport.rmd file and hit the knitr button, if you use RStudio.

#### `openMLReport()`
With the function `openMLReport()` you can create a full EDA Report for a data set stored in the [openML Database](https://www.openml.org/search?type=data). The approach is similar to the `fastReport()` call above.
```r 
openMLReport(data.id = 61L) 
#data.id = 61L is the iris data set in the openML database
rmarkdown::render("MainReport.rmd")
```

### Create a customized `AEDA`-Report
In order to select different methods for each report we provide the user the possibility to choose between several methods and algorithms. In general, to conduct a `AEDA`-Pipeline for each step 3 functions need to be called (except for the basic data summary):

* `my.task = make*Task()`
* `my.analysis = make*Analysis()`
* `my.report = makeReport(my.analysis)`

Or since these three multiple function calls do not provide much additional
functionality, if the user does not modify the parameters for a few reports in the `AEDA`-Pipeline, there is a shortcut to get a report:
* `my.report = create*Report()`


In the following code chunk we will show you how to modify the automated exploratory data analysis:

### Create a customized report using `AEDA`-Pipeline

``` r
#load library
library(AEDA)
data("survey", package = "MASS")

#get data types
data.types = getDataType(data = survey, target = "Exer")
print(data.types)
#there are integer, numerical and categorical columns in the dataset.

######### AEDA Pipeline: long version #########
##### This pipeline should be conducted, if the user wants to modify parameters for the analysis reports

###1 - Basic Report
#create task
basic.report.task = makeBasicReportTask(id = "students.survey", data = survey, target = "Exer")
#create report
basic.report = makeReport(basic.report.task)


###2 - Categorical Data Summary
#create task
cat.sum.task = makeCatSumTask(id = "students.survey", data = survey, target = "Exer",
  position = "stack")
#compute analysis
cat.sum = makeCatSum(cat.sum.task)
#create report
cat.sum.report = makeReport(cat.sum)


###3 - Numeric Data Summary
#create task
num.sum.task = makeNumSumTask(id = "students.survey", data = survey, target = "Exer",
  geom.hist.args = list(bins = 20L, alpha = 0.8))
#compute analysis
num.sum = makeNumSum(num.sum.task)
#create report
num.sum.report = makeReport(num.sum)


###4 - Correlation Analysis
#Since we do not modify the default paramters we will call the shortcut version:
corr.report = createCorrReport(data = survey)


###5 - Cluster Analysis
#create task
cluster.task = makeClusterTask(id = "students.survey", data = survey,
  method = "cluster.h", par.vals = list(method = "average"))
#compute analysis
cluster.analysis = makeClusterAnalysis(cluster.task)
#create the report
cluster.report = makeReport(cluster.analysis)


###6 - Principal Component Analysis
#create task
pca.task = makePCATask(id = "students.survey", data = survey, target = "Exer",
  center = TRUE)
#compute analysis
pca.result = makePCA(pca.task)
#create report
pca.report = makeReport(pca.result)


##7 - Multidimensional Scaling Analysis
#create task
mds.task = makeMDSTask(id = "students.survey", data = survey,
  method = "isoMDS", par.vals = list(maxit = 100L))
#compute analysis
mds.result = makeMDSAnalysis(mds.task)
#create report
mds.report = makeReport(mds.result)


###8 - Exploratory Factor Analysis
#create task
fa.task = makeFATask(id = "students.survey", data = survey,
  rotate = "varimax", par.vals = list(max.iter = 20L))
#compute analysis
fa.result = makeFA(fa.task)
#create report
fa.report = makeReport(fa.result)


###9 - create the HTML-report
finishReport(basic.report, cat.sum.report, num.sum.report, corr.report,
  cluster.report, pca.report, mds.report, fa.report)
  
  
###10 - render the final HTML-report
rmarkdown::render("MainReport.rmd")
``` 

For more information, you can check out our [Wiki](https://github.com/ptl93/AEDA/wiki).

## Contributing
We are very happy about feedback and contributions from you in order to improve this package.

### Issue
If you believe that our package lacks several analysis steps or should enhance more methods/algorithms for each report, do not hesitate and let us know by opening a [new issue](https://github.com/ptl93/AEDA/issues).

### Contribute
In case you want to contribute please go after our styleguide. We are following the styleguide from [mlr](https://github.com/rdatsci/PackagesInfo/wiki/R-Style-Guide). In general, we follow the "fork-and-pull" Git workflow.

1. **Fork** the repo on GitHub
2. **Clone** the project to your own machine
3. **Commit** changes to your own branch
4. **Push** your work back up to your fork
5. **Submit** a Pull request so that we can review your changes
NOTE: Be sure to merge the latest from "upstream" before making a pull request!
