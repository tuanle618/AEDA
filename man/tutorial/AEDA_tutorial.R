library(AEDA)
library(MASS)

data(VA)
#convert the status column as factor for tutorial
VA$status = as.factor(VA$status)
data.types = getDataType(VA, target = "status")
data.types

### 1 - Basic Report
##create task:
basic.report.task = makeBasicReportTask(id = "veteran", data = VA, target = "status")
##create the report:
basic.report = makeReport(basic.report.task)

##look into the created infos about the dataset:

#print the task
basic.report.task
#Task: veteran
#Dataset: VA
#Target: status
#Features: stime, status, treat, age, Karn, diag.time, cell, prior
#Observations: 137
#Missing Values: 0

#look into the computed basic summary
#basic.report$basic.data.summary
basic.report$na.summary
#There are no missing values in the dataset: VA

#example with missing values:
data("survey", package = "MASS")
basic.report.task2 = makeBasicReportTask(id = "students.survey", data = survey, target = "Exer")
basic.report.2 = makeReport(basic.report.task2)
##have a look at the na.df
basic.report.2$na.summary
#In total there are 107 NAs in the dataset: survey
#   feature num_missing pct_missing
#6    Pulse          45 0.189873418
#10  Height          28 0.118143460
#11     M.I          28 0.118143460
#1      Sex           1 0.004219409
#2   Wr.Hnd           1 0.004219409
#3   NW.Hnd           1 0.004219409
#4    W.Hnd           1 0.004219409
#7     Clap           1 0.004219409
#9    Smoke           1 0.004219409
#5     Fold           0 0.000000000
#8     Exer           0 0.000000000
#12     Age           0 0.000000000
#Printing image object with NAs according to observation index:
#Printing ggplot object according to number of missing values:

### 2 - Categorical Data Summary
#create task
cat.sum.task = makeCatSumTask(id = "veteran", data = VA, target = "status",
  position = "stack")
#compute analysis
cat.sum = makeCatSum(cat.sum.task)
#create report
cat.sum.report = makeReport(cat.sum)

#look into the created infos about the categorical variables
#e.g 2-D contingecy table
cat.sum.report$cat.sum$contg.list
#[[1]]
#     treat
#status   1   2 Sum
#0        5   4   9
#1       64  64 128
#Sum     69  68 137

#[[2]]
#      cell
#status   1   2   3   4 Sum
#0        4   3   1   1   9
#1       31  45  26  26 128
#Sum     35  48  27  27 137

##....
##....

#[[6]]
#    prior
#cell    0  10 Sum
#1      21  14  35
#2      37  11  48
#3      22   5  27
#4      17  10  27
#Sum    97  40 137

#created plots through provided multiplot function to arrange ggplots into a grid
multiplot(plotlist = cat.sum.report$cat.sum$plot.list, cols = 2L)
#will display a plot in RStudio plots-panel

###3 - Correlation Analysis

#create the task
corr.task = makeCorrTask(id = "veteran", data = VA)
#compute the analysis
corr.result = makeCorr(corr.task)
#create the report
corr.report = makeReport(corr.result)
#default method is "pearson"
corr.report$method
#print the correlation matrix
corr.report$corr.matrix

#The correlation report will create a circle shaped correlation plot:
library(GGally)
ggcorr(data = NULL, cor_matrix = corr.report$corr.matrix,
  geom = "circle", nbreaks = 10)

###4 - Numeric Data Summary
#create task
num.sum.task = makeNumSumTask(id = "veteran", data = VA, target = "status",
  geom.hist.args = list(bins = 25L, alpha = 0.5))
#compute analysis
num.sum = makeNumSum(num.sum.task)
#create report
num.sum.report = makeReport(num.sum)

#look into the created infos about the categorical variables
#numeric summary dataframe
num.sum.report$num.sum.df
#histogramm, kernel density, boxplot for stime variable
multiplot(plotlist = num.sum.report$num.sum.var$stime[-1], cols = 2L)
#will display a plot in RStudio plots-panel

###5 - Cluster Analysis
#create the first task, default k-means
cluster.task.1 = makeClusterTask(id = "veteran", data = VA)
#compute the first analysis
cluster.analysis.1 = makeClusterAnalysis(cluster.task.1)
#create the first report
cluster.report.1 = makeReport(cluster.analysis.1)

#create the second task, model-based clustering
cluster.task.2 = makeClusterTask(id = "veteran", data = VA, method = "cluster.mod")
#compute the second analysis
cluster.analysis.2 = makeClusterAnalysis(cluster.task.2)
#create the second report
cluster.report.2 = makeReport(cluster.analysis.2)

#compare clusters for kmeans and model-based with respect to "stime" and "age"
p1 = cluster.report.1$cluster.analysis$comb.cluster.list[[1]]$cluster.plot
p2 = cluster.report.2$cluster.analysis$comb.cluster.list[[1]]$cluster.plot

multiplot(plotlist = list(kmeans = p1, modbased = p2), cols = 2L)
#will display a plot in RStudio plots-panel

###6 - Principal Component Analysis
#create the task
pca.task = makePCATask(id = "veteran", data = VA, target = "status")
#compute the analysis
pca.result = makePCA(pca.task)
#create the report
pca.report = makeReport(pca.result)

#extract standard deviations for each principal component
pca.report$pca.result$pca.result$sdev
#[1] 158.00694  18.66417  10.71397  10.10507

#show scree- and scatterplot:
multiplot(plotlist = list(scree = pca.report$pca.result$plotlist$pca.scree,
  scatter = pca.report$pca.result$plotlist$pca.scatter.1), cols = 2L)
#will display a plot in RStudio plots-panel

###7 - Multidimensional Scaling Analysis

#create the first task
mds.task.1 = makeMDSTask(id = "veteran", data = VA)
#compute the first analysis
mds.analysis.1 = makeMDSAnalysis(mds.task.1)
#create the first report
mds.report.1 = makeReport(mds.analysis.1)

#create the second task
mds.task.2 = makeMDSTask(id = "veteran", data = VA, method = "isoMDS")
#compute the second analysis
mds.analysis.2 = makeMDSAnalysis(mds.task.2)
#create the second report
mds.report.2 = makeReport(mds.analysis.2)

#compare the two methods
mds.report.1$mds.analysis$mds.plot
mds.report.2$mds.analysis$mds.plot


###8 - Exploratory Factor Analysis
#create task
fa.task = makeFATask(id = "veteran", data = VA,
  rotate = "varimax", par.vals = list(max.iter = 40L))
#compute analysis
fa.result = makeFA(fa.task)
#create report
fa.report = makeReport(fa.result)

#have a look at the results:
psych::fa.diagram(fa.report$fa.result)


###9 - create the final EDA HTML-report
finishReport(basic.report, cat.sum.report, corr.report, num.sum.report,
  cluster.report.1, cluster.report.2, pca.report,
  mds.report.1, mds.report.2, fa.report)

###10 - render the final report
rmarkdown::render("MainReport.rmd")
