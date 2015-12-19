
# Boston Marathon Finish Time Predictions
# Harvard Stats E139 Fall 2015
# Nathaniel Burbank, Pooja Singh, David Wihl
#
# December 21, 2015

library(ggplot2)

# Common Functions


percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

create_folds = function (df, k){
  # Perform k-Fold Cross Validation and return average score
  # Inspired by Scikit-Learn's cross_val_score
  if (missing(k)) {
    k = 10
  }
  
  train1_start = train1_end = train2_start = train2_end = test_start = test_end = c()
  testsize = nrow(df) / k 
  for (i in 1:k){
    if (i < k){
      train1_start[i] = 1
      train1_end[i] = floor((k-i) * testsize)
    } else {
      train1_start[i] = 0
      train1_end[i] = 0
    }
    test_start[i] = max(ceiling((k-i) * testsize),1)
    test_end[i]   = floor((k-i+1) * testsize)
    train2_start[i] = 0
    train2_end[i] = 0
    if (i > 1){
      train2_start[i] = ceiling((k-i+1) * testsize)
      train2_end[i] = nrow(df)
    }
  }
  return (data.frame(train1_start, train1_end, train2_start, train2_end, test_start, test_end))
}

# we'll do 10-fold cross validation
k = 10 

# Read in the data for multiple years
dfm <- read.csv("Previous Boston Marathon study/BAA data.txt",header=T,sep=" ")
dfm$Age2014 = NULL # remove unneeded column which is mostly NA
ok = complete.cases(dfm)
dfm = dfm[ok,] # remove rows that have any NA values
times = as.matrix(dfm[,7:15], ncol=9)
dfm$totaltime = rowSums(times)
dfm<- dfm[c("totaltime","Age","Gender1F2M","K0.5","HalfMar")] # keep only columns we need
dfm$Gender1F2M = as.factor(dfm$Gender1F2M) # make gender into a factor
dfm = dfm[!is.na(dfm$totaltime), ]  # eliminate rows with no finish times
dfm = dfm[sample(nrow(dfm)),]  # in case the data is sorted, randomize the order
# Find mean finish time by gender
agg = aggregate(dfm$totaltime, by=list(dfm$Gender1F2M), FUN=mean)[2]
men = as.integer(agg$x[2])
women = as.integer(agg$x[1])
# Remove outliers (5k > 3 sigma from mean)
hist (dfm$K0.5, breaks=15, main="Distribution of 5k Split Times", xlab="5k Split Time (min)")
mean5k = mean(dfm$K0.5)
sd5k = sd(dfm$K0.5)
outliers5k = dfm$K0.5>(mean5k + 3*sd5k)
outliers5ksum = sum(outliers5k)
dfm.rows = nrow(dfm)
dfm = dfm[!outliers5k,]
malepct = table(dfm$Gender1F2M)[2]  / (table(dfm$Gender1F2M)[2] + table(dfm$Gender1F2M)[1])
# Create a set of k-fold sets for cross-validation
folds = create_folds(dfm,k)
score = c()

# Baseline regression
base.mod = lm(totaltime~.-HalfMar,data=dfm)

# get baseline score 
for (i in 1:k) {
  train = dfm[c(folds[i,"train1_start"]:folds[i,"train1_end"],folds[i,"train2_start"]:folds[i,"train2_end"]),]
  test = dfm[folds[i,"test_start"]:folds[i,"test_end"],]

  cvmodel = lm(totaltime~.-HalfMar,data=train)
  score[i] = sum((test$totaltime - predict(cvmodel,new=test))^2) / as.numeric(nrow(test))
}
score.mean.5k = mean(score)
summary(base.mod)
# Half Marathon Baseline regression
base.mod.half = lm(totaltime~.-K0.5,data=dfm)

for (i in 1:k) {
  train = dfm[c(folds[i,"train1_start"]:folds[i,"train1_end"],folds[i,"train2_start"]:folds[i,"train2_end"]),]
  test = dfm[folds[i,"test_start"]:folds[i,"test_end"],]

  cvmodel = lm(totaltime~.-K0.5,data=train)
  score[i] = sum((test$totaltime - predict(cvmodel,new=test))^2) / as.numeric(nrow(test))
}
score.mean.half = mean(score)
dfm$HalfMar = NULL # Remove half marathon column as it is no longer needed
# Display histograms of response variable untransformed and log transformed
par(mfrow=c(1,2))
hist(dfm$totaltime,breaks=50, main="Untransformed", xlab="Finish time (min)")
hist(log(dfm$totaltime),breaks=50, main="Log Transformed", xlab="Finish time (min)")
par(mfrow=c(2,2))
plot(base.mod, pch=23 ,bg="seashell",cex=.8)
# Try regression on log(response)
dfm$logtotaltime = log(dfm$totaltime)
folds = create_folds(dfm,k)
score = c()

tx.mod = lm(logtotaltime~.-totaltime,data=dfm)

for (i in 1:k) {
  train = dfm[c(folds[i,"train1_start"]:folds[i,"train1_end"],folds[i,"train2_start"]:folds[i,"train2_end"]),]
  test = dfm[folds[i,"test_start"]:folds[i,"test_end"],]

  cv.tx.mod = lm(logtotaltime~.-totaltime,data=train)
  score[i] = sum((test$totaltime - exp(predict(cv.tx.mod,new=test)))^2) / as.numeric(nrow(test))
}
score.mean.tx = mean(score)

# Clustering the Data into Subgroups
# Let's try subsetting the data set in subgroups using an unsupervised learning algorithm.

minclusters = 3
maxclusters = 20
overallclustererror = c()
for (num_clusters in minclusters:maxclusters) {
  fit.km <- kmeans(dfm, num_clusters)
  meanclustererror = c()
  # Find cv score for each cluster
  for (i in 1:num_clusters){
    df = dfm[ fit.km$cluster == i, ]
    folds = create_folds(df,k)
    foldscore = c()
    for (j in 1:k) {
      train = df[c(folds[j,"train1_start"]:folds[j,"train1_end"],folds[j,"train2_start"]:folds[j,"train2_end"]),]
      test = df[folds[j,"test_start"]:folds[j,"test_end"],]

      model = lm(totaltime~.-logtotaltime,data=train)
      foldscore[j] = sum((test$totaltime - predict(model,new=test))^2) / as.numeric(nrow(test))
    }
    meanclustererror[i] = mean(foldscore)
  }
  # store mean error for a given k clusters
  overallclustererror[num_clusters-2] = mean(meanclustererror)
}
# TODO fix x axis of plot because it is off by -2
plot(overallclustererror, xlab="Number of Clusters", ylab="CV Error", main="CV Error vs Number of Clusters")
# TODO: fix 
# Show the resulting clusters
fit.km = kmeans(dfm, 8)
dfm$cluster = fit.km$cluster
# Create indicator variables for fast and slow runners to plot different slopes
mean.total = mean(dfm$totaltime)
sd.total = sd(dfm$totaltime)
dfm$fastrunner = (dfm$totaltime < (mean.total - (2*sd.total)))
dfm$slowrunner = (dfm$totaltime > (mean.total + (2*sd.total)))
slope.model = lm(totaltime~.,data=dfm)
ggplot(dfm, aes(x = K0.5, y = totaltime, color=factor(cluster))) + geom_point(shape=1) +
  labs(list(title = "Clustered Total time Vs. First split time", x = "K0-5", y = "Total Time", colour="Cluster")) +
  theme(legend.title = element_text(size=6, face="bold") , title = element_text(size=8, face="bold")) +
  geom_abline(intercept = 37, slope = 6,color="red") +
  geom_abline(intercept = 37, slope = 3,color="blue")
## 
