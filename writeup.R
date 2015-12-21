
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
#Set aside 10% of our data as a validation set 
indexes = sample(1:nrow(dfm), size=0.1*nrow(dfm))
validate_dfm = dfm[indexes,]
dfm = dfm[-indexes,]

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

tx.mod = lm(logtotaltime~.-totaltime-HalfMar,data=dfm)

for (i in 1:k) {
  train = dfm[c(folds[i,"train1_start"]:folds[i,"train1_end"],folds[i,"train2_start"]:folds[i,"train2_end"]),]
  test = dfm[folds[i,"test_start"]:folds[i,"test_end"],]

  cv.tx.mod = lm(logtotaltime~.-totaltime-HalfMar,data=train)
  score[i] = sum((test$totaltime - exp(predict(cv.tx.mod,new=test)))^2) / as.numeric(nrow(test))
}
score.mean.tx = mean(score)

# Clustering the Data into Subgroups
# Let's try subsetting the data set in subgroups using an unsupervised learning algorithm.

minclusters = 3
maxclusters = 20
overallclustererror = c()
dfm_clus <- dfm[c("Age","Gender1F2M","K0.5","totaltime")] #Ensure that we're clustering on our predictors and total time only
for (num_clusters in minclusters:maxclusters) {
  fit.km <- kmeans(dfm_clus, num_clusters)
  meanclustererror = c()
  # Find cv score for each cluster
  for (i in 1:num_clusters){
    df = dfm_clus[ fit.km$cluster == i, ]
    folds = create_folds(df,k)
    foldscore = c()
    for (j in 1:k) {
      train = df[c(folds[j,"train1_start"]:folds[j,"train1_end"],folds[j,"train2_start"]:folds[j,"train2_end"]),]
      test = df[folds[j,"test_start"]:folds[j,"test_end"],]

      model = lm(totaltime~.,data=train)
      foldscore[j] = sum((test$totaltime - predict(model,new=test))^2) / as.numeric(nrow(test))
    }
    meanclustererror[i] = mean(foldscore)
  }
  # store mean error for a given k clusters
  overallclustererror[num_clusters-2] = mean(meanclustererror)
}
plot(overallclustererror, xlab="Number of Clusters", ylab="CV Error", main="CV Error vs Number of Clusters")
# Show the resulting clusters
fit.km = kmeans(dfm_clus, 8)
dfm$cluster = fit.km$cluster
# Create indicator variables for fast and slow runners to plot different slopes
mean.total = mean(dfm$totaltime)
sd.total = sd(dfm$totaltime)
dfm$fastrunner = (dfm$totaltime < (mean.total - (2*sd.total)))
dfm$slowrunner = (dfm$totaltime > (mean.total + (2*sd.total)))
dfm.sample = dfm[sample(nrow(dfm), 10000), ] #Use a (large) random sample of points so that this does not break PDF readers
slope.model = lm(totaltime~.,data=dfm.sample)
ggplot(dfm.sample, aes(x = K0.5, y = totaltime, color=factor(cluster), shape = (Gender1F2M ))) + geom_point(shape=1,alpha = 0.5 ) +
  stat_smooth(method = lm, aes(x = K0.5, y = predict(slope.model)), se = FALSE) +
  labs(list(title = "Clustered Total time Vs. First split time", x = "K0-5", y = "Total Time", colour="Cluster")) +
  theme(legend.title = element_text(size=6, face="bold") , title = element_text(size=8, face="bold"))
# Show the resulting clusters

ggplot(dfm.sample, aes(x = Age, y = K0.5, color=factor(cluster), shape = (Gender1F2M ))) + geom_point( alpha=.5) +
  labs(list(title = "Cluster Vs. Age Vs. Gender Vs. 5k Split time", x = "Runner Age", y = "5k Split Time", colour="Cluster")) +
  theme(legend.title = element_text(size=6, face="bold") , title = element_text(size=8, face="bold")) 

# Save the different regression models per cluster
clust.mod = c()
for (i in 1:length(fit.km$size)) {
  df = dfm[which(dfm$cluster==i), ]
  df = df[c("Age","Gender1F2M","K0.5","totaltime")]
  clust.mod[[i]] = lm(totaltime~.,data=df)
}


# Validating model against Chicago Marathon
dfChi14 <- read.csv("ChicagoScraper/Chicago2014Formated.csv",header=T)
dfChi15 <- read.csv("ChicagoScraper/Chicago2015Formated.csv",header=T)
dfChi <- rbind(dfChi14,dfChi15)


dfChi$Gender1F2M  <- as.factor(dfChi$Gender1F2M) # added converter to assign.cluster to handle gender as factor
Chitimes = as.matrix(dfChi[,7:15], ncol=9)
dfChi$StartHr = NULL
dfChi$StartMin = NULL
dfChi$totaltime = rowSums(Chitimes)
dfChi$logtotaltime = log(dfChi$totaltime)
#Filter out any rows with NAs -- they'll cause headaches later 
dfChi = dfChi[!is.na(dfChi$totaltime), ]
dfChi = dfChi[!is.na(dfChi$HalfMar), ]
dfChi = dfChi[!is.na(dfChi$K0.5), ]
dfChi = dfChi[!is.na(dfChi$Age), ]

#Draw a random sample 
dfChi = dfChi[sample(nrow(dfChi), 2500), ][c("Age","Gender1F2M","K0.5","totaltime","HalfMar")]
# Remove outliers
mean5k = mean(dfChi$K0.5)
sd5k = sd(dfChi$K0.5)
outliers5k = dfChi$K0.5>(mean5k + 3*sd5k)
dfChi = dfChi[!outliers5k,]

assign.cluster <- function(df, centers) {
  # compute squared Euclidean distance from each sample to each cluster center
  # There is probably a vectorized way of doing this.
  cols = c("Age","Gender1F2M","K0.5")
  clusters = c()
  # If gender in incoming DF is a factor, convert it to numeric 
  if (is.factor(df$Gender1F2M)){
    df$Gender1F2M <- as.numeric(levels(df$Gender1F2M))[df$Gender1F2M] 
  }
  for (i in 1:nrow(df)){
    diff = c()
    for (j in 1:nrow(centers) ){
      y = centers[j,cols]
      x = as.matrix(df[i,cols])
      diff[j] = sum((x-y)^2)
    }
    clusters[i] = which.min(diff)
  }
  return (clusters)
}

# Calculates predicted finish time using assigned cluster and cluster-specific regression models 
predict.finish <- function(df) {
  newtot = c()
  for (n in 1:nrow(df)) {
    newtot[n] = predict(clust.mod[[df[n,"cluster"]]], newdata=df[n,])
  }
  return(newtot)
}

# Calculate sum of squared errors (SSE) on submitted DF for both baseline model and clustered model 
assign.SSE <- function(df){
  
  y.hat.base = predict(base.mod,new=df)
  y.hat.half = predict(base.mod.half,new=df)
  df$logtotaltime = log(df$totaltime)
  y.hat.log = exp(predict(tx.mod,new=df))
  
  base.mod.SSE <- as.integer(sqrt(sum((y.hat.base - df$totaltime)^2) / nrow(df)))
  log.mod.SSE <- as.integer(sqrt(sum((y.hat.log - df$totaltime)^2) / nrow(df)))
  half.mod.SSE <- as.integer(sqrt(sum((y.hat.half - df$totaltime)^2) / nrow(df)))
  clus.mod.SSE <- as.integer(sqrt(sum((predict.finish(df) - df$totaltime)^2) / nrow(df)))
  
  return (list("base.mod" = base.mod.SSE, "log.mod"=log.mod.SSE, "half.mod" = half.mod.SSE ,"clus.mod" = clus.mod.SSE))
} 

# Test on Chicago sample 
# find which cluster would be most appropriate
# dfChi$Cluster = predict(svm,dfChi )

dfChi$cluster = assign.cluster(dfChi, fit.km[["centers"]])
SSE.Chi = assign.SSE(dfChi)

# Test on our validation set: 
# validate_dfm$Cluster = predict(svm,validate_dfm)

validate_dfm$cluster = assign.cluster(validate_dfm, fit.km[["centers"]])
SSE.Bos.validate = assign.SSE(validate_dfm)

# As a final check, let’s see how our classifier does on the training data. 
# When we remove finish time from the equation, how often does our Euclidian distance 
# based algorithm pick the right cluster ID? 

dfm$PredictedCluster = assign.cluster(dfm, fit.km[["centers"]]) 
predicted.cluster.accuracy  = nrow(dfm[dfm$PredictedCluster == dfm$cluster,])/nrow(dfm)
#I get about 40% correct with distance classifer  

## 
