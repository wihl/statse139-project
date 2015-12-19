
# Boston Marathon Finish Time Predictions
# Harvard Stats E139 Fall 2015
# Nathaniel Burbank, Pooja Singh, David Wihl
#
# December 21, 2015


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
dfm<- dfm[c("totaltime","Age","Gender1F2M","K0.5")] # keep only columns we need
dfm$Gender1F2M = as.factor(dfm$Gender1F2M) # make gender into a factor
dfm = dfm[!is.na(dfm$totaltime), ]  # eliminate rows with no finish times
dfm = dfm[sample(nrow(dfm)),]  # in case the data is sorted, randomize the order
# Find mean finish time by gender
agg = aggregate(dfm$totaltime, by=list(dfm$Gender1F2M), FUN=mean)[2]
men = as.integer(agg$x[2])
women = as.integer(agg$x[1])

#Set aside 10% of our data as a validation set 
indexes = sample(1:nrow(dfm), size=0.1*nrow(dfm))
dfm_validate = dfm[indexes,]
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
base.mod = lm(totaltime~.,data=dfm)

# get baseline score 
for (i in 1:k) {
  train = dfm[c(folds[i,"train1_start"]:folds[i,"train1_end"],folds[i,"train2_start"]:folds[i,"train2_end"]),]
  test = dfm[folds[i,"test_start"]:folds[i,"test_end"],]

  cvmodel = lm(totaltime~.,data=train)
  score[i] = sum((test$totaltime - predict(cvmodel,new=test))^2) / as.numeric(nrow(test))
}
score.mean = mean(score)
summary(base.mod)
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
score.mean = mean(score)

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
# Show the resulting clusters
#fit.km <- kmeans(dfm, 8)
# install.packages("fpc")
#library(fpc)
#plotcluster(dfm, fit.km$cluster)
## 

num_clusters = 8 
fit.km <- kmeans(dfm, num_clusters) 

dfm_train_clus <- cbind(dfm, Cluster = fit.km$cluster)
dfm_train_clus$Cluster  <- as.factor(dfm_train_clus$Cluster)
dfm_train_clus$Gender1F2M  <- as.factor(dfm_train_clus$Gender1F2M)
dfm_train_clus_svm = dfm_train_clus[c("Age","Gender1F2M","K0.5", "Cluster")]
dfm_train_clus_ols = dfm_train_clus[c("Age","Gender1F2M","K0.5","Cluster","totaltime")]

clus_model = lm(totaltime~.^2,data=dfm_train_clus_ols) 

score <- c()
for (i in 1:k) {
  train = dfm_train_clus_ols[c(folds[i,"train1_start"]:folds[i,"train1_end"],folds[i,"train2_start"]:folds[i,"train2_end"]),]
  test = dfm_train_clus_ols[folds[i,"test_start"]:folds[i,"test_end"],]
  
  cvmodel = lm(totaltime~.^2,data=dfm_train_clus_ols)
  score[i] = sum((test$totaltime - predict(cvmodel,new=test))^2) / as.numeric(nrow(test))
}
sqrt(mean(score))
# were about 7.2 min off, with looks like a great improvment
summary(clus_model)

# This is where we try to train our support vector machine on our 
# previously identified clusters. Warning -- takes a couple of minutes to run on whole dateset. 
svm <- ksvm(Cluster ~ ., data = dfm_train_clus_svm)
svm 

#Assign clusters to test set based on svp 
dfm_validate$Cluster = predict(svm,dfm_validate )

test_score = sum((dfm_validate$totaltime - predict(clus_model,new=dfm_validate))^2) / as.numeric(nrow(dfm_validate))
plot(dfm_validate$totaltime, predict(clus_model,new=dfm_validate))

sqrt(test_score) #On the test set, we're off by about 17 min -- not so good! 

# Let's try our model on the Chicago data 
#import data from the Chicago marathons 

dfChi14 <- read.csv("ChicagoScraper/Chicago2014Formated.csv",header=T)
dfChi15 <- read.csv("ChicagoScraper/Chicago2015Formated.csv",header=T)
dfChi <- rbind(dfChi14,dfChi15)

dfChi$Gender1F2M  <- as.factor(dfChi$Gender1F2M)

Chitimes = as.matrix(dfChi[,7:15], ncol=9)

dfChi$totaltime = rowSums(Chitimes)
dfChi$logtotaltime = log(dfChi$totaltime)
dfChi = dfChi[!is.na(dfm$totaltime), ]

#Draw a random sample 
ChicagoSample = dfChi[sample(nrow(df), 1000), ][c("Age","Gender1F2M","K0.5","totaltime")]
#Assign clusters to test set based on svp 
ChicagoSample$Cluster = predict(svm,ChicagoSample)

Chicago_score = sum((ChicagoSample$totaltime - predict(clus_model,new=ChicagoSample))^2) / as.numeric(nrow(ChicagoSample))
sqrt(Chicago_score) #12 min 
plot(ChicagoSample$totaltime, predict(clus_model,new=ChicagoSample))



