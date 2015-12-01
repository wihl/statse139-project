install.packages("class")
library(class)

#knn
fname <- file.choose() 
dfm <- read.csv(fname, header=T,sep=" ")

#Add Total time
times <- as.matrix(dfm[,7:15], ncol=9)
dfm$totaltime <- rowSums(times)

#Factor Age
dfm$agegroup <- cut(dfm$Age, breaks=c(18, 35, 55, 85))
levels(dfm$agegroup)
factor(dfm$agegroup, labels=c("Young", "Middle-age", "Plus-years"))
levels(dfm$agegroup)

#2010 data
dfm.2010 <- subset(dfm, (dfm$Year==2010))
dim(dfm.2010)
names(dfm.2010)

#Divide into train and test data
set.seed(1234)
ind <- sample(2, nrow(dfm.2010), replace=TRUE, prob=c(0.67, 0.33))
dfm.2010.training <- dfm.2010[ind==1, 7:15]
dfm.2010.test <- dfm.2010[ind==2, 7:15]
dim(dfm.2010.training)
dim(dfm.2010.test)

dfm.2010.trainLabels <- dfm.2010[ind==1, 19]
dfm.2010.testLabels <- dfm.2010[ind==2, 19]
head(dfm.2010.testLabels)

dfm.2010.pred <- knn(train = dfm.2010.training, test = dfm.2010.test, cl = dfm.2010.trainLabels, k=3)
length(dfm.2010.pred)

x <- setdiff(dfm.2010.pred,dfm.2010.testLabels)
length(x)

z <- dfm.2010.pred %in% dfm.2010.testLabels
length(z[z==TRUE])

