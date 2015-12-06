#Set a seed for random generator in R so that the results can be reproduced precisely at a later time
set.seed(1234)

#1. Read input data
#fname <- file.choose() 
fname = "/Users/poojasingh/Documents/HStatE139/git/statse139-project2/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm <- read.csv(fname, header=T,sep=" ")
names(dfm)

#2. Add all times
times <- as.matrix(dfm[,7:15], ncol=9)
dfm$totaltime <- rowSums(times)

#3. Split data by year
dfm.2010 <- subset(dfm, (dfm$Year==2010))
dfm.2011 <- subset(dfm, (dfm$Year==2011))
dfm.2013 <- subset(dfm, (dfm$Year==2013))

#4. Cluster of TotalTime and K0.5 for 2010
df <- data.frame(dfm.2010$K0.5, dfm.2010$totaltime)
#nstart arguments suggests the number of kmeans clustering that is performed using multipe random assignments in the begining
kc <- kmeans(df, 3, nstart=20)
plot(df, col=kc$cluster, xlab="K0-5 Split-time", ylab="Total Time", main="Clusters using kmeans")
legend("topright", legend = kc$size, lwd=c(1.5,1.5,1.5),cex = 0.8, col=c(3,2,1))


