#install.packages("tripack")
#install.packages("HSAUR")
library(tripack)
library(RColorBrewer)

CL5 <- brewer.pal(5, "Pastel1")
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

#4. Cluster analysis of TotalTime and K0.5 for 2010
df <- data.frame(scale(dfm.2010$K0.5), scale(dfm.2010$totaltime))

#nstart arguments suggests the number of kmeans clustering that is performed using multipe random assignments in the begining
kc <- kmeans(df, 3, nstart=20, algorithm = c("Lloyd"))
kc <- kmeans(df, centers=3, nstart = 1, algorithm = "Lloyd")
CL5 <- brewer.pal(4, "Accent") #For nice colors
plot(df, col=CL5[kc$cluster], xlab="K0-5 Split-time", ylab="Total Time", main="Clusters using kmeans")
legend("topright", legend = kc$size, lwd=c(1.5,1.5,1.5),cex = 0.8, col=CL5[c(3,2,1)])

#Voronoi diagram
#Reference:https://dzone.com/articles/k-means-clustering-and-voronoi
V <- voronoi.mosaic(kc$centers[,1], kc$centers[,2])
#V
P <- voronoi.polygons(V)
points(kc$centers[,1],kc$centers[,2],pch=3,cex=1.0,lwd=2)
plot(V,add=TRUE) #Why is polygon not getting plotted?
#V 






