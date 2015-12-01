install.packages("useful")
library(ggplot2)

#1. Read input data
fname <- file.choose() 
#fname = "/Users/poojasingh/Documents/HStatE139/git/statse139-project2/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm <- read.csv(fname, header=T,sep=" ")
names(dfm)


#2. Add all times
times <- as.matrix(dfm[,7:15], ncol=9)
dfm$totaltime <- rowSums(times)

#3. Factor Age
dfm$agegroup <- (cut(dfm$Age, breaks=c(18, 35, 55, 85)))
levels(dfm$agegroup)
factor(dfm$agegroup, labels=c("Young", "Middle-age", "Plus-years"))
head(paste(dfm$agegroup,dfm$Age))
dfm$agegroup <- factor(dfm$agegroup, labels=c(1,2,3))
levels(dfm$agegroup)
#is.numeric(as.numeric(dfm$agegroup))

#4. Split data by year
dfm.2010 <- subset(dfm, (dfm$Year==2010))
dfm.2011 <- subset(dfm, (dfm$Year==2011))
dfm.2013 <- subset(dfm, (dfm$Year==2013))

#total time by agegroup
dfm.2010.1 = subset(dfm.2010, (dfm.2010$agegroup==1))
dfm.2010.2 = subset(dfm.2010, (dfm.2010$agegroup==2))
dfm.2010.3 = subset(dfm.2010, (dfm.2010$agegroup==3))
mean(dfm.2010.1$totaltime)
mean(dfm.2010.2$totaltime)
mean(dfm.2010.3$totaltime)

#5 Summary statistics
attach(dfm.2010)
summarystats.2010 <- cbind(by(K0.5,Gender1F2M,mean), by(K5.10,Gender1F2M,mean), by(K10.15,Gender1F2M,mean), +
                             by(K15.20,Gender1F2M,mean), by(K20.25,Gender1F2M,mean), by(K25.30,Gender1F2M,mean), +
                             by(K30.35,Gender1F2M,mean), by(K35.40,Gender1F2M,mean), by(K40.Fin,Gender1F2M,mean))
summarystats.2010
detach(dfm.2010)

#6. Cluster by agegroup

#Dataframe that has split-times and agegroup 

dfm.2010.byage <- data.frame(dfm.2010["K0.5"], dfm.2010["totaltime"], c(as.numeric(dfm.2010$agegroup)))
names(dfm.2010.byage)
colnames(dfm.2010.byage)=c("K0.5", "totaltime", "agegroup")

#Find cluster by agegroup
dfm.2010.byage.new <- dfm.2010.byage
dfm.2010.byage.new$agegroup <- NULL

#Use Kmeans to determine clusters by agegroup
kc <- kmeans(dfm.2010.byage.new, 3)
kc

#Compare the agegroup with the clustering result
table(dfm.2010.byage$agegroup, kc$cluster)

#Plot the cluster 
par(xpd=T, mar=par()$mar+c(0,0,0,5))
plot(dfm.2010.byage.new[c("K0.5","totaltime")], col=kc$cluster, main="Cluster of K0.5 ~ TotalTime by AgeGroup", ylab="Total Time")
legend("topright", inset=c(-0.35,0), legend=c("Age (18,35]","Age (35,55]","Age (55,85]"), cex = 0.6, lwd=c(1.5,1.5,1.5), col=1:3,xpd=TRUE)

#A Standardize variables
# dfm.2010.scaled <- data.frame(dfm.2010[1:6],scale(dfm.2010[7:16]),dfm.2010[17:18], scale(dfm.2010[19])) # standardize variables
# head(dfm.2010.scaled)
# 
# #Summary statistics - scaled data
# attach(dfm.2010.scaled)
# summarystats.2010.scaled <- cbind(by(K0.5,Gender1F2M,mean), by(K5.10,Gender1F2M,mean), by(K10.15,Gender1F2M,mean), +
#                              by(K15.20,Gender1F2M,mean), by(K20.25,Gender1F2M,mean), by(K25.30,Gender1F2M,mean), +
#                              by(K30.35,Gender1F2M,mean), by(K35.40,Gender1F2M,mean), by(K40.Fin,Gender1F2M,mean))
# summarystats.2010.scaled
# detach(dfm.2010.scaled)


