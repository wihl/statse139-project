library(ggplot2)
#Contains some utility R -cdoe
fname <- file.choose() 
#fname = "/Users/poojasingh/Documents/HStatE139/git/statse139-project2/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm <- read.csv(fname, header=T,sep=" ")
#names(dfm)

#1. Add all times
times <- as.matrix(dfm[,7:15], ncol=9)
dfm$totaltime <- rowSums(times)

#2. Create factor for gender
#factor(dfm$Gender1F2M, ordered=TRUE) #identify sorted levels in input source
dfm$gender <- factor(dfm$Gender1F2M, labels=c("Female", "Male")) 
#levels(dfm$gender) #output levels

#3. Create factor for agegroup
#factor(dfm$Age, ordered=TRUE)  #identify sorted levels in input source
dfm$agegroup <- cut(dfm$Age, breaks=c(15, 25, 35, 45, 55, 65, 75, 85))
#levels(dfm$agrgroup)

#4. Split data by year
dfm.2010 <- subset(dfm, (dfm$Year==2010))
dfm.2011 <- subset(dfm, (dfm$Year==2011))
dfm.2013 <- subset(dfm, (dfm$Year==2013))

#Sanity test for year subset
#length(dfm.2010$BibNum) + length(dfm.2011$BibNum) + length(dfm.2013$BibNum)

#5. Split data by agegroup
dfm.age.18.34 <- subset(dfm, (dfm$Age>=18 & dfm$Age<=34))
dfm.age.35.39 <- subset(dfm, (dfm$Age>=35 & dfm$Age<=39))
dfm.age.40.44 <- subset(dfm, (dfm$Age>=40 & dfm$Age<=44))
dfm.age.45.49 <- subset(dfm, (dfm$Age>=45 & dfm$Age<=49))
dfm.age.50.54 <- subset(dfm, (dfm$Age>=50 & dfm$Age<=54))
dfm.age.55.59 <- subset(dfm, (dfm$Age>=55 & dfm$Age<=59))
dfm.age.60.64 <- subset(dfm, (dfm$Age>=60 & dfm$Age<=64))
dfm.age.65.69 <- subset(dfm, (dfm$Age>=65 & dfm$Age<=69))
dfm.age.70.74 <- subset(dfm, (dfm$Age>=70 & dfm$Age<=74))
dfm.age.75.79 <- subset(dfm, (dfm$Age>=75 & dfm$Age<=79))
dfm.age.80.plus <- subset(dfm, (dfm$Age>=80))

#Sanity Test for agegroup subgroups - the sum of lengths of subset should equal to length of original data
#length(dfm.age.18.34$BibNum) + length(dfm.age.35.39$BibNum) + length(dfm.age.40.44$BibNum) + length(dfm.age.45.49$BibNum) +
#  length(dfm.age.50.54$BibNum) + length(dfm.age.55.59$BibNum) + length(dfm.age.60.64$BibNum) + length(dfm.age.65.69$BibNum) + 
#  length(dfm.age.70.74$BibNum) + length(dfm.age.75.79$BibNum) + length(dfm.age.80.plus$BibNum)
#length(dfm$BibNum)

#Plot histogram
#hist(dfm.2010$Age, breaks=c(18, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 83), 
#       main="Age Distribution for 2010", xlab="Age", freq=TRUE)

#6. Plot histogram
ggplot(dfm.2010, aes(x=Age)) + 
  stat_bin(breaks=c(17, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 83), geom="bar", fill="lightblue", colour="white") +
  stat_bin(breaks=c(17, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 83), geom="text", cex=3.25, aes(label=..count..), vjust=-0.5) +
  labs(title="Age Distribution for 2010")

# ggplot(dfm.2010, aes(x=Age)) + 
#   stat_bin(breaks=c(17, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 83), geom="bar", fill="lightblue", colour="white") +
#   stat_bin(breaks=c(17, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 83), geom="text", cex=3.25, aes(label=..count..), vjust=-0.5) +
#   facet_wrap( ~ gender) +
#   labs(title="Age Distribution for 2010") 


#Sanity test for histogram count labels
#length(dfm.age.18.34$BibNum[dfm.age.18.34$Year==2010])
#length(dfm.age.75.79$BibNum[dfm.age.75.79$Year==2010])

#7. top  10% Male runners - profile plot for 2010
n.2010.male <- length(dfm.2010$totaltime[dfm.2010$Gender1F2M=="2"])
#n.2010.male
n.2010.10pct.male <- .10 * n.2010.male 
#round(n.2010.10pct.male)
# 
#Get sorted list of top 10% male runners based on total time
x1.desc <- sort(dfm.2010$totaltime[dfm.2010$Gender1F2M==2], decreasing=FALSE)[1:min(round(n.2010.10pct.male), n.2010.male)]

#Subset 2010 data based on top 10% male runners
dfm.2010.top10.male <- subset(dfm.2010, ((dfm.2010$totaltime %in% x1.desc) & (dfm.2010$Gender1F2M==2)))
attach(dfm.2010.top10.male)
#Mean of top 10% male runners
x.mean.times.top.male <- cbind(by(K0.5,Gender1F2M,mean), by(K5.10,Gender1F2M,mean), by(K10.15,Gender1F2M,mean), +
                                   by(K15.20,Gender1F2M,mean), by(K20.25,Gender1F2M,mean), by(K25.30,Gender1F2M,mean), +
                                   by(K30.35,Gender1F2M,mean), by(K35.40,Gender1F2M,mean), by(K40.Fin,Gender1F2M,mean))
x.mean.times.top.male <- as.vector(x.mean.times.top.male)

plot(x.mean.times.top.male, xaxt = "n", yaxt='n', ylim = c(9,24), xlab="Split Distance", ylab="Split Time", main="Top 10% Runners")
axis(1, at=1:9, cex.axis=0.6, labels=c("K0.5","K5.10","K10.15","K15.20","K20.25","K25.30","K30.35","K35.40","K40.Fin")) 
axis(2, at=9:24, cex.axis=0.6, labels=c(9:24)) 
lines(x.mean.times.top.male, col="darkorange1",lwd=5)
detach(dfm.2010.top10.male)

# top  10% Female runners - profile plot for 2010
n.2010.female <- length(dfm.2010$totaltime[dfm.2010$Gender1F2M=="1"])
#n.2010.female
n.2010.10pct.female <- .10 * n.2010.female 
#round(n.2010.10pct.female)
#Get sorted list of top 10% female runners based on total time
x1.desc <- sort(dfm.2010$totaltime[dfm.2010$Gender1F2M==1], decreasing=FALSE)[1:min(round(n.2010.10pct.female), n.2010.female)]

#Subset 2010 data based on top 10% female runners
dfm.2010.top10.female <- subset(dfm.2010, ((dfm.2010$totaltime %in% x1.desc) & (dfm.2010$Gender1F2M==1)))
attach(dfm.2010.top10.female)
#Mean of top 10% female runners
x.mean.times.top.female <- cbind(by(K0.5,Gender1F2M,mean), by(K5.10,Gender1F2M,mean), by(K10.15,Gender1F2M,mean), +
                                by(K15.20,Gender1F2M,mean), by(K20.25,Gender1F2M,mean), by(K25.30,Gender1F2M,mean), +
                                by(K30.35,Gender1F2M,mean), by(K35.40,Gender1F2M,mean), by(K40.Fin,Gender1F2M,mean))
x.mean.times.top.female <- as.vector(x.mean.times.top.female)

axis(1, at=1:9, cex.axis=0.6, labels=c("K0.5","K5.10","K10.15","K15.20","K20.25","K25.30","K30.35","K35.40","K40.Fin")) 
lines(x.mean.times.top.female, pch=22, lty=2, col="green",lwd=5)
legend("topright",c("Female","Male"), cex = 0.6, lwd=c(2.5,2.5), col=c("green", "darkorange1"))
detach(dfm.2010.top10.female)

#8. Bottom 10% Runners - profile plot for 2010
#Get sorted list of bottom 10% male runners based on total time
x1 <- sort(dfm.2010$totaltime[dfm.2010$Gender1F2M==2], decreasing=TRUE)[1:min(round(n.2010.10pct.male), n.2010.male)]

#Subset 2010 data based on bottom 10% male runners
dfm.2010.bottom10.male <- subset(dfm.2010, ((dfm.2010$totaltime %in% x1) & (dfm.2010$Gender1F2M==2)))
attach(dfm.2010.bottom10.male)
#Mean of bottom 10% male runners
x.mean.times.bottom.male <- cbind(by(K0.5,Gender1F2M,mean), by(K5.10,Gender1F2M,mean), by(K10.15,Gender1F2M,mean), +
                                 by(K15.20,Gender1F2M,mean), by(K20.25,Gender1F2M,mean), by(K25.30,Gender1F2M,mean), +
                                 by(K30.35,Gender1F2M,mean), by(K35.40,Gender1F2M,mean), by(K40.Fin,Gender1F2M,mean))
x.mean.times.bottom.male <- as.vector(x.mean.times.bottom.male)

plot(x.mean.times.bottom.male, xaxt="n", xlab="Split Distance", ylab="Split Time", main="Bottom 10% Runners")
axis(1, at=1:9, cex.axis=0.6, labels=c("K0.5","K5.10","K10.15","K15.20","K20.25","K25.30","K30.35","K35.40","K40.Fin")) 
lines(x.mean.times.bottom.male, col="darkorange1",lwd=5)
detach(dfm.2010.bottom10.male)

#8. bottom 10% female runners - profile plot for 2010
x2 <- sort(dfm.2010$totaltime[dfm.2010$Gender1F2M==1], decreasing=TRUE)[1:min(round(n.2010.10pct.female), n.2010.female)]
dfm.2010.bottom10.female <- subset(dfm.2010, ((dfm.2010$totaltime %in% x2) & (dfm.2010$Gender1F2M==1)))
attach(dfm.2010.bottom10.female)
#Mean of bottom 10% female runners
x.mean.times.bottom.female <- cbind(by(K0.5,Gender1F2M,mean), by(K5.10,Gender1F2M,mean), by(K10.15,Gender1F2M,mean), +
                                   by(K15.20,Gender1F2M,mean), by(K20.25,Gender1F2M,mean), by(K25.30,Gender1F2M,mean), +
                                   by(K30.35,Gender1F2M,mean), by(K35.40,Gender1F2M,mean), by(K40.Fin,Gender1F2M,mean))
x.mean.times.bottom.female <- as.vector(x.mean.times.bottom.female)

axis(1, at=1:9, cex.axis=0.6, labels=c("K0.5","K5.10","K10.15","K15.20","K20.25","K25.30","K30.35","K35.40","K40.Fin")) 
lines(x.mean.times.bottom.female, pch=22, lty=2, col="green",lwd=5)
legend("topright",c("Female","Male"), cex = 0.6, lwd=c(2.5,2.5), col=c("green", "darkorange1"))
detach(dfm.2010.bottom10.female)

#summary(x.mean.times.male)
#summary(x.mean.times.female)
#length(dfm.2010.bottom10.male$totaltime)
#unique(dfm.2010.bottom10.male$Gender1F2M)


###
#summary(dfm.2010.top10.male$totaltime)
#summary(dfm.2010.top10.female$totaltime)
#summary(dfm.2010.bottom10.male$totaltime)
#summary(dfm.2010.bottom10.female$totaltime)



