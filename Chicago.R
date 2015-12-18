#Read in Chicago data 
rm(list=ls())
dfChi14 <- read.csv("ChicagoScraper/Chicago2014Formated.csv",header=T)
dfChi15 <- read.csv("ChicagoScraper/Chicago2015Formated.csv",header=T)
dfChi <- rbind(dfChi14,dfChi15)
dfChi$City <- "Chicago"

#Read in Boston data 
dfBos <- read.csv("Previous Boston Marathon study/BAA data.txt",header=T,sep=" ")
dfBos$City <- "Boston" 

dfm <- rbind(dfBos,dfChi)
dfm$City <-  as.factor(dfm$City) 
times = as.matrix(dfm[,7:15], ncol=9)

dfm$totaltime = rowSums(times)
dfm = dfm[!is.na(dfm$totaltime), ]

# It might seem continuative at first, but Boston is actually “Faster” than Chicago.  
boxplot(totaltime ~ City, data=dfm)

library(lattice)
histogram(~ totaltime | City, data=dfm)
par(mfrow=c(1,2))
qqnorm(dfm$totaltime[dfm$City == "Boston"], main="Boston 10,11,13")
qqnorm(dfm$totaltime[dfm$City == "Chicago"], main="Chicago 14 & 15")
par(mfrow=c(1,1)) 

# While finish times for both races are left skewed, we can see from the plots
# above that the race results from the Chicago marathon are more evenly
# distributed than for Boston. This makes sense. While the Chicago Marathon is
# open-entry, the Boston Athletic Association maintains rigorous qualifying
# times that they require runners to achieve in another marathon before gaining
# a coveted entry to the Boston Marathon.


# Todo: Now let’s see how our previously defined model works with Chicago data in place of Boston data... 