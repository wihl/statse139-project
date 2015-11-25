#Read Marathon data dile
#fname = file.choose() 
fname = "/Users/poojasingh/Documents/HStatE139/git/statse139-project2/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm = read.csv(fname, header=T,sep=" ")
names(dfm)
n= length(dfm)
n
#Add all times
times = as.matrix(dfm[,7:15], ncol=9)
times.20.40 = times = as.matrix(dfm[,11:15], ncol=9)
dfm$totaltime = rowSums(times)
dfm$cummtime.20.40 = rowSums(times)
attach(dfm)

#Split data by year
dfm.2010 = subset(dfm, (dfm$Year==2010))
length(dfm.2010$Year) ## of rows for 2010

#Fit AOV model - varitions between two split times
model2 = aov(dfm.2010$K0.5~dfm.2010$K5.10)
model2
#plot(model2)

#Define matrix with column1 representing different groups of split times and column 2 representing distribution of split times
m = matrix(nrow = 5, ncol = 2)
c1 = c("K0.5", "K5.10", "K10.15", "K15.20", "K20.25", "K25.30", "K30.35", "K35.40", "K40.Fin")
c2 = c(dfm.2010$K0.5, dfm.2010$K5.10, dfm.2010$K10.15, dfm.2010$K15.20, dfm.2010$K20.25, dfm.2010$K25.30, dfm.2010$K30.35, dfm.2010$K35.40, dfm.2010$K40.Fin)
m=cbind(c1,c2)
head(m)

#Fit AOV model - variation across all split times
model1 = aov(m[,2]~m[,1])
model1
#plot(model1)

#Fit ANOVA model - using the recommendation in the article 
#TotalTime ~ mean of response + half.mar.time + scaled-time(20-40/half.mar.time)

#Calculate mean of the totaltime
dfm.2010.totaltime.mean = mean(dfm.2010$totaltime)
dfm.2010.totaltime.mean

#Add the mean to half marathon
dfm.mean.plus.halfmar = dfm.2010.totaltime.mean  + dfm.2010$HalfMar

#AOV model
modelr = aov(dfm.2010$totaltime ~ dfm.mean.plus.halfmar  + (dfm.2010$cummtime.20.40/dfm.2010$HalfMar))
modelr
plot(modelr)



