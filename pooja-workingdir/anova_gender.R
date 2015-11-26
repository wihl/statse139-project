#Read Marathon data dile
#fname = file.choose() 
fname = "/Users/poojasingh/Documents/HStatE139/git/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm = read.csv(fname, header=T,sep=" ")
names(dfm)
n= length(dfm)
attach(dfm)
#Add all times
times = as.matrix(dfm[,7:15], ncol=9)
dfm$totaltime = rowSums(times)

#Distinct Years
unique(Year)

#Plot the data first
hist(dfm$totaltime,breaks=50, main="Boston Marathon Finish Time (min) Distribution for '10, '11, '13")
length(dfm$totaltime) #sanity check

#ANOVA - boxplot by Gender
boxplot(dfm$totaltime~dfm$Gender1F2M, data=dfm, main="Finish Time by Gender")
legend("topright",c("1 - Female","2 - Male"), cex = 0.8)

#ANOVA - boxplot by Age
boxplot(dfm$totaltime~dfm$Age, data=dfm, main="Finish Time by Age")

#F-test to copmare variances by Gender
var.test(dfm$totaltime~dfm$Gender1F2M, data=dfm)

#T-test by Gender
t.test(dfm$totaltime[Gender1F2M==1], dfm$totaltime[Gender1F2M==2])

#Two-way ANOVA by Gender and Age
model.gender.age = aov(dfm$totaltime ~ dfm$Gender1F2M*dfm$Age)
summary(model.gender.age)

#Distinct Age
age = as.vector(unique(Age))
sort(age)

#Split data into agegroup
dfm.age.18.25 = subset(dfm, (dfm$Age>=18 & dfm$Age>=25))
dfm.age.26.35 = subset(dfm, (dfm$Age>=26 & dfm$Age>=35))
dfm.age.36.45 = subset(dfm, (dfm$Age>=36 & dfm$Age>=45))
dfm.age.46.55 = subset(dfm, (dfm$Age>=46 & dfm$Age>=55))
dfm.age.56.65 = subset(dfm, (dfm$Age>=56 & dfm$Age>=65))
dfm.age.66.75 = subset(dfm, (dfm$Age>=66 & dfm$Age>=75))
dfm.age.76.83 = subset(dfm, (dfm$Age>=76 & dfm$Age>=83)) #TotalTime is NA

#(Todo: Paiwise) T-test by Agegroup
t.test(dfm.age.18.25$totaltime, dfm.age.26.35$totaltime)  
t.test(dfm.age.18.25$totaltime[dfm.age.18.25$Gender1F2M==1], dfm.age.26.35$totaltime[dfm.age.18.25$Gender1F2M==1]) #Aggroup and same gender


#Boxplots FinishTime by Gender per ager group
par(mfrow = c(2, 3), mar=c(5.1, 4.1, 4.1, 8.1))
#par(xpd=FALSE) # this is usually the default
boxplot(dfm.age.18.25$totaltime~dfm.age.18.25$Gender1F2M, data=dfm, main="FinishTime - age18-25")
boxplot(dfm.age.26.35$totaltime~dfm.age.26.35$Gender1F2M, data=dfm, main="FinishTime - age26-35")
boxplot(dfm.age.36.45$totaltime~dfm.age.36.45$Gender1F2M, data=dfm, main="FinishTime - age36-45")
boxplot(dfm.age.46.55$totaltime~dfm.age.46.55$Gender1F2M, data=dfm, main="FinishTime - age46-55")
boxplot(dfm.age.56.65$totaltime~dfm.age.56.65$Gender1F2M, data=dfm, main="FinishTime - age56-65")
boxplot(dfm.age.66.75$totaltime~dfm.age.66.75$Gender1F2M, data=dfm, main="FinishTime - age66-75")
#boxplot(dfm.age.76.83$totaltime~dfm.age.76.83$Gender1F2M, data=dfm, main="FinishTime - age76-83")  #TotalTime is NA
#legend("topright",c("1 - Female","2 - Male"), cex = 0.8)

#ANOVA by Age
model.age = aov(dfm$totaltime ~ dfm$Age)
summary(model.age)

#ANOVA by AgeGroup
model.age.18.25 = aov(dfm.age.18.25$totaltime ~ dfm.age.18.25$Age)
summary(model.age.18.25)

#Two ANOVA by Gender and AgeGroup
model.gender.age.18.25 = aov(dfm.age.18.25$totaltime ~ dfm.age.18.25$Gender1F2M * dfm.age.18.25$Age)
summary(model.gender.age.18.25)













#Plot by year 
# par(mfrow = c(1, 3))
# hist(dfm$totaltime, subset=(Year==2010), breaks=50, main="Boston Marathon Finish Time (min) Distribution for 2010")
# hist(dfm$totaltime, subset=(Year==2011), breaks=50, main="Boston Marathon Finish Time (min) Distribution for 2011")
# hist(dfm$totaltime, subset=(Year==2013), breaks=50, main="Boston Marathon Finish Time (min) Distribution for 2013")

#Split data by Year
# dfm.2010 = subset(dfm, Year==2010)
# dfm.2011 = subset(dfm, Year==2011)
# dfm.2013 = subset(dfm, Year==2013)
# 
# n.2010 = length(dfm.2010$Gender1F2M)
# n.2011 = length(dfm.2011$Gender1F2M)
# n.2013 = length(dfm.2013$Gender1F2M)
# n.2010 + n.2011 + n.2013 #equals  69923 sanity check

# par(mfrow = c(1, 3))
# #For 2010
# boxplot(dfm.2010$totaltime~dfm.2010$Gender1F2M, data=dfm.2010, main="Finish time for 2010")
# legend("topright",c("1 - Female","2 - Male"), cex = 0.8)
# 
# #For 2011
# boxplot(dfm.2011$totaltime~dfm.2011$Gender1F2M, data=dfm.2011, main="Finish time for 2011")
# legend("topright",c("1 - Female","2 - Male"), cex = 0.8)
# 
# #For 2013
# boxplot(dfm.2013$totaltime~dfm.2013$Gender1F2M, data=dfm.2013, main="Finish time for 2013")
# legend("topright",c("1 - Female","2 - Male"), cex = 0.8)

# dfms = dfm[!is.na(dfm$totaltime), ]
# dfms = dfms[order(dfms$totaltime),]
# model = lm(totaltime~Age+Gender1F2M+K0.5, data=dfms)
# summary(model)
# model.resid = resid(model)
# 
# plot(dfms$totaltime,model.resid,ylim=c(-100,100))

# #Summary statistics for FinishTime by Gender
# summarystats.finishtime.gender = cbind(by(dfm$totaltime/60, Gender1F2M, mean, na.rm=TRUE),
#                      by(dfm$totaltime/60, Gender1F2M, sd, na.rm=TRUE),
#                      by(dfm$totaltime/60, Gender1F2M, var, na.rm=TRUE)) #Divided by 60 to report in hours 
# colnames(summarystats.finishtime.gender)=c("mean", "sd", "var")
# rownames(summarystats.finishtime.gender)=c("female", "male")
# summarystats.finishtime.gender 
# 
# #Split data by Gender
# dfm.female = subset(dfm, dfm$Gender1F2M==1)
# dfm.male = subset(dfm, dfm$Gender1F2M==2)
# length(dfm.female$Gender1F2M) + length(dfm.male$Gender1F2M) #69923 sanity test








