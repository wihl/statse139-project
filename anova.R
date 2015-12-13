#Read Marathon data dile
#fname = file.choose() 
fname <- "/Users/poojasingh/Documents/HStatE139/git/statse139-project2/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm <- read.csv(fname, header=T,sep=" ")
#names(dfm)
#head(dfm)

#Histogram of Total Time - is it skewed?
hist(dfm.2010$TotalTime)
hist(log(dfm.2010$TotalTime))

#hist(dfm.2010$K0.5)
#hist(log(dfm.2010$K0.5))

#Log transform split-time data
dfm <- cbind(dfm[,1:6], log(dfm[7]), log(dfm[8]), log(dfm[9]), log(dfm[10]), log(dfm[11]),log(dfm[12]),log(dfm[13]),log(dfm[14]),+
               log(dfm[15]), dfm[,16:18])

#Code Gender1F2M as a factor to treat it as a categorical variable
dfm$Gender <- factor(dfm$Gender1F2M, labels=c("female", "male"))
#levels(dfm$Gender)

#Create factor for agegroup
dfm$AgeGroup <- cut(dfm$Age, breaks=c(15, 35, 55, 85))
#levels(dfm$AgeGroup) #Levels "(15,35]" "(35,55]" "(55,85]"

#Calculate Total time 
times <- as.matrix(dfm[,7:15], ncol=9)
dfm$TotalTime <- rowSums(times)

#Calculate first half 
firstHalf <- cbind(dfm[,7:10],(dfm[11]/5)*1.1)
dfm$FirstHalf <- rowSums(firstHalf)

#Calculate 2nd half
secondHalf <- cbind((dfm[11]/5)*3.9, dfm[12:15])
dfm$SecondHalf <- rowSums(secondHalf)

#Split data by year
dfm.2010 <- subset(dfm, (dfm$Year==2010))
attach(dfm.2010)
#unique(Year) #Test - returns Year = 2010 only

#Split data by gender
dfm.2010.male <- subset(dfm, (Gender1F2M==2))
dfm.2010.female <- subset(dfm, (Gender1F2M==1))

#1. Do men and women run differently first K0-5 - let's look at the ANOVA model
boxplot(K0.5 ~ Gender, main="K0-5 Split time")

#ANOVA By Gender - first K0-5
model1 <- aov(K0.5 ~ Gender)
summary(model1)

#2. Do men and women total time are different - let's look at the ANOVA model
boxplot(TotalTime ~ Gender, main="Total Time for running marathon")

#ANOVA By Gender
model2 <- aov(TotalTime ~ Gender)
summary(model2)

#3. Do men and women run differently by age group - let's look at the ANOVA model
boxplot(TotalTime ~ AgeGroup, main="Total time vs. Age group", xlab="Age group", ylab="Total time in minutes")

#ANOVA By Age group
model3 <- aov(K0.5 ~ AgeGroup)
summary(model3)

#4. Do men and women run different by age group and gender
boxplot(TotalTime ~ AgeGroup + Gender, main="Total time vs. Age group and Gender", xlab="Age group", ylab="Total time in minutes")

#ANOVA By Age group and Gender
model4 <- aov(K0.5 ~ AgeGroup + Gender)
summary(model4)

#ANOVA By Age group and Gender - interaction effect
model5 <- aov(K0.5 ~ AgeGroup * Gender)
summary(model5)

#5. TukeyHSD test for K0-5 and Age group - all pairwise tests are significant
TukeyHSD(aov(K0.5 ~ AgeGroup * Gender)) #Look out for one pair. Same results are not obtained for TotalTime

TukeyHSD(aov(TotalTime ~ AgeGroup * Gender)) #All are significant

#6a. Top 2000 male runners in 2010
length(dfm.2010[,1])
dfm.2010.male.top2000 <- dfm.2010.male[order(dfm.2010.male$K0.5, decreasing=FALSE)[1:2000],]
#6b. Top 10% female 
dfm.2010.female.top2000 <- dfm.2010.female[order(dfm.2010.female$K0.5, decreasing=FALSE)[1:2000],]

#6c. Do top 2000 men and women run same by gender
top2000 <- rbind(dfm.2010.female.top2000, dfm.2010.male.top2000)
boxplot(top2000$K0.5 ~ top2000$Gender, main="Total time vs. Gender for top 2000 fastest runners",  ylab="Total time in minutes")

model6 <- aov(K0.5 ~ Gender)
summary(model6)

#7a. First Half-time vs 2nd half-time
attach(dfm.2010)

model7 <- aov(TotalTime ~ FirstHalf)
summ <- summary(model7)
summ
#summary(lm(TotalTime ~ FirstHalf)) #check R^2 values here

r2.model7 <- 1 - (summ[[1]]$'Sum Sq'[[2]]/sum(summ[[1]]$'Sum Sq'))
r2.model7

model7b <- aov(TotalTime ~ SecondHalf)
summ <- summary(model7b)
#summary(lm(TotalTime ~ SecondHalf)) #check R^2 values here

r2.model7b <- 1 - (summ[[1]]$'Sum Sq'[[2]]/sum(summ[[1]]$'Sum Sq'))
r2.model7b
length(FirstHalf); length(Age); length(Gender)
model7c <- aov(TotalTime ~ FirstHalf + Age + Gender)
summ <- summary(model7c)
summary(lm(TotalTime ~ FirstHalf + Age + Gender)) #check R^2 values here

r2.model7c <- 1 - (summ[[1]]$'Sum Sq'[[4]]/sum(summ[[1]]$'Sum Sq'))
r2.model7c

model7d <- aov(TotalTime ~ K0.5)
summ <- summary(model7d)
summ
summary(lm(TotalTime ~ K0.5)) #check R^2 values here

r2.model7d <- 1 - (summ[[1]]$'Sum Sq'[[2]]/sum(summ[[1]]$'Sum Sq'))
r2.model7d

r2.model7b #aov(TotalTime ~ SecondHalf) -> 0.9594059
r2.model7c #aov(TotalTime ~ FirstHalf + Age + Gender) -> 0.9233338
r2.model7 #aov(TotalTime ~ FirstHalf) -> 0.9203939
r2.model7d #aov(TotalTime ~ K0.5) -> 0.8419144



