#Read Marathon data dile
#fname = file.choose() 
fname = "/Users/poojasingh/Documents/HStatE139/git/statse139-project2/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm = read.csv(fname, header=T,sep=" ")
names(dfm)

#Create factor for gender
#factor(dfm$Gender1F2M, ordered=TRUE) #identify sorted levels in input source
dfm$gender = factor(dfm$Gender1F2M, labels=c("female", "male")) 
#levels(dfm$gender) #output levels

#Create factor for agegroup
#factor(dfm$Age, ordered=TRUE)  #identify sorted levels in input source
dfm$agegroup = cut(dfm$Age, breaks=c(15, 25, 35, 45, 55, 65, 75, 85))
#levels(dfm$agegroup) #output levels

#Split data by year
dfm.2010 = subset(dfm, (dfm$Year==2010))

#Anova by agegroup
model1 = aov(dfm.2010$K0.5 ~ dfm.2010$agegroup)
summary(model1)

#Conduct Tukey's multiple comparision test
TukeyHSD(model1)


