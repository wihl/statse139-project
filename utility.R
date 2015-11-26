#Contains some utility R -cdoe
fname = file.choose() 
#fname = "/Users/poojasingh/Documents/HStatE139/git/statse139-project2/statse139-project/Previous Boston Marathon study/BAA data.txt"
dfm = read.csv(fname, header=T,sep=" ")
names(dfm)
#head(dfm) #peek into the contents of dfm for sanity test


#1. Create factor for gender
factor(dfm$Gender1F2M, ordered=TRUE) #identify sorted levels in input source
dfm$gender = factor(dfm$Gender1F2M, labels=c("female", "male")) 
levels(dfm$gender) #output levels

#2. Create factor for agegroup
factor(dfm$Age, ordered=TRUE)  #identify sorted levels in input source
dfm$agrgroup = cut(dfm$Age, breaks=c(15, 25, 35, 45, 55, 65, 75, 85))
levels(dfm$agrgroup)

#3. Split data by year
dfm.2010 = subset(dfm, (dfm$Year==2010))
dfm.2011 = subset(dfm, (dfm$Year==2011))
dfm.2013 = subset(dfm, (dfm$Year==2013))

#4. Split data by agegroup
dfm.age.18.25 = subset(dfm, (dfm$Age>=16 & dfm$Age>=25))
dfm.age.26.35 = subset(dfm, (dfm$Age>=26 & dfm$Age>=35))
dfm.age.36.45 = subset(dfm, (dfm$Age>=36 & dfm$Age>=45))
dfm.age.46.55 = subset(dfm, (dfm$Age>=46 & dfm$Age>=55))
dfm.age.56.65 = subset(dfm, (dfm$Age>=56 & dfm$Age>=65))
dfm.age.66.75 = subset(dfm, (dfm$Age>=66 & dfm$Age>=75))
dfm.age.76.83 = subset(dfm, (dfm$Age>=76 & dfm$Age>=85))
