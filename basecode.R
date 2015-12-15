dfm <- read.csv("Previous Boston Marathon study/BAA data.txt",header=T,sep=" ")
times = as.matrix(dfm[,7:15], ncol=9)
dfm$totaltime = rowSums(times)
dfm = dfm[!is.na(dfm$totaltime), ]
base.mod = lm(totaltime~Age+Gender1F2M+K0.5,data=dfm)
summary(base.mod)
y.hat = predict(base.mod)
xydata = data.frame(x=y.hat, y=resid(base.mod))
xydata = xydata[sample(1:nrow(xydata), 5000, replace=FALSE),]
plot(xydata$x,xydata$y,ylim=c(-100,100), xlab="Predicted", ylab="Residuals")

hist(dfm$totaltime,breaks=50, main="Boston Marathon Finish Time (min) Distribution for '10, '11, '13")
hist(log(dfm$totaltime),breaks=50, main="Boston Marathon Finish Time (log-min) Distribution for '10, '11, '13")
tx.mod = lm(log(totaltime)~Age+Gender1F2M+K0.5, data=dfm)
summary(tx.mod)
#plot(dfms$totaltime,model.resid,ylim=c(-100,100))
y.hat = predict(tx.mod)
xydata = data.frame(x=y.hat, y=exp(resid(tx.mod)))
xydata = xydata[sample(1:nrow(xydata), 5000, replace=FALSE),]
plot(xydata$x,xydata$y,ylim=c(-100,100), xlab="Predicted", ylab="Residuals")
#bestresidsum = 9e9
#bestpoly = 0
#for (i in seq(from=1.0, to= 2.5, by=0.1)){
#  dfms$k5xform = dfms$K0.5 ^ i
#  model = lm(totaltime~Age+Gender1F2M+k5xform, data=dfms)
#  model.residsum = sum(resid(model)^2)
# if (model.residsum < bestresidsum) {
#    bestresid = resid(model)
#    bestresidsum = model.residsum
#    bestpoly = i
#  }
#}

#cat("total error went from ",sum(model.resid^2), " (untransformed) to ", bestresidsum," (transformed)\n")
#plot(dfms$totaltime,bestresid,ylim=c(-100,100))
#xydata = data.frame(x=dfms$totaltime, y=bestresid)
#xydata = xydata[sample(1:nrow(xydata), 5000, replace=FALSE),]
#plot(xydata$x,xydata$y,ylim=c(-100,100), xlab="total time(min)", ylab="Residuals")

# Code inspired from https://datayo.wordpress.com/2015/05/06/using-k-means-to-cluster-wine-dataset/
columnstokeep <- c("totaltime","Age","Gender1F2M","K0.5")
dfm2<- dfm[columnstokeep]

# Warning: using NbClust never finished even when I tried 2-4 instead of 2-15 clusters.
# install.packages("NbClust")
#library(NbClust)
#nc <- NbClust(data=dfm2,
#              min.nc=2, max.nc=4,
#              method="kmeans")
#barplot(table(nc$Best.n[1,]),
#        xlab="Numer of Clusters",
#        ylab="Number of Criteria",
#        main="Number of Clusters Chosen by 26 Criteria")


for (num_clusters in c(4,8,10,20)) {
  fit.km <- kmeans(dfm2, num_clusters)


  cluster.resid = c()

  cat ("Number of clusters:",num_clusters,"\n")
  for (i in 1:num_clusters){
    df = dfm2[ fit.km$cluster == i, ]
    mod = lm(totaltime~Age+Gender1F2M+K0.5,data=df)
    cluster.resid[i] = sqrt(mean(mod$residuals^2))
    cat("For cluster ",i," the mean error is ", cluster.resid[i], "\n")
  }
  cat ("Mean error rate overall:", mean(cluster.resid),"\n\n")
}
# Warning: this takes awhile to run because it includes all data points
# install.packages("fpc")
library(fpc)
plotcluster(dfm2, fit.km$cluster)
