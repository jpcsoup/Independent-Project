#This code is not finalized and is still being modified while finalizing analysis
#Please contact me at jpcsoup@gmail.com if you intend to use this code outside personal or academic use

#Large portions of the clustering method were taken from http://www.mattpeeples.net/kmeans.html

#Starting points for each cluster
#------------------------------------------------------------------------------------#

library(psych)
library(cluster)
#omit lines that contain a NA and subset
sub1=na.omit(df1)[,2:20]
sub=sub1[,2:20]

sub2=na.omit(df2)[,2:10]
sub=sub2[,2:10]

sub3=na.omit(df3)[,2:20]
sub=sub3[,2:20]

sub4=na.omit(df4)[,2:10]
sub=sub4[,2:10]

#create plot of cluster explanatory power
wss <- (nrow(sub)-1)*sum(apply(sub,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(sub, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")  
### see where the "elbow" is in the number of clusters ###

####################################################################
#Inset plots of clusters standardized or corrected
k.rand <- function(x){
  km.rand <- matrix(sample(x),dim(x)[1],dim(x)[2])
  rand.wss <- as.matrix(dim(x)[1]-1)*sum(apply(km.rand,2,var))
  for (i in 2:15) rand.wss[i] <- sum(kmeans(km.rand, centers=i, iter.max=50, nstart=10)$withinss)
  rand.wss <- as.matrix(rand.wss)
  return(rand.wss)}

k.1 <- function(x) { 
  for (i in 1:250) {
    r.mat <- as.matrix(suppressWarnings(k.rand(sub)))
    rand.mat[,i] <- r.mat}
  return(rand.mat)}

rand.mat <- matrix(0,15,250)
rand.mat <- k.1(sub)
xrange <- range(1:15)
yrange <- range(log(rand.mat),log(wss))
plot(xrange,yrange, type='n', xlab='Cluster Solution', ylab='Log of Within Group SSE', main='Cluster Solutions against Log of SSE')
for (i in 1:250) lines(log(rand.mat[,i]),type='l',col='red')
lines(log(wss), type="b", col='blue')
legend('topright',c('Actual Data', '250 Random Runs'), col=c('blue', 'red'), lty=1)

yrange <- range(rand.mat,wss)
plot(xrange,yrange, type='n', xlab="Cluster Solution", ylab="Within Groups SSE", main="Cluster Solutions against SSE")
for (i in 1:250) lines(rand.mat[,i],type='l',col='red')
lines(1:15, wss, type="b", col='blue')
legend('topright',c('Actual Data', '250 Random Runs'), col=c('blue', 'red'), lty=1)


####################################################################

#Cluster Analysis for 1st transformation
#------------------------------------------------------------------------------------#
fit <- kmeans(sub1, 8) ### 8 k cluster solution ###
aggregate(sub1,by=list(fit$cluster),FUN=mean)  ### get cluster means ###
table1<-data.frame(aggregate(sub1,by=list(fit$cluster),FUN=mean))
fit  ### see what cluster command did in detail ###
sub1 <- data.frame(sub1, fit$cluster)  ### append cluster assignment  ###
head(sub1) ### look at the dataset again ###



#Cluster Analysis for 2nd transformation
#------------------------------------------------------------------------------------#

fit <- kmeans(sub2, 8) ### 8 k cluster solution ###
aggregate(sub2,by=list(fit$cluster),FUN=mean)  ### get cluster means ###
table2<-data.frame(aggregate(sub2,by=list(fit$cluster),FUN=mean))
fit  ### see what cluster command did in detail ###
sub2 <- data.frame(sub2, fit$cluster)  ### append cluster assignment  ###
head(sub2) ### look at the dataset again ###



#Cluster Analysis for 3rd transformation
#------------------------------------------------------------------------------------#

fit <- kmeans(sub3, 8) ### 8 k cluster solution ###
aggregate(sub3,by=list(fit$cluster),FUN=mean)  ### get cluster means ###
table3<-data.frame(aggregate(sub3,by=list(fit$cluster),FUN=mean))
fit  ### see what cluster command did in detail ###
sub3 <- data.frame(sub3, fit$cluster)  ### append cluster assignment  ###
head(sub3) ### look at the dataset again ###

#Cluster Analysis for 4th transformation
#------------------------------------------------------------------------------------#

fit <- kmeans(sub4, 8) ### 8 k cluster solution ###
aggregate(sub4,by=list(fit$cluster),FUN=mean)  ### get cluster means ###
table4<-data.frame(aggregate(sub4,by=list(fit$cluster),FUN=mean))
fit  ### see what cluster command did in detail ###
sub4 <- data.frame(sub4, fit$cluster)  ### append cluster assignment  ###
head(sub4) ### look at the dataset again ###
