#This code is not finalized and is still being modified while finalizing analysis
#Please contact me at jpcsoup@gmail.com if you intend to use this code outside personal or academic use

#Large portions of the clustering method were taken from http://www.mattpeeples.net/kmeans.html

# Calculate the mean and standard deviation of difference between SSE of actual data and SSE of 250 randomized datasets
r.sse <- matrix(0,dim(rand.mat)[1],dim(rand.mat)[2])
wss.1 <- as.matrix(wss)
for (i in 1:dim(r.sse)[2]) {
  r.temp <- abs(rand.mat[,i]-wss.1[,1])
  r.sse[,i] <- r.temp}
r.sse.m <- apply(r.sse,1,mean)
r.sse.sd <- apply(r.sse,1,sd)
r.sse.plus <- r.sse.m + r.sse.sd
r.sse.min <- r.sse.m - r.sse.sd

xrange <- range(1:15)
yrange <- range(log(r.sse.plus),log(r.sse.min))
plot(xrange,yrange, type='n',xlab='Cluster Solution', ylab='Log of SSE - Random SSE', main='Cluster Solustions against (Log of SSE - Random SSE)')
lines(log(r.sse.m), type="b", col='blue')
lines(log(r.sse.plus), type='l', col='red')
lines(log(r.sse.min), type='l', col='red')
legend('bottomright',c('SSE - random SSE', 'SD of SSE-random SSE'), col=c('blue', 'red'), lty=1)

xrange <- range(1:15)
yrange <- range(r.sse.plus,r.sse.min)
plot(xrange,yrange, type='n',xlab='Cluster Solution', ylab='SSE - Random SSE', main='Cluster Solutions against (SSE - Random SSE)')
lines(r.sse.m, type="b", col='blue')
lines(r.sse.plus, type='l', col='red')
lines(r.sse.min, type='l', col='red')
legend('bottomright',c('SSE - random SSE', 'SD of SSE-random SSE'), col=c('blue', 'red'), lty=1)

library(reshape)
library(ggplot2)
# dfplotck<-rbind(melt(table1,id.vars=c("Group.1")),melt(table2,id.vars=c("Group.1")),
#           melt(table3,id.vars=c("Group.1")),melt(table4,id.vars=c("Group.1")), deparse.level=2)
# dfplotck$table<-NA
# dfplotck$table[1:152]<- "table1"
# dfplotck$table[153:224]<- "table2"
# dfplotck$table[225:376]<- "table3"
# dfplotck$table[377:448]<- "table4"

dfplotck <- melt(table2,id.vars=c("Group.1"))
ggplot(dfplotck, aes(x=variable, y=value, color=as.factor(Group.1))) + 
  geom_point(size=4) + theme_bw() +
  theme( # remove the horizontal grid lines
    panel.grid.major.y = element_blank() ,
    # explicitly set the vertical lines (or they will disappear too)
    panel.grid.major.x = element_line( size=1, color="honeydew3")) + 
  #scale_colour_brewer(palette="PuOr") +
  labs(title="K-Means Cluster Values\nTransformation 2", color = "Cluster") +
  facet_grid(.~Group.1)



