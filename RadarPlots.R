#This code is not finalized and is still being modified while finalizing analysis
#Please contact me at jpcsoup@gmail.com if you intend to use this code outside personal or academic use

#Adding Pew typology clusters to dataframes for aggregate comparison
df1 <- merge(df1,typo,by.x="V1",by.y="mergeid")
df2 <- merge(df2,typo,by.x="V1",by.y="mergeid")
df3 <- merge(df3,typo,by.x="V1",by.y="mergeid")
df4 <- merge(df4,typo,by.x="V1",by.y="mergeid")

#Create summary tables of cluster means for Pew's typologies
pew1<-aggregate(df1[,2:20],by=list(df1$finaltypo),FUN=mean,na.rm=TRUE)
pew2<-aggregate(df2[,2:10],by=list(df2$finaltypo),FUN=mean,na.rm=TRUE)
pew3<-aggregate(df3[,2:20],by=list(df3$finaltypo),FUN=mean,na.rm=TRUE)
pew4<-aggregate(df4[,2:10],by=list(df4$finaltypo),FUN=mean,na.rm=TRUE)

#melting dataframes for line plot
pewplot <- melt(pew2,id.vars=c("Group.1"))
#Reverse the order of the levels such that they align with the red/blue scale
pewplot$Group.1 <- factor(pewplot$Group.1, levels = rev(levels(pewplot$Group.1)))

ggplot(pewplot, aes(x=variable, y=value, color=as.factor(Group.1))) + 
  geom_point(size=6) + theme_bw() +
  theme( # remove the horizontal grid lines
    panel.grid.major.y = element_blank() ,
    # explicitly set the vertical lines (or they will disappear too)
    panel.grid.major.x = element_line( size=5, color="honeydew3")) +
  scale_colour_brewer(palette="RdBu") +
  labs(title="Pew Typology Values\nTransformation 1", color = "Typology") 
  
  
#
  ggplot(pewplot, aes(x=variable, y=value, color=as.factor(Group.1))) + 
  geom_point(size=2) + theme_bw() +
  theme( # remove the horizontal grid lines
    panel.grid.major.y = element_blank() ,
    # explicitly set the vertical lines (or they will disappear too)
    panel.grid.major.x = element_line( size=1, color="honeydew3")) +
  #scale_colour_brewer(palette="RdBu") +
  labs(title="Pew Typology Values\nTransformation 1", color = "Typology") +
  facet_grid(.~Group.1)

#install.packages("fmsb")
library(fmsb)

#plots for 1st transformation
class1 <- c(6,2,4,7,8,3,1,5)
par(oma=c(0,0,3,0),mfrow=c(2,4)) #combine 8 radar plots into one
for ( i in 1:8){
  radar <- as.data.frame(matrix(2,4,19))
  radar[2,]<-rep(-2,19)
  radar[3,] <- pew1[i,2:20]
  radar[4,] <- table1[class1[i],2:20]
  radarchart(radar, title = pew1[i,1])}
mtext("Transformation 1\nPew Typology and K-means Cluster Values",
      side=3, line=-.75, font=2, cex=1, outer = TRUE) #plot title
par(usr=c(0,1,0,1), # Reset the coordinates
    xpd=NA) #Allow plottting outside the last plot
legend(0,4.2, # Placement at top right of plot
       c("Pew Typology", "K-means Analysis"), lty=1, lwd=3, 
       col=c("black", "red"))
#plots for 2nd transformation
class2 <- c(3,7,6,4,1,2,5,8)
par(oma=c(0,0,3,0),mfrow=c(2,4)) #combine 8 radar plots into one
for (i in 1:8){
  radar <-as.data.frame(matrix(2,4,9))
  radar[2,] <- rep(-2,9)
  radar[3,] <- pew2[i,2:10]
  radar[4,] <- table2[class2[i],2:10]
  radarchart(radar, title = pew2[i,1])
}
mtext("Transformation 2\nPew Typology and K-means Cluster Values",
      side=3, line=-.75, font=2, cex=1, outer = TRUE) # plot title
par(usr=c(0,1,0,1), # Reset the coordinates
    xpd=NA) #Allow plottting outside the last plot
legend(0,4.2, # Placement at top right of plot
       c("Pew Typology", "K-means Analysis"), lty=1, lwd=3, 
       col=c("black", "red"))
#plots for 3rd transformation
class3 <- c(7,2,5,6,1,8,3,4)
par(oma=c(0,0,3,0),mfrow=c(2,4)) #combine 8 radar plots into one
for (i in 1:8){
  radar <- as.data.frame(matrix(2,4,19))
  radar[2,]<-rep(-2,19)
  radar[3,]<-pew3[i,2:20]
  radar[4,]<-table3[class3[i],2:20]
  radarchart(radar, title = pew3[i,1])
}
mtext("Transformation 3\nPew Typology and K-means Cluster Values",
      side=3, line=-.75, font=2, cex=1, outer = TRUE) #plot title
par(usr=c(0,1,0,1), # Reset the coordinates
    xpd=NA) #Allow plottting outside the last plot
legend(0,4.2, # Placement at top right of plot
       c("Pew Typology", "K-means Analysis"), lty=1, lwd=3, 
       col=c("black", "red"))
#plots for 4th transformation
class4 <- c(6,7,2,3,8,4,5,1)
par(oma=c(0,0,3,0),mfrow=c(2,4)) #combine 8 radar plots into one
for (i in 1:8){
  radar <-as.data.frame(matrix(2,3,9))
  radar[2,] <- rep(-2,9)
  radar[3,] <- pew4[i,2:10]
  radar[4,] <- table4[class4[i],2:10]
  #radarchart(radar, title = as.character(table4[i,1]))
  radarchart(radar, title = pew4[i,1])
}
mtext("Transformation 4\nPew Typology and K-means Cluster Values",
      side=3, line=-.75, font=2, cex=1, outer = TRUE) #plot title
par(usr=c(0,1,0,1), # Reset the coordinates
    xpd=NA) #Allow plottting outside the last plot
legend(0,4.2, # Placement at top right of plot
       c("Pew Typology", "K-means Analysis"), lty=1, lwd=3, 
       col=c("black", "red"))