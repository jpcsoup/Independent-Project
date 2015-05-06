# Using the values defined for Solid Liberals by Pew, I will flip the sign of the variable such that Liberal valuea are all negative

# Transformations are listed first, and then the charts are reproduced after

# Transformation 1 & 3
x<- rep(-1,19)
x[c(3:7,16:17)] <- 1
pew1a <- pew1
table1a<-table1
pew3a <- pew3
table3a<-table3
for (i in 1:19){
  pew1a[,(i+1)] <- as.numeric(pew1a[,(i+1)])*x[i]
  table1a[,(i+1)]<- as.numeric(table1a[,(i+1)])*x[i]
  pew3a[,(i+1)] <- as.numeric(pew3a[,(i+1)])*x[i]
  table3a[,(i+1)]<- as.numeric(table3a[,(i+1)])*x[i]
}

# Transformation 2 & 4
x <- rep(-1,9)
x[c(2,3,8)] <- 1
pew2a <- pew2
table2a<-table2
pew4a <- pew4
table4a<-table4
for (i in 1:9){
  pew2a[,(i+1)] <- as.numeric(pew2a[,(i+1)])*x[i]
  table2a[,(i+1)] <- as.numeric(table2a[,(i+1)])*x[i]
  pew4a[,(i+1)] <- as.numeric(pew4a[,(i+1)])*x[i]
  table4a[,(i+1)] <- as.numeric(table4a[,(i+1)])*x[i]
}

# Plot 1 (Replace values for 3)
class1 <- c(6,2,4,7,8,3,1,5)
par(oma=c(0,0,3,0),mfrow=c(1,8)) #combine 8 radar plots into one
for ( i in 1:8){
  radar <- as.data.frame(matrix(2,4,19))
  radar[2,]<-rep(-2,19)
  radar[3,] <- pew1a[i,2:20]
  radar[4,] <- table1a[class1[i],2:20]
  radarchart(radar, title = pew1a[i,1])}
mtext("Transformation 1\nPew Typology and K-means Cluster Values",
      side=3, line=-.75, font=2, cex=1, outer = TRUE) #plot title


# Plot 2 (Replace values for 4)
par(oma=c(0,0,3,0),mfrow=c(1,8)) #combine 8 radar plots into one
for (i in 1:8){
     radar <-as.data.frame(matrix(2,3,9))
     radar[2,] <- rep(-2,9)
     radar[3,] <- pew2a[i,2:10]
     radar[4,] <- table2a[class2[i],2:10]
     radarchart(radar, title = pew2a[i,1])
 }
mtext("Transformation 2\nPew Typology and K-means Cluster Values",
             side=3, line=-.75, font=2, cex=1, outer = TRUE) #plot title

# ggplot lines
library(reshape)
dfplotcka<-rbind(melt(pew1a,id.vars=c("Group.1")),melt(pew2a,
              id.vars="Group.1"),melt(pew3a,id.vars="Group.1"),
              melt(pew4a,id.vars="Group.1"),deparse.level=2)

dfplotcka$table<-NA
dfplotcka$table[1:152]<- "table1"
dfplotcka$table[153:224]<- "table2"
dfplotcka$table[225:376]<- "table3"
dfplotcka$table[377:448]<- "table4"

table_i <- c("table1","table2","table3","table4")
graph<- par(oma=c(0,0,3,0),mfrow=c(2,2))
for (i in 1:4){
  dfplotcka_i <- dfplotcka[dfplotcka$table==table_i[i],]
  ggplot(dfplotcka_i, aes(x=variable, y=value, color=rev(as.factor(Group.1)))) + 
    geom_point(size=4) + theme_bw() +
    theme( # remove the horizontal grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the vertical lines (or they will disappear too)
      panel.grid.major.y = element_line( size=3.5, color="honeydew3")) + 
    scale_colour_brewer(palette="RdYlBu", 
                        labels=c("Stanch Conservatives","Main Street Republicans","Libertarians","Disaffecteds","Post-Moderns","New Coalition Democrats","Hard-Pressed Democrats", "Solid Liberals")) +
    labs(title=sprintf("K-Means Cluster Values\nTransformation %i",i), 
         color = "Cluster") + coord_flip()
}
mtext("Pew Typology and K-means Cluster Values\nLeft-Right Scale",
      side=3, line=-.75, font=2, cex=1, outer = TRUE) #plot title