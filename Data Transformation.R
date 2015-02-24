#This code is not finalized and is still being modified while finalizing analysis
#Please contact me at jpcsoup@gmail.com if you intend to use this code outside personal or academic use

#Upload data into R dataframe
#------------------------------------------------------------------------------------#

library(foreign)
df2011 <- read.spss("/Users/JonathanCampbell/Desktop/Pew Typology/Pew's 2011 Typology Survey/2011 Political Typology Survey/2011 Political Typology public.sav",to.data.frame=TRUE)

#Create subset for manipulation and analysis
#------------------------------------------------------------------------------------#

#subset cluster variables based off of: http://www.people-press.org/2011/05/04/about-the-political-typology/
cvar <- c("mergeid", "q17a", "q17b", "q37u", "q37w", "q37aa", "q17m", "q17n", "q37q", "q37r",
          "q17g", "q37dd", "q17f", "q37hh", "q17c", "q17d", "q17i", "q37bb", "q37y", "q37z")
df <- subset(df2011, df2011$bys == 0, select = cvar)

#Data transformation No. 1
#------------------------------------------------------------------------------------#

#recode variables on an ordinal scale
library(plyr)
df1<-matrix(nrow=2818,ncol=20)
for (i in 1:20){
  df1[,i] <- mapvalues(df[,i], from = levels(df[,2]), to = c(1,2,5,4,3,NA))}
#step above does not work, converts factor levels to factor level defined by to
#to fix switch the level 3 and 5
for (i in 1:20){
  df1[,i] <- mapvalues(df1[,i], from = c(5,3), to = c(3,5))}
#change to a -2 to +2 scale
for (i in 1:20){
  df1[,i] <- mapvalues(df1[,i], from = c(1:5,NA), to = c(-2,-1,0,1,2,NA))}

#recode individual questions so they are on the same/comparable value scales
recode <- c(1,1,-1,1,-1,1,1,1,-1,1,-1,1,-1,1,1,-1,-1,-1,-1,1)
for (i in 1:20){
  df1[,i]<- recode[i]*df1[,i]}

#Data transformation No. 2
#------------------------------------------------------------------------------------#

#Make a matrix of NA positions for summed totals
dfna<-matrix(nrow=2818,ncol=10)
dfna[, 1] =is.na(df1[, 1])
dfna[, 2] =is.na(df1[, 2])+is.na(df1[, 3])
dfna[, 3] =is.na(df1[, 4])+is.na(df1[, 5])+is.na(df1[, 6])
dfna[, 4] =is.na(df1[, 7])+is.na(df1[, 8])
dfna[, 5] =is.na(df1[, 9])+is.na(df1[,10])
dfna[, 6] =is.na(df1[,11])+is.na(df1[,12])
dfna[, 7] =is.na(df1[,13])+is.na(df1[,14])
dfna[, 8] =is.na(df1[,15])+is.na(df1[,16])
dfna[, 9] =is.na(df1[,17])+is.na(df1[,18])
dfna[,10] =is.na(df1[,19])+is.na(df1[,20])
#recode column to account for 3 columns
dfna[, 3]=mapvalues(dfna[,3], from = c(0,1,2,3), to = c(0,1,1,2))
#recode so that only locations that are both NA's will be NA
for (i in 1:10){
  dfna[,i]=mapvalues(dfna[,i], from = c(0,1,2), to = c(0,0,NA))}

#Create new dataframe of summed values
#takes the mean of the values (rm NA) and adds the value of the NA matrix
df2<-matrix(nrow=2818,ncol=10)
for (i in 1:2818){
  df2[i, 1]=df1[i,1]
  df2[i, 2]=sum(mean(c(df1[i, 2],df1[i, 3]),na.rm=TRUE),dfna[i,2])
  df2[i, 3]=sum(mean(c(df1[i, 4],df1[i, 5],df1[i,6]),na.rm=TRUE),dfna[i,3])
  df2[i, 4]=sum(mean(c(df1[i, 7],df1[i, 8]),na.rm=TRUE),dfna[i,4])
  df2[i, 5]=sum(mean(c(df1[i, 9],df1[i,10]),na.rm=TRUE),dfna[i,5])
  df2[i, 6]=sum(mean(c(df1[i,11],df1[i,12]),na.rm=TRUE),dfna[i,6])
  df2[i, 7]=sum(mean(c(df1[i,13],df1[i,14]),na.rm=TRUE),dfna[i,7])
  df2[i, 8]=sum(mean(c(df1[i,15],df1[i,16]),na.rm=TRUE),dfna[i,8])
  df2[i, 9]=sum(mean(c(df1[i,17],df1[i,18]),na.rm=TRUE),dfna[i,9])
  df2[i,10]=sum(mean(c(df1[i,19],df1[i,20]),na.rm=TRUE),dfna[i,10])
}

#Data transformation No. 3
#------------------------------------------------------------------------------------#

df3<-matrix(nrow=2818,ncol=20)
#map NA values to zero
for (i in 1:20){
  df3[,i] <- mapvalues(df1[,i], from = c(NA), to = c(0))}

#Data transformation No. 4
#------------------------------------------------------------------------------------#

df4<-matrix(nrow=2818,ncol=10)
for (i in 1:2818){
  df4[i, 1]=df3[i,1]
  df4[i, 2]=mean(c(df3[i, 2],df3[i, 3]),na.rm=FALSE)
  df4[i, 3]=mean(c(df3[i, 4],df3[i, 5],df3[i,6]),na.rm=FALSE)
  df4[i, 4]=mean(c(df3[i, 7],df3[i, 8]),na.rm=FALSE)
  df4[i, 5]=mean(c(df3[i, 9],df3[i,10]),na.rm=FALSE)
  df4[i, 6]=mean(c(df3[i,11],df3[i,12]),na.rm=FALSE)
  df4[i, 7]=mean(c(df3[i,13],df3[i,14]),na.rm=FALSE)
  df4[i, 8]=mean(c(df3[i,15],df3[i,16]),na.rm=FALSE)
  df4[i, 9]=mean(c(df3[i,17],df3[i,18]),na.rm=FALSE)
  df4[i,10]=mean(c(df3[i,19],df3[i,20]),na.rm=FALSE)
}