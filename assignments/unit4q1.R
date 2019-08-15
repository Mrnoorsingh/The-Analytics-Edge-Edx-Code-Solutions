claim=read.csv("ClaimsData.csv")
table(claim$bucket2009)/nrow(claim)#proportion of people in each bucket

#spltiing data
library(caTools)
set.seed(88)
split=sample.split(claim$bucket2009,SplitRatio = 0.6)
train=subset(claim,split==TRUE)
test=subset(claim,split==FALSE)

table(train$diabetes)/nrow(train)#proportion of people having diabetes

#baseline model
table(test$bucket2009,test$bucket2008)
#here baseline model is 2008 bucket as such is used to predict 2009 bucket
#accuracy is calculated by adding diagnol values and dividing nrows(test)

#penality matrix
penality=matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE,nrow = 5)

#finding penality error
sum(as.matrix(table(test$bucket2009,test$bucket2008))*penality)/nrow(test)

#conclusion- basline line model has penality erro of 0.738 and accuracy of 0.68
#OUR GOAL - decreasing the penality error and increasing accuracy above 0.68

#baseline model penality error if baseline model is defined as most frequent outcome i.e 1
tab=table(test$bucket2009)
vec=c(0,2,4,6,8)
pe=sum(tab*vec)/nrow(test)

