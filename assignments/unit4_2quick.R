#reading the file and splitting the data
steven=read.csv("stevens.csv")
library(caTools)
set.seed(3000)
split=sample.split(steven$Reverse,SplitRatio = 0.7)
train=subset(steven,split==TRUE)
test=subset(steven,split==FALSE)
#installing paackages for trees
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#tree model for steven and display tree structure
steventree=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method = "class",minbucket=100)
prp(steventree)
#predictng on test data
predtest=predict(steventree,newdata=test,type="class")
table(test$Reverse,predtest)

#finding rocr and plot rocr
library(ROCR)
predrocr=predict(steventree,newdata=test)
pred=prediction(predrocr[,2],test$Reverse)
perf=performance(pred,"tpr","fpr")#tpr for true positive rate and fpr for false postitive rate (for x and y axis)
plot(perf)
auc=as.numeric(performance(pred, "auc")@y.values)

#random forest


install.packages("randomForest")
library(randomForest)
set.seed(200)
frmodel=randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,nodsize=25,ntree=200)
train$Reverse=as.factor(train$Reverse)
test$Reverse=as.factor(test$Reverse)
predict_test=predict(frmodel,newdata = test)
table(test$Reverse,predict_test)


#cross validation
install.packages("caret")
install.packages("e1071")
library(lattice)
library(ggplot2)
library(e1071)
library(caret)

numfolds=trainControl(method ="cv",number = 10)#here 10 is number of folds and 'cv' cross validation
cpgrid=expand.grid(.cp=seq(0.01,0.5,0.01))#cpgrid will be assigned all values in sequence with increment of 0.01.
                                          #these values will be used as cp values in testing data
train(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method="rpart",trControl=numfolds,tuneGrid=cpgrid)#performing cross validation

#building model using cp=0.19
steventreecv=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method = "class",cp=0.19)
stevenpredict=predict(steventreecv,newdata=test,type="class")
table(test$Reverse,stevenpredict)
prp(steventreecv)
#accuracy of .72 which is greater than randomforest which is further greater than cart