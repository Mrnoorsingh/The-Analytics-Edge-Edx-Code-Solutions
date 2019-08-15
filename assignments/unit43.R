census=read.csv("census.csv")
library(caTools)
set.seed(2000)
split=sample.split(census$over50k,SplitRatio = 0.6)
train=subset(census,split==TRUE)
test=subset(census,split==FALSE)

#logitsic regression model
lgmodel=glm(over50k~.,data = train,family = "binomial")
test_pred=predict(lgmodel,newdata = test,type = "response")
table(test$over50k,test_pred>0.5)#accuracy of 0.855
table(test$over50k)#baseline accuracy is 0.759

#ROCR and AUC
library(ROCR)
rocr_pred=prediction(test_pred,test$over50k)
rocr_plot=performance(rocr_pred,"tpr","fpr")
plot(rocr_plot)
rocr_perf=as.numeric(performance(rocr_pred,"auc")@y.values)#area under the curve is 0.906


#CART Model
library(rpart)
library(rpart.plot)
cart_model=rpart(over50k~.,data = train,method = "class")
prp(cart_model)
test_pred=predict(cart_model,newdata = test,type = "class")#You can either add the argument type="class", or generate probabilities and use a threshold of 0.5 like in logistic regression.)
table(test$over50k,test_pred)#accuracy of 0.84
#This highlights a very regular phenomenon when comparing CART and logistic regression. 
#CART often performs a little worse than logistic regression in out-of-sample accuracy. 
#However, as is the case here, the CART model is often much simpler to describe and understand.


#regression tree
test_pred1=predict(cart_model,newdata = test)
rocr_pred1=prediction(test_pred1[,2],test$over50k)
rocr_plot1=performance(rocr_pred1,"tpr","fpr")#for ploting AUC
plot(rocr_plot1)
rocr_perf1=as.numeric(performance(rocr_pred1,"auc")@y.values)#for area under the curve(0.84)

#plotting side by side(for comparison)
par(mfrow=c(1,2))#no. of rows and columns
plot(rocr_plot,main="Logistic Regression")
plot(rocr_plot1,main="CART")#read question 2.5 of third assignment unit 4


#RANDOM FOREST
#before building the model, create new small training dataset from original training set(for computational purposes)
#selected randomly
library(randomForest)
set.seed(1)
smalltrain=train[sample(nrow(train),2000),]#here 2000 is number of observations for new  training set
set.seed(1)
rfmodel=randomForest(over50k~.,data = smalltrain)#default nodisze and ntree
test_pred2=predict(rfmodel,newdata = test)
table(test$over50k,test_pred2)


#since RANDOM FOREST is not interpretable as CART but still we can compute 
#metrics that give us insight into which variables are important.

var=varUsed(rfmodel,count = TRUE)
sorted=sort(var,decreasing = FALSE,index.return=TRUE)
dotchart(sorted$x,names(rfmodel$forest$xlevels[sorted$ix]))


varImpPlot(rfmodel)

#cross validation
library(ggplot2)
library(lattice)
library(e1071)
library(caret)
set.seed(2)
numfold=trainControl(method = "cv",number = 10)
cpgrid=expand.grid(.cp=seq(0.002,0.1,0.002))
train(over50k~.,data=train,method="rpart",tuneGrid=cpgrid,trControl=numfold)


#CART model using cp=0.002
cart_model1=rpart(over50k~.,data = train,method = "class",cp=0.002)
cart_pred=predict(cart_model1,newdata = test,type = "class")
table(test$over50k,cart_pred)#accuracy of 0.8612
par(mfrow=c(1,1))
prp(cart_model1)
