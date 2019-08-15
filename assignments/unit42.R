letters=read.csv("letters_ABPR.csv")
letters$isB=as.factor(letters$letter=="B")
library(caTools)
set.seed(1000)
split=sample.split(letters$letter,SplitRatio = 0.5)
train=subset(letters,split==TRUE)
test=subset(letters,split==FALSE)
#baseline model(not B)
table(test$letter)#0.754172
#CART model
library(rpart)
library(rpart.plot)
ctmodel=rpart(isB~.-letter,data = train,method = "class")
prp(ctmodel)
test_pred=predict(ctmodel,newdata = test,type = "class")
table(test$isB,test_pred)#accuracy is 0.94


#random forest
library(randomForest)
set.seed(1000)
rfmodel=randomForest(isB~.-letter,data=train)#ntree and nodsize default value
frtest_pred=predict(rfmodel,newdata=test)
table(test$isB,frtest_pred)#accuracy is 0.98 which is very significant


#prediction for ABPR  letters using CART
letters$letter=as.factor(letters$letter)
set.seed(2000)
split=sample.split(letters$letter,SplitRatio = 0.5)
train=subset(letters,split==TRUE)
test=subset(letters,split==FALSE)
#baseline model
table(test$letter)
cart_model=rpart(letter~.-isB,data = train,method="class")
cart_test=predict(cart_model,newdata = test,type = "class")
table(test$letter,cart_test)#accuracy is 0.87

#prediction using random forests
set.seed(1000)
rf_model1=randomForest(letter~.-isB,data = train)#default ntree and nodsize values
rf_pred1=predict(rf_model1,newdata=test)
table(test$letter,rf_pred1)#accuracy is 0.98
#3 conclusions 
# 1- CART model accuracy decreased significantly in multi categorical dependent variable which was about 0.94 in binary variable(B,not B) and decreased
#to 0.87 in 4 letter prediction
# 2- RANDOM FOREST model of 4 letters accuracy is significantly higher than CART model i.e 0.98 of rf model and 0.87 of CART model
# 3- RANDOM FOREST model's accuracy decreased by only tiny amount in 4 letter prediction than in binary dependent variable 