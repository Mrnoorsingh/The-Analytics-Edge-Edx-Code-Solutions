gerber=read.csv("gerber.csv")
table(gerber$voting)[2]/nrow(gerber)#proportion who voted in the election
table(gerber$neighbors,gerber$voting)[2,2]/table(gerber$neighbors)[2]#percentage of neighnor group who voted

              ####logistic regression model####
lgmodel=glm(voting~hawthorne+civicduty+neighbors+self,data = gerber,family = binomial)
prediction=predict(lgmodel,type = "response")#prediction on training data iteself (so no newdata to be specified)
table(gerber$voting,prediction>0.5)#confusion matrix
#accuracy of the model(with 0.3 threshold) is 0.5419578 and for 0.5 0.681 and of baseline 0.684

#rocr and finding AUC
library(ROCR)
rocr_pred=prediction(prediction,gerber$voting)
rocr_perf=as.numeric(performance(rocr_pred,"auc")@y.values)#AUC is 0.53 
#conclusion- our predicted model is weak if compared to baseline model even though ind variables are significant




                                ###decision tree(CART)###
library(rpart)
library(rpart.plot)
cartmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)#regression tree
prp(cartmodel)
cartmodel1=rpart(voting ~ civicduty ,data = gerber,cp=0.0)
prp(cartmodel1)
pred1=predict(cartmodel1)
table(gerber$voting,pred1)
cartmodel2=rpart(voting ~sex+civicduty + hawthorne + self + neighbors+control, data=gerber,cp=0.0)
prp(cartmodel2)
cartmodel3=rpart(voting~sex+control,data=gerber,cp=0.0)
prp(cartmodel3,digits=6)
cartmodel4=rpart(voting~control,data=gerber,cp=0.0)
prp(cartmodel3,digits = 6)
men=abs(0.302795-0.345818)
women=abs(0.290456-0.334176)
abs(men-women)
lgmodel1=glm(voting~control+sex,data=gerber,family ="binomial")
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))#data frame containing all possible combinations of contorl and sex
predict(lgmodel1, newdata=Possibilities, type="response")
lgmodel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")#new variable added sex:control(combination of sex and control)
                                                                                    #which means if sex:control is 1 then person is woman AND in control group
predict(lgmodel2, newdata=Possibilities, type="response")
