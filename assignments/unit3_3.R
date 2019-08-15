loan=read.csv("loans_imputed.csv")
#proportion of loans not paid
table(loan$not.fully.paid)
#subsetting data frame with na values
nadf=subset(loan,is.na(loan$log.annual.inc)==TRUE | is.na(loan$days.with.cr.line)==TRUE | is.na(loan$revol.util)==TRUE| is.na(loan$inq.last.6mths)==TRUE|is.na(loan$delinq.2yrs)==TRUE|is.na(loan$pub.rec==TRUE) )
#splitting datasets
set.seed(144)
library(caTools)
split=sample.split(loan$not.fully.paid,SplitRatio = 0.7)
nfp_train=subset(loan,split==TRUE)
nfp_test=subset(loan,split==FALSE)

model2=glm(not.fully.paid~.,data = nfp_train,family = binomial)
predicted.risk=predict(model2,newdata = nfp_test,type = "response")
nfp_test$predicted_risk=predicted.risk

#confusion matrix
table(nfp_test$not.fully.paid,predicted.risk>0.5)

#rocr
library(ROCR)
rocr_pred=prediction(predicted.risk,nfp_test$not.fully.paid)
rocr_perf=as.numeric(performance(rocr_pred,"auc")@y.values)