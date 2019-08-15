setwd("/home/noor/The Analytics Edge/csv files")
flutrain=read.csv("FluTrain.csv")
flutest=read.csv("FluTest.csv")
index=which.max(flutrain$ILI)
flutrain$Week[303]
flutrain$Week[which.max(flutrain$Queries)]
hist(flutrain$ILI)
plot(flutrain$ILI,flutrain$Queries)
plot(log(flutrain$ILI),flutrain$Queries)

#linear regression model
model1=lm(log(ILI)~Queries,data = flutrain)
corr=cor(log(flutrain$ILI),flutrain$Queries)
corr^2
log(1/corr)

#predicting values on testdata
prediction=exp(predict(model1,newdata = flutest))
which(flutest$Week=="2012-03-11 - 2012-03-17")
prediction[11]

#relative error
obs=flutest$ILI[which(flutest$Week=="2012-03-11 - 2012-03-17")]
est=prediction[which(flutest$Week=="2012-03-11 - 2012-03-17")]
rel_error=1-est/obs

#rmse
sse=sum((flutest$ILI-prediction)^2)
rmse=sqrt(sse/52)


#install zoo
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(flutrain$ILI), -2, na.pad=TRUE)
flutrain$ILILag2 = coredata(ILILag2)
tapply(is.na(), index, function)
table(is.na(flutrain$ILILag2))

#plot
plot(flutrain$ILILag2,log(flutrain$ILI))
model2=lm(log(ILI)~Queries+log(ILILag2),data = flutrain)

ILILag2 = lag(zoo(flutest$ILI), -2, na.pad=TRUE)
flutest$ILILag2 = coredata(ILILag2)
table(is.na(flutest$ILILag2))
prediction1=exp(predict(model2,newdata = flutest))
flutest$ILILag2[1]=flutrain$ILI[nrow(flutrain)-1]
flutest$ILILag2[2]=flutrain$ILI[nrow(flutrain)]

sse2=sum((flutest$ILI-prediction1)^2)
rmse1=sqrt(sse2/nrow(flutest))