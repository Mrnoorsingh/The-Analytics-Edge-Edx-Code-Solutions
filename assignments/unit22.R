pisatrain=read.csv("pisa2009train.csv")
pisatest=read.csv("pisa2009test.csv")
str(pisatrain)
tapply(pisatrain$readingScore,pisatrain$male==1,mean)
any(is.na(pisatrain$readingScore))
pisatrain=na.omit(pisatrain)
pisatest=na.omit(pisatest)
factor(pisatrain$raceeth)
class(pisatrain$raceeth)
levels(pisatrain$raceeth)
pisatrain$raceeth=relevel(pisatrain$raceeth,"White")
pisatest$raceeth=relevel(pisatest$raceeth,"White")
model1=lm(readingScore~.,data = pisatrain)
summary(model1)
predict_train=predict(model1,newdata = pisatrain)
sse=sum((pisatrain$readingScore-predict_train)^2)
rmse=sqrt(sse/2414)



#prediction for test data
predict_test=predict(model1,newdata = pisatest)
sse1=sum((pisatest$readingScore-predict_test)^2)
rmse=sqrt(sse1/990)

#baseline model
average=mean(pisatrain$readingScore)
sst=sum((pisatest$readingScore-average)^2)
r_sq=1-sse1/sst

#r square for test data
sse1=sum((pisatest$readingScore-predict_test)^2)
average1=mean(pisatest$readingScore)
sst=sum((pisatest$readingScore-average1)^2)
r_sq1=1-sse1/sst
r_sq1