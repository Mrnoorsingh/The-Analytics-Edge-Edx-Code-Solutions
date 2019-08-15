email=read.csv("emails.csv",stringsAsFactors = FALSE)
table(email$spam)#1368 emails spam
email$text[2]
max_email=max(nchar(email$text))#43952 is the length of longest email
min_email=min(nchar(email$text))#13 is the length of shortest email
which.min(nchar(email$text))#index of email with shortest length(13)

library(tm)
library(SnowballC)
corpus=Corpus(VectorSource(email$text))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,c(stopwords("english")))
corpus=tm_map(corpus,stemDocument)
dtm=DocumentTermMatrix((corpus))
spdtm=removeSparseTerms(dtm,0.95)#limiting terms i.e taking more reasonable terms
emailsparse=as.data.frame(as.matrix(spdtm))
colnames(emailsparse)=make.names(colnames(emailsparse))#use the make.names function to make the variable names of emailsSparse valid.
which.max(colSums(emailsparse))#stem word appearing the most in all emails
emailsparse$spam=(email$spam)#adding dependent variable in the data frame
table(emailsparse$spam==1, colSums(emailsparse[,1:30])>=5000)
ham=subset(emailsparse[,1:331],emailsparse$spam==1)
sort(colSums(ham))>=1000
sort(colSums(subset(emailsparse,emailsparse$spam==0)))
emailsparse$spam=as.factor(emailsparse$spam)




#make models
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
#logistic model
set.seed(123)
split=sample.split(emailsparse$spam,SplitRatio = 0.7)
train=subset(emailsparse,split==TRUE)
test=subset(emailsparse,split==FALSE)
logmodel=glm(spam~.,data = train,family = "binomial")
log_pred=predict(logmodel,newdata = test,type = "response")
table((log_pred)>0.00001 & log_pred<0.99999)
table(test$spam,log_pred>0.5)

library(ROCR)
rocr_log=prediction(log_pred,test$spam)
auc=as.numeric(performance(rocr_log,"auc")@y.values)
#WARNING messages
#Both of these messages often indicate overfitting and the first indicates particularly severe overfitting,
#often to the point that the training set observations are fit perfectly by the model. 

#cart model
spamcart=rpart(spam~.,data = train,method = "class")
cart_pred=predict(spamcart,newdata = test,type = "class")
table(test$spam,cart_pred)
prp(spamcart)

rocr_test=predict(spamcart,newdata = test)
rocr_cart=prediction(rocr_test[,2],testspam)
auc=as.numeric(performance(rocr_cart,"auc")@y.values)


#random forest
spamrf=randomForest(spam~.,data = train)
rf_pred=predict(spamrf,newdata = test)
table(test$spam,rf_pred)

#rocr
rocr_test1=predict(spamrf,newdata = test,type = "prob")
rocr_rf=prediction(rocr_test1[,2],test$spam)
auc=as.numeric(performance(rocr_rf,"auc")@y.values)
