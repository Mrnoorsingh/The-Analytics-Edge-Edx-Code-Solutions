trial=read.csv("clinical_trial.csv",stringsAsFactors = FALSE)
library(tm)
library(SnowballC)
colnames(trial)[1:2]=c("corpus_title","corpus_abstract")#changing 1st and 2nd column names
corpus1=Corpus(VectorSource(trial$corpus_title))
corpus1=tm_map(corpus1,tolower)
corpus1=tm_map(corpus1,removePunctuation)                
corpus1=tm_map(corpus1,removeWords,c(stopwords("english")))
corpus1=tm_map(corpus1,stemDocument)
#repeat same for corpus_abstact
corpus2=Corpus(VectorSource(trial$corpus_abstract))
corpus2=tm_map(corpus2,tolower)
corpus2=tm_map(corpus2,removePunctuation)
corpus2=tm_map(corpus2,removeWords,c(stopwords("english")))
corpus2=tm_map(corpus2,stemDocument)
dtmtitle=DocumentTermMatrix((corpus1))
dtmabstract=DocumentTermMatrix((corpus2))
dtmtitle=removeSparseTerms(dtmtitle,0.95)
dtmabstract=removeSparseTerms(dtmabstract,0.95)
dtmtitle=as.data.frame(as.matrix(dtmtitle))
dtmabstract=as.data.frame((as.matrix(dtmabstract)))

sort(colSums(dtmabstract))#find term which occurs the most(colSum sums each column)
colnames(dtmtitle) = paste0("T", colnames(dtmtitle))#adding T in front of every dtmtitle variable
colnames(dtmabstract) = paste0("A", colnames(dtmabstract))#adding A in front of every dtmabstract variable
#T and A  are added because some common variable names occur in both dataframes

dtm=cbind(dtmtitle,dtmabstract)
trial$trial=as.factor(trial$trial)
dtm$trial=trial$trial

#DATA FRAME IS PRPEARED

#spliting datasets into training and testing 
library(caTools)
set.seed(144)
split=sample.split(dtm$trial,SplitRatio = 0.7)
train=subset(dtm,split==TRUE)
test=subset(dtm,split==FALSE)
table(test$trial)#0.56 accuracy of baseline model


#CART model
library(rpart)
library(rpart.plot)
trialcart=rpart(trial~.,data = train,method = "class")
prp(trialcart)
train_pred=predict(trialcart,newdata = test)[,2]#type argument is not used as we are to find probablities of 1(trial)
                                                 #taking only 2nd column
max(train_pred)
#The maximum predicted probability will likely be exactly the same in the testing set. 
#Because the CART tree assigns the same predicted probability to each leaf node and there are a small number of leaf nodes 
#compared to data points, we expect exactly the same maximum predicted probability.

train_pred=predict(trialcart,newdata = train,type = "class")#0.82 accuracy on training data itself
table(train$trial,train_pred)
test_pred=predict(trialcart,newdata = test,type = "class")
table(test$trial,test_pred)

#ROCR
library(ROCR)
test_pred1=predict(trialcart,newdata = test)
rocr_pred=prediction(rocr_pred[,2],test$trial)
rocr_perf=performance(rocr_pred,"tpr","fpr")
plot(rocr_perf)
auc=as.numeric(performance(rocr_pred,"auc")@y.values)
