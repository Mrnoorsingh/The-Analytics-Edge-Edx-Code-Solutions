wiki=read.csv("wiki.csv",stringsAsFactors = FALSE)
wiki$Vandal=as.factor(wiki$Vandal)
table(wiki$Vandal)#1815 cases of vandalism
library(tm)
library(SnowballC)
addedcorpus=VCorpus(VectorSource(wiki$Added))
addedcorpus=tm_map(addedcorpus,removeWords,c(stopwords("english")))
addedcorpus=tm_map(addedcorpus,stemDocument)
dtmadded=DocumentTermMatrix((addedcorpus))#make matrix
sparse_matrix=removeSparseTerms(dtmadded,0.997)#keeping only terms which occurs in 0.3% or more tweets
word_added=as.data.frame(as.matrix(sparse_matrix))#data frame "word_added"
colnames(word_added)=paste("A",colnames(word_added))#prepend all the words(column names) with the letter A


#repeat bag of words for removed
removedcorpus=VCorpus(VectorSource(wiki$Removed))
removedcorpus=tm_map(removedcorpus,removeWords,c(stopwords("english")))
removedcorpus=tm_map(removedcorpus,stemDocument)
dtmremoved=DocumentTermMatrix((removedcorpus))
sparse_matrix1=removeSparseTerms(dtmremoved,0.997)
word_removed=as.data.frame(as.matrix(sparse_matrix1))
colnames(word_removed)=paste("R",colnames(word_removed))


#combining both the dataframes
wikiwords=cbind(word_added,word_removed)#column wise binding
wikiwords$vandal=wiki$Vandal
library(caTools)
set.seed(123)
split=sample.split(wikiwords$vandal,SplitRatio = 0.7)
train=subset(wikiwords,split==TRUE)
test=subset(wikiwords,split==FALSE)
table(test$vandal)#baseline accuracy is 0.53


#prediction using CART
library(rpart)
library(rpart.plot)
cart_model=rpart(vandal~.,data = train,method = "class")
test_pred=predict(cart_model,newdata = train,type = "class")#type="class" also means 0.5 threshold
table(train$vandal,test_pred)#predicted accuracy is 0.544
prp(cart_model)#using VCorpus gave true answer

#conclusion-Although it beats the baseline, bag of words is not very predictive for this problem


#new approach for prediction
#read question 2.1 of unit 5
#The grepl function returns TRUE if a string is found in another string
wikiwords2=wikiwords#copy dataframe
wikiwords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)#ifelse in one statement(if TRUE then return 1 else 0)
table(wikiwords2$HTTP)#217 added url(web links)
#CART model on wikiwords2
train1=subset(wikiwords2,split==TRUE)#split define earlier is used
test1=subset(wikiwords2,split==FALSE)
cart_model1=rpart(vandal~.,data = train1,method = "class")
test_pred1=predict(cart_model1,newdata = test1,type = "class")#using thresold 0.5
table(test1$vandal,test_pred1)#accuracy is 0.575

#perhaps number of words added and removed are predictive
wikiwords2$numwordsadded = rowSums(as.matrix(dtmadded))#Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2 
#rowSums calculates sum of all the rows
wikiwords2$numwordsremoved = rowSums(as.matrix(dtmremoved))


#CART model including new variable along with other ind. variables
train2=subset(wikiwords2,split==TRUE)#split define earlier is used
test2=subset(wikiwords2,split==FALSE)
cart_model2=rpart(vandal~.,data = train2,method = "class")
test_pred2=predict(cart_model2,newdata = test2,type = "class")
table(test2$vandal,test_pred2)#accuracy 0.65


#CART with minor and loggedin(metadata) included
wikiwords3=wikiwords2
wikiwords3$minor=wiki$Minor
wikiwords3$loggedin=wiki$Loggedin
train3=subset(wikiwords3,split==TRUE)#split define earlier is used
test3=subset(wikiwords3,split==FALSE)
cart_model3=rpart(vandal~.,data = train3,method = "class")
test_pred3=predict(cart_model3,newdata = test3,type = "class")
table(test3$vandal,test_pred3)
prp(cart_model3)# 3 splits









