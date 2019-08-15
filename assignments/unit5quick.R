tweet=read.csv("tweets.csv",stringsAsFactors = FALSE)#second argument is important when working on text analytics problem
tweet$negative=as.factor(tweet$Avg<=-1)#creating new variable negative with FALSE and TRUE observation values
table(tweet$negative)#182 out of 1181 are negative 
library(tm)#tm for text mining. library is used for preprocessing
library(SnowballC)
corpus=Corpus(VectorSource(tweet$Tweet))#corpus is collection of documents and "corpus" var is 1181 files or doc
corpus=tm_map(corpus,tolower)#lowercasing all the letters
corpus[[1]]$content#to see first file's content
corpus=tm_map(corpus,removePunctuation)#removes panctuations e.g @
stopwords("english")
corpus=tm_map(corpus,removeWords,c("apple",stopwords("english")))#here apple is not in stopword collection
corpus=tm_map(corpus,stemDocument)#stemming
                         ###end of preprocessing###

frequency=DocumentTermMatrix((corpus))#matrixform of document
inspect(frequency[1000:1005,505:515])#1000:1005 for documents in corpus and 505:515 for total words(repeated ofcourse)
                                     #creates sparse matrix
findFreqTerms(frequency,lowfreq = 100)#terms repeating atleast 20 times
#This means that we probably have a lot of terms
#that will be pretty useless for our prediction model.
#The number of terms is an issue for two main reasons.One is computational.
#More terms means more independent variables,
#which usually means it takes longer to build our models.
#The other is in building models, as we mentioned
#before, the ratio of independent variables to observations
#will affect how good the model will generalize.


#remove terms which dont appear often
sparse=removeSparseTerms(frequency,0.995)#0.995 means keep only terms which repeat in 0.5 or more of the tweets

#making data frame from sparse matrix
tweetsparse=as.data.frame(as.matrix(sparse))
#since R struggles with variable names with numbers so converting into approp. names
colnames(tweetsparse)=make.names(colnames(tweetsparse))
tweetsparse$negative=tweet$negative
library(caTools)
set.seed(123)
split=sample.split(tweetsparse$negative,SplitRatio = 0.7)#negative is dep. var.(pred negtive sentiment)
train=subset(tweetsparse,split==TRUE)
test=subset(tweetsparse,split==FALSE)
#sentiment prediction
library(rpart)
library(rpart.plot)
#prediction using CART
tweetmodel=rpart(negative~.,data = train,method = "class")
prp(tweetmodel)
predict_test=predict(tweetmodel,newdata = test,type = "class")
table(test$negative,predict_test)#0.87 accuracy and 0.84 baseline accuracy
#conclusion- CART model does better job than baseline modelzz

#Random Forest
library(randomForest)
set.seed(123)
rfmodel=randomForest(negative~.,data = train)
rfpredict=predict(rfmodel,newdata=test)
table(test$negative,rfpredict)#0.88 accuracy
#conclusion-since rfmodel accuracy and cartmodel accuracy is almost same
#cartmodel is preferred for its interpretibility


#logistic regression
lgmodel=glm(negative~.,data = train,family = "binomial")
lgpredict=predict(lgmodel,newdata = test,type = "response")
table(test$negative,lgpredict>0.5)
