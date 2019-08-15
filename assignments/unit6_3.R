
                                          ####CLUSTER THEN PREDICT#####
stock=read.csv("StocksCluster.csv")
library(caTools)
set.seed(144)
spl = sample.split(stock$PositiveDec, SplitRatio = 0.7)
stocktrain = subset(stock, spl == TRUE)
stocktest = subset(stock, spl == FALSE)

#logistic model
lgmodel=glm(PositiveDec~.,data = stocktrain,family = "binomial")
train_pd=predict(lgmodel,newdata = stocktrain,type = "response")
table(stocktrain$PositiveDec,train_pd>0.5)

#prediction on testing data
test_pd=predict(lgmodel,newdata = stocktest,type = "response")
table(stocktest$PositiveDec,test_pd>0.5)
table(stocktest$PositiveDec)#0.567 accuracy and 0.54 baseline accuracy

#In cluster-then-predict, our final goal is to predict the dependent variable,
#which is unknown to us at the time of prediction. Therefore, if we need to know 
#the outcome value to perform the clustering, the methodology is no longer useful
#for prediction of an unknown outcome value.

#This is an important point that is sometimes mistakenly overlooked. 
#If you use the outcome value to cluster, you might conclude your method 
#strongly outperforms a non-clustering alternative. However, this is because it
#is using the outcome to determine the clusters, which is not valid.


#removing dependent variable before clustering
limitedtrain = stocktrain
limitedtrain$PositiveDec = NULL
limitedtest = stocktest
limitedtest$PositiveDec = NULL



#preprocessing and noramlizing
library(caret)
preproc = preProcess(limitedtrain)
normtrain = predict(preproc, limitedtrain)
normtest = predict(preproc, limitedtest)



#In cases where we have a training and testing set, we'll want to normalize by the mean and 
#standard deviation of the variables in the training set. We can do this by passing just the
#training set to the preProcess function



#k means clustering
k=3
set.seed(144)
cluster_k=kmeans(normtrain,centers = 3)
split=split(normtrain,cluster_k$cluster)
nrow(split[[2]])

#clustering on training and testing data using flexclust
library(flexclust)
km.kcca = as.kcca(cluster_k, normtrain)
clustertrain = predict(km.kcca)
clustertest = predict(km.kcca, newdata=normtest)
table(clustertest)


#data frames of subsets of stocktrain 
stocktrain1=subset(stocktrain,clustertrain==1)
stocktrain2=subset(stocktrain,clustertrain==2)
stocktrain3=subset(stocktrain,clustertrain==3)


#dataframes of subsets of stocktest base of different clusters
stocktest1=subset(stocktest,clustertest==1)
stocktest2=subset(stocktest,clustertest==2)
stocktest3=subset(stocktest,clustertest==3)



#logistic model of three training datasets
lg_model1=glm(PositiveDec~.,data = stocktrain1,family = "binomial")
lg_model2=glm(PositiveDec~.,data = stocktrain2,family = "binomial")
lg_model3=glm(PositiveDec~.,data = stocktrain3,family = "binomial")


#predictions on three different testing datasets
test_pd1=predict(lg_model1,newdata=stocktest1,type="response")
test_pd2=predict(lg_model2,newdata=stocktest2,type="response")
test_pd3=predict(lg_model3,newdata=stocktest3,type="response")


table(stocktest1$PositiveDec,test_pd1>0.5)# 0.6194145
table(stocktest2$PositiveDec,test_pd2>0.5)# 0.5504808
table(stocktest3$PositiveDec,test_pd3>0.5)# 0.6458333

#overall prediction
#we put all the predicted outcomes in one vector and actual outcomes in other
AllPredictions = c(test_pd1,test_pd2,test_pd3)
AllOutcomes = c(stocktest1$PositiveDec, stocktest2$PositiveDec, stocktest3$PositiveDec)
table(AllOutcomes,AllPredictions>0.5)
2011/length(AllOutcomes)#0.57

#We see a modest improvement over the original logistic regression model. 
#Since predicting stock returns is a notoriously hard problem, this is a good increase in accuracy.
#By investing in stocks for which we are more confident that they will have positive returns 
#(by selecting the ones with higher predicted probabilities), 
#this cluster-then-predict model can give us an edge over the original logistic regression model
