song=read.csv("songs.csv")
song_2010=subset(song,year==2010)
table(song$artistname=="Michael Jackson")
#subsetting data frame and dropping factors in the subsetted dataframe
michael=subset(song,artistname=="Michael Jackson")
michael$songtitle=factor(michael$songtitle)
table(michael$Top10==1,michael$songtitle)
table(song$timesignature)
max(song$tempo)
table(song$songtitle=="Until The Day I Die",song$tempo==max(song$tempo))

#splittng dataset into training and testing
song_train=subset(song,song$year<=2009)
song_test=subset(song,song$year==2010)

#removing some indpendent variables from training and testing
nonvar=c("songtitle","year","artistname","songID","artistID")
song_train = song_train[,!(names(song_train)%in%nonvar)]
song_test=song_test[,!(names(song_test)%in%nonvar)]
model1=glm(Top10~.,data = song_train,family = binomial)


#multicollinearity in the model
cor(song_train$loudness,song_train$energy)#both variables are highly correlated

#new model 2 without loudness variable(loudness is numeric variable so it can be subtracted from model itself)
model1=glm(Top10~.-loudness,data = song_train,family = binomial)

#new model no.3 without energy variable(it can be subtracted too) and predicting test data and accuracy check
model3=glm(Top10~.-energy,data = song_train,family = binomial)
test_predict=predict(model3,type = "response",newdata = song_test)
table(song_test$Top10,test_predict >0.45)
table(song_test$Top10)

