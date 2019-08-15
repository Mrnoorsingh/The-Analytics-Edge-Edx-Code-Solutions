setwd("/home/noor/The Analytics Edge/csv files")
climate=read.csv("climate_change.csv")
train=subset(climate,Year<=2006)
test=subset(climate,Year>2006)
model1=lm(Temp~Aerosols+TSI+CFC.11+CFC.12+CO2+CH4+N2O+MEI,data = train)
summary(model1)
cor(train)
model2=lm(Temp~Aerosols+TSI+MEI++N2O,data=train)
summary(model2)
new_model=step(model1)
summary(new_model)
prediction=predict(new_model,newdata = test
prediction
str(test)
sse=sum((test$Temp-prediction)^2)
sst=sum((test$Temp-mean(train$Temp))^2)
r_sq=1-sse/sst
r_sq