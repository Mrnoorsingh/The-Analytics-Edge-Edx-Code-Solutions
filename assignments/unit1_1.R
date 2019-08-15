who=read.csv("WHO.csv")
minimum=which.min(who$Over60)
minimum
country=who$Country[183]
country
lit_rate=which.max(who$LiteracyRate)
lit_rate
country1=who$Country[44]
country1
setwd("/home/noor/Downloads")
mvt=read.csv("mvtWeek1.csv")
mort=tapply(who$ChildMortality, who$Region, mean)
sum(mvt$Arrest==TRUE)
sum(mvt$LocationDescription=="ALLEY")
dat=mvt$Date
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$DateConvert<-DateConvert
summary(mvt)
mvt$Month = months(DateConvert)


mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
tab=table(mvt$Month,mvt$Arrest=="TRUE")
max(tab)
hist(mvt$Date,breaks = 100)
boxplot(mvt$Date)
table(mvt$Year)
sum(mvt$Year==2012 & mvt$Arrest==TRUE)
table(mvt$LocationDescription)
sort(table(mvt$LocationDescription))
top5=subset(mvt,LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY"| LocationDescription=="GAS STATION"| LocationDescription=="DRIVEWAY - RESIDENTIAL"|LocationDescription=="STREET")
top5$LocationDescription
top5$LocationDescription = factor(top5$LocationDescription)
table(top5$LocationDescription,top5$Arrest)
table(top5$LocationDescription)
table(top5$LocationDescription=="DRIVEWAY - RESIDENTIAL",top5$Weekday)
str(mvt)
sum(top5$Weekday=="GAS STATION")