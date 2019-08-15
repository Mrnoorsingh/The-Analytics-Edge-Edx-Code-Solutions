who=read.csv("WHO.csv")
minimum=which.min(who$Over60)
minimum
country=who$Country[183]
country
lit_rate=which.max(who$LiteracyRate)
lit_rate
country1=who$Country[44]
country1
mort=tapply(who$ChildMortality, who$Region, mean)
mort