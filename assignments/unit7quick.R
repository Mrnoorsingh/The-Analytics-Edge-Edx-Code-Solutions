who=read.csv("WHO.csv")
library(ggplot2)

# Create the ggplot object with the data and the aesthetic mapping:
scatterplot = ggplot(who, aes(x = GNI, y = FertilityRate))

# Add the geom_point geometry
scatterplot + geom_point()

scatterplot + geom_point(colour = "blue", size = 3, shape = 15) + ggtitle("Fertility Rate vs. Gross National Income")

pdf("MyPlot.pdf")

print(fertilityGNIplot)

dev.off()


# Color the points by region: 
ggplot(who, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()


# Is the fertility rate of a country was a good predictor of the percentage of the population under 15?
ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()

# Let's try a log transformation:
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()


# Simple linear regression model to predict the percentage of the population under 15, using the log of the fertility rate:
mod = lm(Under15 ~ log(FertilityRate), data = who)

# Add this regression line to our plot:
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")

# 99% confidence interval
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

# No confidence interval in the plot
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE)


# Change the color of the regression line:
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", colour = "orange")



ggplot(who, aes(x = FertilityRate, y = Under15,color= Region)) + geom_point()
