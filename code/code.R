setwd("C/Users/IIM")
air<-read.csv(paste("SixAirlines.csv",sep=""))
#1)Now we would check whether the first hypothesis,whether PRICE_ECONOMY and PRICE_PREMIUM are related or not.
chisq.test(air$PRICE_ECONOMY,air$PRICE_PREMIUM)

#Further we may confirm that by correlation test
cor.test(air$PRICE_PREMIUM,air$PRICE_ECONOMY)

#2)Now we would create a new column with the difference of prices of economy and premium economy tickets
tf<-air$PRICE_PREMIUM-air$PRICE_ECONOMY
air$tf<-tf # This added a column in the data set

#3)Plotting the corrgram
library(corrgram)
corrgram(air, order = TRUE, upper.panel = panel.pie, lower.panel = panel.shade)

#4)Plotting the correlation plot
library(corrplot)
corrplot(corr = cor(air [,c(2,3,6,7,8,9,10,11,12,13,14,15,16,18,17)] ,use="complete.obs"),method= "ellipse")
#From this we get an idea of what are the factors that depend on "tf".

#5)We would now check individual correlations of different factors.
cor.test(air$tf,air$FLIGHT_DURATION) #a

cor.test(air$tf,air$MONTH)  #b

cor.test(air$tf,air$INTERNATIONAL)  #c

cor.test(air$tf,air$SEATS_ECONOMY)  #d

cor.test(air$tf,air$SEATS_PREMIUM)  #e

cor.test(air$tf,air$PITCH_ECONOMY) #f

cor.test(air$tf,air$PITCH_PREMIUM)  #g

cor.test(air$tf,air$WIDTH_ECONOMY)  #h

cor.test(air$tf,air$WIDTH_PREMIUM)  #i

cor.test(air$tf,air$QUALITY)  #j

cor.test(air$tf,air$LAMBDA)  #k

#6)Now looking at the P-values of the above tests, the factors that affect the difference in the prices of the two are-
#FLIGHT_DURATION ,INTERNATIONAL ,SEATS_ECONOMY ,SEATS_PREMIUM ,WIDTH_PREMIUM ,WIDTH_ECONOMY

#7)Plotting the linear regression model
model<-lm(tf ~ FLIGHT_DURATION+INTERNATIONAL+SEATS_ECONOMY+SEATS_PREMIUM+WIDTH_PREMIUM+WIDTH_ECONOMY,data = air)
summary(model)