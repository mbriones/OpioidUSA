library(readr) 
library(dplyr) 
library(lubridate) 
library(data.table) 
library(ggplot2)
library(caret)

#Set up working directory
setwd("~/Desktop/Insight ")

#Load the data into R - OpioidDeath from 1999-2014
OpioidDeath <- read.csv("https://query.data.world/s/sEJEHauCDs9uCJr1MCtolM61NjAvdg", header=TRUE, stringsAsFactors=FALSE)

#Which state has the most overall deaths?
StateDeath <- data_frame(OpioidDeath$State, OpioidDeath$Deaths)
colnames(StateDeath) <- c("State", "Deaths")
StateDeath <- subset(StateDeath, Deaths != "Suppressed")
StateDeath$Deaths <- as.numeric(as.character(StateDeath$Deaths))
StateDeath <- aggregate(. ~  State, data = StateDeath, sum)
StateDeath <- arrange(StateDeath, desc(Deaths))
StateDeath.Top <- StateDeath[1:10,]

ggplot(StateDeath.Top, aes(x=State, y=Deaths, fill = State)) + geom_bar(stat = "Identity")
StateDeath.Top

#-->California had the most deaths across all years with 27,044 deaths

#What year was most fatal for California in relation to that years population size?
CaliDeath <- data_frame(OpioidDeath$State, OpioidDeath$Year, OpioidDeath$Deaths, OpioidDeath$Population)
colnames(CaliDeath) <- c("State", "Year", "Deaths", "Population")
CaliDeath <- subset(CaliDeath, Deaths != "Suppressed" & State == "California")
CaliDeath$Deaths <- as.numeric(as.character(CaliDeath$Deaths))
CaliDeath <- arrange(CaliDeath, desc(Deaths))
CaliDeath[,2:3]

#-->Before population normalization, 2014 was the most fatal year with 2,159 deaths.

CaliDeath$Porportion <- CaliDeath$Deaths/CaliDeath$Population *exp(10)
CaliDeath <- arrange(CaliDeath, desc(Porportion))
CaliDeath[,c("Year", "Porportion")]
#--> When normalized to size of population, 2009 was the most fatal year

#Is there a relationship between perscriptions dispensed and number of deaths for the state of California?
CaliPrescrip <- data_frame(OpioidDeath$State, OpioidDeath$Year, OpioidDeath$Deaths, OpioidDeath$Prescriptions.Dispensed.by.US.Retailers.in.that.year..millions.)
colnames(CaliPrescrip) <- c("State", "Year", "Deaths", "Prescriptions")
CaliPrescrip <- subset(CaliPrescrip, Deaths != "Suppressed" & State == "California")
CaliPrescrip$Deaths <- as.numeric(as.character(CaliPrescrip$Deaths))

ggplot(CaliPrescrip, aes(x = Deaths, y = Prescriptions)) + geom_point()
CaliPrescripRelation <-lm(Prescriptions~Deaths, data=CaliPrescrip)
summary(CaliPrescripRelation)
#-->Coefficients:
#-->Estimate Std. Error t value Pr(>|t|)    
#-->(Intercept) 66.90685   24.09571   2.777 0.014845 *  
#-->Deaths       0.06373    0.01387   4.593 0.000418 ***
#-->  ---
#-->  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-->Residual standard error: 22.14 on 14 degrees of freedom
#-->Multiple R-squared:  0.6011,	Adjusted R-squared:  0.5726 
#-->F-statistic:  21.1 on 1 and 14 DF,  p-value: 0.0004178

#--> Our intercept is 66.9 and our slope is 0.06. We can intepret it as California having an 
#--> average increase of 67 deaths per year over the course of 15 years.
#--> The slope indicates that  every 1 increase in death is equal to 1/6th of a prescription. 
#--> Our R squared is 60%, indicating that roughly 60% of the variance in deaths can
#--> be explained by the number ofprescriptions. Our F-statstic is 21.1, with a p-value
#--> of >0.01, indicating that there is a strong relationship Prescriptions and Number of Deaths
#--> in the state of California. However, it can be stronger. 

cor.test(x=CaliPrescrip$Deaths, y=CaliPrescrip$Prescriptions, method = 'pearson')
#-->t = 4.5932, df = 14, p-value = 0.0004178
#-->alternative hypothesis: true correlation is not equal to 0
#-->95 percent confidence interval:
#-->  0.4541534 0.9181498
#-->sample estimates:
#-->  cor 
#-->0.775315. Therefore, there is a strong, positive correlation between number of deaths 
#--> and number of prescriptions given in California. Our p-value is >0.01, indicating
#-->that our correlation is statistically significant. 
#--> We can reject the null hypothesis of no correlation between the two variables. 

#Is there a relationship between perscriptions dispensed and number of deaths Nationwide between 1999-2014?
UsPrescripYear <- data_frame(OpioidDeath$Year, OpioidDeath$Prescriptions.Dispensed.by.US.Retailers.in.that.year..millions.)
colnames(UsPrescripYear) <- c("Year", "Prescriptions")
UsPrescripYear <- UsPrescripYear[1:16,]
UsYearDeaths <- data_frame(OpioidDeath$Year, OpioidDeath$Deaths)
colnames(UsYearDeaths) <- c("Year", "Deaths")
UsYearDeaths <- subset(UsYearDeaths, Deaths != "Suppressed")
UsYearDeaths$Deaths <- as.numeric(as.character(UsYearDeaths$Deaths))
UsYearDeaths <- aggregate(. ~  Year, data = UsYearDeaths, sum)

UsPrescripDeath <- merge(UsPrescripYear, UsYearDeaths, by = 'Year')
ggplot(UsPrescripDeath, aes(x = Deaths, y = Prescriptions)) + geom_point()
UsPrescripDeathRelation <-lm(Prescriptions~Deaths, data=UsPrescripDeath)
summary(UsPrescripDeathRelation)
#Coefficients:
 #             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 8.494e+01  1.037e+01   8.190 1.04e-06 ***
#Deaths      4.958e-03  5.431e-04   9.129 2.85e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 13.29 on 14 degrees of freedom
#Multiple R-squared:  0.8562,	Adjusted R-squared:  0.8459 
#F-statistic: 83.34 on 1 and 14 DF,  p-value: 2.852e-07

#--> Our intercept is 84.94 and our slope is 0.0049. We can intepret it as the US having an 
#--> average increase of 85 deaths per year over the course of 15 years.
#--> The slope indicates that for every 1 increase in death is equal to 1/400th of a prescription. 
#--> Our R squared is 86%, indicating that roughly 86% of the variance in deaths can
#--> be explained by the number ofprescriptions. Our F-statstic is 83.34, with a p-value
#--> of >0.01, indicating that there is a strong relationship Prescriptions and Number of Deaths
#--> in the US. 

cor.test(x=UsPrescripDeath$Deaths, y=UsPrescripDeath$Prescriptions, method = 'pearson')
#t = 9.1293, df = 14, p-value = 2.852e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.7935987 0.9741745
#sample estimates:
#  cor 
#0.9252994. There is a strong, positive correlation between number of deaths 
#--> and number of prescriptions given in the US. Our p-value is >0.01, indicating
#-->that our correlation is statistically significant. 
#--> We can reject the null hypothesis of no correlation between the two variables. 

## Using our lm model for US deaths related to prescriptions, can we predict furture deaths due to prescriptions?
USPrescripModel <- train(Prescriptions ~ Deaths, data = UsPrescripDeath, method = "lm")

USPrescripcoef.icept <- coef(USPrescripModel$finalModel)[1]
USPrescripcoef.slope <- coef(USPrescripModel$finalModel)[2]

ggplot(data = UsPrescripDeath, aes(x = Deaths, y = Prescriptions)) +
  geom_point() +
  geom_abline(slope = USPrescripcoef.slope, intercept = USPrescripcoef.icept, color = "orange")

# We can predict that as prescriptions increase across the U.S., so will deaths. 

