beers<- read.csv("C:\\Users\\Drew\\Desktop\\Doing Data Science MSDS 6306\\Case File\\Beers.csv")
breweries<- read.csv("C:\\Users\\Drew\\Desktop\\Doing Data Science MSDS 6306\\Case File\\Breweries.csv")
str(breweries)
head(breweries)
str(beers)
head(beers)

#######   packages    #############

library(tidyverse)
library(plyr)
#######   Questions    #############

#######1 How many Breweries are present in each state? ############

count(breweries,'State')

#######2 Merge beer data with breweries data by brewery id. Print first 6 observations and the 
#last six observations to check the merged file. #######

#merging data
colnames(beers)
colnames(breweries)
colnames(beers)[colnames(beers)=="Brewery_id"] <- "Brew_ID"
colnames(beers)

master_beer <- merge(breweries,beers,by="Brew_ID")
str(master_beer)
#First 6 obs master_beer
head(master_beer, n=6)

#######3  Report the number of NA's in each column. #######

colSums(is.na(master_beer))

#######4 Compute the median alcohol content and international bitterness unit for each state. Plot
#a bar chart to compare. #######

med.abv<-tapply(master_beer$ABV, master_beer$State, median)
med.IBU<-tapply(master_beer$IBU, master_beer$State, median)

barplot(med.abv)
barplot(med.IBU)

#######5 Which state has the maximum alcoholic beer? Which state has the most bitter beer? #######
master_beer[which.max(master_beer$ABV),]
master_beer[which.max(master_beer$IBU),]

#######6 Summary statistics for ABV (Alcohol by volume) variable. #######
summary(master_beer$ABV)

#######7 Is there a relationship between the bitterness of the beer and its alcoholic content? Draw
#a scatter plot. #######
ggplot(data = master_beer, mapping = aes(x = master_beer$ABV, y = master_beer$IBU))+
  geom_point() +
  geom_smooth(method = lm) 

cor.test(master_beer$ABV,master_beer$IBU)
myfit <- lm(master_beer$ABV ~ master_beer$IBU, data=master_beer)

summary(myfit)
confint(myfit)

