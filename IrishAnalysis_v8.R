##---------------------------------------------------------------------------##

# Douglas Masotti
# x18151493
# Version 8

##---------------------------------------------------------------------------##


# Load the libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

setwd("/Users/douglasmasotti/Documents/College/NCI/PROJ/Datasets")

# Read province csv file 
province <- read.csv("Province.csv")
head(province)

# Check empty column 
colSums(is.na(province) | province == "")

# Clean data
province[province==0] <- NA #Set zero to NA
province<-province[complete.cases(province),] # Delete rows associated with NA


##---------------------------------------------------------------------------##
#1st trend
# Distribution by year
speaker_by_year <- province %>%
  group_by(Year) %>%
  summarize(
    n=n(),
    sum=sum(Irish.Speakers)
  )

names(province)

head(speaker_by_year)

# Plot
ggplot(speaker_by_year, 
       aes(x = Year ,
           y = sum)) +
  geom_line() +
  labs(title = "Yearly change Irish Speakers", x = "year",
       y = "Change",color = "Name")



##---------------------------------------------------------------------------##
# 2nd trend
# plot irish speakers percentage by province 
ggplot(province, 
       aes(x = Province, 
           y = Irish.Speakers...)) +
  geom_bar(stat = "identity")+  
  labs(title = "Percentage of Irish Speakers by Province", x = "Province",
       y = "Percentage of Irish Speakers",color = "Name")



##---------------------------------------------------------------------------##
#3rd trend
#plot between year, irish speakers with province
ggplot(province, aes(x= Year,
                     y = Irish.Speakers..., 
                     color=Province, size = Total.Population)) +
  geom_point() +
  labs(title = "")



##---------------------------------------------------------------------------##
# Read Age Group csv file
age <- read.csv("AgeGroup.csv")
head(age)

# Check empty column 
colSums(is.na(age) | age == "")

# Clean data
age[age==0] <- NA #Set zero to NA
age<-age[complete.cases(age),] # Delete rows associated with NA
head(age)




##---------------------------------------------------------------------------##
# 4th trend
# Distribution by age
speaker_by_age <- age %>%
  group_by(Age.Group) %>%
  summarize(
    n=n(),
    sum=sum(Irish.Speakers)
  )

speaker_by_age<- speaker_by_age[!speaker_by_age$Age.Group=="All ages", ]
speaker_by_age<- speaker_by_age[order(-speaker_by_age$sum),]
speaker_by_age

# Plot sum of Irish speakers by age group
pie(speaker_by_age$sum, labels = speaker_by_age$Age.Group)





##---------------------------------------------------------------------------##
# Read csv file 
SE <- read.csv("SocioEconomic.csv")
head(SE)


##---------------------------------------------------------------------------##
# Null/Alternative hypothesis
# Pearson's Chi-squared Test
chiSquared <-table(SE$Population,SE$Irish.Speakers)
chisq.test(chiSquared) 


##---------------------------------------------------------------------------##
#Krustal Test  - I DECIDED TO NOT USE
#kruskal.test(SE$SocioEconomic.Class ~ SE$Irish.Speakers, data = SE)



##---------------------------------------------------------------------------##
#Regression
#The predictor vector year
x <- c(2011,2016)

# The response vector (sum of Irish Speakers)
y <- c(7362700,7300336)

# Apply the lm() function.
relation <- lm(y~x)
relation

# Find for year 2021
a <- data.frame(x = 2021)
result <-  predict(relation,a)
print(result)


plot(y,x,col = "blue",main = "Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Irish Speakers",ylab = "Year")



##---------------------------------------------------------------------------##
# Decision Tree
library(rpart)

# create subset of the data 
data_df<-SE[,c(5,7)]
head(data_df)


#use rpart library to create a decision tree model 
dt<-rpart(data_df$Irish.Speakers	~data_df$Non.Irish.Speakers,
          data=data_df,control = rpart.control(minsplit = 3))


## create new  df to test
new<-data.frame(data_df$Irish.Speakers)


## run the predict function to test the new height data against the formulated dt dataframe.
pd<-predict(dt,newdata = new)

# merge the original data set with predicted model
SE$pd<-pd
head(SE)


#Plot
ggplot() + geom_point(aes(x=SE$Irish.Speakers,y=SE$Non.Irish.Speakers,),color=3) +
  geom_line(aes(x=SE$Irish.Speakers,y=SE$pd),color=2) + ggtitle("Decision Tree")  + xlab("Irish Speaker Perc") + ylab("Population") + theme(plot.title = element_text(hjust=0.5)) +theme_bw()

# Correaltion Analysis
#install.packages("psych")
library(psych)

# Get the data 
dat<-SE[,c(5,6,7,8,9)]
dat
describe(dat)

# store data set' covariances in matrix
matrix1 <- cov(dat) 
matrix1

# store correlation values in matrix
matrix2 <- cor(dat)


# apply spearman method on correlation coefficients
matrix3 <- cor(dat, method="spearman")
matrix3

