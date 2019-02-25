#Import the Titanic Dataset from the following link:
#https://drive.google.com/file/d/1JTJCjdGuUxzKXYlwOavwovB01k6FWg3r/view?ts=5b42ea10
#Perform the below operations:
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
titanic3 <- read_excel("C:/Users/Tyke/Downloads/titanic3.xls",
                       col_types = c("numeric", "numeric", "text",
                                     "text", "numeric", "numeric", "numeric",
                                     "text", "numeric", "text", "text",
                                     "text", "numeric", "text"))
View(titanic3)

#a. Pre-process the passenger names to come up with a list of
#titles that represent families and represent using appropriate
#visualization graph.
titanic3$name<-as.character(titanic3$name)

titanic3$SubTitle <- gsub("\\..*", "", titanic3$name)
titanic3$Title <- gsub(".*\\ ", "", titanic3$subtitle)

table(titanic3$Title)  # Count of Titles


#b. Represent the proportion of people survived by family size
#using a graph.

Title <- barplot(table(titanic3$Title),
                 main = "No. of Passangers by Title", xlab = "Title",
                 ylab = "No. of Passangers", col = "Blue")
text(Title, 0,table(titanic3$Title), pos = 3, srt = 90)

x <- table(titanic3$survived, titanic3$Title)
p <- x[1,]
p
prop <- round(p*100/sum(p),1)
prop

pie_chart <- pie(p, labels = p, main = " No.of passengers of Survival by Family",
                 col = rainbow(length(p)), cex = 1)
legend("right", names(p), cex= 0.5, fill = rainbow(length(p)))


pie(prop, labels = prop, main = " Proportion of Survival by Family",
    col = rainbow(length(prop)), cex = 1)
legend("right", names(prop), cex= 0.5, fill = rainbow(length(prop)))

#c. Impute the missing values in Age variable using Mice library,
#create two different graphs showing Age distribution before
#and after imputation
install.packages("mice")
library(mice)
sum(is.na(titanic3$age))
str(titanic3)
#Removing columns 1,2,3,4,5,7,12,13,14,16,17,18
mini_data <- titanic3
mini_data[-c(1,3,12,13,14)]
View(mini_data)
md.pattern(mini_data)
library(dplyr)
mini_data <- mini_data %>%
  mutate(
    survived = as.factor(survived),
    sex = as.factor(sex),
    age = as.numeric(age),
    sibsp = as.factor(sibsp),
    parch = as.factor(parch),
    embarked = as.factor(embarked)
  ).
str(mini_data)
mice_data <- mice(mini_data, m=5, maxit=10,seed=500)
summary(mini_data)
Imputed=complete(mice_data,5)
hist(titanic3$age,  main='Actual Data',col="green")
hist(Imputed$age, main='Imputed Data',col="black")
