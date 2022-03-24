library(tidyverse)
library(moderndive)
library(skimr)
library(readr)
library(Stat2Data)
library(ggplot2)
library(GGally)
library(knitr)
library(gridExtra)
library(dplyr)
library(car)
library(corrplot)

data <- read.csv("dataset1.csv", na.strings = "") %>% rename("Number_of_Family"=7,
                                                             "Income" = 1,
                                                             "FoodExpenditure" = 3,
                                                             "Gender" = 4,
                                                             "Age" = 5,
                                                             "Type" = 6,
                                                             "Area" = 8,
                                                             "HouseAge" = 9,
                                                             "Bedrooms" = 10)
data$Region <- factor(data$Region)
data$Gender <- factor(data$Gender)
data$Type[which(data$Type == "Two or More Nonrelated Persons/Members")] <- "Other"
data$Type <- factor(data$Type)   
data$Electricity <- factor(data$Electricity) 
levels(data$Electricity) <- c("No", "Yes")
glimpse(data)

# raw data model, AIC:7015.8
model_full = glm(data = data, Number_of_Family ~ Income + FoodExpenditure + 
                   Gender + Age + Type + Area + HouseAge + Bedrooms + Electricity,
                 family = poisson)
summary(model_full)
outlierTest(model_full)

# drop data
data <- data[-943,]

# updated model w/ drop[943] only AIC:6997.4
model_drop = glm(data = data, Number_of_Family ~ Income + FoodExpenditure + 
                 Gender + Age + Type + Area + HouseAge + Bedrooms + Electricity,
                 family = poisson)
summary(model_drop)
outlierTest(model_drop)

tstep <- step(model_drop)
summary(tstep)
drop1(tstep)

# remove Electricity and drop[303] and log Income, Food 
#AIC:6829.9

data <- data[-303,]
model_drop2 = glm(data = data, Number_of_Family ~ log(Income) + log(FoodExpenditure) + 
                  Gender + Age + + Area + Type + HouseAge + Bedrooms,
                  family = poisson)
summary(model_drop2)
outlierTest(model_drop2)

tstep <- step(model_drop)
summary(tstep)
drop1(tstep)

# remove  Area per AIC esult from drop1(tstep), drop[1197]
#AIC:6823.6
data <- data[-1197,]
model_drop3 = glm(data = data, Number_of_Family ~ log(Income) + log(FoodExpenditure) + 
                  Gender + Age + Type + HouseAge + Bedrooms,
                  family = poisson)
summary(model_drop3)
outlierTest(model_drop3)

# remove Age per AIC result from drop1(tstep), drop[1197]
#AIC:6818.6
data <- data[-1197,]
model_drop4 = glm(data = data, Number_of_Family ~ log(Income) + log(FoodExpenditure) + 
                    Gender + Type + HouseAge + Bedrooms,
                  family = poisson)
summary(model_drop4)
outlierTest(model_drop4)
# it seems not much difference even dropping variables with low AIC. 
# Review boxplots to see more 
#============ data recast====================
p1 <- ggplot(data, aes(x = Number_of_Family, y = Income, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Income") +
  theme(legend.position = "none")
p2 <- ggplot(data, aes(x = Number_of_Family, y = FoodExpenditure, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Food Expenditure") +
  theme(legend.position = "none")
p3 <- ggplot(data, aes(x = Number_of_Family, y = Age, fillp = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Age") +
  theme(legend.position = "none")
p4 <- ggplot(data, aes(x = Number_of_Family, y = Type, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Type") +
  theme(legend.position = "none")
p5 <- ggplot(data, aes(x = Number_of_Family, y = Area, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Area") +
  theme(legend.position = "none")
p6 <- ggplot(data, aes(x = Number_of_Family, y = HouseAge, fillp = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "HouseAge") +
  theme(legend.position = "none")
p7 <- ggplot(data, aes(x = Number_of_Family, y = Bedrooms , fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Bedrooms") +
  theme(legend.position = "none")
p8 <- ggplot(data, aes(x = Number_of_Family, y = Electricity , fillp = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Electricity") +
  theme(legend.position = "none")
p <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol = 2)
p
# remove obvious outliers 
data <- data %>% filter(
  Income<6000000 & Area<300 & HouseAge<80 & Bedrooms<6  & FoodExpenditure<300000)

#===============
model_drop2 = glm(data = data, Number_of_Family ~ Income + FoodExpenditure + 
                    Gender + Age + Type + HouseAge + Bedrooms,
                  family = gaussian)
summary(model_drop2)

tstep1 <- step(model_drop2)
summary(tstep1)
