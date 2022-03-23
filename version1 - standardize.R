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
#corrplot(corr<-cor(data))
#=================================== Check continuous variables ===============
continuous <-select_if(data, is.numeric)
summary(continuous)

data_rescale <- data %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
summary(data_rescale)
#==================== First Model ====================
model_full = glm(data = data_rescale, Number_of_Family ~ Income + FoodExpenditure + 
                   Gender + Age + Type + Area + HouseAge + Bedrooms + Electricity,
                 family = gaussian)
summary(model_full)
# AIC: 4149.5

#========================== continuous variable model ===========================
model_con = glm(data = data_rescale, Number_of_Family ~ Income + FoodExpenditure + 
                  Age + Area + HouseAge + Bedrooms,
                family = gaussian)
summary(model_con)
# AIC: 4428.3
#========================== factor model ===========================
model_fac = glm(data = data_rescale, Number_of_Family ~ Gender + Type + Electricity,
                family = gaussian)
summary(model_fac)
# AIC: 4504.5

# Move forward with model_full as its AIC is the lowest 
tstep <- step(model_full)
summary(tstep)

drop1(tstep)
#============================== data recast ===============================
p1 <- ggplot(data_rescale, aes(x = Number_of_Family, y = Income, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Income") +
  theme(legend.position = "none")
p1  

p2 <- ggplot(data_rescale, aes(x = Number_of_Family, y = FoodExpenditure, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Food Expenditure") +
  theme(legend.position = "none")
p3 <- ggplot(data_rescale, aes(x = Number_of_Family, y = Age, fillp = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Age") +
  theme(legend.position = "none")
p4 <- ggplot(data_rescale, aes(x = Number_of_Family, y = Type, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Type") +
  theme(legend.position = "none")
p5 <- ggplot(data_rescale, aes(x = Number_of_Family, y = Area, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Area") +
  theme(legend.position = "none")
p6 <- ggplot(data_rescale, aes(x = Number_of_Family, y = HouseAge, fillp = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "HouseAge") +
  theme(legend.position = "none")
p7 <- ggplot(data_rescale, aes(x = Number_of_Family, y = Bedrooms , fill = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Bedrooms") +
  theme(legend.position = "none")
p8 <- ggplot(data_rescale, aes(x = Number_of_Family, y = Electricity , fillp = Gender)) +
  geom_boxplot() +
  labs(x = "Number of Family grouped by Gender", y = "Electricity") +
  theme(legend.position = "none")

p <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol = 2)
p

data <- data_rescale %>% filter(
  Income<20 & Area<=8 & HouseAge<4 & Bedrooms<3  & FoodExpenditure<=6)

#============ Model 3 with dropped outlines===========

model_3 = glm(data = data_rescale, Number_of_Family ~ Income + FoodExpenditure + 
                   Gender + Age + Type + Area + HouseAge + Bedrooms,
                 family = gaussian)
summary(model_3)

tstep1 <- step(model_3)
summary(tstep1)

