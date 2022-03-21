library(tidyverse)
library(moderndive)
library(skimr)
library(kableExtra)
library(dplyr)
library(readr)
library(Stat2Data)
library(ggplot2)
library(GGally)

# load data from csv files
data <- read.csv("dataset1.csv", na.strings = "") %>% rename("Number_of_Family"=7,
                                                             "Income" = 1,
                                                             "FoodExpenditure" = 3,
                                                             "Gender" = 4,
                                                             "Age" = 5,
                                                             "Type" = 6,
                                                             "Area" = 8,
                                                             "HouseAge" = 9,
                                                             "bedrooms" = 10)
                                                             
data$Region <- factor(data$Region)
data$Gender <- factor(data$Gender)
data$Type <- factor(data$Type)    

glimpse(data)
summary(data)

#========================= Step1: Check variables================
# first time check, confirm outliers, 
# remove "Region" 
data_check <- data %>% summarize( Number_of_Family = (Number_of_Family),
                                  Income = (Income),
                                  FoodExpenditure = (FoodExpenditure),
                                  Gender = (Gender),
                                  Age = (Age),
                                  Type = (Type),
                                  Area = (Area),
                                  HouseAge = (HouseAge),
                                  bedrooms = (bedrooms),
                                  Electricity = (Electricity))
model1 <- glm(data = data_check, formula = Number_of_Family ~.,family=binomial(link="logit"))
model2 <- glm(data = data_check, formula = Number_of_Family ~.,family=binomial(link="logit"))

summary(model)

data_check[c(1:6,8)] %>% ggpairs() #pchisq(deviance(model),df.residual(model))

# Check continuous variables
continuous <-select_if(data, is.numeric)
summary(continuous) #data have totally different scales and many of them have large outliers

#Income
ggplot(continuous, aes(x = Income)) + geom_density(alpha = .2, fill = "#FF0000FF")
remove_one <- quantile(data$Income , .99)
remove_one #9% is 1170662, drop the observations above this threshold
data_drop <- data %>% 
  filter(Income <remove_one)
dim(data_drop)
ggplot(data_drop, aes(x = Income)) + geom_density(alpha = .2, fill = "#FF0000FF")

#FoodExpenditure
ggplot(continuous, aes(x = FoodExpenditure)) + geom_density(alpha = .2, fill = "#FFDB00FF")
remove_one <- quantile(data_drop$FoodExpenditure , .99)
remove_one #99% is 210024.8, drop the observations above this threshold
data_drop <- data_drop %>%
  filter(FoodExpenditure <remove_one)
dim(data_drop)
ggplot(data_drop, aes(x = FoodExpenditure)) + geom_density(alpha = .2, fill = "#FFDB00FF")

#Age
ggplot(continuous, aes(x = Age)) + geom_density(alpha = .2, fill = "#FF00DBFF")
top_one_percent <- quantile(data_drop$Age , .99)
top_one_percent #99% is 85.12, keep it since it looks fine

#Area
ggplot(continuous, aes(x = Area)) + geom_density(alpha = .2, fill = "#49FF00FF")
remove_one <- quantile(data_drop$Area , .99)
remove_one #99% is 482.4, drop the observations above this threshold
data_drop <- data_drop %>%
  filter(Area <remove_one)
dim(data_drop)
ggplot(data_drop, aes(x = Area)) + geom_density(alpha = .2, fill = "#49FF00FF")

#HouseAge 
ggplot(continuous, aes(x = HouseAge)) + geom_density(alpha = .2, fill = "#FF00DBFF")
remove_two <- quantile(data_drop$HouseAge , .98)
remove_two #98% is 65, drop the observations above this threshold
data_drop <- data_drop %>%
  filter(HouseAge <remove_two)
dim(data_drop)
ggplot(data_drop, aes(x = HouseAge)) + geom_density(alpha = .2, fill = "#FF00DBFF")

#bedrooms      
ggplot(continuous, aes(x = bedrooms)) + geom_density(alpha = .2, fill = "#0092FFFF")
remove_one <- quantile(data_drop$bedrooms , .99)
remove_one #99% is 6, drop the observations above this threshold
data_drop <- data_drop %>%
  filter(bedrooms <remove_one)
dim(data_drop)
ggplot(data_drop, aes(x = bedrooms)) + geom_density(alpha = .2, fill = "#0092FFFF")

#standardize variables so that they can have the same scale
data_rescale <- data_drop %>% 
  mutate_if(is.numeric, funs(as.numeric(scale(.))))

summary(data_rescale) 
glimpse(data_rescale)

#factor variables 
factor <- data.frame(select_if(data_rescale, is.factor))
ncol(factor)

# Create graph for each column
#data$Number_of_Family <- factor(data$Number_of_Family)

graph <- lapply(names(factor), 
                function(x) 
                ggplot(factor, aes(get(x))) +
                geom_bar(width = 0.1) +
                theme(axis.text.x = element_text(angle = 90)))
graph
#========================= Step 2: Change level family number =====================
corr <- data.frame(lapply(data, as.integer)) #Convert data to numeric
ggcorr(corr, method = c("pairwise", "spearman"), 
             nbreaks = 8, 
             hjust = 0.9,
             label = TRUE,
             label_size = 2,
             color = "grey50")
#========================= Step 4: Train/test set ========================= 
set.seed(1234)
create_train_test <- function(data1, size = 0.8, train = TRUE) {
  n_row = nrow(data1)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data1[train_sample, ])
  } else {
    return (data1[-train_sample, ])}}

data_train <- create_train_test(data, 0.8, train = TRUE)
data_test <- create_train_test(data, 0.8, train = FALSE)
#========================= Step 5: Build the model ========================= 
#model <- glm(Number_of_Family~.,data = data_train, family = 'binomial')
data_train$Number_of_Family_new <- factor(ifelse(data_train$Number_of_Family >= 7,1,0))

model2 <- glm(Number_of_Family_new ~ FoodExpenditure+Gender+Age+Type+HouseAge, 
             data = data_train, family = 'binomial')
summary(model2)
##========================= Step 6: Assess the performance of the model #===
lapply(model2, class)[1:5]
model2$aic
predict <- predict(model2, data_train, type = 'response')

table_mat <- table(data_train$Number_of_Family_new, predict > 0.5)
table_mat
#check model accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#RDC
pred <- predict(predict, data_test$Number_of_Family_new)
perf <- performance(pred, 'tpr', 'fpr')
plot(perf, colorize = TRUE, text.adj = c(-0.2, 1.5))