#Adrian Duka R Assignment 4

library(readxl)
W13759_XLS_ENG <- read_excel("~/Downloads/W13759-XLS-ENG.xlsx")
View(W13759_XLS_ENG)

#1

#In the following code the survival rate of both males and females was computed
#The survival rates tell us that 19.1% of male passengers in the titanic
#survived, and that 72.6% of female passengers survived meaning that 
#females were in fact more likely to survive

#males
males <- subset(W13759_XLS_ENG, sex == 0)
total_males <- nrow(males)
male_survivors <- sum(males$survived)
survival_rate_males <- male_survivors / total_males
survival_rate_males
#19.1%

#females
females <- subset(W13759_XLS_ENG, sex == 1)
total_females <- nrow(females)
female_survivors <- sum(females$survived)
survival_rate_females <- female_survivors / total_females
survival_rate_females
#72.6%

#2

#In the following code the survival rate of both adults and children was computed
#The survival rates tell us that 36.2% of adults survived, compared to 
#49.2% of children surviving meaning that children were more likely to survive
#however, the dataset only considers people between the ages 0-5
#as children, not people between ages 0-17

#adults
adults <- subset(W13759_XLS_ENG, adult == 1)
total_adults <- nrow(adults)
adult_survivors <- sum(adults$survived)
survival_rate_adults <- adult_survivors / total_adults
survival_rate_adults
#36.2%

#children
children <- subset(W13759_XLS_ENG, adult == 0)
total_children <- nrow(children)
child_survivors <- sum(children$survived)
survival_rate_children <- child_survivors / total_children
survival_rate_children
#49.2%

#3

#In the following code, the survival rate of each adult age class is computed.
#The survival rates indicate that the survival rate between adult
#age classes is similar, aside from adults ages 60+ of which there was
#no survivors

#ages 6-18, class 2
class2 <- subset(W13759_XLS_ENG, age_categories == 2)
total_class2 <- nrow(class2)
class2_survivors <- sum(class2$survived)
survival_rate_class2 <- class2_survivors / total_class2
survival_rate_class2
#38.8%

#ages 19-39, class 3
class3 <- subset(W13759_XLS_ENG, age_categories == 3)
total_class3 <- nrow(class3)
class3_survivors <- sum(class3$survived)
survival_rate_class3 <- class3_survivors / total_class3
survival_rate_class3
#39.2%

#ages 40-59, class 4
class4 <- subset(W13759_XLS_ENG, age_categories == 4)
total_class4 <- nrow(class4)
class4_survivors <- sum(class4$survived)
survival_rate_class4 <- class4_survivors / total_class4
survival_rate_class4
#40%

#ages 60+, class 5
class5 <- subset(W13759_XLS_ENG, age_categories == 5)
total_class5 <- nrow(class5)
class5_survivors <- sum(class5$survived)
survival_rate_class5 <- class5_survivors / total_class5
survival_rate_class5
#0%

#4

#In the following code the survival rate for each passenger class was computed.
#The survival rates between the classes tell us that as class number increases,
#probability of survival decreases, meaning that passenger class was likely
#a factor in survival if there is a similar rate of men and women and children in each class

#first class
first_class <- subset(W13759_XLS_ENG, pclass == 1)
total_first_class <- nrow(first_class)
first_class_survivors <- sum(first_class$survived)
survival_rate_first_class <- first_class_survivors / total_first_class
survival_rate_first_class
#61.9%

#second class
second_class <- subset(W13759_XLS_ENG, pclass == 2)
total_second_class <- nrow(second_class)
second_class_survivors <- sum(second_class$survived)
survival_rate_second_class <- second_class_survivors / total_second_class
survival_rate_second_class
#42.9%

#third class
third_class <- subset(W13759_XLS_ENG, pclass == 3)
total_third_class <- nrow(third_class)
third_class_survivors <- sum(third_class$survived)
survival_rate_third_class <- third_class_survivors / total_third_class
survival_rate_third_class
#25.5%

#5

# Using the data given, S class had a signifcant count of civilians leading to a
#high death total. Q had the smallest class, while C was the middle class. Majority 
# of civilians aboard had perished, regardless of embarked. 

library(ggplot2)

ggplot(W13759_XLS_ENG, aes(x = embarked, fill = factor(survived))) +
  geom_bar() +
  labs(title = "Survival by Port of Embarkation",
       x = "Port of Embarkation",
       y = "Count") +
  scale_fill_discrete(name = "Survived")

#6

# We take an the combination of females and males and seperate them into two groups.
# One set of data is males in all three different pclasses with survival or not.
# Also we have females using the same data which reflect an entire differnt set of data. 

library(dplyr)
W13759_XLS_ENG %>%
group_by(sex, pclass) %>%
summarize(survival_rate = mean(survived))


#7

#here is the code to run the logistic regression

clean_W13759_XLS_ENG <- na.omit(W13759_XLS_ENG)
set.seed(1234)
indx <- sample(2, nrow(W13759_XLS_ENG), replace = TRUE, prob = c(0.8, 0.2))
train <- W13759_XLS_ENG[indx == 1, ]
test <- W13759_XLS_ENG[indx == 2, ]

logitModel <- glm(survived ~.-age_categories -boat, data = train, family = "binomial")
summary(logitModel)


#8

#For this case study I would like to use precision as my measure of performance.
#That is because the tradeoff between a human life is too great. Given that the
#model has a high precision rate, it is a good measure.

pred <- predict(logitModel, newdata = test, type  = "response")
predclass <- as.factor(ifelse(pred >= .5, "YES", "NO"))

calculate_metrics <- function(conf_matrix) {
  TP <- conf_matrix[1,1]
  FP <- conf_matrix[1,2]
  FN <- conf_matrix[2,1]
  TN <- conf_matrix[2,2]
  
  recall <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  false_alarm <- FP/ (FP + TN)
  results <- c(Recall = recall, Precision = precision, FalseAlarm = false_alarm)
  
  return(results)
}
