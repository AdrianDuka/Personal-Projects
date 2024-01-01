#Adrian Duka R Assignment 3

#1 (a)
install.packages("readxl")
library(readxl)
Pima_Indian_Diabetes <- read_excel("~/Downloads/Pima Indian Diabetes")
                        View(Pima_Indian_Diabetes)  
attach(Pima_Indian_Diabetes)
                        
# There are 9 Columns & 768 Row
numberofrows <- nrow(Pima_Indian_Diabetes)
numberofcol <- ncol(Pima_Indian_Diabetes)


#1 (b)
colnames(Pima_Indian_Diabetes) <- c("Pregnancies", "Glucose", "BP", 
                                    "SkinThickness", "Insulin", "BMI",
                                    "DPF", "Age", "Target")

#1 (c)
# All of my data has been imported correctly as numeric data type 

#1 (d)

#There are no missing values in this data set.
colSums( is.na(Pima_Indian_Diabetes))


#1 (e)
# This appears to be a Bernoulli Distribution (right-tailed)
hist(Pima_Indian_Diabetes$Target)


# There are 268 positive-cases & 500 negative-cases
sum(Pima_Indian_Diabetes$Target)
 

#1 (f)
# Age seems to be skew towards younger individuals (right-tailed)
hist(Pima_Indian_Diabetes$Age)
 

#1 (g)
library(dplyr)

BMI_Summary <- Pima_Indian_Diabetes %>% summarise(mean_BMI = mean(BMI),
                                                  median_BMI = median(BMI),
                                                  sd_BMI = sd(BMI))

Glucose_Summary <- Pima_Indian_Diabetes %>% summarise(mean_glucose = mean(Glucose),
                                                     median_glucose = median(Glucose),
                                                     sd_glucose = sd(Glucose))

BMI_Glucose_Summary <- c(BMI_Summary, Glucose_Summary)
head(BMI_Glucose_Summary)

#1 (h)

HandleOutliers <- function(x) {
  
  Myoutliers <- boxplot(x)$out
  x[x %in% Myoutliers] <- NA
  x[is.na(x)] <- max(x, na.rm=TRUE)
  return(x)
}

AdjustedTable <- mutate_if(Pima_Indian_Diabetes, is.numeric , HandleOutliers)


#1 (i)
AgeCount <- table(AdjustedTable$Age)
chisq.test()

#1(j)
# BP seems to be normally distributed
ggplot(AdjustedTable, aes(x=BP)) +
  geom_histogram() +
    facet_grid(Target ~ .)
  

#1(k)
#It appears that those without diabetes have a higher probability of having
# a lower BMI
ggplot(AdjustedTable, aes(x=BMI)) +
      geom_histogram() +
      facet_grid(Target ~ .) +


#1(L)
#The pattern with pregnancies is that there are less pregnancies than more
#It appears that there is a weak positive correlation with target and pregnancies

hist(Pregnancies)
cor(Pregnancies,Target)

#1(m)
#It appears that Insulin levels vary greatly from those without diabetes
# If you are not diabetic, your insulin levels are probably very low

ggplot(AdjustedTable, aes(x=Insulin)) +
      geom_histogram(binwidth = 50) +
      facet_grid(Target ~ .)

#1(n)
#It looks like skin thickness is right-tailed for non-diabetics, which means they
# more than likely have thinner skin, and the opposite for those with diabetes.
ggplot(AdjustedTable, aes(x=SkinThickness)) +
  geom_histogram() +
  facet_grid(Target ~ .)

#1(o)
#
AdjustedTable$diabetes <- factor(AdjustedTable$Target, levels = c(0, 1), labels = c("No Diabetes", "Diabetes"))

hist(AdjustedTable$DPF[AdjustedTable$diabetes == "Diabetes"], main="DPF Distribution Among Diabetics", xlab="DPF")
hist(AdjustedTable$DPF[AdjustedTable$diabetes == "No Diabetes"], main="DPF Distribution Among Non-Diabetics", xlab="DPF")

#1(p)
#The non-diabetic group is more left oriented relating in age, which means they
# more likely to have lower bmi scores than diabetic people.
ggplot(AdjustedTable, aes(x = Age, y = BMI, color)) +
  geom_point() +
  facet_grid(Target ~ .)
 
#1(q)
#You can see a large amount of pregnancies are between the age 21 and 36. 
barplot(table(Pima_Indian_Diabetes$Pregnancies, Pima_Indian_Diabetes$Age), xlab = "Age Group", ylab = "Count", 
       main = "Number of Pregnancies by Age Group", beside = TRUE)
#1(r)
# The average for diabetic is 37 and the average of non-diabetic is 31.
aggregate(Pima_Indian_Diabetes$Age ~ Pima_Indian_Diabetes$Target, Pima_Indian_Diabetes, mean)

#1(t)
# The differences between diabetic and non-diabetic is signifcant. The comparisons
# are clear and evident. 


#1(u)
set.seed(38)
indx <- sample(1:nrow(AdjustedTable),0.8 * nrow(AdjustedTable))
training_data <- AdjustedTable[indx, ]
test_data <- AdjustedTable[-indx, ]


#1(v)
#
library(rpart)
tree_model <- rpart(Target ~., data = training_data, method = "class")
predictions_training <- predict(tree_model, training_data, type = "class")

accuracy_training <- sum(predictions_training == training_data$Target) /nrow(training_data)

cat("Accuracy on training data:", round(accuracy_training * 100, 2), "%\n")
#Training data is 83.88% accurate.  
#1(w)
library(rpart.plot)
rpart.plot(tree_model)

#1(x)
print(tree_model)

number_leaves <- tree_model$n
cat("Number of Leaves:", number_leaves, "\n")
#If gluclose is greater than 128 and your age is 30 or above with a bmi score higher
#than 27 then you'll likely be diagnosed with diabetes.

#If gluclose is less than 128 and a bmi score less than 29, you're likely non-diabetic


#1(y)
# The most important variables are gluclose, age, and bmi.
summary(tree_model)


#1(z)

predictions_test <- predict(tree_model,newdata = test_data, type = "class")
head(predictions_test)
accuracy_test <- sum(predictions_test == test_data$ Target) / nrow(test_data)
cat("Accuracy on test data:", round(accuracy_test * 100, 2), "%\n")

#Test data is 75.97% accurate