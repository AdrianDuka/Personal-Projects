#Adrian Duka R Assignment 1

#1 

#new column in mtcars dataset is created called highmpg
#that indicates whether each car has an mpg of >=20

data(mtcars)

mtcars$high_mpg <- mtcars$mpg >=20

View(mtcars$high_mpg)

#2

#loops 1 through all rows in the mtcars dataset
#and checks if the horsepower is greater than 150 then prints
#the miles per gallon of the cars that meet that criteria

for (i in 1:nrow(mtcars)) {
  if (mtcars$hp[i] > 150) {at
    print(mtcars$mpg[i])
  }
}

#3

#function called checkcarweight is created that
#indicates whether or not a car is considered a lightweight or 
#heavy car depending on whether they are >3 or <3

checkcarweight <- function(weight) {
  if (weight < 3) {
    print("Lightweight car.")
  } else {
    print("Heavy car.")
  }
}

#4

#new dataset called selected_cars is created containing
#only columns mpg, cyl, and gear

selected_cars <- mtcars[, c("mpg", "cyl", "gear")]

#5

#mpg column in newly created selected_cars dataset gets
#renamed to fuel_efficiency

names(selected_cars)[names(selected_cars) == "mpg"] <- "fuel_efficiency"

#6

#creates a function called calculate_stats, and within the 
#function it calculates the mean as mean_val, median as median_val,
#and standard deviation as sd_val and returns a list with those
#values. Then, the variable hp_stats stores the result given by the function 
#calculate_stats. Which then hp_stats is called multiple times to print the 
#relevant data that was calculated using the function calculate_stats

calculate_stats <- function(x) {
  mean_val <- mean(x)
  median_val <- median(x)
  sd_val <- sd(x)
  return(list(mean = mean_val, median = median_val, sd = sd_val))
}

hp_stats <- calculate_stats(mtcars$hp)

print(paste("Mean HP:", hp_stats$mean))
print(paste("Median HP:", hp_stats$median))
print(paste("Standard Deviation HP:", hp_stats$sd))

#7

#mtcars dataset gets sorted by weight in descending order

sort(mtcars$wt, decreasing = TRUE)

#8

#prints the maxiumum value in each row in the selected_cars dataset

print(apply(selected_cars, 1, max))

install.packages("tidyverse",dependencies = TRUE)
#ignore^

#9

#these lines create the variable, sub_cyl,
#grabs all rows from the columns cyl and am,
#and adds the constraint that cyl == 6 & am ==0

sub_cyl <- mtcars[ , c("cyl", "am")]  %>%
  filter(cyl == 6,
         am == 0)

#10

#this line creates the variable disp_per_cycle,
#and it attaches the values of cyl/disp for each car

disp_per_cyl <- mtcars$cyl/mtcars$disp

#11

#These lines are an if statement a function called return_mpg is 
#created mpg is the argument that determines what category 
#should be returned

return_mpg <- function(mpg) {
  if (mpg < 15) {
    return("Low MPG")
  } else if (mpg > 15 & mpg <= 20) {
    return("Moderate MPG")
  } else if (mpg > 20) {
    return("High MPG")
  } 
}

#12

#the variable sub_cyl2 is created all rows from columns 
#am & qsec are selected filter is then used to grab only the
#rows that meet the questions criteria

sub_cyl2 <- mtcars[ , c("am", "qsec")] %>%
  filter(am == 1,
         qsec > 14 & qsec < 16)

#13

#We filter the cars into a common ground of unique gear variable

gear_count <- table(mtcars$gear)
print(gear_count)

#14

#We divide the cars into two categories of differing between 
#transmissions, and take the average of those.

split_mpg <- split(mtcars$mpg, mtcars$am)
average_mpg_by_am <- sapply(split_mpg, mean)
print(average_mpg_by_am)

#15

#Creates another set to filter out the data for cars with 6 
#cylinders and a manual transmission

subset_mtcars <- mtcars[which(mtcars$cyl == 6 & mtcars$am == 1), ]
print(subset_mtcars)

#16

#we calculate the average of miles you get per gallon across the subset.

mean_mpg <- mean(subset_mtcars$mpg)
median_mpg <- median(subset_mtcars$mpg)

cat("Mean MPG:", mean_mpg, "\n")
cat("Median MPG:", median_mpg, "\n")

#17

#This problem requires us to build an efficiency ration using mpg 
#divided by horsepower.

subset_mtcars$efficiency_ratio <- subset_mtcars$mpg / subset_mtcars$hp
print(subset_mtcars)