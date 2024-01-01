#Adrian Duka R Assignment 2

install.packages("tidyverse",dependencies = TRUE)
library(ggplot2)
data("mpg")

#1

#The graph is skewed left, so a conclusion that can be drawn
#is that the majority of observations are low fuel effeciency vehicles.
#There are very few vehicles with HWY MPG above 30.

ggplot(mpg, aes(x=hwy)) + 
  geom_histogram(fill="blue", binwidth = 2, color="black") +
  labs(title = "Histogram of HWY MPG",
       x= "HWY MPG",
       y= "Number of Instances")

#2

#Typically, all vehicles are obtaining more HWY MPG compared to CTY MPG
#which is expected. However, some high fuel effeciencty
#compact and subcompact models are receiving ~20%-25% more HWY MPG
#compared to CTY MPG which is a very sizable difference.

ggplot(mpg, aes(x= cty, y= hwy, color= class)) +
  geom_point() +
  labs(title= "City MPG vs HWY MPG by Class",
        x= "City MPG",
        y= "HWY MPG")

#3

#Compact and Subcompact vehicles typically have the highest average
#city MPG based on the boxplots presented

ggplot(mpg, aes(x = class, y = cty, fill = "lightgreen")) +
  geom_boxplot() +
  labs(x = "Vehicle Class",
       y = "City MPG")

#4

#Vehicles with more cylinders typically have lower HWY MPG 
#according to the scatterplot

ggplot(mpg, aes(x = cyl, y = hwy, color = factor(cyl), size = cyl)) +
  geom_point() +
  labs(x = "Number of Cylinders", y = "Highway MPG")

#5

#For this question I created a function to calculate the avg HWY MPG
#by brand, and then created a bar chart illustrating that amount
#by brand. It appears that Honda, Hyundai, and Volkswagen have typically
#higher than average HWY MPG, but more specifically only Honda & Volkswagen

library(dplyr)
avghwybybrand <- mpg %>%
  group_by(manufacturer) %>%
  summarise(avg_hwy = mean(hwy))

ggplot(avghwybybrand, aes(x = manufacturer, y = avg_hwy, fill=manufacturer)) +
  geom_bar(stat = "identity") +
  labs(x = "Brand", 
       y = "Average Highway MPG") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#6

#Some noticeable patterns are that typically, lower # of cylinders
#will mean more fuel effeciency in terms of HWY MPG

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl+"Number of Cylinders") +
  labs(x = "Displacement",
       y = "Highway MPG")

#7

#A noticeable pattern displayed across all the scatter plots is that
#typically, a abnormally low displacement will result in higher HWY MPG.
#Conversely, on average as displacement increases HWY MPG will decrease 

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~class+"Class") +
  labs(x = "Displacement", y = "Highway MPG")

#8

# This problem wants us to filter the data into a specific category.
# The category is "hwy", the cars in the data set with highway miles per gallon
# We filtered the data to bring cars with hwy above 30 gallons. 

library(dplyr)
library(magrittr)

mpg %>%
  filter(hwy > 30)

#9

#I filter the data using the "filter" function and make it only audi. 
# This filtering only bring results of my desired request.

data(mpg)

mpg %>%
  filter(manufacturer == "audi")

#10

# This data is now filtered into three columns only. This keeps the data
# tight knit and compact keeping it simple. 

mpg %>%
  select(manufacturer, model, year)

#11

#I only select every column besides cylinders and drv as requested.
# The select function targets columns rather rows. 

mpg %>%
  select(-cyl, -drv)

#12

# I calculate the city miles by using the summarize function.
# I took the average for all the cars and displayed the average of those numbers. 

average_cty <- mpg %>%
  summarize(average_cty = mean(cty))

print(average_cty)
#13

# I began by grouping the tibble by car manufacturer with 'group_by'
# Then I selected the columns I wished to be returned with 'select'
# By using 'slice' and 'which.max', I was able to return a single value for 
# max hwy for each car manufacturer

max_hwy <- mpg %>% group_by(manufacturer) %>%
  select(manufacturer,hwy) %>%
  slice(which.max(hwy))

print(max_hwy)


#14

# I used 'mutate' to create a new column 'mileage_difference'
# I then assigned 'hwy - cty' to this variable

mpg %>% mutate(mileage_difference = hwy - cty)


#15

# I used 'mutate' to create new columns 'km_per_1_hwy' & 'km_per_1_cty'
# I then assigned 'hwy/cty * 0.425144' to their respective variable
# I multiplied by 0.425144 because this is how you can convert mpg to km/l

mpg %>% 
  mutate(km_per_1_hwy = hwy * 0.425144,
         km_per_1_cty = cty * 0.425144,)

#16

# I began with 'group_by' to gather by 'class' types
# I then used 'summarise' to calculate the mean for 'hwy' & 'cty' respectively
# I then assigned the results to 'avg_hwy' & 'avg_cty' respectively

mpg %>% 
  group_by(class) %>%
  summarise(avg_hwy = mean(hwy),
            avg_cty = mean(cty))

#17

# I used mutate to create the new column 'mileage_rating'
# I then created a nested 'ifelse' statement to convert numeric to categorical

mpg %>% mutate(mileage_rating = ifelse(hwy < 20, "Low",
                                       ifelse(hwy>20 & hwy<=30, "Medium",
                                              ifelse(hwy>30, "High", "NA"))))

