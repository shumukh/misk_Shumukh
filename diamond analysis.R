# Diamond Analysis
# Shumukh Alshreef
# 29.09.2020
# A small case study for EDA and stats

# load packages
library(tidyverse)
library(MASS)

# Read in the data (csv format):
# Newer methods from tidyr package
jems <- read_csv("data/diamonds.csv")

# super convenient way
# library(rio) # R i/o
# jems2 <- import("data/diamonds.csv")

# Get familiar with our data:
summary(jems)
names(jems)
glimpse(jems)

# more detail:
attributes(jems)
typeof(jems)


#How many diamonds with a clarity of category “IF” are present in the data-set?

clarity <- jems %>%
  filter(clarity == "IF")
nrow(clarity)


#What fraction of the total do they represent?

table(jems$clarity =="IF")/53940


#An other way
nrow(clarity)/nrow(jems)

#What proportion of the whole is made up of each category of clarity?

table(jems$clarity)/53940


#What is the cheapest diamond price overall?  
  
min(jems$price)

#What is the range of diamond prices?
  range(jems$price)

  
#What is the average diamond price in each category of cut and color?
  
  jems%>%
    group_by(cut,color)%>%
    summarise(avg=mean(price))

  
#Make a scatter plot that shows the price of a diamond as described by another continous variable, like the carat.
  
  ggplot(jems,aes(x=carat, y=price))+
  geom_point()

  #What can you say about the relationship between these two variables?
  #there is an positive relationship
  
  #Do you think that you can use the carat weight of a diamond to predict its price?
  #there is not enough information we need to know clarity and cut elc .. forexamble 
  
  ggplot(jems,aes(x=carat, y=price , color=clarity))+
    geom_point()

  #Using the functions we discuss earlier, and in class, apply a log10 transformation to both the price and carat.
  
  jems%>%
    mutate(price_log10 = log10(price),
           carat_log10 = log10(carat))

  
#You can save these as new columns in the data set called price_log10 and carat_log10

    carat_log10 <-log10(jems$carat)
  price_log10 <-log10(jems$price)


#Make a scatter plot that shows the price of a diamond as described by another continous variable, like the carat.
  
  ggplot(jems,aes(x=log10(carat), y=log10(price)))+
    geom_point()

  
#At the beginnin of the course we used the PlantGrowth data set to produce a model.
 
#Can you use the same function we used earlier, lm() to recreate a model that describes the relatioship shown in the plot?

    ggplot(jems,aes(x=log10(carat), y=log10(price)))+
    geom_point() +geom_smooth()
 
    
  
#We’ll get into the details of exactly what that model is doing later on. For now, we’ll just take a look at it in action 
#Now that we’ve described the diamond price given a single variable, 
  
#can you display that on the plot? Tryto use the geom_smooth() function to add this new layer.

      ggplot(jems,aes(x=log10(carat), y=log10(price)))+
     geom_point() +geom_smooth(se=FALSE, method = "lm")
  
  