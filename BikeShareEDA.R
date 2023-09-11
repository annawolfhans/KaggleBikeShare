## 
## Bike Share EDA Code
##

## Libraries
library(tidyverse)
library(vroom)
library(DataExplorer)
library(ggplot2)

# read in the data

bike <- vroom::vroom("./train.csv")
view(bike)
dplyr::glimpse(bike)
DataExplorer::plot_intro(bike)
skimr::skim(bike)
DataExplorer::plot_bar(bike)
DataExplorer::plot_histograms(bike)
DataExplorer::plot_missing(bike)
plot_correlation(bike)
#registered and casual
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# two variables are not available because we only know the kind of rental 
#   when the rental occurs
GGally::ggpairs(bike)
bike_numerical <- bike %>%
  select(atemp,humidity,windspeed, count)
plot_correlation(bike_numerical)

# From heat map, make scatterplot of highly correlated variables
# Count vs temp etc.

q <- ggplot(data = bike_numerical, mapping=aes(x=atemp,y=count)) + 
  geom_point() +
  geom_smooth(se=TRUE) + 
  ggtitle("Comparing what temperature feels like to no. total rentals")
r <- ggplot(data = bike_numerical, mapping=aes(x=windspeed,y=atemp)) + 
  geom_point() +
  geom_smooth(se=TRUE) +
  ggtitle("Comparing windspeed to what temp feels like")


weather_mapping <- c("Clear", "Mist", "Light Storm", "Heavy Rain")

bike <- bike %>%
  mutate(weather = factor(weather, levels= 1:4, labels = weather_mapping))
  
week_mapping <- c("Weekend", "Weekday")
bike <- bike %>%
  mutate(workingday = factor(workingday, labels = week_mapping))

p <- ggplot(data = bike, mapping = aes(x=weather)) +
  geom_bar() +
  ggtitle("Comparing weather to total bike rentals")
  # Count is response variable
s <- ggplot(data = bike, mapping = aes(x=workingday)) +
  geom_bar() +
  ggtitle("Non working day to working day counts")
library(patchwork)
(p + s) / (q + r)
