## 
## Bike Share EDA Code
##

## Libraries
library(tidyverse)
library(vroom)

# read in the data

bike <- vroom("./train.csv")
view(bike)
