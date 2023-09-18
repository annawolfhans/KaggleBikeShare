# put data category 4 in category 3
# change weather to factors, create time of day variable

library(tidyverse)
library(vroom)
library(tidymodels)

bike_train <- vroom::vroom("./train.csv", col_names = TRUE, show_col_types = F)
bike_test <- vroom::vroom("./test.csv", col_names = TRUE, show_col_types = F)

# Cleaning and replace 4 with 3
# bike_train <- bike_train %>%
#   mutate(weather = ifelse(weather == 4, 3, weather))

# Remove casual and registered from train (test DNC)
bike_train <- bike_train %>%
  select(-casual, -registered)

# Engineering
bike_train_recipe <- recipe(count ~ ., data=bike_train) %>%
  step_mutate((weather = ifelse(weather == 4, 3, weather))) %>%
  step_num2factor(weather, levels=c("Sunny", "Misty", "Storm")) %>%
  step_date(datetime, features = "dow") 
prepped_rec <- prep(bike_train_recipe)
bake(prepped_rec, bike_train)

# create a categorical variable of night vs day
# could also create step_poly

my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(bike_train_recipe) %>%
  add_model(my_mod) %>%
  fit(data=bike_train)

bike_predictions <- predict(bike_workflow,
                            new_data=bike_test)

bike_submit <- cbind(bike_test, bike_predictions) %>%
  select(datetime, .pred) 

colnames(bike_submit) <- c("datetime", "count")
bike_submit$count <- ifelse(bike_submit$count < 0, 0, bike_submit$count)
bike_submit$count <- ifelse(is.na(bike_submit$count), 0, bike_submit$count)
vroom_write(bike_submit,file = "bike_submit2.csv", delim = ",")
view(bike_submit)

## rank deficient error means num columns is too small
# you have variables that are the same thing 
# ex: dow variable in bike recipe
# bc friday was the problem, you could jsut drop that one from the recipe

## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)
## Cleaning & Feature Engineering
bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime)
prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = bikeTrain) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
lin_model <- linear_reg() %>%
  set_engine("lm")

## Set up the whole workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=bikeTrain)

## Look at the fitted LM model this way
extract_fit_engine(bike_workflow) %>%
  summary()
extract_fit_engine(bike_workflow) %>%
  tidy()
# You can see winter is pretty significant etc (should remind u of 330)
dim(predict(bike_workflow, new_data = bikeTest))

## Get Predictions for test set AND format for Kaggle
test_preds <- predict(bike_workflow, new_data = bikeTest) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unelss you make it character string
vroom_write(x=test_preds, file="./TestPreds.csv", delim=",")


################# POISSON REGRESSION ######################

library(tidyverse)
library(tidymodels)
library(vroom)
library(poissonreg)

bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")

bikeTrain <- bikeTrain %>%
  select(-casual, - registered)

bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime)
prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = bikeTrain) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test

pois_mod <- poisson_reg() %>% 
  set_engine("glm")

bike_pois_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bikeTrain) 

bike_predictions <- predict(bike_pois_workflow,
                            new_data = bikeTest)

extract_fit_engine(bike_pois_workflow) %>%
  summary()
extract_fit_engine(bike_pois_workflow) %>%
  tidy()
