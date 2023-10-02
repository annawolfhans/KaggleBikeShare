#########################
### LINEAR REGRESSION ###
#########################

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
  # step_mutate(count=log(count)) %>%
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

bike_predictions <- (predict(bike_pois_workflow,
                            new_data = bikeTest))

extract_fit_engine(bike_pois_workflow) %>%
  summary()
extract_fit_engine(bike_pois_workflow) %>%
  tidy()

## Get Predictions for test set AND format for Kaggle
test_preds_pois <- predict(bike_pois_workflow, new_data = bikeTest) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unelss you make it character string
vroom_write(x=test_preds_pois, file="./TestPredsPois.csv", delim=",")

# make sure to fix poiss and with linear modeling has log(exp()) stuff


### PENALIZED REGRESSION ###

## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)%>%
  mutate(count=log(count)) 
## Cleaning & Feature Engineering
bike_preg_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_preg_recipe <- prep(bike_preg_recipe)
bake(prepped_preg_recipe, new_data = bikeTrain) #Make sure recipe work on train
bake(prepped_preg_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
preg_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") 

## Set up the whole workflow
preg_workflow <- workflow() %>%
  add_recipe(bike_preg_recipe) %>%
  add_model(preg_model) %>%
  fit(data=bikeTrain)

## Look at the fitted LM model this way
extract_fit_engine(preg_workflow) %>%
  summary()
extract_fit_engine(preg_workflow) %>%
  tidy()
# You can see winter is pretty significant etc (should remind u of 330)
dim(predict(preg_workflow, new_data = bikeTest))

## Get Predictions for test set AND format for Kaggle
test_preg_preds <- predict(preg_workflow, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred)) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unless you make it character string
vroom_write(x=test_preg_preds, file="./TestPredsPenalized.csv", delim=",")

### ################## TUNING PARAMETERS ################# ###

## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)%>%
  mutate(count=log(count)) 
## Cleaning & Feature Engineering
bike_preg_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_preg_recipe <- prep(bike_preg_recipe)
# bake(prepped_preg_recipe, new_data = bikeTrain) #Make sure recipe work on train
# bake(prepped_preg_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
preg_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") 

## Set up the whole workflow
preg_wf <- workflow() %>%
  add_recipe(bike_preg_recipe) %>%
  add_model(preg_model) 

## Look at the fitted LM model this way
# extract_fit_engine(preg_workflow) %>%
  # summary()
# extract_fit_engine(preg_workflow) %>%
  # tidy()
# You can see winter is pretty significant etc (should remind u of 330)
# dim(predict(preg_workflow, new_data = bikeTest))
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5)

folds <- vfold_cv(bikeTrain, v = 5, repeats = 1)

CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y = mean, color = factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best("rmse")

final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bikeTrain)

final_wf %>%
  predict(new_data = bikeTest)
## Get Predictions for test set AND format for Kaggle
test_preg_preds <- predict(final_wf, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred)) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unless you make it character string
vroom_write(x=test_preg_preds, file="./TestPredsTuning.csv", delim=",")

########################  REGRESSION TREES #######################
# don't need to normalize etc...


## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)%>%
  mutate(count=log(count)) 
## Cleaning & Feature Engineering
bike_preg_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_preg_recipe <- prep(bike_preg_recipe)
# bake(prepped_preg_recipe, new_data = bikeTrain) #Make sure recipe work on train
# bake(prepped_preg_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
tree_model <- decision_tree(tree_depth = tune(),
                            cost_complexity = tune(),
                            min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

## Set up the whole workflow
tree_wf <- workflow() %>%
  add_recipe(bike_preg_recipe) %>%
  add_model(tree_model) 

## Look at the fitted LM model this way
# extract_fit_engine(preg_workflow) %>%
# summary()
# extract_fit_engine(preg_workflow) %>%
# tidy()
# You can see winter is pretty significant etc (should remind u of 330)
# dim(predict(preg_workflow, new_data = bikeTest))
tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(bikeTrain, v = 5, repeats = 2)

tree_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq))

# collect_metrics(tree_results) %>%
#   filter(.metric=="rmse") %>%
#   ggplot(data=., aes(x=penalty, y = mean, color = factor(mixture))) +
#   geom_line()

bestTune <- tree_results %>%
  select_best("rmse")

final_wf <- tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bikeTrain)

final_wf %>%
  predict(new_data = bikeTest)
## Get Predictions for test set AND format for Kaggle
test_tree_preds <- predict(final_wf, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred)) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unless you make it character string
vroom_write(x=test_tree_preds, file="./TestPredsTree.csv", delim=",")

#######################################
######### REGRESSION FORESTS ##########
#######################################

## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)%>%
  mutate(count=log(count)) 
## Cleaning & Feature Engineering
bike_preg_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_preg_recipe <- prep(bike_preg_recipe)
# bake(prepped_preg_recipe, new_data = bikeTrain) #Make sure recipe work on train
# bake(prepped_preg_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
forest_model <- rand_forest(mtry = tune(),
                            min_n=tune(),
                            trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

## Set up the whole workflow
forest_wf <- workflow() %>%
  add_recipe(bike_preg_recipe) %>%
  add_model(forest_model) 

tuning_grid <- grid_regular(mtry(range =c(1,10)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(bikeTrain, v = 5, repeats = 2)

forest_results <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq))

# collect_metrics(tree_results) %>%
#   filter(.metric=="rmse") %>%
#   ggplot(data=., aes(x=penalty, y = mean, color = factor(mixture))) +
#   geom_line()

bestTune <- forest_results %>%
  select_best("rmse")

final_wf <- forest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bikeTrain)

final_wf %>%
  predict(new_data = bikeTest)
## Get Predictions for test set AND format for Kaggle
test_forests_preds <- predict(final_wf, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred)) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unless you make it character string
vroom_write(x=test_forests_preds, file="./TestPredsForest.csv", delim=",")


###########################
##### STACKING MODELS #####
###########################

## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)%>%
  mutate(count=log(count)) 
## Cleaning & Feature Engineering
bike_preg_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_preg_recipe <- prep(bike_preg_recipe)
# bake(prepped_preg_recipe, new_data = bikeTrain) #Make sure recipe work on train
# bake(prepped_preg_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
tree_model <- decision_tree(tree_depth = tune(),
                            cost_complexity = tune(),
                            min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

## Set up the whole workflow
tree_wf <- workflow() %>%
  add_recipe(bike_preg_recipe) %>%
  add_model(tree_model) 

## Look at the fitted LM model this way
# extract_fit_engine(preg_workflow) %>%
# summary()
# extract_fit_engine(preg_workflow) %>%
# tidy()
# You can see winter is pretty significant etc (should remind u of 330)
# dim(predict(preg_workflow, new_data = bikeTest))
tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(bikeTrain, v = 5, repeats = 2)

tree_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq))

# collect_metrics(tree_results) %>%
#   filter(.metric=="rmse") %>%
#   ggplot(data=., aes(x=penalty, y = mean, color = factor(mixture))) +
#   geom_line()

bestTune <- tree_results %>%
  select_best("rmse")

final_wf <- tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bikeTrain)

final_wf %>%
  predict(new_data = bikeTest)
## Get Predictions for test set AND format for Kaggle
test_tree_preds <- predict(final_wf, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred)) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unless you make it character string
vroom_write(x=test_tree_preds, file="./TestPredsTree.csv", delim=",")

#######################################
######### REGRESSION FORESTS ##########
#######################################

## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)%>%
  mutate(count=log(count)) 
## Cleaning & Feature Engineering
bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_recipe <- prep(bike_recipe)
# bake(prepped_recipe, new_data = bikeTrain) #Make sure recipe work on train
# bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test

## Cross-validation folds
folds <- vfold_cv(bikeTrain, v=10)

## Control settings for stacking models
untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

#########
## LN ###
#########

lin_model <- linear_reg() %>%
  set_engine("lm")

linreg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model)

linreg_folds_fit <- linreg_wf %>%
  fit_resamples(resamples=folds,
                control=tunedModel)

##########
## PNR ###
##########

pen_reg_model <- linear_reg(mixture=tune(),
                            penalty=tune()) %>%
  set_engine("glmnet")

pen_reg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(pen_reg_model)

pen_reg_tuneGrid <- grid_regular(mixture(),
                                 penalty(),
                                 levels=5)

penreg_folds_fit <- pen_reg_wf %>%
  tune_grid(resamples=folds,
                grid = pen_reg_tuneGrid,
            metrics = metric_set(rmse),
                control=untunedModel)

###########
## trees ##
###########

reg_tree <- decision_tree(tree_depth = tune(),
                          cost_complexity = tune(),
                          min_n=tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

regTree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(reg_tree)

regTree_tuneGrid <- grid_regular(tree_depth(),
                                 cost_complexity(),
                                 min_n(),
                                 levels=5)

tree_folds_fit <- regTree_wf %>%
  tune_grid(resamples=folds,
            grid=regTree_tuneGrid,
            metrics=metric_set(rmse),
            control=untunedModel)


#############
## FORESTS ##
#############

forest_model <- rand_forest(mtry = tune(),
                            min_n=tune(),
                            trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

## Set up the whole workflow
forest_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(forest_model) 

forest_tuning_grid <- grid_regular(mtry(range =c(1,10)),
                            min_n(),
                            levels = 5)

forest_folds_fit <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=forest_tuning_grid,
            metrics=metric_set(rmse, mae, rsq),
            control=untunedModel)

#### STACKING #####

bike_stack <- stacks() %>%
  add_candidates(linreg_folds_fit) %>%
  add_candidates(penreg_folds_fit) %>%
  add_candidates(tree_folds_fit) %>%
  add_candidates(forest_folds_fit) %>%
as_tibble(bike_stack)

fitted_bike_stack <- bike_stack %>%
  blend_predictions() %>%
  fit_members()

collect_parameters(fitted_bike_stack, "tree_folds_fit")

## Get Predictions for test set AND format for Kaggle
stacked_predictions <- predict(fitted_bike_stack, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred)) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  # run downs rows and if 0 is bigger, it will replace it with 0, otherwise use count
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

# vroom will add rando variables unless you make it character string
vroom_write(x=stacked_predictions, file="./TestStackedPredictions.csv", delim=",")








