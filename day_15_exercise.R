library(tidymodels)
library(palmerpenguins)
penguins <- palmerpenguins::penguins

set.seed(101)
split <- initial_split(penguins, prop = 0.7)

penguins_train <- training(split)
penguins_test <- testing(split)

penguins_cv <- vfold_cv(penguins_train, v = 10)

#Daily Exercise 16
#Model Fitting and Workflow
penguins <- penguins |> 
  mutate(logmass = log(body_mass_g))

rec <-  recipe(sex ~ body_mass_g, data = penguins_train) %>%
  step_naomit(all_predictors(), all_outcomes())

rf_model <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

rf_wf <- workflow() %>%
  # Add the recipe
  add_recipe(rec) %>%
  # Add the model
  add_model(rf_model) %>%
  # Fit the model
  fit(data = penguins_train) 

log_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

log_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(log_model) %>%
  fit(data = penguins_train)

wf <- workflow_set(list(rec), list(log_model, rf_model)) %>%
  workflow_map('fit_resamples', resamples = penguins_cv) 

autoplot(wf)

rank_results(wf, rank_metric = "accuracy", select_best = TRUE)

#For my model, which was testing to see if body mass was a predictor of penguin sex, since in many bird species there is a difference
#in weight between males and females, I found that the random forest model was a better model. The random forest model had an accuracy
#of 0.74, much higher than the 0.64 accuracy of the logistic regression model. 