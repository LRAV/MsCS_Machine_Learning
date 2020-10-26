# 1) load libraries and data ----------------------------------------------

library(AmesHousing)
library(tidymodels)
library(stacks)
library(purrr)
library(janitor)
library(dplyr)
library(rsample)
library(recipes)
library(dials)
library(parsnip)
library(yardstick)
library(tune)
library(workflows)
library(stacks)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(123456)

house_data <- read.csv("data/casas_entrena.csv") %>% 
  mutate(SalePrice = log(SalePrice+1)) %>% 
  rename(sale_price = SalePrice)   

split_and_preprocess <- function(data){
  
  split <- 
    rsample::initial_split(
      data, 
      prop = 0.80, 
      strata = sale_price
    )
  
  train <- training(split)
  test <- testing(split)
  
  preprocessing_recipe <- 
    recipes::recipe(sale_price ~ ., data = train) %>%
    recipes::step_string2factor(all_nominal()) %>%
    recipes::step_knnimpute(all_predictors(), neighbors = 5) %>% 
    recipes::step_other(all_nominal(), threshold = 0.01) %>%
    recipes::step_nzv(all_nominal()) %>%
    recipes::step_YeoJohnson(all_numeric(), -sale_price) %>% 
    prep()
  
  folds <- 
    recipes::bake(
      preprocessing_recipe, 
      new_data = train
    ) %>%  
    rsample::vfold_cv(v = 5)
  
  list(train = train, test = test, folds = folds, recipe = preprocessing_recipe)
}
def_glmnet <- function(){
  
  model <- 
    linear_reg(
      penalty = tune(), 
      mixture = tune()
    ) %>% 
    set_engine("glmnet")
  
  params <- 
    dials::parameters(
      penalty(), 
      mixture()
    )
  
  list(model = model, params = params)
}
def_xgboost <- function(){
  model <- 
    parsnip::boost_tree(
      mode = "regression",
      learn_rate = tune(),
      trees = 4000,
      tree_depth = 3, 
      min_n = 0,
      mtry = .7
    ) %>%
    set_engine("xgboost", objective = 'reg:squarederror', gamma = 0, alpha = 0.00006, 
               scale_pos_weight = 1, subsample = .7, nthread=-1)
  
  params <- 
    dials::parameters(
      learn_rate()
    )
  list(model = model, params = params)
}
def_lgbm <- function(){
  model <- 
    parsnip::boost_tree(
      mode = "regression",
      learn_rate = tune(),
      trees = 5000
    ) %>%
    set_engine("lightgbm", objective = "reg:squarederror", num_leaves=4, max_bin = 200, 
               bagging_fraction = 0.75, bagging_freq = 5, bagging_seed = 7, feature_fraction = .2, 
               feature_fraction_seed = 7, verbose=-1)
  
  params <- 
    dials::parameters(
      learn_rate()
    )
  list(model = model, params = params)
}
grid_and_tune <- function(model, preproc, size_n){
  if(model == "glmnet"){
    definition = def_glmnet()
  }

  if(model == "xgboost"){
    definition = def_xgboost()
  }
  
  if(model == "lgbm"){
    definition = def_lgbm()
  }

    
  grid <-
    dials::grid_max_entropy(
      definition$params,
      size = size_n
    )
  
  workflow <- 
    workflows::workflow() %>%
    add_model(definition$model) %>% 
    add_formula(sale_price ~ .)
  
  tuned <- 
    tune::tune_grid(
      object = workflow,
      resamples = preproc$folds,
      grid = grid,
      metrics = yardstick::metric_set(rmse, rsq, mae)
    )
  
  model_final <- definition$model %>% 
    finalize_model(tuned %>% tune::select_best("rmse"))
  
  list(model_final = model_final)
}
train_final <- function(tuned, preproc){
  
  train_processed <- bake(preproc$recipe,  new_data = preproc$train)
  
  train_prediction <- tuned$model_final %>% 
    fit(
      formula = sale_price ~ ., 
      data    = train_processed
    ) %>%
    predict(new_data = train_processed) %>%
    bind_cols(preproc$train)
  
  score_train <- 
    train_prediction %>%
    yardstick::metrics(sale_price, .pred) %>%
    mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
  
  test_processed  <- bake(preproc$recipe, new_data = preproc$test)
  
  test_prediction <- tuned$model_final %>%
    fit(
      formula = sale_price ~ ., 
      data    = train_processed
    ) %>%
    predict(new_data = test_processed) %>%
    bind_cols(preproc$test)
  
  score_test <- 
    test_prediction %>%
    yardstick::metrics(sale_price, .pred) %>%
    mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
  
  tt <- test_prediction %>% select(sale_price, .pred)
  score = sqrt(sum((tt$sale_price - tt$.pred)^2)/nrow(tt))
  
  list(score = score, data_f = test_prediction) 
}


# glmnet
preproc <- split_and_preprocess(data = house_data)
tuned <- grid_and_tune(model = "glmnet", preproc = preproc, size_n = 2)
score <- train_final(tuned = tuned, preproc = preproc)
score$score

# xgboost
preproc <- split_and_preprocess(data = house_data)
tuned <- grid_and_tune(model = "xgboost", preproc = preproc, size_n = 2)
score <- train_final(tuned = tuned, preproc = preproc)
score$score

# lightgbm
preproc <- split_and_preprocess(data = house_data)
tuned <- grid_and_tune(model = "lgbm", preproc = preproc, size_n = 2)
score <- train_final(tuned = tuned, preproc = preproc)
score$score




