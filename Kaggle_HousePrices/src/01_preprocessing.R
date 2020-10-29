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
source("src/fit_members.R")
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(1234568)

house_data <- read.csv("data/casas_entrena.csv") %>% 
  mutate(SalePrice = log(SalePrice+1)) %>% 
  rename(sale_price = SalePrice)   

control_grid <- control_stack_grid()

split_and_preprocess <- function(data, prop_p){
  
  split <- 
    rsample::initial_split(
      data, 
      prop = prop_p, 
      strata = sale_price
    )
  
  train <- training(split)
  test <- testing(split)
  
  preprocessing_recipe <- 
    recipes::recipe(sale_price ~ ., data = train) %>%

    recipes::step_filter(Sale.Condition == "Normal") %>% 
    recipes::step_filter(Gr.Liv.Area < 4000) %>% 
    recipes::step_rm(contains("Pool")) %>%
    
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
  
  workflow <- 
    workflows::workflow() %>%
    add_formula(sale_price ~ .)
  
  list(train = train, test = test, folds = folds, recipe = preprocessing_recipe, 
       workflow = workflow)
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
grid_and_tune <- function(model, preproc, size_n, ctrl_grid_all){
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
    preproc$workflow %>% 
    add_model(definition$model)
  
  tuned <- 
    tune::tune_grid(
      object = workflow,
      resamples = preproc$folds,
      grid = grid,
      metrics = yardstick::metric_set(rmse, rsq, mae), 
      control = ctrl_grid_all
    )
  
  model_final <- definition$model %>% 
    finalize_model(tuned %>% tune::select_best("rmse"))
  
  list(model_final = model_final, tuned = tuned)
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
  print(score)
  
  list(score = list(score_train = score_train, score_test = score_test), data_f = test_prediction) 
}

model_list <- list()
preproc <- split_and_preprocess(data = house_data, prop_p = 0.99)

# glmnet
tuned <- grid_and_tune(model = "glmnet", preproc = preproc, 
                       size_n = 20, ctrl_grid_all = control_grid)
score <- train_final(tuned = tuned, preproc = preproc)
score$score
model_list$glmnet <- list(preproc = preproc, tuned = tuned, score = score)

# xgboost
tuned <- grid_and_tune(model = "xgboost", preproc = preproc, 
                       size_n = 20, ctrl_grid_all = control_grid)
score <- train_final(tuned = tuned, preproc = preproc)
score$score
model_list$xgboost <- list(preproc = preproc, tuned = tuned, score = score)

# lightgbm
tuned <- grid_and_tune(model = "lgbm", preproc = preproc, 
                       size_n = 20, ctrl_grid_all = control_grid)
score <- train_final(tuned = tuned, preproc = preproc)
score$score
model_list$lgbm <- list(preproc = preproc, tuned = tuned, score = score)

model_stack <- 
  stacks::stacks() %>%
  stacks::add_candidates(model_list$glmnet$tuned$tuned) %>%
  stacks::add_candidates(model_list$xgboost$tuned$tuned) %>%
  stacks::add_candidates(model_list$lgbm$tuned$tuned) %>%
  stacks::blend_predictions() 

model_stack2 <- 
  model_stack %>% 
  fit_members()

pred_data <- bake(preproc$recipe, new_data = preproc$test)
star_pred <- bind_cols(pred_data, predict(model_stack2, new_data = pred_data))

model_list$glmnet$score$score
model_list$xgboost$score$score
model_list$lgbm$score$score
score = sqrt(sum((star_pred$sale_price - star_pred$.pred)^2)/nrow(star_pred))
score
  


# prediccion final --------------------------------------------------------
pred_data <- read.csv("data/casas_prueba.csv")
pred_data1 <- bake(preproc$recipe, new_data = pred_data)
star_pred <- bind_cols(pred_data, predict(model_stack2, new_data = pred_data1))
star_pred %>% 
  select(id, .pred) %>% 
  mutate(.pred = exp(.pred)-1) %>% 
  set_names(c("id", "SalePrice")) %>% 
  write.csv("20201025_out_model.csv")


