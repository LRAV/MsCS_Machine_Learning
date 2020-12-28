# 1) load libraries and data ----------------------------------------------

library(AmesHousing)
library(tidyverse)
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
library(lightgbm)
library(treesnip)
library(catboost)

source("src/wf/fit_members.R")
source("src/wf/split_and_preprocess.R")
source("src/wf/grid_and_tune.R")
source("src/wf/train_final.R")
source("src/wf/def_glmnet.R")
source("src/wf/def_xgboost.R")
source("src/wf/def_lgbm.R")
source("src/wf/def_catboost.R")
source("src/wf/def_svm.R")

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(1234568)

house_data <- read.csv("data/casas_entrena.csv") %>% 
  mutate(SalePrice = log(SalePrice+1)) %>% 
  rename(sale_price = SalePrice)   

control_grid <- control_stack_grid()
model_list <- list()
preproc <- split_and_preprocess(data = house_data, prop_p = 0.995)

# svm
try({
  tuned <- grid_and_tune(model = "svm", preproc = preproc, 
                         size_n = 400, ctrl_grid_all = control_grid)
  score <- train_final(tuned = tuned, preproc = preproc)
  score$score
  model_list$svm <- list(preproc = preproc, tuned = tuned, score = score)
})

# glmnet
try({
  tuned <- grid_and_tune(model = "glmnet", preproc = preproc, 
                         size_n = 400, ctrl_grid_all = control_grid)
  score <- train_final(tuned = tuned, preproc = preproc)
  score$score
  model_list$glmnet <- list(preproc = preproc, tuned = tuned, score = score)  
})

# xgboost
try({
  tuned <- grid_and_tune(model = "xgboost", preproc = preproc, 
                         size_n = 400, ctrl_grid_all = control_grid)
  score <- train_final(tuned = tuned, preproc = preproc)
  score$score
  model_list$xgboost <- list(preproc = preproc, tuned = tuned, score = score)  
})

# lightgbm
try({
  tuned <- grid_and_tune(model = "lgbm", preproc = preproc, 
                         size_n = 400, ctrl_grid_all = control_grid)
  score <- train_final(tuned = tuned, preproc = preproc)
  score$score
  model_list$lgbm <- list(preproc = preproc, tuned = tuned, score = score)  
})

# catboost
try({
  tuned <- grid_and_tune(model = "catboost", preproc = preproc, 
                         size_n = 4, ctrl_grid_all = control_grid)
  score <- train_final(tuned = tuned, preproc = preproc)
  score$score
  model_list$catboost <- list(preproc = preproc, tuned = tuned, score = score)  
})

#saveRDS(model_list, "20201027_model_list.RDS")

model_stack <- 
  stacks::stacks() %>%
  stacks::add_candidates(model_list$glmnet$tuned$tuned) %>%
  #stacks::add_candidates(model_list$catboost$tuned$tuned) %>%
  #stacks::add_candidates(model_list$svm$tuned$tuned) %>%
  #stacks::add_candidates(model_list$lgbm$tuned$tuned) %>%
  #stacks::add_candidates(model_list$xgboost$tuned$tuned) %>%
  stacks::blend_predictions() 

model_stack2 <- 
  model_stack %>% 
  fit_members()

saveRDS(model_stack2, "20201027_model_stack.RDS")

pred_data <- bake(preproc$recipe, new_data = preproc$test)
star_pred <- bind_cols(pred_data, predict(model_stack2, new_data = pred_data))

model_list$glmnet$score$score
model_list$xgboost$score$score
model_list$lgbm$score$score
score = sqrt(sum((star_pred$sale_price - star_pred$.pred)^2)/nrow(star_pred))
score
  
# prediccion final --------------------------------------------------------
pred_data <- read.csv("data/casas_prueba.csv")
#pred_data <- read.csv("~/Desktop/test_NOTOCAR.csv")

pred_data1 <- bake(preproc$recipe, new_data = pred_data)
star_pred <- bind_cols(pred_data, predict(model_stack2, new_data = pred_data1))
star_pred %>% 
  select(id, .pred) %>% 
  mutate(.pred = exp(.pred)-1) %>% 
  set_names(c("id", "SalePrice")) %>% 
  write.csv("20201027_out_model2.csv")
