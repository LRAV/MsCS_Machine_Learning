# 1) load libraries and data ----------------------------------------------

library(gridExtra)
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
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(123456)

data <- read.csv("data/casas_entrena.csv")
split <- initial_split(data, prop = .7)
train <- training(split)
test <- testing(split)

glimpse(train %>% select(all_nominal()))
glimpse(train %>% select(all_numeric()))

# 1) variable transformation -------------------------------------------------

grid.arrange(
  train %>% 
    select(all_numeric()) %>% 
    gather(variable, value) %>% 
    ggplot(aes(x = value))+
    geom_histogram()+
    facet_wrap(~variable, scales = "free"),
  recipes::bake(
    recipe(train) %>% recipes::step_YeoJohnson(all_numeric()) %>% prep(), 
    new_data = train
  ) %>% 
    select(all_numeric()) %>% 
    gather(variable, value) %>% 
    ggplot(aes(x = value))+
    geom_histogram()+
    facet_wrap(~variable, scales = "free"),
  ncol = 2
)

# 3) test -----------------------------------------------------------------
impute_recipe_test <- function(recipe, train, test){
  na_kkn_train <- 
    recipes::bake(
      recipe,
      new_data = train
    )  
  
  na_kkn_test <- 
    recipes::bake(
      recipe,
      new_data = test
    )
  
  pp <- randomForest::randomForest(SalePrice~., data = na_kkn_train, ntree = 1000)
  pred = predict(pp, na_kkn_test %>% select(-SalePrice))
  sqrt(sum(((log(1+test$SalePrice) - log(1+pred))^2))/length(pred))
}

# 0.157684
na_knn_recipe <-
  recipe(SalePrice ~ ., data = train) %>% 
  recipes::step_string2factor(all_nominal()) %>%
  recipes::step_knnimpute(all_predictors(), neighbors = 5) %>% 
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  recipes::step_nzv(all_predictors(), -SalePrice) %>% 
  recipes::step_YeoJohnson(all_numeric(), -SalePrice) %>% 
  prep

impute_recipe_test(na_knn_recipe, train, test)



