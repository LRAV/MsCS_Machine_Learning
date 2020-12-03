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

# 1) NA exploratory ------------------------------------------------------

# 2) autofill NA ----------------------------------------------------------

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

# 0.1581191
na_knn_recipe <-
  recipe(SalePrice ~ ., data = train) %>% 
  recipes::step_string2factor(all_nominal()) %>%
  recipes::step_knnimpute(all_predictors(), neighbors = 5) %>% prep()

impute_recipe_test(na_knn_recipe, train, test)

# 0.1600061
na_knn_recipe <-
  recipe(SalePrice ~ ., data = train) %>% 
  recipes::step_string2factor(all_nominal()) %>%
  recipes::step_meanimpute(all_numeric()) %>% 
  recipes::step_modeimpute(all_nominal()) %>% prep()

impute_recipe_test(na_knn_recipe, train, test)

# 0.1601012
na_knn_recipe <-
  recipe(SalePrice ~ ., data = train) %>%
  recipes::step_string2factor(all_nominal()) %>%
  recipes::step_bagimpute(all_numeric()) %>% 
  recipes::step_knnimpute(all_nominal(), neighbors = 5) %>%
  prep()

impute_recipe_test(na_knn_recipe, train, test)

# 3) custom fill ----------------------------------------------------------
# 0.1597817
custom_fill <- function(data){
data %>%   
    mutate(
      Pool.QC = if_else(is.na(Pool.QC), "nopool", Pool.QC),
      Misc.Feature = if_else(is.na(Misc.Feature), "none", Misc.Feature),
      Alley = if_else(is.na(Alley), "none", Alley),
      Fence = if_else(is.na(Fence), "none", Fence),
      Fireplace.Qu = if_else(is.na(Fireplace.Qu), "none", Fireplace.Qu),
      Lot.Frontage = if_else(is.na(Lot.Frontage), 60, as.numeric(Lot.Frontage)),
      Garage.Cond = if_else(is.na(Garage.Cond), "none", Garage.Cond),
      Garage.Type = if_else(is.na(Garage.Type), "none", Garage.Type),
      Garage.Finish = if_else(is.na(Garage.Finish), "none", Garage.Finish),
      Garage.Qual = if_else(is.na(Garage.Qual), "none", Garage.Qual),
      Garage.Cars = if_else(is.na(Garage.Cars), 0, as.numeric(Garage.Cars)),
      BsmtFin.SF.1  = if_else(is.na(BsmtFin.SF.1), 0, as.numeric(BsmtFin.SF.1)),
      BsmtFin.SF.2  = if_else(is.na(BsmtFin.SF.2), 0, as.numeric(BsmtFin.SF.2)),
      Bsmt.Unf.SF  = if_else(is.na(Bsmt.Unf.SF), 0, as.numeric(Bsmt.Unf.SF)),
      Total.Bsmt.SF  = if_else(is.na(Total.Bsmt.SF), 0, as.numeric(Total.Bsmt.SF)),
      Bsmt.Full.Bath  = if_else(is.na(Bsmt.Full.Bath), 0, as.numeric(Bsmt.Full.Bath)),
      Bsmt.Half.Bath  = if_else(is.na(Bsmt.Half.Bath), 0, as.numeric(Bsmt.Half.Bath)),
      
      Bsmt.Qual = if_else(is.na(Bsmt.Qual), "none", Bsmt.Qual),
      Bsmt.Cond = if_else(is.na(Bsmt.Cond), "none", Bsmt.Cond),
      Bsmt.Exposure = if_else(is.na(Bsmt.Exposure), "none", Bsmt.Exposure),
      BsmtFin.Type.1 = if_else(is.na(BsmtFin.Type.1), "none", BsmtFin.Type.1),
      BsmtFin.Type.2 = if_else(is.na(BsmtFin.Type.2), "none", BsmtFin.Type.2),
      
      Mas.Vnr.Area = if_else(is.na(Mas.Vnr.Area), 0, as.numeric(Mas.Vnr.Area)),
      MS.Zoning = if_else(is.na(MS.Zoning), "RL" ,MS.Zoning),
      Functional = if_else(is.na(Functional), "Typ" ,Functional),
      Electrical = if_else(is.na(Electrical), "SBrkr" ,Electrical),
      Kitchen.Qual = if_else(is.na(Kitchen.Qual), "TA" ,Kitchen.Qual),
      MS.SubClass = as.character(MS.SubClass),
      Yr.Sold = as.character(Yr.Sold),
      Garage.Yr.Blt = if_else(is.na(Garage.Yr.Blt), "2005" ,as.character(Garage.Yr.Blt)),
      Mas.Vnr.Type = if_else(is.na(Mas.Vnr.Type), "None" ,Mas.Vnr.Type),
    )
}

pp <- randomForest::randomForest(SalePrice~., data = custom_fill(train), ntree = 1000)
pred = predict(pp, custom_fill(test) %>% select(-SalePrice))
sqrt(sum(((log(1+test$SalePrice) - log(1+pred))^2))/length(pred))


# 4) custom fill 2 ---------------------------------------------------------
# .1601
custom_fill <- function(data){
  data %>%   
    mutate(
      Pool.QC = if_else(is.na(Pool.QC), "nopool", Pool.QC),
      Misc.Feature = if_else(is.na(Misc.Feature), "none", Misc.Feature),
      Alley = if_else(is.na(Alley), "none", Alley),
      Fence = if_else(is.na(Fence), "none", Fence),
      Fireplace.Qu = if_else(is.na(Fireplace.Qu), "none", Fireplace.Qu),
      Garage.Cond = if_else(is.na(Garage.Cond), "none", Garage.Cond),
      Garage.Type = if_else(is.na(Garage.Type), "none", Garage.Type),
      Garage.Finish = if_else(is.na(Garage.Finish), "none", Garage.Finish),
      Garage.Qual = if_else(is.na(Garage.Qual), "none", Garage.Qual),
      Garage.Cars = if_else(is.na(Garage.Cars), 0, as.numeric(Garage.Cars)),
      BsmtFin.SF.1  = if_else(is.na(BsmtFin.SF.1), 0, as.numeric(BsmtFin.SF.1)),
      BsmtFin.SF.2  = if_else(is.na(BsmtFin.SF.2), 0, as.numeric(BsmtFin.SF.2)),
      Bsmt.Unf.SF  = if_else(is.na(Bsmt.Unf.SF), 0, as.numeric(Bsmt.Unf.SF)),
      Total.Bsmt.SF  = if_else(is.na(Total.Bsmt.SF), 0, as.numeric(Total.Bsmt.SF)),
      Bsmt.Full.Bath  = if_else(is.na(Bsmt.Full.Bath), 0, as.numeric(Bsmt.Full.Bath)),
      Bsmt.Half.Bath  = if_else(is.na(Bsmt.Half.Bath), 0, as.numeric(Bsmt.Half.Bath)),
      
      Bsmt.Qual = if_else(is.na(Bsmt.Qual), "none", Bsmt.Qual),
      Bsmt.Cond = if_else(is.na(Bsmt.Cond), "none", Bsmt.Cond),
      Bsmt.Exposure = if_else(is.na(Bsmt.Exposure), "none", Bsmt.Exposure),
      BsmtFin.Type.1 = if_else(is.na(BsmtFin.Type.1), "none", BsmtFin.Type.1),
      BsmtFin.Type.2 = if_else(is.na(BsmtFin.Type.2), "none", BsmtFin.Type.2),
      
      Mas.Vnr.Area = if_else(is.na(Mas.Vnr.Area), 0, as.numeric(Mas.Vnr.Area)),
      MS.SubClass = as.character(MS.SubClass),
      Yr.Sold = as.character(Yr.Sold),
      Mas.Vnr.Type = if_else(is.na(Mas.Vnr.Type), "None" ,Mas.Vnr.Type),
    )
}

na_knn_recipe <-
  recipe(SalePrice ~ ., data = custom_fill(train)) %>% 
  recipes::step_string2factor(all_nominal()) %>%
  recipes::step_knnimpute(all_predictors(), neighbors = 5) %>% prep()

na_kkn_train <- 
  recipes::bake(
    na_knn_recipe,
    new_data = custom_fill(train)
  )  

na_kkn_test <- 
  recipes::bake(
    na_knn_recipe,
    new_data = custom_fill(test)
  )

pp <- randomForest::randomForest(SalePrice~., data = na_kkn_train, ntree = 1000)
pred = predict(pp, na_kkn_test %>% select(-SalePrice))
sqrt(sum(((log(1+test$SalePrice) - log(1+pred))^2))/length(pred))
