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
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(123456)

house_data <- read.csv("data/casas_entrena.csv")

# 2) splitting and variable selection -------------------------------------

house_data <- 
  house_data %>% 
  rename(sale_price = SalePrice) %>% 
  select(Total.Bsmt.SF, Overall.Qual, Overall.Cond, Gr.Liv.Area, Garage.Area, Garage.Cars, sale_price, Garage.Cond,
         Garage.Qual,Garage.Type, Functional, Half.Bath ,Bsmt.Half.Bath, Bsmt.Full.Bath,
         Low.Qual.Fin.SF, X2nd.Flr.SF,X1st.Flr.SF, Electrical,Bsmt.Unf.SF,Roof.Style,Roof.Matl,Exterior.1st, 
         Exterior.2nd, Mas.Vnr.Area, Land.Slope, Lot.Config, Land.Contour, Lot.Shape , Street, Lot.Frontage,
         Sale.Condition, Yr.Sold, Mo.Sold, Misc.Val, Pool.Area, Screen.Porch, X3Ssn.Porch, Open.Porch.SF,
         
         Sale.Type, Wood.Deck.SF, Paved.Drive, Garage.Finish, Garage.Yr.Blt, Fireplace.Qu, Fireplaces,
         TotRms.AbvGrd, Kitchen.Qual, Kitchen.AbvGr, Bedroom.AbvGr, Full.Bath, Central.Air,  Heating.QC,Heating,
         Bsmt.Exposure, MS.Zoning, Lot.Area, Utilities, Neighborhood)
         
         #Bldg.Type, House.Style, Year.Built, Year.Remod.Add, Mas.Vnr.Type , Exter.Qual, Exter.Cond , Foundation, 
         #BsmtFin.Type.2, BsmtFin.SF.1, BsmtFin.Type.1, 
         #MS.SubClass, Alley, Condition.1 , Condition.2, Bsmt.Qual, Bsmt.Cond,BsmtFin.SF.2 ,Enclosed.Porch)

house_split <- 
  rsample::initial_split(
    house_data, 
    prop = 0.65, 
    strata = sale_price
    )

# 3) tidymodel recipe -----------------------------------------------------

preprocessing_recipe <- 
  recipes::recipe(sale_price ~ ., data = training(house_split)) %>%
  recipes::step_string2factor(all_nominal()) %>%
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  recipes::step_nzv(all_nominal()) %>%
  prep()

house_cv_folds <- 
  recipes::bake(
    preprocessing_recipe, 
    new_data = training(house_split)
  ) %>%  
  rsample::vfold_cv(v = 5)

# 4) model especification -------------------------------------------------

xgboost_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = tune(),
    tree_depth = 3,
    learn_rate = 0.05,
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")

xgboost_params <- 
  dials::parameters(
    min_n(),
    #tree_depth(),
    #learn_rate(),
    loss_reduction()
  )

ctrl_grid <- control_stack_grid()

xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 60, 
    control = ctrl_grid
  )

knitr::kable(xgboost_grid)

# 5) workflow -------------------------------------------------------------

xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(sale_price ~ .)

xgboost_tuned <- tune::tune_grid(
  object = xgboost_wf,
  resamples = house_cv_folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE)
)

xgboost_tuned %>%
  tune::show_best(metric = "rmse", n = 10) %>%
  knitr::kable()

# xgboost_best_params <- (xgboost_tuned %>%
#   tune::show_best(metric = "rmse", n = 10))

xgboost_best_params <-
  xgboost_tuned %>%
  tune::select_best("rmse")

# xgboost_best_params <- xgboost_tuned %>%
#   tune::collect_metrics()

knitr::kable(xgboost_best_params)

xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

# 6) final model ----------------------------------------------------------

train_processed <- bake(preprocessing_recipe,  new_data = training(house_split))
train_prediction <- 
  xgboost_model_final %>%
  fit(
    formula = sale_price ~ ., 
    data    = train_processed
  ) %>%
  predict(new_data = train_processed) %>%
  bind_cols(training(house_split))

xgboost_score_train <- 
  train_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

test_processed  <- bake(preprocessing_recipe, new_data = testing(house_split))
test_prediction <- xgboost_model_final %>%
  fit(
    formula = sale_price ~ ., 
    data    = train_processed
  ) %>%
  predict(new_data = test_processed) %>%
  bind_cols(testing(house_split))

xgboost_score <- 
  test_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(xgboost_score)

tt <- test_prediction %>% select(sale_price, .pred)

sqrt(sum((tt$sale_price - tt$.pred)^2)/nrow(tt))
sqrt(sum(((log(1+tt$sale_price) - log(1+tt$.pred))^2)/nrow(tt)))


  
  







