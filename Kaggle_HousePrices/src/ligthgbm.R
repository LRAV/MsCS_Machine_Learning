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
library(treesnip)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(123456)

ames_data <- read.csv("data/casas_entrena.csv")
ames_data <- ames_data %>% rename(sale_price = SalePrice) %>% 
  select(Total.Bsmt.SF, Overall.Qual, Overall.Cond, Gr.Liv.Area, Garage.Area, Garage.Cars, sale_price, Garage.Cond,
         Garage.Qual,Garage.Type, Functional, Half.Bath ,Bsmt.Half.Bath, Bsmt.Full.Bath,
         Low.Qual.Fin.SF, X2nd.Flr.SF,X1st.Flr.SF, Electrical,Bsmt.Unf.SF,Roof.Style,Roof.Matl,Exterior.1st, 
         Exterior.2nd, Mas.Vnr.Area, Land.Slope, Lot.Config, Land.Contour, Lot.Shape , Street, Lot.Frontage,
         Sale.Condition, Yr.Sold, Mo.Sold, Misc.Val, Pool.Area, Screen.Porch, X3Ssn.Porch, Open.Porch.SF,

         Sale.Type, Wood.Deck.SF, Paved.Drive, Garage.Finish, Garage.Yr.Blt, Fireplace.Qu, Fireplaces,
         TotRms.AbvGrd, Kitchen.Qual, Kitchen.AbvGr, Bedroom.AbvGr, Full.Bath, Central.Air,  Heating.QC,Heating,
         Bsmt.Exposure, MS.Zoning, Lot.Area, Utilities, Neighborhood,

          Bldg.Type, House.Style, Year.Built, Year.Remod.Add, Mas.Vnr.Type , Exter.Qual, Exter.Cond , Foundation, 
         BsmtFin.Type.2, BsmtFin.SF.1, BsmtFin.Type.1,
         MS.SubClass, Alley, Condition.1 , Condition.2, Bsmt.Qual, Bsmt.Cond,BsmtFin.SF.2 ,Enclosed.Porch)

ames_split <- rsample::initial_split(
  ames_data, 
  prop = 0.80, 
  strata = sale_price
)

preprocessing_recipe <- 
  recipes::recipe(sale_price ~ ., data = training(ames_split)) %>%
  recipes::step_string2factor(all_nominal()) %>%
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  recipes::step_nzv(all_nominal()) %>%
  prep()

ames_cv_folds <- 
  recipes::bake(
    preprocessing_recipe, 
    new_data = training(ames_split)
  ) %>%  
  rsample::vfold_cv(v = 5)

# lgbm_model <- 
#   linear_reg(penalty = tune(), mixture = tune()
#   ) %>%
#   set_mode("regression") %>%
#   set_engine("glmnet", objective = "reg:squarederror")

lgbm_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 5000,
    min_n = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("lightgbm", objective = "reg:squarederror",verbose=-1)


# grid specification
# lgbm_params <- 
#   dials::parameters(
#     penalty(),
#     mixture()
#   )

lgbm_params <- 
  dials::parameters(
    # The parameters have sane defaults, but if you have some knowledge 
    # of the process you can set upper and lower limits to these parameters.
    min_n(), # 2nd important
    tree_depth() # 3rd most important
  )

lgbm_grid <- 
  dials::grid_max_entropy(
    lgbm_params,
    size = 30 # set this to a higher number to get better results
    # I don't want to run this all night, so I set it to 30
  )
knitr::kable(head(lgbm_grid))

lgbm_wf <- 
  workflows::workflow() %>%
  add_model(lgbm_model) %>% 
  add_formula(sale_price ~ .)

lgbm_tuned <- tune::tune_grid(
  object = lgbm_wf,
  resamples = ames_cv_folds,
  grid = lgbm_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE)
)

lgbm_tuned %>%
  tune::show_best(metric = "rmse") %>%
  knitr::kable()

lgbm_best_params <- lgbm_tuned %>%
  tune::select_best("rmse")
knitr::kable(lgbm_best_params)

lgbm_model_final <- lgbm_model %>% 
  finalize_model(lgbm_best_params)

train_processed <- bake(preprocessing_recipe,  new_data = training(ames_split))
train_prediction <- lgbm_model_final %>%
  # fit the model on all the training data
  fit(
    formula = sale_price ~ ., 
    data    = train_processed
  ) %>%
  # predict the sale prices for the training data
  predict(new_data = train_processed) %>%
  bind_cols(training(ames_split))

lgbm_score_train <- 
  train_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))


test_processed  <- bake(preprocessing_recipe, new_data = testing(ames_split))
test_prediction <- lgbm_model_final %>%
  # fit the model on all the training data
  fit(
    formula = sale_price ~ ., 
    data    = train_processed
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(ames_split))

# measure the accuracy of our model using `yardstick`
lgbm_score <- 
  test_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(lgbm_score)

tt <- test_prediction %>% select(sale_price, .pred)

sqrt(sum((tt$sale_price - tt$.pred)^2)/nrow(tt))
sqrt(sum(((log(1+tt$sale_price) - log(1+tt$.pred))^2)/nrow(tt)))


