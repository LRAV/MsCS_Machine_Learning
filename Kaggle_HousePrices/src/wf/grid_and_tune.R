grid_and_tune <- function(model, preproc, size_n, ctrl_grid_all){
  if(model == "glmnet"){
    definition = def_glmnet()
  }
  
  if(model == "xgboost"){
    definition = def_xgboost()
  }
  
  if(model == "svm"){
    definition = def_svm()
  }
  
  if(model == "lgbm"){
    definition = def_lgbm()
  }
  
  if(model == "catboost"){
    definition = def_catboost()
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