def_xgboost <- function(){
  model <- 
    parsnip::boost_tree(
      mode = "regression",
      learn_rate = tune(),
      trees = 5000,
      tree_depth = tune(), 
      min_n = tune(),
      mtry = .3
    ) %>%
    set_engine("xgboost", objective = 'reg:squarederror', gamma = 0, alpha = 0.00006, 
               scale_pos_weight = 1, subsample = .7, nthread=-1)
  
  params <- 
    dials::parameters(
      learn_rate(),
      tree_depth(),
      min_n()
    )
  list(model = model, params = params)
}