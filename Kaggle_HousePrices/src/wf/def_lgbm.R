def_lgbm <- function(){
  model <- 
    parsnip::boost_tree(
      mode = "regression",
      learn_rate = tune(),
      trees = 7000,
      min_n = tune(), 
      tree_depth = tune(),
      mtry = .3
    ) %>%
    set_engine("lightgbm", objective = "reg:squarederror", num_leaves=4, max_bin = 200, 
               bagging_fraction = 0.75, bagging_freq = 5, bagging_seed = 7, feature_fraction = .2, 
               feature_fraction_seed = 7, verbose=-1)
  
  params <- 
    dials::parameters(
      learn_rate(),
      min_n(),
      tree_depth()
    )
  list(model = model, params = params)
}