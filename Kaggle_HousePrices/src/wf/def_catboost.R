def_catboost <- function(){
  model <- 
    parsnip::boost_tree(
      mode = "regression",
      learn_rate = tune(),
      trees = 7000,
      min_n = tune(), 
      tree_depth = tune(),
      mtry = .3
    ) %>%
    set_engine("catboost")
  
  params <- 
    dials::parameters(
      learn_rate(),
      min_n(),
      tree_depth()
    )
  list(model = model, params = params)
}