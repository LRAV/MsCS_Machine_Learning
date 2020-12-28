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
    recipes::step_string2factor(all_nominal()) %>%
    recipes::step_knnimpute(all_predictors(), neighbors = 5) %>% 
    recipes::step_other(all_nominal(), threshold = 0.01) %>%
    recipes::step_nzv(all_nominal()) %>%
    recipes::step_YeoJohnson(all_numeric(), -sale_price) %>% 
    recipes::step_mutate(
      tot.area = X2nd.Flr.SF+X1st.Flr.SF+Total.Bsmt.SF,
      avg_rm_sf = Gr.Liv.Area / TotRms.AbvGrd,
      total_baths = Bsmt.Full.Bath + (Bsmt.Half.Bath * 0.5) + Full.Bath + (Half.Bath * 0.5),
      age = Yr.Sold - Year.Built,
      new = if_else(Yr.Sold == Year.Built, 1, 0),
      old = if_else(Year.Built < 1940, 1, 0),
      pool = if_else(Pool.Area > 0, 1, 0),
      basement = if_else(Total.Bsmt.SF > 0, 1, 0),
      garage = if_else(Garage.Area > 0, 1, 0),
      remodeled = if_else(Year.Remod.Add > Year.Built, 1, 0),
      porch_area = Open.Porch.SF + X3Ssn.Porch + Wood.Deck.SF + Screen.Porch + Enclosed.Porch,
      overall_rating = Overall.Cond + Overall.Qual,
      total_sf2 = tot.area^2,
      avg_rm_sf2 = avg_rm_sf^2,
      age2 = age^2
    ) %>%
    recipes::step_interact(terms = ~ Gr.Liv.Area:Overall.Qual) %>% 
    recipes::step_interact(terms = ~ Gr.Liv.Area:tot.area) %>% 
    recipes::step_interact(terms = ~ Overall.Qual:tot.area) %>% 
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
