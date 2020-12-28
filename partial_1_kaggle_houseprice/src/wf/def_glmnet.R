def_glmnet <- function(){
  
  model <- 
    linear_reg(
      penalty = tune(), 
      mixture = tune()
    ) %>% 
    set_engine("glmnet")
  
  params <- 
    dials::parameters(
      penalty(), 
      mixture()
    )
  
  list(model = model, params = params)
}