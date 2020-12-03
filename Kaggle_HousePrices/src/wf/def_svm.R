def_svm <- function(){
  model <- 
    parsnip::svm_poly(
      mode = "regression",
      scale_factor = tune(),
      degree = tune(),
      cost = tune()
    ) %>%
    set_engine("kernlab",
               scaled = FALSE)
  
  params <- 
    dials::parameters(
      scale_factor(),
      degree(),
      cost()
    )
  list(model = model, params = params)
}