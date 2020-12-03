train_final <- function(tuned, preproc){
  
  train_processed <- bake(preproc$recipe,  new_data = preproc$train)
  
  train_prediction <- tuned$model_final %>% 
    fit(
      formula = sale_price ~ ., 
      data    = train_processed
    ) %>%
    predict(new_data = train_processed) %>%
    bind_cols(preproc$train)
  
  score_train <- 
    train_prediction %>%
    yardstick::metrics(sale_price, .pred) %>%
    mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
  
  test_processed  <- bake(preproc$recipe, new_data = preproc$test)
  
  test_prediction <- tuned$model_final %>%
    fit(
      formula = sale_price ~ ., 
      data    = train_processed
    ) %>%
    predict(new_data = test_processed) %>%
    bind_cols(preproc$test)
  
  score_test <- 
    test_prediction %>%
    yardstick::metrics(sale_price, .pred) %>%
    mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
  
  tt <- test_prediction %>% select(sale_price, .pred)
  score = sqrt(sum((tt$sale_price - tt$.pred)^2)/nrow(tt))
  print(score)
  
  list(score = list(score_train = score_train, score_test = score_test), data_f = test_prediction) 
}