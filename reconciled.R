library(tidyverse)
library(tsibble)
library(fable)
library(tsibbledata)
library(feasts)
library(fable.prophet)

# reconciled tourism
tourism_aggregated <- 
  tourism %>% 
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))

tourism_fit <- 
  tourism_aggregated %>%
  filter(Quarter < yearquarter("2015 Q1")) %>% 
  model(
    ets_n = ETS(Trips ~ trend("N")), # benchmark
    arima = ARIMA(Trips), # benchmark
    prophet = fable.prophet::prophet(Trips ~ season("year"))
  ) %>% 
  mutate(combn = (ets_n + arima + prophet)/3)

tourism_fc_reconciled <- 
  tourism_fit %>% 
  reconcile(coherent = min_trace(combn, method = "ols")) %>%
  forecast(h = "3 years")

tourism_fc_reconciled %>% 
  accuracy(tourism_aggregated) %>% 
  group_by(.model) %>%
  summarise_at(vars(ME:ACF1), median) %>% 
  arrange(MASE)

# La unidad básica para tsibble permite almacenamiento y manipulación de múltiples series de tiempo. 
# Contiene tres elementos básicos: Indice, Llaves y mediciones
# Trabaja con funciones de tidyverse
# Diferentes tipos de index variables que pueden ser utilizadas:

- Anuak
- quarterly
- Monhtly
- weekly
- Daily
- Subdaily
