# This code is useful as an example of forecast reconciliation
# Salvador Garcia Gonzalez

# 0) Load libraries -------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(tsibbledata)
library(feasts)
library(fable.prophet)
library(lubridate)

# 2) Create all possible groups for forecast ------------------------------

tourism_aggregated <- 
  tourism %>% 
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))

# 3) Filter data and then train all especified models ---------------------

tourism_fit <- 
  tourism_aggregated %>%
  filter(Quarter < yearquarter("2015 Q1")) %>% 
  model(
    ets_n = ETS(Trips ~ trend("N")), # benchmark
    arima = ARIMA(Trips), # benchmark
    prophet = fable.prophet::prophet(Trips ~ season("year"))
  ) %>% 
  mutate(combn = (ets_n + arima + prophet)/3)

# 4) Forecast reconciliation ----------------------------------------------

# Use OLS method to reconcile forecast

tourism_fc_reconciled <- 
  tourism_fit %>% 
  reconcile(coherent = min_trace(combn, method = "ols")) %>%
  forecast(h = "3 years")

# plot reconciled forecasts

tourism_fc_reconciled %>% 
  filter(State == "TAS", .model == "coherent", Purpose == "Holiday", Region != "<aggregated>") %>% 
  ggplot(aes(x = Quarter, y = .mean, fill = factor(Region))) +
  geom_bar(stat = "identity", alpha = .5)+
  geom_line(data = tourism_fc_reconciled %>% 
              filter(State == "TAS", .model == "coherent", Purpose == "Holiday", Region == "<aggregated>") %>% 
              tibble(), 
            aes(x = Quarter, y = .mean), , color = "dodgerblue", linetype = 2)+
  geom_point(data = tourism_fc_reconciled %>% 
              filter(State == "TAS", .model == "coherent", Purpose == "Holiday", Region == "<aggregated>") %>% 
              tibble(), 
            aes(x = Quarter, y = .mean), color = "dodgerblue")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_fill_manual(values =  c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  labs(title = "Comparación de reconciliated forecast",
      subtitle = "La comparación es a nivel Estado: TAS, Proposito: Holiday, All regions", 
      fill = "")

# plot all models for reconciliation

tourism_fc_reconciled %>% 
  filter(State == "TAS",  Purpose == "Holiday", Region != "<aggregated>") %>% 
  ggplot(aes(x = Quarter, y = .mean)) +
  geom_bar(stat = "identity", alpha = .5, aes(fill = factor(Region)))+
  geom_line(data = tourism_fc_reconciled %>% 
              filter(State == "TAS",  Purpose == "Holiday", Region == "<aggregated>") %>% 
              tibble(), 
            aes(x = Quarter, y = .mean), , color = "dodgerblue", linetype = 2)+
  geom_point(data = tourism_fc_reconciled %>% 
               filter(State == "TAS",  Purpose == "Holiday", Region == "<aggregated>") %>% 
               tibble(), 
             aes(x = Quarter, y = .mean), color = "dodgerblue")+
  geom_line(data = tourism %>% 
              tibble() %>% 
              filter(Quarter >= yearquarter("2015 Q1")) %>% 
              filter(State == "TAS", Purpose == "Holiday") %>% 
              group_by(Quarter) %>% 
              summarise(Trips = sum(Trips, na.rm = T)),
            aes(x = Quarter, y = Trips),
            linetype = 2, color = "gray10")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_fill_manual(values =  c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  labs(title = "Comparación de forecast NO reconciliated",
       subtitle = "La comparación es a nivel Estado: TAS, Proposito: Holiday, All regions", 
       fill = "")+
  facet_wrap(~.model, ncol = 2)

# NOT reconciled forecast table 

tourism_fc_reconciled %>% 
  filter(State == "TAS",  Purpose == "Holiday", Region == "<aggregated>", .model  == "ets_n") %>% 
  tibble() %>% 
  select(State, Purpose, Quarter, .mean) %>% 
  rename(Desagregado = .mean) %>% 
  left_join(
    tourism_fc_reconciled %>% 
      filter(State == "TAS",  Purpose == "Holiday", Region != "<aggregated>", .model  == "ets_n") %>% 
      tibble() %>% 
      select(State, Purpose, Quarter, .mean) %>%
      group_by(State, Purpose, Quarter) %>% 
      summarise(Agregado =sum(.mean)),
    by = c("State" = "State", "Purpose" = "Purpose", "Quarter" = "Quarter")
  )

# reconciled forecast

tourism_fc_reconciled %>% 
  filter(State == "TAS",  Purpose == "Holiday", Region == "<aggregated>", .model  == "coherent") %>% 
  tibble() %>% 
  select(State, Purpose, Quarter, .mean) %>% 
  rename(Desagregado = .mean) %>% 
  left_join(
    tourism_fc_reconciled %>% 
      filter(State == "TAS",  Purpose == "Holiday", Region != "<aggregated>", .model  == "coherent") %>% 
      tibble() %>% 
      select(State, Purpose, Quarter, .mean) %>%
      group_by(State, Purpose, Quarter) %>% 
      summarise(Agregado =sum(.mean)),
    by = c("State" = "State", "Purpose" = "Purpose", "Quarter" = "Quarter")
  )



