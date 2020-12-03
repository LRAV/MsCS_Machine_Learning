library(tidyverse)
library(fpp3)
library(patchwork)
library(sf)

state_colors <- c(
  `New South Wales` = "#56b4e9",
  `Victoria` = "#0072b2",
  `Queensland` = "#009e73",
  `South Australia` = "#f0e442",
  `Northern Territory` = "#d55e00",
  `Western Australia` = "#e69f00",
  `Tasmania` = "#cc79a7",
  `Australian Capital Territory` = "#cccccc"
)

read_sf("Forecast_reconciliation/tourism/Tourism_Regions_2020.shp") %>%
  dplyr::rename(State = "STE_NAME16") %>%
  ggplot() +
  geom_sf(aes(fill = State), alpha = 0.8) +
  theme_void() +
  scale_fill_manual(values = state_colors)

OvernightTrips_Region <- readr::read_csv("Forecast_reconciliation/tourism/OvernightTrips_2017.csv")[,-(1:3)] %>%
  # Replace outlier from Adelaide Hills
  mutate(
    `Adelaide Hills` = case_when(
      `Adelaide Hills` > 80 ~ 10,
      TRUE ~ `Adelaide Hills`
    )
  )

# Convert to tsibble
tourism <- hts::hts(
  ts(OvernightTrips_Region, start=1998, frequency=12),
  list(7, c(6,5,4,4,3,3,2), c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,3,4,2,3,1,1,1,2,2,3,4))
) %>%
  as_tsibble() %>%
  dplyr::rename(
    state = "Level 1",
    zone = "Level 2",
    region = "Level 3",
    month = index,
    visitors = value
  ) %>%
  mutate(
    state = recode(state,
                   A = "NSW",
                   B = "VIC",
                   C = "QLD",
                   D = "SA",
                   E = "WA",
                   F = "TAS",
                   G = "NT"
    ),
    zone = recode(zone,
                  AA = "Metro NSW",
                  AB = "North Coast NSW",
                  AC = "South Coast NSW",
                  AD = "South NSW",
                  AE = "North NSW",
                  AF = "ACT",
                  BA = "Metro VIC",
                  BB = "West Coast VIC",
                  BC = "East Coast VIC",
                  BC = "North East VIC",
                  BD = "North West VIC",
                  CA = "Metro QLD",
                  CB = "Central Coast QLD",
                  CC = "North Coast QLD",
                  CD = "Inland QLD",
                  DA = "Metro SA",
                  DB = "South Coast SA",
                  DC = "Inland SA",
                  DD = "West Coast SA",
                  EA = "West Coast WA",
                  EB = "North WA",
                  EC = "South WA",
                  FA = "South TAS",
                  FB = "North East TAS",
                  FC = "North West TAS",
                  GA = "North Coast NT",
                  GB = "Central NT"
    )
  ) %>%
  select(month, everything())

# Show first 10 rows of data
tourism

p1 <- tourism %>%
  dplyr::summarise(visitors = sum(visitors)) %>%
  autoplot(visitors) +
  ylab("Overnight trips") + xlab("Time") +
  scale_y_log10() +
  ggtitle("Total domestic travel: Australia")
p2 <- tourism %>%
  group_by(state) %>%
  dplyr::summarise(visitors = sum(visitors)) %>%
  autoplot(visitors) +
  ylab("Overnight trips") +
  scale_y_log10() +
  ggtitle("Total domestic travel: by state")
p3 <- tourism %>%
  filter(state=="NSW") %>%
  group_by(zone) %>%
  dplyr::summarise(visitors = sum(visitors)) %>%
  mutate(zone = paste0("NSW/",zone)) %>%
  autoplot(visitors) +
  ylab("Overnight trips") +
  scale_y_log10() +
  ggtitle("Total domestic travel: NSW by zone") +
  guides(colour = guide_legend(title = "state/zone"))
p4 <- tourism %>%
  filter(zone=="South NSW") %>%
  autoplot(visitors) +
  ylab("Overnight trips") +
  scale_y_log10() +
  ggtitle("Total domestic travel: South NSW by region")
aligned_plots <- align_patches(p1, p2, p3, p4)


tourism_agg <- tourism %>%
  aggregate_key(state/zone/region, visitors = sum(visitors))

tourism_agg

fit <- tourism_agg %>%
  filter(year(month) <= 2015) %>%
  model(ets = ETS(visitors))

fit

fc <- fit %>%
  reconcile(
    ols = min_trace(ets, method="ols"),
    wlsv = min_trace(ets, method="wls_var"),
    wlss = min_trace(ets, method="wls_struct"),
    #mint_c = min_trace(ets, method="mint_cov"),
    mint_s = min_trace(ets, method="mint_shrink"),
  ) %>%
  forecast(h = "2 years")

fc


fc %>%
  filter(is_aggregated(state)) %>%
  autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)


fc %>%
  filter(state == "NSW" & is_aggregated(zone)) %>%
  autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)


fc %>%
  filter(region == "Melbourne") %>%
  autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)


fc %>%
  filter(region == "Snowy Mountains") %>%
  autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)


fc %>%
  filter(region == "Barossa") %>%
  autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)

fc %>%
  accuracy(data = tourism_agg,
           measures = list(rmsse = RMSSE))

fc %>%
  accuracy(tourism_agg,
           measures = list(mase = MASE, rmsse = RMSSE)) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), rmsse = sqrt(mean(rmsse^2))) %>%
  arrange(rmsse)


fc %>%
  accuracy(tourism_agg,
           measures = list(mase = MASE, rmsse = RMSSE)) %>%
  mutate(
    level = case_when(
      is_aggregated(state) ~ "National",
      is_aggregated(zone) ~ "State",
      is_aggregated(region) ~ "Zone",
      TRUE ~ "Region"
    ),
    level = factor(level, levels=c("National","State","Zone","Region"))
  ) %>%
  group_by(.model, level) %>%
  dplyr::summarise(mase = mean(mase), rmsse = sqrt(mean(rmsse^2))) %>%
  arrange(level, rmsse)

## Example: Australian electricity generation

energy <- readr::read_csv('Forecast_reconciliation/energy/daily.csv') %>%
  head(-1)%>% #Remove last observation
  select(date,contains(' -  GWh'))%>%
  dplyr::rename_all(~gsub(' -  GWh','',.x))%>%
  mutate(date=as.Date(date),
         Battery=rowSums(select(., contains("Battery"))),
         Gas = rowSums(select(., contains("Gas"))),
         Solar = rowSums(select(., contains("Solar"))),
         Coal = rowSums(select(., contains("Coal"))),
         `Hydro (inc. Pumps)` = Hydro + Pumps,
         Renewable=Biomass+Hydro+Solar+Wind,
         `non-Renewable`=Coal+Distillate+Gas,
         Total=Renewable+`non-Renewable`+Battery+Pumps)%>%
  pivot_longer(cols=-date,names_to = 'Source',values_to = 'Generation') %>%
  as_tsibble(key = Source)



energy %>%
  filter(
    Source %in% c('Total', 'Wind', 'Solar', 'Distillate')
  ) %>%
  mutate(
    Source = ordered(Source,
                     levels = c('Total','Wind','Solar','Distillate'))
  ) %>%
  ggplot(aes(x=date, y=Generation)) +
  geom_line() +
  facet_wrap(~Source, nrow = 4,  ncol = 1, scales = 'free_y')


