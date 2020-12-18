# La funcionalidad de tidyverts se explicará brevemente en esta sección. 
# Primero partiremos de los datos que se plantea en la sección xx.xx. La base de datos de turismo está a un nivel 
# de tiempo de Trimestre (Quarter), un nivel de agrupación de Estado, Región (State, Region) y con una variable agrupadora
# de Propósito de viaje (Purpose) Esta base de datos es una base adecuada para demostrar jerarquias. 

# como un breve EDA, tenemos que la base contiene 24,320 registros. 
# Al ver todas las series de tiempo que se pueden generar
# hay que considerar los distintos niveles de agregación:

# Al considerar las estados podemos tener 8 series de tiempo a un nivel estado:

tourism <- tourism %>%
  mutate(
    State = recode(
      State,
      "Australian Capital Territory" = "ACT",
      "New South Wales" = "NSW",
      "Northern Territory" = "NT",
      "Queensland" = "QLD",
      "South Australia" = "SA",
      "Tasmania" = "TAS",
      "Victoria" = "VIC",
      "Western Australia" = "WA"
    )
  )

tourism_levels  <- as.character((tourism %>% tibble %>% group_by(State) %>% summarise(t = sum(Trips)) %>% arrange(desc(t)))$State)

tourism_mod <- 
  tourism %>% 
  data.frame() %>% 
  mutate(State = fct_relevel(State, tourism_levels)) 

plot1 <- tourism_mod %>%
  group_by(State, Quarter) %>% 
  summarise(Trips = sum(Trips)) %>% 
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = State, color = State), alpha = .5) +
  facet_wrap(.~State, nrow = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values =  c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Series de tiempo a nivel Estado",
       subtitle = "Contamos con 8 series de tiempo, correspondientes a cada Estado",
       caption = "Total base: 24,320 registros (76 estados, 4 propósitos de viaje, 80 trimestres)")
  
ggsave(plot1, filename = "01_ts_state.png", height = 3, width = 7)


# Al considerar las estados y propósitos podemos tener 32 series de tiempo:

plot2 <- 
  tourism_mod %>%
  group_by(State, Quarter, Purpose) %>% 
  summarise(Trips = sum(Trips)) %>% 
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = State, color = State), alpha = .5) +
  facet_grid(Purpose~State, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values =  c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Series de tiempo a nivel Estado y Propósito",
       subtitle = "Contamos con 32 series de tiempo, correspondientes a cada Estado/Propósito",
       caption = "Total base: 24,320 registros (76 estados, 4 propósitos de viaje, 80 trimestres)")
  
ggsave(plot2, filename = "02_ts_state_purpose.png", height = 5.5, width = 7)


# Finalmente, cuando consideramos el nivel de agregación más pequeño podemos llegar a un nivel región -  propósitos podemos tener 304 series de tiempo:

plot3 <- 
  tourism_mod %>%
  group_by(Region, State, Quarter, Purpose) %>% 
  summarise(Trips = sum(Trips)) %>% 
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region, color = State), alpha = .5) +
  facet_grid(Purpose~State, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values =  c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Series de tiempo a nivel Región y Propósito",
       subtitle = "Contamos con 304 series de tiempo, correspondientes a cada Región/Propósito",
       caption = "Total base: 24,320 registros (76 estados, 4 propósitos de viaje, 80 trimestres)")

ggsave(plot3,  filename = "03_ts_region_purpose.png", height = 5.5, width = 7)

# El paquete nos permite realizar series de tiempo agrupadas. Por ejemplo en esta base de datos
# tenemos establecida como key: {Estado, Region, Propósito} que como se exploró previamente, se tienen 
# 304 grupos. Al aplicar la funcion "Model" se puede específicar que modelos intentar, por ejemplo como 
# modo exploratorio, podemos ver que el siguiente código nos genera 3 modelos distintos para los 304 grupos
# específicados: 

fit <- 
  tourism %>%
  filter(year(Quarter) <= 2014) %>% 
  model(
    snaive = SNAIVE(Trips ~ lag("year")),
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  )

fit

# Por ejemplo, cuando consideramos la región "Snowy Mountains" y el propósito "Holiday" y seleccionando los 
# respectivos modelos que se probaron: snaive, ets, arima

# Para el modelo ARIMA podemos ver la especificación del modelo, asi como sus respectivos coeficientes con sus s.e. 
# Además proporciona tu log likelihood, AIC y BIC
fit %>%
  filter(Region == "Snowy Mountains", Purpose == "Holiday") %>%
  select(arima) %>%
  report()

model_for <- 
  fit %>%
  filter(Region == "Snowy Mountains", Purpose == "Holiday") %>%
  forecast(h = "3 years")

model_for %>%
  autoplot(tourism %>% 
             filter(year(Quarter) <= 2014), level = NULL) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Forecast para la serie tiempo de tourism",
       subtitle = "Se considera la región Snowy Mountains y el propósito Holiday", 
       caption = "Modelo considerado Autoarima con un periodo de predicción de 3 años", 
       x = "Año", y = "Número de Viajes")

accuracy(model_for, tourism)

fit %>%
  glance() %>% 
  arrange(MSE)

fit %>%
  select(Region, State, Purpose, arima) %>%
  coef()

fit %>%
  augment()



# De la misma manera, para el ets, nos proporciona la especificación del modelo, sus parámetros de smoothing y el AIC y BIC
fit %>%
  filter(Region == "Snowy Mountains", Purpose == "Holiday") %>%
  select(ets) %>%
  report()

# Para el Snaive, solamente nos considera el sigma^2
fit %>%
  filter(Region == "Snowy Mountains", Purpose == "Holiday") %>%
  select(snaive) %>%
  report()




