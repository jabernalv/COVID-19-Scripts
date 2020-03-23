# Cargamos las librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
# library(plyr) # Esta se debe cargar más adelante porque genera problemas con sumarize y group_by
url_confirmados <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
url_fallecidos <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
url_recuperados <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

data_confirmados <- read.csv(url_confirmados, sep = ',', check.names=FALSE, stringsAsFactors=FALSE) %>%
  select(-Lat, -Long) %>% 
  pivot_longer(-(1:2), names_to="Fecha", values_to='Confirmados')
data_fallecidos <- read.csv(url_fallecidos, sep = ',', check.names=FALSE, stringsAsFactors=FALSE) %>%
  select(-Lat, -Long) %>% 
  pivot_longer(-(1:2), names_to="Fecha", values_to='Fallecidos')
data_recuperados <- read.csv(url_recuperados, sep = ',', check.names=FALSE, stringsAsFactors=FALSE) %>%
  select(-Lat, -Long) %>% 
  pivot_longer(-(1:2), names_to="Fecha", values_to='Recuperados')

data_total <- data_confirmados %>% 
  inner_join(data_fallecidos) %>% 
  inner_join(data_recuperados) %>% 
  select(-`Province/State`) %>%
  mutate(Fecha = as.Date(Fecha, format = "%m/%d/%y")) %>%
  filter(!(Confirmados == 0 & Fallecidos == 0 & Recuperados == 0)) %>%
  rename('País' = `Country/Region`) %>%
  arrange(País, Fecha) %>%
  group_by(País, Fecha) %>%
  summarize(Confirmados = sum(Confirmados), Fallecidos = sum(Fallecidos), Recuperados = sum(Recuperados)) %>%
  #filter(!(Confirmados.sum == 0 & Fallecidos.sum == 0 && Recuperados.sum == 0)) %>%
  arrange(País, Fecha)

# Creamos una columna que contenga los días transcurridos desde el primer caso confirmado
library(plyr) # Lo atachamos para usar la función setDT
setDT(data_total)[, Días := as.integer(Fecha - min(Fecha) + 1, units="days"), by = País]
detach(package:plyr) # Lo desatachamos porque genera problemas en el summariza by_group

# Escogemos cuáles países vamos a incluir en el reporte
# paises <- c('date', 'Colombia', 'Ecuador', 'Peru', 'Venezuela', 'Chile', 'Italy', 'Spain', 'Switzerland', 'Germany')
paises <- c('Italy', 'Spain')

ggplot(data=data_total %>% filter(País %in% paises), mapping=aes(x=Días, y=Fallecidos, colour=País)) + 
  geom_line()  + 
  geom_point( size=1, shape=21, fill="white") +
  geom_text(aes(label = Fallecidos), vjust = "inward", hjust = "inward", show.legend = FALSE) + 
  theme_minimal()


  