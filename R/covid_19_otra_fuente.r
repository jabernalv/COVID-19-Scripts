# Cargamos las librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
# library(plyr) # Esta se debe cargar más adelante porque genera problemas con sumarize y group_by
# Fuente https://ourworldindata.org/coronavirus-source-data
url_confirmed_cases <- "https://covid.ourworldindata.org/data/total_cases.csv"
url_total_deaths <- "https://covid.ourworldindata.org/data/total_deaths.csv"
url_new_confirmed_cases <- "https://covid.ourworldindata.org/data/new_cases.csv"
url_new_deaths <- "https://covid.ourworldindata.org/data/new_deaths.csv"
url_full_dataset <- "https://covid.ourworldindata.org/data/full_data.csv"

# Escogemos cuáles países vamos a incluir en el reporte
# paises <- c('date', 'Colombia', 'Ecuador', 'Peru', 'Venezuela', 'Chile', 'Italy', 'Spain', 'Switzerland', 'Germany')
paises <- c('date', 'Colombia', 'Venezuela', 'Chile', 'Brazil')


data_confirmed_cases <- read.csv(url_confirmed_cases, sep = ',', header = T)
data_confirmed_cases <- data_confirmed_cases %>% select(paises)
# Transpone todas las columnas menos la primera
df_transpose <- cbind(colnames(data_confirmed_cases)[-1], t(data_confirmed_cases[-1]) )
# Añadimos los nombres de las columnas
colnames(df_transpose) <- c('País', as.character(data_confirmed_cases[, 1]))

df_gather <- gather(data = data.frame(df_transpose), key = "Fecha", value = "Casos", 2:dim(df_transpose)[2]) %>%
  filter(!is.na(Casos)) %>%
  mutate(
    Fecha = as.Date(gsub('\\.', '-', substring(Fecha, 2, 11))),
    Casos = as.numeric(Casos)
    )
# Creamos una columna que contenga los días transcurridos desde el primer caso confirmado
library(plyr) # Lo atachamos para usar la función setDT
setDT(df_gather)[, Días := as.integer(Fecha - min(Fecha) + 1, units="days"), by = País]
detach(package:plyr) # Lo desatachamos porque genera problemas en el summariza by_group


ggplot(data=df_gather, mapping=aes(x=Días, y=Casos, colour=País)) + 
  geom_line()  + 
  geom_point( size=1, shape=21, fill="white") +
  geom_text(aes(label = round(Casos, 1)), vjust = "inward", hjust = "inward", show.legend = FALSE) + 
  theme_minimal()

