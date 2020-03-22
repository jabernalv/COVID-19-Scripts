# Cargamos las librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggstance)
library(ggformula)
# library(plyr)
# Se debe descargar el Dataset de https://www.ins.gov.co/Noticias/Paginas/Coronavirus.aspx
# En este caso se guarda con el nombre CasosColombia.csv
# se debe guardar en el directorio de trabajo
# Se lee la data del dataset
data <- read.csv("CasosColombia.csv", sep = ',') %>% 
  # Se renombran algunas de las columnas
  rename(
    'Fecha_diagnóstico'='Fecha.de.diagnóstico',
    'Ciudad_ubicación' = 'Ciudad.de.ubicación',
    'ID' = 'ID.de.caso'
    ) %>%
  # Se ajusta el tipo de algunas de las columnas
  # El dataset tiene error en la fechas:
  # hasta el ID 13 vienen como m/d/yyyy y 
  # de ahí en adelante d/m/yyyy, así que hay que corregir esto
  mutate(
    Fecha_diagnóstico = as.Date(as.character(Fecha_diagnóstico), "%d/%m/%y"),
    Edad = as.character(Edad),
    Ciudad_ubicación = as.character(Ciudad_ubicación)
    )

# Hay unos registros que vienen como Medellin así que se corrigen
data$Ciudad_ubicación = ifelse(data$Ciudad_ubicación=='Medellin', 'Medellín', data$Ciudad_ubicación)

# Se crea conjunto de datos por edad y se grafica
data.edad <- data %>% group_by(Edad) %>% summarise(n = n())
ggplot(data = data.edad) +
  geom_col(aes(Edad, y = n)) + 
  geom_text(aes(x = Edad, y = n + 1, label = n)) + 
  theme_minimal()

# Se crea conjunto de datos por fecha y se grafica
data.fecha <- data %>% group_by(Fecha_diagnóstico) %>% summarise(n = n())
ggplot(data = data.fecha) +
  geom_line(aes(Fecha_diagnóstico, y = n)) + 
  geom_text(aes(x = Fecha_diagnóstico, y = n + 1, label = n)) + 
  theme_minimal()

# Se crea conjunto de datos por ciudad y se grafica
# En este caso, por visualización, se filtran solo las ciudades que reporten más de tres casos,
# mas adelante es posible que toque cambiar este filtro
data.ciudad <- data %>% group_by(Ciudad_ubicación) %>% summarise(n = n())
ggplot(data = data.ciudad %>% filter(n>3)) +
  geom_col(aes(Ciudad_ubicación, y = n)) + 
  geom_text(aes(x = Ciudad_ubicación, y = n + 1, label = n)) + 
  theme_minimal()

# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-stacked-bar-chart-2/

# Se crea conjunto de datos por ciudad y sexo, y se grafica
data.ciudadsexo <- data %>% group_by(Ciudad_ubicación, Sexo) %>% summarise(n = n()) %>%
  group_by(Ciudad_ubicación) %>%
  arrange(Ciudad_ubicación, desc(Sexo)) %>%
  mutate(lab_ypos = cumsum(n) - 0.5 * n)
ggplot(data = data.ciudadsexo %>% filter(n>3), aes(x = Ciudad_ubicación, y = n)) +
  geom_col(aes(fill = Sexo), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = n, group =Sexo), color = "black") + 
  theme_minimal()

# Se crea conjunto de datos por edad y sexo, y se grafica
data.edadsexo <- data %>% group_by(Edad, Sexo) %>% summarise(n = n()) %>%
  group_by(Edad) %>%
  arrange(Edad, desc(Sexo)) %>%
  mutate(lab_ypos = cumsum(n) - 0.5 * n)

ggplot(data = data.edadsexo, aes(x = Edad, y = n)) +
  geom_col(aes(fill = Sexo), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = n, group =Sexo), color = "black") + 
  theme_minimal()

