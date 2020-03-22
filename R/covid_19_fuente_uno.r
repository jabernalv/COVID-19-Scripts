# Cargamos las librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggstance)
library(ggformula)
# library(plyr)
# https://data.humdata.org/m/dataset/novel-coronavirus-2019-ncov-casestmp <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
# La URL donde está todos los datasets de COVID19 es https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases
# URL donde se encuentran los datos actualizados
urlconfirmados <- "https://data.humdata.org/hxlproxy/data/download/time_series-ncov-Confirmed.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv"
urlfallecidos <- "https://data.humdata.org/hxlproxy/data/download/time_series-ncov-Deaths.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Bkilled%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Deaths.csv"
# Leemos el archivo desde la web
time_series_ncov_Confirmed<-read.csv(file=urlconfirmados, sep=',')
# Eliminamos la primera fila que trae basura (revisen el .csv para confirmarlo)
time_series_ncov_Confirmed<-time_series_ncov_Confirmed[-1,]
# Hacemos algunas conversiones en los tipos de datos leídos
# y filtramos por las filas que tengan un valor diferente a 0
time_series_ncov_Confirmed<-time_series_ncov_Confirmed %>%
  mutate(
    Country.Region = as.character(time_series_ncov_Confirmed$Country.Region),
    Date = as.Date(time_series_ncov_Confirmed$Date),
    Value = as.integer(as.character(time_series_ncov_Confirmed$Value))) %>%
  filter(time_series_ncov_Confirmed$Value != 0)

# Creamos una columna que contenga los días transcurridos desde el primer caso confirmado
library(plyr) # Lo atachamos para usar la función setDT
setDT(time_series_ncov_Confirmed)[, Días := as.integer(Date - min(Date) + 1, units="days"), by = Country.Region]
detach(package:plyr) # Lo desatachamos porque genera problemas en el summariza by_group
# Seleccionamos solo algunas columnas, agrupamos por país y día, luego sumamos los valores agrupados
# por último, filtramos hasta 30 días
time_series_ncov_Confirmed <- time_series_ncov_Confirmed %>%
  select(c(2,6,7)) %>% 
  group_by(Country.Region, Días) %>% 
  summarize(Value.sum = sum(Value))

# Escogemos cuáles países vamos a incluir en el reporte
# paises <- c('Colombia', 'Ecuador', 'Peru', 'Venezuela', 'Chile', 'Italy', 'Spain', 'Switzerland', 'Germany')
paises <- c('Italy', 'Israel')

# Filtramos solo por los países que vamos a graficar
serie_Grafica<-time_series_ncov_Confirmed %>% filter (Country.Region %in% paises) %>% filter(Días <= 30)

# Convertimos a data.frame
serie_Grafica<-data.frame(
  País=serie_Grafica$Country.Region,
  Días=serie_Grafica$Días,
  Casos=as.integer(serie_Grafica$Value.sum))
# Graficamos
ggplot(data=serie_Grafica, mapping=aes(x=Días, y=Casos, colour=País)) + 
  geom_line()  + 
  geom_point( size=1, shape=21, fill="white") +
  # geom_spline() +
  geom_text(aes(label = round(Casos, 1)), vjust = "inward", hjust = "inward", show.legend = FALSE) + 
  theme_minimal() +
  ggtitle(paste("Casos fallecidos COVID-19 por país. ", Sys.time())) +
  theme(plot.title = element_text(hjust = 0.5))

