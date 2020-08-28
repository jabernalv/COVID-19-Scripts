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
urlconfirmados <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
urlfallecidos <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&merge-replace02=on&merge-overwrite02=on&filter03=explode&explode-header-att03=date&explode-value-att03=value&filter04=rename&rename-oldtag04=%23affected%2Bdate&rename-newtag04=%23date&rename-header04=Date&filter05=rename&rename-oldtag05=%23affected%2Bvalue&rename-newtag05=%23affected%2Binfected%2Bvalue%2Bnum&rename-header05=Value&filter06=clean&clean-date-tags06=%23date&filter07=sort&sort-tags07=%23date&sort-reverse07=on&filter08=sort&sort-tags08=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"
urlpopulation <- "populationworld.csv"
# Leemos el archivo desde la web
time_series_ncov_Confirmed <- read.csv(file=urlconfirmados, sep=',')
time_series_ncov_Fallecidos <- read.csv(file=urlfallecidos, sep=',')
populationworld <- read.csv(file= urlpopulation)
# Eliminamos la primera fila que trae basura (revisen el .csv para confirmarlo)
time_series_ncov_Confirmed<-time_series_ncov_Confirmed[-1,]
time_series_ncov_Fallecidos<-time_series_ncov_Fallecidos[-1,]
# Hacemos algunas conversiones en los tipos de datos leídos
# y filtramos por las filas que tengan un valor diferente a 0
time_series_ncov_Confirmed<-time_series_ncov_Confirmed %>%
  mutate(
    Country.Region = as.character(time_series_ncov_Confirmed$Country.Region),
    Date = as.Date(time_series_ncov_Confirmed$Date),
    Value = as.integer(as.character(time_series_ncov_Confirmed$Value)))
time_series_ncov_Confirmed <- time_series_ncov_Confirmed %>%
  filter(time_series_ncov_Confirmed$Value >= 500)
time_series_ncov_Fallecidos<-time_series_ncov_Fallecidos %>%
  mutate(
    Country.Region = as.character(time_series_ncov_Fallecidos$Country.Region),
    Date = as.Date(time_series_ncov_Fallecidos$Date),
    Value = as.integer(as.character(time_series_ncov_Fallecidos$Value))) %>%
  filter(time_series_ncov_Fallecidos$Value >= 500)

# Creamos una columna que contenga los días transcurridos desde el primer caso confirmado
library(plyr) # Lo atachamos para usar la función setDT
setDT(time_series_ncov_Confirmed)[, Días := as.integer(Date - min(Date) + 1, units="days"), by = Country.Region]
setDT(time_series_ncov_Fallecidos)[, Días := as.integer(Date - min(Date) + 1, units="days"), by = Country.Region]
detach(package:plyr) # Lo desatachamos porque genera problemas en el summariza by_group
# Seleccionamos solo algunas columnas, agrupamos por país y día, luego sumamos los valores agrupados
time_series_ncov_Confirmed <- time_series_ncov_Confirmed %>%
  select(c(Country.Region, Date, Value, Días)) %>% 
  group_by(Country.Region, Date, Días) %>% 
  summarize(Value.sum = sum(Value))
time_series_ncov_Fallecidos <- time_series_ncov_Fallecidos %>%
  select(c(Country.Region, Date, Value, Días)) %>% 
  group_by(Country.Region, Date, Días) %>% 
  summarize(Value.sum = sum(Value))

# Escogemos cuáles países vamos a incluir en el reporte
# paises <- c('Colombia', 'Ecuador', 'Peru', 'Venezuela', 'Chile', 'Italy', 'Spain', 'Switzerland', 'Germany')
paises <- c('Colombia', 'Mexico', 'Argentina', 'Peru')

# Filtramos solo por los países que vamos a graficar
# por último, filtramos hasta 30 días
serie_Grafica<-time_series_ncov_Fallecidos %>% filter (Country.Region %in% paises) # %>% filter(Días <= 30)

# Convertimos a data.frame
serie_Grafica<-data.frame(
  País=serie_Grafica$Country.Region,
  Fecha=serie_Grafica$Date,
  Días=serie_Grafica$Días,
  Casos=as.integer(serie_Grafica$Value.sum))
# Graficamos
ggplot(data=serie_Grafica, mapping=aes(x=Días, y=Casos, colour=País)) + 
  geom_line()  + 
  geom_point( size=1, shape=21, fill="white") +
  # geom_spline() +
  #geom_text(aes(label = round(Casos, 1)), vjust = "inward", hjust = "inward", show.legend = FALSE) + 
  theme_minimal() +
  ggtitle(paste("Casos confirmados desde el día con 100 casos fallecidos COVID-19 por país. ", Sys.time())) +
  theme(plot.title = element_text(hjust = 0.5))

# https://www.doctormetrics.com/introduccion-al-forecasting-con-r-statistics/
library(prophet)
df_prophet <- data.frame(ds = serie_Grafica$Fecha, y = serie_Grafica$Casos)
m <- prophet(df_prophet)
future_df_prophet <- make_future_dataframe(m, periods = 120, freq = 'day')
forecast <- predict(m, future_df_prophet)


