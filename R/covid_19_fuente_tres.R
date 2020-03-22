# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# Cargamos las librerías necesarias
library(dplyr)
library(readxl)
library(httr)

# Este dataset no está acualizado
#create the URL where the dataset is stored with automatic updates every day
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
data <- read_excel(tf) %>% mutate (DateRep = as.Date(DateRep)) %>% 
  filter(Cases != 0 & Deaths != 0) %>%
  arrange(`Countries and territories`, Year, Month, Day)

# Creamos una columna que contenga los días transcurridos desde el primer caso confirmado
library(plyr) # Lo atachamos para usar la función setDT
setDT(data)[, Días := as.integer(DateRep - min(DateRep) + 1, units="days"), by = `Countries and territories`]
detach(package:plyr) # Lo desatachamos porque genera problemas en el summariza by_group

# Escogemos cuáles países vamos a incluir en el reporte
# paises <- c('Colombia', 'Ecuador', 'Peru', 'Venezuela', 'Chile', 'Italy', 'Spain', 'Switzerland', 'Germany')
paises <- c('Colombia')


# Graficamos
ggplot(data = data %>% filter (`Countries and territories` %in% paises), mapping = aes(x = Días, y = Cases, colour = `Countries and territories`)) + 
  geom_line()  + 
  geom_point( size=1, shape=21, fill="white") +
  # geom_spline() +
  geom_text(aes(label = round(Cases, 1)), vjust = "inward", hjust = "inward", show.legend = FALSE) + 
  theme_minimal() +
  ggtitle(paste("Casos COVID-19 por país. ", Sys.time())) +
  theme(plot.title = element_text(hjust = 0.5))

