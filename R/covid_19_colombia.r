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

data_por_dia <- data.frame(table(data$Fecha_diagnóstico))
colnames(data_por_dia) <- c('Fecha','Casos')
data_por_dia$Fecha <- as.Date(data_por_dia$Fecha)
data_por_dia$Total <- cumsum(data_por_dia$Casos)
data_por_dia$Días <- as.integer(data_por_dia$Fecha - min(data_por_dia$Fecha) + 1, units="days")
# Hay unos registros que vienen como Medellin así que se corrigen
data$Ciudad_ubicación = ifelse(data$Ciudad_ubicación=='Medellin', 'Medellín', data$Ciudad_ubicación)

# https://dlegorreta.wordpress.com/2015/04/20/series-con-tendencia-y-estacionalidad/
#Hacemos las estimaciones de los modelos
#Cuadrático
fit.cuadratico <- lm(data_por_dia$Total ~ data_por_dia$Días + data_por_dia$Días ^ 2)
#Calculamos el logaritmo de la serie
log.sp <- log(data_por_dia$Total)

#Estimamos este ajuste como auxiliar
fit.Log.Lin <- lm(log.sp ~ data_por_dia$Días)

#Usamos ciertos datos para estimar el mod. exponecial
beta_0 <- fit.Log.Lin$coefficient[1]
beta_1 <- fit.Log.Lin$coefficient[2]
Datos <- data.frame('Total' = data_por_dia$Total, 'Días' = data_por_dia$Días)
#Calculamos por regresión no lineal el modelo exponencial
mod.fit.exp <- nls(Datos$Total ~ exp(beta0 + beta1 * Datos$Días), data = Datos, start = list(beta0 = beta_0, beta1 = beta_1))
#Log.Lineal
Log.Lin.Mod <- exp(beta_0 + beta_0 * data_por_dia$Días)
#Lineal 
fit.lineal <- lm(data_por_dia$Total ~ data_por_dia$Días)

#Hacemos la gráfica para ver los ajustes
plot(ts(data_por_dia$Total), xlab="Días", ylab = "Casos acumulados", main = "Cálculo de tendencias")
# lines(predict(fit.lineal), col="2")
lines(predict(mod.fit.exp), col="3")
# lines(Log.Lin.Mod, col="4")
# lines(fit.cuadratico$fitted, col="6")
# legend("topleft",c('Lineal', 'Exponencial', 'Log-Lineal', 'Cuadrática'), col = c(2, 3, 4, 6), pch=1, text.col = c(2,3,4,6))
legend("topleft",c('Exponencial'), col = c(3), pch=1, text.col = c(3))

# Se calculan las medidas estadísticas para cada modelo, usamos una función 
# que concentra todos los estadísticos.
medidas <- function(m, y, k){ # y = serie, m = modelo, k = número parametros
  T <- length(y)
  yest <- fitted(m)
  sse <- sum((yest - y)^2)
  ssr <- sum((y - mean(y))^2)
  mse <- sse / (T - k)
  R2 <- 1 - (sse / ssr)
  Ra2 <- 1 - ((T - 1) * (1 - R2) / (T - k))
  aic <- log((T - k) * exp(2 * k / T) * mse / T)
  bic <- log(T^(k / T) * (T - k) * mse / T)
  M <- c(Ra2, mse, aic, bic)
  names(M) = c("R2-ajus", "MSE", "logAIC", "logBIC")
  return(M)
}

#Elección del modelo
m.lin <- medidas(fit.lineal, data_por_dia$Total, 2)
m.exp <- medidas(mod.fit.exp, data_por_dia$Total, 2)
m.cuad <- medidas(fit.cuadratico, data_por_dia$Total, 3)
m.LogLin <- medidas(fit.Log.Lin, log(data_por_dia$Total), 2)
M <- cbind(m.lin, m.exp, m.cuad, m.LogLin)
M
# Se nota que el modelo que resulta tener mejores estadísticos es 
# el exponencial. Lo cual desde la gráfica se visualizaba un poco.


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

