#Para realizar la limpieza de datos sobre el archivo que contine la informacion a trabajar se llevará a cabo los siguientes pasos
#Cargar el archivo desde R
library(readxl)
myData9 <- read_excel("Documents/MBA/6to trimestre/Escenarios/Archivos/myData9.xlsx")
myData9
library(readxl)
is.na(myData9$`Close TSLA`)


#La figura 1.1 muestra reporte donde se han resaltado los valores faltantes, la calificación de servicio para la observación 2 y la calificación de limpieza para la observación 13 están marcados como TRUE porque no están presentes, El resto de los valores en la figura están etiquetados como FALSE porque están presentes

#Base de datos cargado en R para llevar a cabo la limpieza de datos
is.na(myData9$`Close TSLA`)

#Para identificar las filas en el marco de datos o los casos que están completos podemos usar la función:
#myData9[complete.cases(myData9), ]
#Cuando los datos están casi completos, es posible que no sea conveniente enlistar todos lo casos completos, para ello podemos usar el operador not (signo ¡) con la función complete.cases que servirá para identificar observaciones con valores faltantes, la función será así:
#myData9[!complete.cases(myData9), ]
#Para implementar la estrategia de omisión utilizaremos la función na.omit con el objetivo de eliminar las observaciones con valores faltantes y almacenar el conjunto de datos resultante en el marco de datos omissionData, para ello la función será así:
omissionData <- na.omit(myData9)
omissionData
#Para implementar la estrategia de imputación de la media simple, se iniciará con el marco original, se calcula el valor promedio utilizando mean, para calcular los valores promedio de las variables AMZN y META ingresar:
ambienceMean <- mean(myData9$`Close AMZN`, na.rm = TRUE) 
serviceMean <- mean(myData9$`Close META`, na.rm = TRUE)
#Para imputar los valores faltantes en las variables AMZN y META, de nuevo utilizamos la función is.na para identificar los valores faltantes y reemplazarlos con las medias calculadas en el paso, ingresar:
AMZNMean <- mean(myData9$AMZN, na.rm = TRUE)
TSLAMean <- mean(myData9$TSLA, na.rm = TRUE)
myData9$AMZN[is.na(myData9$AMZN)] <- AMZNMean
myData9$TSLA[is.na(myData9$TSLA)] <- TSLAMean

#Medidas de Posición en R
#Para obtener la mediana y el promedio en R, es necesario ejecutar estos comandos

#Promedio:
mean(myData9$AMZN)

#Mediana:
median(myData9$TSLA)

#En el caso de la función summary nos permitirá obtener los valores: mìnimo, cuartil, mediana, media, tercel cuartíl y máximo para cada variable de la base de datos que se está trabajando
summary(myData9)

#En el caso de la función tapply nos servirá para encontrar medias o desviaciones estándar
tapply(myData9$AMZN, myData9$TSLA, mean)

#En el caso de necesitar un percentil que no se sea un cuartil utilizamos la función:
quantile(myData9$AMZN,0.30)

#Para calcular mínimos y máximos utilizamos la funciòn:
max(myData9$AMZN) - min(myData9$AMZN)

#Para calcular el rango intercuartilico utilizamos la función:
quantile(myData9$AMZN, 0.75) - quantile(myData9$AMZN, 0.25)

#Para calcular la desviación mediana absoluta utilizamos la función:
mean(abs(myData9$AMZN-mean (myData9$AMZN)))

#Para obtener la varianza y la desviación estándar muestrales utilizamos la función:
var(myData9$AMZN)
sd(myData9$AMZN)

#Para calcular el coeficiente de correlación entre las variables AMZN y TSLA se debe utilizar la función:
cor(myData9)

#Para diagramas de caja utilizando nuestra información, esta en la función:
boxplot(myData9$AMZN, myData9$TSLA, main= "Boxplots for AMZN and TSLA" , xlab="Annual Returnos, 2019-2024 (in percent)", names=c("AMZN","TSLA"), horizontal = TRUE, col="gold")

#Para tartar los valores atìpicos de las 2 variables que estamos trabajando, las funciones son las siguientes
outliersAMZN <- boxplot(myData9$AMZN)$out; outliersAMZN
outliersTSLA <- boxplot(myData9$TSLA)$out; outliersTSLA

#Para tratar los valores atípicos utilizamos esta función:
myData9$newAMZN <- ifelse(myData9$AMZN %in% outliersAMZN, NA, myData9$AMZN)

#Al haber reemplazado el valor atípico podemos de nuevo ejecutar la función de summary:
summary(myData9)

#VISUALIZACIÓN DE DATOS
#La siguiente base de datos fue cargada a RStudio para poder realizar un 
#análisis y visualización de la data de precio de acciones. 
#De la cual se necesita conocer el comportamiento de las acciones en los
#últimos 6 meses.
library(readxl)
myData <- read_excel
myData
#Se procedió a determinar cuál es la fecha más reciente y la fecha de 6 meses 
#atrás con los siguientes códigos:
fecha_mas_reciente <- max(myData$Fecha)
fecha_6_meses_atras <- fecha_mas_reciente - months(6)
#Para poder graficar el comportamiento de las acciones es necesario instalar
#los siguientes paquetes:
install.packages("ggplot2") 
install.packages("dplyr")
#Se Cargan las bibliotecas necesarias:
library(ggplot2) 
library(dplyr)
#Creamos un gráfico utilizando ggplot2 donde cada línea representa el precio 
#de cierre de una acción (Close TSLA, Close AMZN, etc.) en función de la fecha.
#Utilizamos dplyr para encontrar las fechas con los valores máximos de cada 
#acción en el período de los últimos 6 meses. Esto nos dará las fechas en las 
#que cada acción alcanzó su pico más alto.
# Graficar el comportamiento de las acciones en los últimos 6 meses
ggplot(myData_ultimos_6_meses, aes(x = Fecha)) +
  +     geom_line(aes(y = Close TSLA), color = "blue", linetype = "solid") +
  +     geom_line(aes(y = Close AMZN), color = "red", linetype = "dashed") +
  +     geom_line(aes(y = Close PARA), color = "green", linetype = "dotted") +
  +     geom_line(aes(y = Close META), color = "purple", linetype = "dotdash") +
  +     # Agrega más geom_line() para otras acciones si es necesario
  +     labs(title = "Comportamiento de Acciones en los Últimos 6 Meses",
             +          x = "Fecha", y = "Precio de Cierre") +
  +     theme_minimal()
#En este Código se puede observer que se graficaron las principals acciones 
#siendo estas Tesla (color azul), Aamazon (color rojo), Paramount (color verde) 
#y Meta (color Morado), con una gráfica de líneas donde muestra en el eje X la 
#Fecha y en el eje Y el precio de cierre
#Del anterior gráfico se puede visualizar cómo Meta ha mantenido los mejores 
#precios de cierre de acciones, seguido de Tesla, Amazone y por último Paramount
#Como siguiente paso deseamos determinar los porcentajes por acción:
fecha_mas_reciente <- max(myData$Fecha)
fecha_6_meses_atras <- fecha_mas_reciente - months(6)
myData_ultimos_6_meses <- myData %>% filter(Fecha >= fecha_6_meses_atras)
precio_inicial <- sapply(myData_ultimos_6_meses[, -1], function(x) head(x, 1))
precio_actual <- sapply(myData_ultimos_6_meses[, -1], function(x) tail(x, 1))
porcentaje_cambio <- (precio_actual - precio_inicial) / precio_inicial * 100
resultado <- data.frame(Accion = names(porcentaje_cambio),
                        Porcentaje_Cambio = porcentaje_cambio)
resultado <- resultado %>% arrange(desc(Porcentaje_Cambio))
print(resultado)
#Ahora se creará la gráfica de estos datos con el siguiente código:
ggplot(resultado, aes(x = Accion, y = Porcentaje_Cambio)) +
  +     geom_bar(stat = "identity", fill = "skyblue") +
  +     labs(title = "Rendimiento en Términos Porcentuales de Acciones",
             +          x = "Acción", y = "Porcentaje de Cambio") +
  +     theme_minimal() +
  +     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  +     geom_text(aes(label = sprintf("%.2f", Porcentaje_Cambio)), vjust 
                  = -0.3, size = 3)
#Esta gráfica indica que la acción con mayor rendimiento son las acciones de 
#Meta y las que tinen un menor rendimiento son las de Tesla. 
#CIENCIA DE DATOS
#se realiza una regresión lineal de la data anteriormente vista.
#Usamos la función lm() para crear un modelo de regresión lineal donde
#Close META es la variable dependiente y Fecha es la variable independiente.
#summary(modelo_META) proporciona un resumen del modelo de regresión, 
#que incluye los coeficientes, el valor de R-squared, y los p-valores para 
#evaluar la significancia estadística.
#Utilizamos ggplot2 para crear una gráfica que muestra los datos reales de 
#los precios de cierre de Meta como puntos (geom_point()) y la línea de 
#regresión ajustada (geom_smooth(method = "lm", se = FALSE)).
library(readxl)
library(dplyr)
myData <- read_excel
myData$Fecha <- as.Date(myData$Fecha, format = "%Y-%m-%d")
fecha_mas_reciente <- max(myData$Fecha)
fecha_6_meses_atras <- fecha_mas_reciente - months(6)
myData_ultimos_6_meses <- myData %>% filter(Fecha >= fecha_6_meses_atras)
modelo_META <- lm(Close META ~ Fecha, data = myData_ultimos_6_meses)
summary(modelo_META)
library(ggplot2)
ggplot(myData_ultimos_6_meses, aes(x = Fecha, y = Close META)) +
  geom_point(color = "blue") +  # Puntos de datos reales
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Línea de regresión
  labs(title = "Regresión Lineal del Precio de Cierre de Meta (META)",
       x = "Fecha", y = "Precio de Cierre (USD)") +
  theme_minimal()
#La línea de regresión te ayuda a visualizar la tendencia general del precio 
#de las acciones de Meta durante los últimos 6 meses. Si la línea tiene una 
#pendiente positiva, indica que el precio ha estado aumentando. Si tiene una 
#pendiente negativa, el precio ha estado disminuyendo.
#La dispersión de los puntos alrededor de la línea de regresión muestra la 
#variabilidad del precio de cierre. Si los puntos están muy dispersos, esto 
#indica una alta variabilidad en los precios de las acciones. Si están más 
#cerca de la línea, la variabilidad es menor.

#Para realizar la lectura de una página web se debe realizar los siguientes pasos:
#Primero, instalamos y cargamos las librerías necesarias.
# Instalar las librerías necesarias
install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyverse")
# Cargar las librerías
library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)

#Segundo
# Leer la página web
url <- 'https://en.wikipedia.org/wiki/R_(programming_language)'
webpage <- read_html(url) 

#Tercero
# Extraer el primer párrafo
first_paragraph <- webpage %>%
  html_node('p') %>%
  html_text()
# Mostrar el primer párrafo
print(first_paragraph) 

#Identificamos la tabla usando el selector adecuado y la convertimos en un dataframe. 
# Extraer la tabla de información (infobox)
infobox <- webpage %>%
  html_node('.infobox') %>%
  html_table()
# Mostrar la tabla de información
print(infobox) 

#Cuarto
# Limpiar el primer párrafo
first_paragraph_clean <- str_trim(first_paragraph)
# Mostrar el párrafo limpio
print(first_paragraph_clean) 

#A veces, las tablas pueden necesitar más limpieza. Podemos usar funciones de dplyr para
#renombrar columnas, filtrar filas, etc. 
# Limpiar y estructurar la tabla de información
infobox_clean <- infobox %>%
  rename(Attribute = 1, Value = 2) %>%
  filter(!is.na(Attribute))
# Mostrar la tabla de información limpia
print(infobox_clean) 

#Quinto paso
# Agregar una columna ficticia de datos numéricos para el análisis
set.seed(123) # Para reproducibilidad
infobox_clean$NumericValue <- sample(1:100, nrow(infobox_clean), replace
                                     = TRUE)
# Mostrar la tabla con la nueva columna
print(infobox_clean) 

#Utilizaremos summarise() de dplyr para calcular medidas como la media, mediana,
#desviación estándar, etc. 
# Calcular medidas de resumen estadístico
summary_stats <- infobox_clean %>%
  summarise(
    Mean = mean(NumericValue),
    Median = median(NumericValue),
    SD = sd(NumericValue),
    Min = min(NumericValue),
    Max = max(NumericValue)
  )
# Mostrar las medidas de resumen estadístico
print(summary_stats) 

#Sexto paso
# Guardar el primer párrafo en un archivo de texto
writeLines(first_paragraph_clean, 'primer_parrafo.txt')
# Guardar la tabla de información en un archivo CSV
write.csv(infobox_clean, 'infobox_R.csv', row.names = FALSE)
# Guardar las medidas de resumen estadístico en un archivo CSV
write.csv(summary_stats, 'summary_stats.csv', row.names = FALSE)

