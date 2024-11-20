# Estad-stica
#Código para crear modelos estadisticos ACP, LOGIT Y conglomerados
###CÓDIGO EN R USADO PARA HACER EL CODIGO DE VARIABLES CATEGÓRICAS HORARIO, TIPO DE VIOLENCIA Y TRANSPORTE DONDE SUFRIÓ VIOLENCIA 


library(stringr)
library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(readxl)

rm(list = ls())
datos <- read_excel("Datos_equipocaracol.xlsx")

# Aquí se coloca el nombre (exacto) de todas las variables que quieras transformar

columnas_dummies <- c("Tipo_de_violencia") (“Horario”) (“Transporte_donde_sufrio_violencia”)



# IMPORTANTE, No modificar el código de abajo, solo correrlo

for (nombre_columna in columnas_dummies) {
  
  categorias_unicas <- datos %>%
    pull(!!sym(nombre_columna)) %>%
    strsplit(", ") %>%
    unlist() %>%
    trimws() %>%
    gsub(",", "", .) %>%
    unique() %>%
    .[!is.na(.) & . != ""]
  
  for (categoria in categorias_unicas) {
    nombre_dummy <- gsub("[^[:alnum:]_]", "", gsub(" ", "_", categoria))
    datos[[nombre_dummy]] <- as.numeric(str_detect(datos[[nombre_columna]], fixed(categoria)))
  }
}

View(datos)


#código para guardar los datos 
#colocar nombre de la nueva base de datos

write.csv(datos, "basededatos_caracol.csv")





#CÓDIGO EN R USADO PARA HACER LOGIT DE VARIABLES: DURACIÓN DE TRAYECTO Y EDAD




rm(list = ls())                 
options(warn = -1)             

library(foreign)                 
library(stats) 
library(tidyverse)
library(readxl) 
library(caTools)
library(dplyr)

caracol = read_excel("base_caracol.xlsx") 

variables_seleccionadas = c("victima", "duracion_trayecto", "edad")

caracol = caracol %>% select(all_of(variables_seleccionadas))

set.seed(123)

dividir <- sample.split(caracol$victima, SplitRatio = 0.7)

caracol_correr_logit = subset(caracol, dividir == TRUE)
caracol_validar_logit = subset(caracol, dividir == FALSE)

caracol_correr_logit$victima <- ifelse(caracol_correr_logit$victima == 1, 0, 1)

resultados_logit <- glm(victima ~ as.factor(duracion_trayecto) + as.factor(edad),
                        data = caracol_correr_logit, 
                        family = "binomial") 

table(caracol_correr_logit$victima)
summary(resultados_logit) 

exp(coefficients(resultados_logit)) %>% round(digits = 4) %>% data.frame() 


CÓDIGO EN R USADO PARA HACER LOGIT DE VARIABLES: HORARIOS Y SEXO

rm(list = ls())                 
options(warn = -1)             

library(foreign)                 
library(stats) 
library(tidyverse)
library(readxl) 
library(caTools)
library(dplyr)

caracol = read_excel("base_caracol.xlsx") 

variables_seleccionadas = c("victima", "1a4pm", "9a11pm", "5a8am", "9a12pm", "5a8pm", "sexo")

caracol = caracol %>% select(all_of(variables_seleccionadas))

set.seed(123)

dividir <- sample.split(caracol$victima, SplitRatio = 0.7)

caracol_correr_logit = subset(caracol, dividir == TRUE)
caracol_validar_logit = subset(caracol, dividir == FALSE)

caracol_correr_logit$victima <- ifelse(caracol_correr_logit$victima == 1, 0, 1)

resultados_logit <- glm(victima ~ as.factor(`1a4pm`) + as.factor(`9a11pm`) + as.factor(`5a8am`) + as.factor(`9a12pm`) + as.factor(`5a8pm`) + as.factor(sexo),
                        data = caracol_correr_logit, 
                        family = "binomial") 

table(caracol_correr_logit$victima)
summary(resultados_logit) 

exp(coefficients(resultados_logit)) %>% round(digits = 4) %>% data.frame() 



CÓDIGO EN R USADO PARA HACER ACP DE VARIABLES CATEGÓRICAS 

if (!require(FactoMineR)) install.packages("FactoMineR")
if (!require(factoextra)) install.packages("factoextra")
if (!require(foreign)) install.packages("foreign")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(psych)) install.packages("psych")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")

library(FactoMineR)
library(factoextra)
library(foreign)
library(ggplot2)
library(psych)
library(dplyr)
library(tidyr)

# Limpiar el entorno
rm(list = ls())

# Pelotita y cargar los datos
datos <- read_excel("acp_caracol.xlsx")

#-------Modificar (las variables, ojo con la nomenclatura)-----------------

variables_seleccionadas <- c("sexo", "victima", "agresor", "acoso", "robo", "violencia_verbal", "violencia_fisica", "carterismo", "discriminacion", "abuso_autoridad", "metro", "metrobus", "combi", "camion", "microbus", "trolebus", "taxi", "tren_ligero", "mototaxi")



#-------No modificar -----------------
# Filtrar el dataframe para incluir solo las variables seleccionadas
datos_filtrados <- datos[, variables_seleccionadas]


#-------Modificar (si lo consideran, el número de factores)-----------------
poly_model <- fa(datos_filtrados, nfactors = 3, cor = "poly", fm = "mle", rotate = "none")

# Mostrar las cargas factoriales y diagrama del modelo
print(poly_model$loadings)
fa.diagram(poly_model)


Código para Análisis de Conglomerados 

rm(list = ls())
graphics.off()

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(klaR)
library(readxl)

# 1. Cargar base
# Cambiar la ruta al directorio donde se encuentra tu archivo
setwd("C:/ruta/a/tu/directorio")
acp_caracol <- read_excel("acp_caracol.xlsx", sheet = "Hoja1")

# 2. Arreglo de la base de datos
# Filtrar variables de interés
var <- c("sexo", "agresor","acoso", "robo", "violencia_sexual", "violencia_verbal", "carterismo", "violencia_fisica", "abuso_autoridad", "discriminacion", "metro", "metrobus", "combi", "camion", "taxi", "microbus", "trolebus" )



# 3. Algoritmo de k-modes con 3 grupos

set.seed(123)  # Para reproducibilidad
fit <- kmodes(acp_caracol[var], 2)

# 4. Pegar grupos a la base original
dat.grupos <- data.frame(acp_caracol, fit$cluster)

# 5. Interpretación
View(dat.grupos)

# Tablas de distribución por grupo
table(dat.grupos$fit.cluster, dat.grupos$sexo) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$agresor) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$robo) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$violencia_sexual) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$acoso) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$violencia_verbal) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$carterismo) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$violencia_fisica) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$abuso_de_autoridad) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$discriminacio) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$metro) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$metrobus) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$combi) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$camion) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$taxi) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$microbus) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)

table(dat.grupos$fit.cluster, dat.grupos$trolebus) %>% 
  prop.table(1) %>% `*`(100) %>% round(1)




# Guardar resultados
write.csv(dat.grupos, "4_caracol_resultados_conglomerados.csv", row.names = FALSE)


