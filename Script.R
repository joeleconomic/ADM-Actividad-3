
# 1. Análisis descriptivo ----

## 1.1 Librerias ----

library(tidyverse)
library(openxlsx)

## 1.2 Carga de datos ----

# Hoja: ST Ventas Totales
ventas <- read.xlsx("DataSet SQL_Act3_ADMN.xlsx",
                   sheet = 2) 

# Var Discreta Adq Bicicleta
bicicleta <- read.xlsx("DataSet SQL_Act3_ADMN.xlsx",
                    sheet = 3) # información sobre clientes

# Var Continuas Gasto Clientes
clientes <- read.xlsx("DataSet SQL_Act3_ADMN.xlsx",
                    sheet = 4)

# Datos sin etiquetar
datos <- read.xlsx("DataSet SQL_Act3_ADMN.xlsx",
                    sheet = 5)

## 1.3 Limpieza de datos ----

glimpse(ventas)

names(ventas) <- make.unique(names(ventas))

ventas[is.na(ventas)] <- 0

ventas <- ventas |>
  mutate(OrderDate = as.Date(OrderDate, origin = "1899-12-30"),
         Sales.1 = as.numeric(Sales.1),
         Sales.2 = as.numeric(Sales.2),
         Sales.3 = as.numeric(Sales.3))

summary(ventas)

  
  
