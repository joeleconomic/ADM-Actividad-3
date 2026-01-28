
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

# Base de datos de ventas

glimpse(ventas)

names(ventas)

names(ventas) <- make.unique(names(ventas))

sum(ventas == "NULL")

ventas[ventas == "NULL"] <- 0

ventas <- ventas |>
  mutate(OrderDate = as.Date(OrderDate, origin = "1899-12-30"),
         Sales.1 = as.numeric(Sales.1),
         Sales.2 = as.numeric(Sales.2),
         Sales.3 = as.numeric(Sales.3))

summary(ventas)

# Base de datos de compra de bicicleta

glimpse(bicicleta)
str(bicicleta)
summary(bicicleta)

bicicleta <- bicicleta |> 
    mutate(DateFirstPurchase = as.Date(DateFirstPurchase, origin = "1899-12-30"),
           BirthDate         = as.Date(BirthDate, origin = "1899-12-30"),
           across(c(Country, CountryRegionCode, Group, PersonType,
                  MaritalStatus, YearlyIncome, Gender,
                  Education, Occupation), as.factor),
           BikePurchase  = factor(BikePurchase, levels = c(0, 1), labels = c("No", "Yes")),
           HomeOwnerFlag = factor(HomeOwnerFlag, levels = c(0, 1), labels = c("No", "Yes")))

summary(bicicleta)

# Base de datos de clientes

glimpse(clientes)
summary(clientes)

sum(clientes == "NULL")

clientes <- clientes |>
  mutate(DateFirstPurchase = as.Date(DateFirstPurchase, origin = "1899-12-30"),
         BirthDate         = as.Date(BirthDate, origin = "1899-12-30"),
         across(c(Country, CountryRegionCode, Group, PersonType,
                  MaritalStatus, YearlyIncome, Gender,
                  Education, Occupation), ~ as.factor(.)),
        HomeOwnerFlag = factor(HomeOwnerFlag, levels = c(0,1), labels = c("No","Yes")))

glimpse(clientes)
summary(clientes)

# Base de datos sin etiquetar

glimpse(datos)
summary(datos)

sum(datos == "NULL")

which(duplicated(names(datos)))

datos <- datos |>
  filter(!if_any(everything(), ~ . == "NULL")) |>
  mutate(Weight = as.numeric(Weight),
         across(c(Name, Color, Size), as.factor))  



