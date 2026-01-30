
# 1. Carga y limpieza de datos ----

## 1.1 Librerias ----

library(tidyverse)
library(openxlsx)
library(scales)
library(factoextra)
library(NbClust)
library(rpart)
library(rpart.plot)
library(caret)
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


# 2. Análisis descriptivo ----

## 2.1 Serie temporal de ventas ----

glimpse(ventas)

ggplot(ventas, aes(x = OrderDate, y = Sales)) +
  geom_line() +
  labs(title = NULL,
       x = NULL,
       y = "Ventas totales") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

# Seleccionamos solo las columnas numéricas
ventas_num <- ventas[, c("Sales", "Sales.1", "Sales.2", "Sales.3")]

# Calculamos la matriz de correlaciones
correlaciones <- cor(ventas_num, use = "complete.obs")

# Redondeamos para mostrar
round(correlaciones, 2)

## 2.2 Tabla de compra de bicicleta
names(bicicleta)

bicicleta |> 
  group_by(BikePurchase) |> 
  summarise(Frecuencia = n()) |> 
  mutate(Porcentaje = round(Frecuencia / sum(Frecuencia) * 100, 2)) |> 
  as.data.frame()

# Seleccionamos solo las columnas numéricas
bicicleta_num <- bicicleta |> 
  select(where(is.numeric))

# Calculamos la matriz de correlaciones
correlaciones_bici <- cor(bicicleta_num, use = "complete.obs")

# Redondeamos para mostrar
round(correlaciones_bici, 2)


# 3. Modelos de clasificación ----
datos_regresion <- bicicleta %>% 
  select(BikePurchase, TotalAmount, Country, Group, Age, MaritalStatus, 
         YearlyIncome, Gender, TotalChildren, Education, Occupation, 
         HomeOwnerFlag, NumberCarsOwned)

# Testeo y entrenamiento
set.seed(123) 
indice <- sample(1:nrow(datos_regresion), size = round(0.8 * nrow(datos_regresion))) 
train <- datos_regresion[indice, ] 
test <- datos_regresion[-indice, ]

## 3.1 Regresión logística (LOGIT) ----
modelo_rlog <- glm(BikePurchase ~ TotalAmount + Country + Group + Age + MaritalStatus + 
                   YearlyIncome + Gender + TotalChildren + Education + Occupation + 
                   HomeOwnerFlag + NumberCarsOwned, 
                   data = train, family ="binomial")

summary(modelo_rlog)

## 3.2 Árbol de decisión ----
modelo_arbol <- rpart(BikePurchase ~ TotalAmount + Country + Group + Age + MaritalStatus + 
                      YearlyIncome + Gender + TotalChildren + Education + Occupation + 
                      HomeOwnerFlag + NumberCarsOwned, 
                      data = train, method ="class")

rpart.plot(modelo_arbol)


# 4. Comparación de la precisión de los modelos y ranking de importancia de variables ----

# 4.1 ¿Cómo de buenos son los modelos? ----
prob_pred <- predict(modelo_rlog, newdata = test, type = "response")
clase_pred <- ifelse(prob_pred > 0.5, "Yes", "No")

pred_clase_factor <- factor(clase_pred, levels = c("No", "Yes")) 
real_factor <- factor(test$BikePurchase, levels = c("No", "Yes"))

print("Matriz de Confusión - Regresión Logística:")
confusionMatrix(pred_clase_factor, real_factor)
# 4.2 Ranking según la importancia de las variables ----
print("Importancia de las variables (Árbol):")
print(modelo_arbol$variable.importance)

# 5. Técnicas de aprendizaje no supervisado ----

# 5.1 Clustering ----

# 5.2 Definición de las diferentes tipologías de clientes ----


# 6. Predicción de las ventas totales ----
