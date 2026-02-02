
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
library(pROC)
library(forecast)

## 1.2 Carga de datos ----

# Hoja: ST Ventas Totales
ventas <- read.xlsx("DataSet SQL_Act3_ADMN.xlsx",
                   sheet = 2) 

# Var Discreta Adq Bicicleta
bicicleta <- read.xlsx("DataSet SQL_Act3_ADMN.xlsx",
                    sheet = 3) # información sobre clientes

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

# 2. Análisis descriptivo ----

## 2.1 Serie temporal de ventas ----

glimpse(ventas)

summary(ventas)

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

## 2.2 Tabla de compra de bicicleta ----
names(bicicleta)

summary(bicicleta)

bicicleta |> 
  group_by(BikePurchase) |> 
  summarise(Frecuencia = n()) |> 
  mutate(Porcentaje = round(Frecuencia / sum(Frecuencia) * 100, 2)) |> 
  as.data.frame()

# Seleccionamos solo las columnas numéricas
bicicleta_num <- bicicleta |> 
  select(where(is.numeric)) |> 
  select(-CustomerID, -PersonID)

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

#ODD Ratios
OR <- exp(coef(modelo_rlog))
print(OR)

prob_pred <- predict(modelo_rlog, newdata = test, type = "response")
clase_pred <- ifelse(prob_pred > 0.5, "Yes", "No")

pred_clase_factor <- factor(clase_pred, levels = c("No", "Yes")) 
real_factor <- factor(test$BikePurchase, levels = c("No", "Yes"))

print("Matriz de Confusión - Regresión Logística:")
confusionMatrix(pred_clase_factor, real_factor)

## 3.2 Árbol de decisión ----
modelo_arbol <- rpart(BikePurchase ~ TotalAmount + Country + Group + Age + MaritalStatus + 
                        YearlyIncome + Gender + TotalChildren + Education + Occupation + 
                        HomeOwnerFlag + NumberCarsOwned, 
                      data = train, method ="class")
summary(modelo_arbol)

rpart.plot(modelo_arbol)


# 4. Evaluación y comparación de modelos ----

# Generación de Predicciones ----

# Predicción con Regresión Logística (Probabilidades y Clases)
pred_logit_prob <- predict(modelo_rlog, newdata = test, type = "response")
pred_logit_class <- factor(ifelse(pred_logit_prob > 0.5, "Yes", "No"), levels = c("No", "Yes"))

# Predicción con Árbol de Decisión (Clases y Probabilidades para ROC)
pred_arbol_class <- predict(modelo_arbol, newdata = test, type = "class")
pred_arbol_prob  <- predict(modelo_arbol, newdata = test, type = "prob")[, "Yes"]


# Evaluación de Rendimiento (Matrices de Confusión)

# Matriz para Logit
mc_logit <- confusionMatrix(pred_logit_class, test$BikePurchase)

# Matriz para Árbol
mc_arbol <- confusionMatrix(pred_arbol_class, test$BikePurchase)

# Imprimir resultados en consola
cat("--- MÉTRICAS MODELO LOGÍSTICO ---\n")
print(mc_logit)

cat("\n--- MÉTRICAS MODELO ÁRBOL DE DECISIÓN ---\n")
print(mc_arbol)


# Comparación de Métricas Globales ----

# Cálculo de objetos ROC y AUC (Requiere librería pROC)
roc_logit <- roc(test$BikePurchase, pred_logit_prob, quiet = TRUE)
roc_arbol <- roc(test$BikePurchase, pred_arbol_prob, quiet = TRUE)

# Creación de tabla comparativa final
comparativa <- data.frame(
  Modelo        = c("Regresión Logística", "Árbol de Decisión"),
  Accuracy      = c(mc_logit$overall["Accuracy"], mc_arbol$overall["Accuracy"]),
  Kappa         = c(mc_logit$overall["Kappa"], mc_arbol$overall["Kappa"]),
  Sensibilidad  = c(mc_logit$byClass["Sensitivity"], mc_arbol$byClass["Sensitivity"]),
  Especificidad = c(mc_logit$byClass["Specificity"], mc_arbol$byClass["Specificity"]),
  AUC           = c(as.numeric(auc(roc_logit)), as.numeric(auc(roc_arbol)))
)

print("TABLA COMPARATIVA DE MODELOS:")
print(comparativa)



# Visualizaciones e Interpretación

# Importancia de Variables

# Importancia Árbol
importancia_arbol <- data.frame(
  Variable    = names(modelo_arbol$variable.importance),
  Importancia = as.numeric(modelo_arbol$variable.importance)
) |> arrange(desc(Importancia))

# Importancia Logit (Basada en Valor absoluto de Z)
importancia_logit <- summary(modelo_rlog)$coefficients |> 
  as.data.frame() |> 
  rownames_to_column(var = "Variable") |> 
  filter(Variable != "(Intercept)") |> 
  mutate(Importancia = abs(`z value`)) |> 
  arrange(desc(Importancia))

# Gráfico Árbol
ggplot(importancia_arbol, aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Importancia de Variables: Árbol de Decisión", x = "Variable", y = "Gini")

# Gráfico Logit
ggplot(importancia_logit, aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Importancia de Variables: Regresión Logística", x = "Variable", y = "Magnitud Z")


# Comparativa Visual de Modelos 

# Gráfico de Accuracy
ggplot(comparativa, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Accuracy, 4)), vjust = -0.5) +
  theme_minimal() + 
  labs(title = "Comparativa de Precisión (Accuracy)") +
  coord_cartesian(ylim = c(0, 1))

# Gráfico Curvas ROC
plot(roc_logit, col = "blue", lwd = 2, main = "Comparativa de Curvas ROC")
plot(roc_arbol, col = "red", lwd = 2, add = TRUE)
legend("bottomright", 
       legend = c(paste("Logit (AUC =", round(auc(roc_logit), 3), ")"),
                  paste("Árbol (AUC =", round(auc(roc_arbol), 3), ")")),
       col = c("blue", "red"), lwd = 2)

# 5. Técnicas de aprendizaje no supervisado ----

# Usaremos el total de la muestra de la hoja Var Discreta Adq Bicicleta (bicicleta)

# Inspeccion inicial de los datos
str(bicicleta)
summary(bicicleta)
table(bicicleta$BikePurchase)

# Existen valores nulos? -> NO
colSums(is.na(bicicleta))

# Selección de las variables numéricas (Excluimos también las columnas de IDs, no aportan nada al análisis de Cluster y no contienen información de comportamiento)
bicicleta_num_noIDs <- bicicleta_num[, -c(2, 3)]
str(bicicleta_num_noIDs)
summary(bicicleta_num_noIDs)

# Inspeccion visual rapida
plot(bicicleta_num_noIDs) 

# Matriz de correlaciones entre las variables numéricas
cor(bicicleta_num_noIDs, use = "complete.obs")

# 5.1 Eleccion del numero de clusteres (k) ----
set.seed(123)   # Dado que el algoritmo es aleatorio, necesitamos fijar semilla para reproducibilidad)

# Metodo general -> k=4
fviz_nbclust(bicicleta_num_noIDs, kmeans)

# Metodo del "codo" usando WSS (Within-cluster sum of squares) -> k= Entre 2 y 3
fviz_nbclust(bicicleta_num_noIDs, kmeans, method = "wss")

# Metodo nbcluster -> k=2
NbClust(bicicleta_num_noIDs,min.nc = 2,max.nc = 8, method = "kmeans")

### EL NUMERO DE CLUSTERS QUE ELEGIMOS ES 2 ###

# 5.2 Clustering ----
# Estandarizamos los valores de las variables (escalas muy distintas)
bicicleta_num_escalado <- scale(bicicleta_num_noIDs)

# Diferencias entre ambos:
summary(bicicleta_num_noIDs)
summary(bicicleta_num_escalado)

# Varianzas de datos estandarizados
apply(bicicleta_num_escalado, 2, var)  # Varianzas = 1 -> ESTANDARIZACIÓN CORRECTA

# K-means con datos escalados
set.seed(123)
km2_escalado <- kmeans(bicicleta_num_escalado, centers = 2, nstart = 20)

# Resumen de K-means
summary(km2_escalado)
km2_escalado$size       # Tamaños de cada cluster
km2_escalado$centers    # Centroides (medias de cada cluster)

# 5.3 Definición de las diferentes tipologías de clientes ----

# Visualización de clústeres en el plano 
fviz_cluster(km2_escalado, data = bicicleta_num_escalado, geom = "point")

# Interpretación de los clusters: Matriz de confusión entre clústeres y Clientes que adquieren bicicletas
table(bicicleta$BikePurchase, km2_escalado$cluster)


# 6. Predicción de las ventas totales ----
ventas <- read_excel("Actividad03_ADMN/DataSet SQL_Act3_ADMN.xlsx", 
                    sheet = "ST Ventas Totales ")
ggplot(ventas, aes(x = OrderDate, y = Sales...2)) +
  geom_line() +
  labs(title = NULL,
       x = NULL,
       y = "Ventas totales") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

# Creamos serie temporal
ts_ventas <- ts(ventas$Sales...2, start(2011,5), frequency = 365)
plot(ts_ventas)

# Creamos modelo Auto ARIMA para predecir las ventas
modelo_arima <- auto.arima(ts_ventas)
summary(modelo_arima)
prediccion_ventas <- forecast(modelo_arima, h = 60)
plot(prediccion_ventas)
