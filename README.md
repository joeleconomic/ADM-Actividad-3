# Actividad Final – Adventure Works

## Descripción del proyecto
Nuestra empresa **Adventure Works**, con más de 20 años vendiendo bicicletas a nivel mundial, nos ha encargado un **análisis exploratorio y predictivo de datos de clientes y ventas**.  
El objetivo es aplicar técnicas analíticas modernas frente a la analítica tradicional, para entender el comportamiento de los clientes y proyectar ventas futuras.

**Archivo de datos:** `DataSet SQL Analisis Masivo de Datos.xlsx`  
Contiene cuatro pestañas, de las cuales se utilizaron principalmente:  

- **ST Ventas Totales**: Ventas diarias totales y por zonas geográficas.  
- **Var Discreta Adq Bicicleta**: Información de los clientes:  

| Variable | Tipo | Descripción |
|----------|------|-------------|
| Total Amount | Numérica | Gasto total del cliente |
| Occupation | Categórica | Nivel de ocupación |
| Marital Status | Categórica | Estado civil (Soltero/Casado) |
| Gender | Categórica | Sexo (Hombre/Mujer) |
| Age | Numérica | Edad en años |
| NumberCarOwned | Numérica | Número de coches que posee |
| Education | Categórica | Nivel educativo |
| TotalChildren | Numérica | Número total de hijos |
| Yearly Income | Categórica | Rango de renta anual |
| Bike Purchase | Categórica (0/1) | Compra o no de bicicleta |

---

## Objetivos de la actividad

1. Analizar las variables del dataset:  
   - Tipos de variables.  
   - Estadísticas descriptivas.  
   - Correlaciones entre variables numéricas.  

2. Construir modelos de clasificación para predecir **si un cliente comprará bicicleta**:  
   - **Regresión logística**.  
   - **Árbol de decisión**.  
   - Evaluación de desempeño y variables más importantes.  

3. Aplicar técnicas de **aprendizaje no supervisado** para segmentar clientes:  
   - Identificación de tipologías.  
   - Selección de variables relevantes para clustering.  

4. Realizar **predicción de ventas totales** para los próximos 2 meses usando la serie histórica.  

---

## Rúbrica de evaluación

| Criterio | Excelente (2 ptos) | Adecuado (1 ptos) | Insuficiente (0.5 ptos) | No logrado (0 ptos) |
|----------|-------------------|-----------------|---------------------|----------------|
| 1. Análisis preliminar | Análisis completo e interpretado | Análisis aceptable | Análisis básico | No realiza análisis |
| 2. Modelo de clasificación | Modelo correcto, interpretación correcta | Modelo correcto, interpretación parcial | Modelo con errores | No realiza modelo |
| 3. Clusterización | Modelo calculado y análisis útil | Modelo correcto, análisis parcial | Modelo débil | No realiza clusterización |
| 4. Predicción de ventas | Predicción fundamentada y correcta | Predicción válida con justificación limitada | Predicción con errores | No realiza predicción |


## Uso
Este proyecto está listo para ser abierto en **RStudio**.  
- Contiene análisis descriptivo, modelado de clasificación, clustering y predicción de series temporales.  
- Los scripts pueden ser compartidos y reproducidos por otros compañeros.  

---

## Notas
- Se recomienda revisar la limpieza de datos (`NA` a 0, conversión de texto a números, nombres de variables únicos).  
- Los modelos pueden ajustarse según necesidades de predicción y segmentación futura.
