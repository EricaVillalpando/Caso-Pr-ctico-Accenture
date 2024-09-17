################################################################################
##########################  CASO PRÁCTICO-ACCENTURE  ###########################
################################################################################


################################################################################
############################## LIMPIEZA DE DATOS ###############################
################################################################################

# Cargamos las librerías que utilizaremos
if(!require(readxl)) install.packages("readxl")
library(readxl)
if(!require(readxl)) install.packages("dplyr")
library(dplyr)
if(!require(plyr)) install.packages("plyr")
library(plyr)
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if(!require(caret)) install.packages("caret")
library(caret)
if(!require(C50)) install.packages("C50")
library(C50)
if(!require(nnet)) install.packages("nnet")
library(nnet)

# Establecemos el directorio en el que se encuentra el archivo con los datos proporcionados.
setwd("C:\\Users\\erica\\Desktop\\Caso Práctico")

# Lectura de los datos.
datos<-read_xlsx('Data (2).xlsx')
hojas<-excel_sheets('Data (2).xlsx') # Lectura de todas las hojas del archivo de excel.

Charges<-read_excel('Data (2).xlsx', sheet=1) # Primera hoja
Other_data<-read_excel('Data (2).xlsx', sheet=2) # Segunda hoja
Churn<-read_excel('Data (2).xlsx', sheet=3) # Tercera hoja

# Unir las hojas por la columna común "customerID".
datos <- Charges %>%
  full_join(Other_data, by = "customerID") %>%
  full_join(Churn, by = "customerID")

# Visualización de los datos
head(datos)
dim(datos) # Nuestro archivo consta de 7043 registros y 21 variables. (tabla 7043x21).
# View(datos)
str(datos) # En la estructura de nuestros datos hay solamente 4 variables de tipo numérico.
           # Una de estas variables (SeniorCitizen) la modificaremos para ser una variable categórica.
summary(datos)

datos2<-datos
sapply(datos2, function(x) sum(is.na(x)))# Buscamos datos faltantes.
                                         # Las únicas variables con NA's son MonthlyCharges y TotalCharges.
                                         # Tenemos la hipótesis de que existe una correlación entre dichas variables.
                                         # Por el momento se decide eliminar los registros con NA's.
datos2<-datos2[complete.cases(datos2),]
dim(datos2) # Se eliminaron 37 registros

# Cambiamos los valores de 0 y 1 en la Variable SeniorCitizen
datos2$SeniorCitizen <- as.factor(mapvalues(datos2$SeniorCitizen, from = c("0", "1"), to = c("No", "Yes")))

# Creamos una matriz de correlación para verificar nuestra hipótesis.
vnum<-sapply(datos2, is.numeric) # Tomamos solo las variables numéricas de nuestro conjunto de datos.
matriz_corr1<-cor(datos2[, vnum]) # Obtenemos la matriz de correlación.
corrplot(matriz_corr1, method = "number")
corrplot(matriz_corr1, main = "\n\nMatriz de correlación entre variables numéricas", method = "number")

# Para verificar que las correlaciones detectadas sean significativas haremos una prueba de hipótesis.
# Para ello debemos saber la distribución de dichas variables.
# Obtendremos los hisogramas de las 3.
hist(datos2$MonthlyCharges, main = "Histogram of Monthly Charges", xlab = "Monthly Charges ($)", col="lightblue", breaks = 50)
hist(datos2$TotalCharges, main = "Histogram of Total Charges", xlab = "Total Charges ($)", col="lightblue", breaks = 50)
hist(datos2$tenure, main = "Histogram of Tenure", xlab = "Tenure (months)", col="lightblue", breaks = 50)
# Dado que ninguna de las variables parece presentar una distribución normal, aplicaremos la prueba de correlación de Spearman.
# Podríamos aplicar pruebas de normalidad, pero en este caso no es necesario.

cor1<-cor.test(datos2$MonthlyCharges,datos2$TotalCharges, method = "spearman")
cor1 # La correlación entre MonthlyCharges y TotalCharges es estadísticamente significativa.
cor2<-cor.test(datos2$tenure,datos2$TotalCharges, method = "spearman")
cor2 # La correlación entre tenure y TotalCharges es estadísticamente significativa.

# Dados los resultados anteriores, se decide eliminar la variable TotalCharges del análisis.
# Tomaremos la matriz de datos original (datos) y solamente eliminaremos los valores faltantes para los 7 registros de MonthlyCharges.

datos$TotalCharges <- NULL
datos$customerID <- NULL # También eliminamos la variable CustomerID por no ser relevante para el análisis.
datos<-datos[complete.cases(datos),]
dim(datos) # Se eliminaron solo 7 registros; La base de datos con la que trabajaremos cuenta con 7036, 18 variables predictoras y una variable dependiente.

# Haremos dos gráficas de caja para verificar que no hay presencia de outliers en nuestras variables numéricas.
boxplot(datos$tenure)
boxplot(datos$MonthlyCharges)

# Finalmente, recodificamos las categorías que indican "No internet service" y "No phone service" a "No" para facilitar el análisis.
datos<-data.frame(lapply(datos,function(x){
  gsub("No internet service","No",x)
}))

datos<-data.frame(lapply(datos,function(x){
  gsub("No phone service","No",x)
}))

################################################################################
############################# ANÁLISIS EXPLORATORIO ############################
################################################################################

#Transformamos nuestras variables categóricas.
datos$gender<-as.factor(datos$gender)
datos$SeniorCitizen <- as.factor(mapvalues(datos$SeniorCitizen, from = c("0", "1"), to = c("No", "Yes")))
datos$Partner<-as.factor(datos$Partner)
datos$Dependents<-as.factor(datos$Dependents)
datos$PhoneService<-as.factor(datos$PhoneService)
datos$MultipleLines<-as.factor(datos$MultipleLines)
datos$InternetService<-as.factor(datos$InternetService)
datos$OnlineSecurity<-as.factor(datos$OnlineSecurity)
datos$OnlineBackup<-as.factor(datos$OnlineBackup)
datos$DeviceProtection<-as.factor(datos$DeviceProtection)
datos$TechSupport<-as.factor(datos$TechSupport)
datos$StreamingTV<-as.factor(datos$StreamingTV)
datos$StreamingMovies<-as.factor(datos$StreamingMovies)
datos$Contract<-as.factor(datos$Contract)
datos$PaperlessBilling<-as.factor(datos$PaperlessBilling)
datos$PaymentMethod<-as.factor(datos$PaymentMethod)
datos$Churn<-as.factor(datos$Churn)
datos$tenure<-as.numeric(datos$tenure)
datos$MonthlyCharges<-as.numeric(datos$MonthlyCharges)
str(datos) # Revisamos la estructura de nuestros datos

prop.table(table(datos$Churn)) # Revisamos la proporción en la variable respuesta.
                               # Vemos que está balanceada (50-50 aproximadamente)
                        
# Crearemos una tabla en la que se pueda apreciar la relación entre nuestras variables numéricas y los clientes.
resumen <- aggregate(cbind(MonthlyCharges, tenure) ~ gender, data = datos, FUN = function(x) c(Count = length(x), Mean = mean(x)))
resumen <- do.call(data.frame, resumen)
colnames(resumen) <- c("Gender", "Clients", "Average monthly charges", "tenure_count", "Average tenure")
resumen <- resumen[, -c(4)] # Eliminamos la columna "tenure_count".
resumen # No se observan diferencias significativas en el monto promedio cobrado por mes para hombres y mujeres.
# El número promedio de meses que los clientes se han quedado en la compañía es 32, independientemente del género.

# Observaremos la existencia de relaciones entre la variable respuesta y las predictoras mediante gráficas.

# Número de clientes con y sin default.
ggplot(data=datos, mapping = aes(x=datos$Churn, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red"))+
  labs(title = "Churn", y="Number of clients", x="Churn", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = -0.5)

# Histograma de Tenure separado por clientes con y sin default.
g1<-ggplot(data=datos, mapping = aes(x=datos$tenure, fill=datos$Churn))+
  geom_histogram(position ="dodge")+
  labs(title = "Tenure Histogram", y="Number of clients", x="Tenure (months)", fill="Churn")

# Histograma de Monthly charges separado por clientes con y sin default.
g2<-ggplot(data=datos, mapping = aes(x=datos$MonthlyCharges, fill=datos$Churn))+
  geom_histogram(position ="dodge")+
  labs(title = "Monthly Charges Histogram", y="Number of clients", x="Monthly Charges ($)", fill="Churn")

# Boxplot de Tenure separado por clientes con y sin default.
g3<-ggplot(data=datos, mapping = aes(x=datos$tenure, fill=datos$Churn))+
  geom_boxplot()+
  labs(x="Tenure (months)", fill="Churn")+
  scale_y_continuous(breaks = NULL)

# Boxplot de Monthly Charges separado por clientes con y sin default.
g4<-ggplot(data=datos, mapping = aes(x=datos$MonthlyCharges, fill=datos$Churn))+
  geom_boxplot()+
  labs(x="Monthly Charges ($)", fill="Churn")+
  scale_y_continuous(breaks = NULL)

# Visualizamos las 4 gráficas anteriores juntas.
grid.arrange(g3,g1,g4,g2, nrow = 2, ncol = 2) 
# Clientes que han estado pocos meses en la compañía incumplen con más frecuencia.
# En general, cuando los clientes han estado en la compañía por más de 18 meses la frecuencia de incumpliento es menor en comparación con la de cumpliento.
# Se puede apreciar que a medida que los cargos mensuales aumentan, es mayor la frecuencia de incumplimiento con respecto a la de cumplimiento.

# Analizaremos ahora las variables categóricas en relación con la variable respuesta.

# Gender
g5<-ggplot(data=datos, mapping = aes(x=datos$gender, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Gender", y="Number of clients", x="Gender", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Senior Citizen
g6<-ggplot(data=datos, mapping = aes(x=datos$SeniorCitizen, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Senior Citizen", y="Number of clients", x="Senior Citizen", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Partner
g7<-ggplot(data=datos, mapping = aes(x=datos$Partner, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Partner", y="Number of clients", x="Partner", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Dependents
g8<-ggplot(data=datos, mapping = aes(x=datos$Dependents, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Dependents", y="Number of clients", x="Dependents", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Phone Service
g9<-ggplot(data=datos, mapping = aes(x=datos$PhoneService, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Phone Service", y="Number of clients", x="Phone Service", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 1.8)

# Multiple lines
g10<-ggplot(data=datos, mapping = aes(x=datos$MultipleLines, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Multiple lines", y="Number of clients", x="Multiple lines", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Internet Service
g11<-ggplot(data=datos, mapping = aes(x=datos$InternetService, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Internet Service", y="Number of clients", x="Internet Service", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Online Security
g12<-ggplot(data=datos, mapping = aes(x=datos$OnlineSecurity, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Online Security", y="Number of clients", x="Online Security", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Online Backup
g13<-ggplot(data=datos, mapping = aes(x=datos$OnlineBackup, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Online Backup", y="Number of clients", x="Online Backup", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Device Protection
g14<-ggplot(data=datos, mapping = aes(x=datos$DeviceProtection, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Device Protection", y="Number of clients", x="Device Protection", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Tech Support
g15<-ggplot(data=datos, mapping = aes(x=datos$TechSupport, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Tech Support", y="Number of clients", x="Tech Support", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Streaming TV
g16<-ggplot(data=datos, mapping = aes(x=datos$StreamingTV, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Streaming TV", y="Number of clients", x="Streaming TV", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Streaming Movies
g17<-ggplot(data=datos, mapping = aes(x=datos$StreamingMovies, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Streaming Movies", y="Number of clients", x="Streaming Movies", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Contract
g18<-ggplot(data=datos, mapping = aes(x=datos$Contract, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Contract", y="Number of clients", x="Contract", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Paperless Billing
g19<-ggplot(data=datos, mapping = aes(x=datos$PaperlessBilling, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Paperless Billing", y="Number of clients", x="Paperless Billing", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Payment Method
g20<-ggplot(data=datos, mapping = aes(x=datos$PaymentMethod, fill=datos$Churn))+
  geom_bar(position ="dodge")+
  labs(title = "Payment Method", y="Number of clients", x="Payment Method", fill="Churn")+
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = 2)

# Agruparemos las gráficas anteriores para su mejor visualización (en 4 gráficas).

grid.arrange(g5,g6,g7,g8, nrow = 2, ncol = 2) # Gender, Senior Citizen, Partner y Dependents.
# Mujeres y hombres incumplen y cumplen en aproximadamente la misma proporción.
# El género no parece ser una variable significativa en el análisis.
# Las personas mayores incumplen con mayor frecuencia, sin embargo constituyen solo el 16% de los clientes de la compañía.
# Clientes sin dependientas (más del 50%) incumplen con mayor frecuencia, sin embargo esta variable no parece ser significativa.
# Clientes sin pareja incumplen con una ligera mayor frecuencia.

grid.arrange(g9,g10,g11,g12, nrow = 2, ncol = 2) # Phone Service, Multiple lines, Internet Service y Online security.
# Más del 60% de los clientes con Fibra Óptica incumplen. representan el 27% de los clientes totales.
# Clientes que sí cuentan con seguridad online cumplen con mayor frecuencia, sin embargo solo representan el 28% de los clientes totales.

grid.arrange(g13,g14,g15,g16, nrow = 2, ncol = 2) # Online Backup, Device Protection, Tech Support, Streaming TV.
# Clientes sin online security, online backup y tech support (la mayoría) incumplen con una ligera mayor frecuencia.

grid.arrange(g17,g18,g19,g20, nrow = 2, ncol = 2) # Streaming Movies, Contract, Paperless Billing, Paymeny Method.
# Clientes con month to month contracts incumplen con mayor frecuencia, al igual que aquellos que pagan mediante cheque electrónico.


################################################################################
####################### PARTICIÓN DE DATOS Y RESAMPLING ########################
################################################################################

set.seed(8) # Establemdemos un valor semilla para que nuestros resultados sean reproducibles.
datos<-as.data.frame(datos)

# Dividimos nuestros datos en conjuntos de training y testing para nuestros predictores y variable respuesta.
trainrows = createDataPartition(datos$Churn, p=0.7,list=FALSE) # Seleccionamos un 70% de los datos para training y 30% para testing.
Xtrain = datos[trainrows,-19] 
Ytrain = datos[trainrows, 19]
Xtest = datos[-trainrows, -19]
Ytest = datos[-trainrows, 19]

# Validamos la proporción de la variable dependiente en el set de prueba y entrenamiento.
prop.table(table(Ytrain))
prop.table(table(Ytest))

# Utilizaremos validación cruzada en 7 capas con 3 repeticiones para afinar y comparar diferentes modelos. 
# Crearemos dichas capas y repeticiones inicialmente para usar los mismos  subconjuntos con cada modelo.
# Con este método, entrenamos el modelo con los datos de entrenamiento y lo utilizamos para predecir las respuestas en el conjunto de prueba.
set.seed(8)
cvSplits = createMultiFolds(Ytrain, k=7, times=3)
str(cvSplits) # Verificamos la estructura.

# Definimos nuestro modelo de entrenamiento. 
ctrl = trainControl(method="repeatedcv", index=cvSplits, 
                    summaryFunction = twoClassSummary,
                    classProbs = TRUE, sampling="down") 

################################################################################
###################### AJUSTE DE MODELOS DE CLASIFICACIÓN ######################
################################################################################

############################# Regresión logística ##############################
set.seed(8)
# Defefinimos nuestro modelo y lo entrenamos.
logisticReg = train(x=Xtrain, y=Ytrain, 
                    method="glm",metric = "ROC", 
                    preProc=c("center","scale"),
                    trControl = ctrl)
# La métrica de evaluación que se usará para seleccionar el mejor modelo es el área bajo la curva ROC.
# Esta métrica es útil para problemas de clasificación binaria.

# Vemos los resultados del modelo.
logisticReg
logisticReg$results # El área bajo la curva es 0.672, lo que indica que el modelo tiene una capacidad moderada para discriminar entre clases.
                    # La sensibilidad del modelo es 0.64, lo que indica que clasifica correctamente el 64% de los clientes que incumplen.
                    # La especificidad del modelo es 0.62, lo que indica que clasifica correctamente al 62% de los clientes que no incumplen.
summary(logisticReg) # Vemos los coeficientes del modelo final
                     # Las variables significativas resultan ser Tenure, Contract, Payment method y Paperless Billing.
p1<-predict(logisticReg,Xtest) # Hacemos las clasificaciones con el modelo resultante utilizando los datos de prueba.
comparacion <- sum(p1 == Ytest) 
comparacion # Nuestro modelo clasifica correctamente a 1343 de un total de 2110 datos en el set de prueba (63.6%).


#################################### C5 ########################################
set.seed(8)

# Primero creamos una matriz que contiene todas las combinaciones posibles de parámetros que definimos para entrenar el modelo.
# Estos son trials (iteraciones del algoritmo), modelo (basado en reglas o árboles de decisión) y winow (False or True).
# Winnowing es una técnica de selección de características en el algoritmo C5.0 que ayuda a mejorar el rendimiento del modelo 
# al eliminar atributos menos relevantes.
c50Grid = expand.grid(trials=c(2,5,8,10), model=c("rules","tree"),
                      winnow=c(FALSE,TRUE))

# Entrenamos nuestro modelo de clasificación.
C5 <- caret::train(Xtrain, 
                   y=Ytrain,
                   tuneGrid = c50Grid, verbose=FALSE,
                   method="C5.0", metric = "ROC", 
                   trControl=ctrl)

plot(C5) # Vemos de forma gráfica la relación entre los parámetros de cada modelo y la métrica ROC en cada uno de ellos.
C5$bestTune # El mejor modelo (maximiza nuestra métrica de comparación).
            # Es un modelo basado en reglas que se entrena utilizando 10 iteraciones y aplica la técnica de winnowing para la selección de características.
            # El área bajo la curva es 0.668, lo que indica que el modelo tiene una capacidad moderada para discriminar entre clases. 
            # El valor anterior es menor al obtenido utilizando regresión logística.
            # La sensibilidad del modelo es 0.66, lo que indica que clasifica correctamente el 66% de los clientes que incumplen.
            # La especificidad del modelo es 0.6, lo que indica que clasifica correctamente al 60% de los clientes que no incumplen.
C5 # Visualizamos los resultados de todos los modelos.
p2<-predict(C5,Xtest) # Hacemos las predicciones con el modelo resultante utilizando los datos de prueba.
comparacion2 <- sum(p2 == Ytest) 
comparacion2 # Nuestro modelo clasifica correctamente a 1329 de un total de 2110 datos en el set de prueba (63%).


############################## REDES NEURONALES ################################
set.seed(8)

# Primero creamos una matriz con todas las combinaciones de size y decay que se utilizarán para la búsqueda de hiperparámetros.
nnetGrid <- expand.grid(size = 1:8, decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$size)

# Entrenamos nuestro modelo.
nnetGrid$bag = FALSE  
nnetFit2 <- caret::train(x = Xtrain, 
                         y = Ytrain,
                         method = "avNNet",
                         metric = "ROC",
                         preProc = c("center", "scale"),
                         tuneGrid = nnetGrid,
                         trace = FALSE,
                         maxit = 100, 
                         MaxNWts = 
                           5*(maxSize * (length(Ytrain) + 1) 
                              + maxSize + 1),
                         trControl = ctrl)
# Configuramos una búsqueda de hiperparámetros para una red neuronal (avNNet) utilizando un grid de parámetros creado con expand.grid. 
# Ajustamos la red neuronal en los datos de entrenamiento Xtrain y Ytrain.
# El objetivo es encontrar la mejor combinación de parámetros para maximizar el rendimiento del modelo basado en la métrica ROC.
# En lugar de utilizar una única red neuronal, avNNet combina las predicciones de varias redes neuronales entrenadas en diferentes 
# subconjuntos de datos. Esto promedia las predicciones para obtener una estimación más estable y precisa.

saveRDS ( nnetFit2, file = "nnetFit2_p.rds")
nnetFit2 = readRDS("nnetFit2.rds") 
plot(nnetFit2) # Vemos de forma gráfica la relación entre los parámetros de cada modelo y la métrica ROC en cada uno de ellos. 
nnetFit2$bestTune # El mejor modelo (maximiza nuestra métrica de comparación).
                  # Podemos ver que el mejor modelo según el criterio de la curva ROC es el que incluye 7 unidades ocultas con una tasa de decaimiento de pesos igual a 2.
                  # El área bajo la curva es 0.675, lo que indica que el modelo tiene una capacidad moderada para discriminar entre clases. 
                  # El valor anterior es mayor al obtenido utilizando regresión logística, por lo que este es nuestro mejor modelo.
                  # La sensibilidad del modelo es 0.7, lo que indica que clasifica correctamente el 70% de los clientes que incumplen.
                  # La especificidad del modelo es 0.566, lo que indica que clasifica correctamente al 57% de los clientes que no incumplen.

nnetFit2 # Visualizamos los resultados de todos los modelos.
p3<-predict(nnetFit2,Xtest)  # Hacemos las predicciones con el modelo resultante utilizando los datos de prueba.
comparacion3 <- sum(p3 == Ytest)
comparacion3 # Nuestro modelo clasifica correctamente a 1350 de un total de 2110 datos en el set de prueba (64%).


############################ COMPARACIÓN DE MODELOS ############################

modelos = list( NNAv = nnetFit2, logistic = logisticReg, C.5 = C5) # Hacemos una lista con nuestros modelos finales.
resamp = resamples(modelos)
summary(resamp) # Vemos una comparación entre las métricas evaluadas en cada modelo (ROC,sensibilidad y especificidad).
dotplot(resamp) # Gráfica compartiva entre métricas en cada modelo.
