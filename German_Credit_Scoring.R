#Define espacio de trabajo
setwd("~/TRABAJO/Scoring")

#Carga los datos
Datos = read.csv("germancredit.csv", header = TRUE, sep = ",")

#Carga los paquetes a usar
library(gmodels)
library(ROCR)
library(MASS)
library(tree)
library(rpart)
library(randomForest)
library(klaR)

###EXPLORACIÓN DE LOS DATOS
attach(Datos) #Carga en memoria las variables de la tabla de datos

#Porcentaje de cada uno de los niveles según variable
p1 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),1),2) * 100)
p2 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),2),2) * 100)
p3 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),3),2) * 100)
p4 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),4),2) * 100)
p5 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),5),2) * 100)
p6 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),6),2) * 100)
p7 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),7),2) * 100)
p8 <- as.matrix(round(margin.table(prop.table(table(Antiguedad_residencia, Aval, Creditos, Numero_creditos, Ocupacion, Hijos, Telefono, Extranjero)),8),2) * 100)

#Función CrossTable del paquete gmodels: tabulación cruzada entre pares de variables
CrossTable(Credibilidad, Saldo, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(Credibilidad, Historial_crediticio, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(Credibilidad, Proposito, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#Algunos gráficos adicionales: histograma de frecuencias y diagrama de caja
#Para la variable Duracion
summary(Duracion)
saltos <- seq(0, 80, 10)
hist(Duracion, breaks=saltos, xlab = "Duracion mensual del crédito", ylab = "Frecuencia", main = " ", cex=0.4, col="lightblue")
boxplot(Duracion, bty="n",xlab = "Duracion mensual del crédito", cex=0.4)
#Para la variable Montante
summary(Montante)
saltos2 <- seq(250, 18500, 250)
hist(Montante, breaks=saltos2, xlab = "Cantidad del cédito", ylab = "Frecuencia", main = " ", cex=0.4,  col="lightblue")
boxplot(Montante, bty="n",xlab = "Cantidad del cédito", cex=0.4)
#Para la variable Edad
summary(Edad)
hist(Edad, breaks=saltos, xlab = "Edad del cliente", ylab = "Frecuencia", main = " ", cex=0.4,  col="lightblue")
boxplot(Edad, bty="n",xlab = "Edad del cliente", cex=0.4)

###PARTICIÓN DE LA MUESTRA
indices <- sample(1:nrow(Datos), size=0.5*nrow(Datos)) 
#Datos de entrenamiento
Entrenamiento50 = Datos[indices,]
#Datos de prueba
Prueba50 <- Datos[-indices,]

###MODELO DE REGRESIÓN LOGÍSTICA
#Usando la función glm (modelo lineal generalizado) del paquete MASS
ModeloLog50 <- glm(Credibilidad ~ Saldo + Historial_crediticio + Proposito + Ahorros + Antiguedad_empleo + Estado_civil +
                     Aval + Vivienda + Creditos + Duracion + Montante + Edad, family=binomial, data = Entrenamiento50)

ModeloLog50final <- glm(Credibilidad ~ Saldo + Historial_crediticio + Proposito + Antiguedad_empleo + Estado_civil, 
                        family=binomial, data = Entrenamiento50)

Solucion50 <- fitted.values(ModeloLog50)
Solucion50 <- round(Solucion50)
#Tabla de rendimiento del modelo
table(Solucion50, Prueba50$Credibilidad)

#Curva ROC
Prueba50$puntuacion <- predict(ModeloLog50,type='response',Prueba50)
pred <- prediction(Prueba50$puntuacion,Prueba50$Credibilidad)
rendimiento <- performance(pred, "tpr", "fpr")
plot(rendimiento)

#Estadístico Kolmogorov-Smirnov
max(attr(rendimiento,'y.values')[[1]]-attr(rendimiento,'x.values')[[1]])

###ANÁLISIS DISCRIMINANTE
#Análisis discriminante linear. Función lda del paquete MASS
SolucionADL <- lda(Credibilidad ~ Ahorros + Antiguedad_empleo + Duracion + Montante + Edad, data = Entrenamiento50)
SolucionADL
plot(SolucionADL)
ADL.pred <- predict(SolucionADL, data=Prueba50)
ADLclass <- ADL.pred$class
table(ADLclass, Prueba50$Credibilidad)

#Análisis discriminante cuadratico. Función qda del paquete MASS
SolucionADC <- qda(Credibilidad ~ Ahorros + Antiguedad_empleo + Duracion + Montante + Edad, data = Entrenamiento50)
SolucionADC
ADC.pred <- predict(SolucionADC, data=Prueba50)
ADCclass <- ADC.pred$class
table(ADCclass, Prueba50$Credibilidad)

####ANÁLISIS BASADO EN ÁRBOLES
#Árbol de regresión. Función tree del paquete tree
ArbolReg50 <- tree(Credibilidad ~ Saldo + Duracion + Historial_crediticio + Proposito + Montante + Ahorros + Antiguedad_empleo
               + Renta_disponible + Estado_civil + Garantia + Antiguedad_residencia + Aval + Edad + Creditos + 
                 Vivienda + Numero_creditos + Ocupacion + Hijos + Telefono, data=Entrenamiento50, 
               method="recursive.partition")
summary(ArbolReg50)
plot(ArbolReg50)
text(ArbolReg50, pretty=0,cex=0.6)

ArbolReg50_pred <- predict(ArbolReg50, Prueba50)
ArbolReg50_pred <- round(ArbolReg50_pred)
table(ArbolReg50_pred, Prueba50$Credibilidad)

#Árbol de clasificación. Función rpart del paquete rpart
ArbolClas50 <- rpart(Credibilidad ~ Saldo + Duracion + Historial_crediticio + Proposito + Montante + Ahorros + Antiguedad_empleo
                    + Renta_disponible + Estado_civil + Garantia + Antiguedad_residencia + Aval + Edad + Creditos + 
                      Vivienda + Numero_creditos + Ocupacion + Hijos + Telefono, data=Entrenamiento50, method="class")
plot(ArbolClas50)
text(ArbolClas50, pretty=0,cex=0.6)

ArbolClas50_pred <- predict(ArbolClas50, Prueba50, type="class")
table(ArbolClas50_pred, Prueba50$Credibilidad)

#Poda del árbol de clasificación. Función prune del paquete rpart
PodaClas50 <- prune(ArbolClas50, cp = 0.8)
PodaClas50_pred <- predict(PodaClas50, Prueba50, type="class")
table(PodaClas50_pred, Prueba50$Credibilidad)

###ANÁLISIS POR MEDIO DE BOSQUES ALEATORIO
#Función randomForest del paquete randomForest
BA50 <- randomForest(Credibilidad ~., data = Entrenamiento50, ntree=200, importance=T, proximity=T, do.trace=100)
plot(BA50, main="")
BA50
BA50_pred <- round(predict(BA50, Prueba50, type="class"))
table(BA50_pred, Prueba50$Credibilidad)
importance(BA50)
varImpPlot(BA50,  main="", cex=0.8)

###WOE
Entrenamiento50$Credibilidad <- as.factor(Entrenamiento50$Credibilidad)
Entrenamiento50$Saldo <- as.factor(Entrenamiento50$Saldo)
Entrenamiento50$Historial_crediticio <- as.factor(Entrenamiento50$Historial_crediticio)
Entrenamiento50$Proposito <- as.factor(Entrenamiento50$Proposito)
Entrenamiento50$Antiguedad_empleo <- as.factor(Entrenamiento50$Antiguedad_empleo)
Entrenamiento50$Estado_civil <- as.factor(Entrenamiento50$Estado_civil)
Entrenamiento50$Garantia <- as.factor(Entrenamiento50$Garantia)

#Función wor del paquete klaR
WOE <- woe(Credibilidad ~., data = Entrenamiento50, zeroadj=0.5, applyontrain = TRUE)
WOE
plot(WOE)
plot(WOE, type = "woes")

traindata <- predict(WOE, Entrenamiento50, replace=T)
str(traindata)

glmodel <- glm(Credibilidad~., traindata, family=binomial)
summary(glmodel)
pred.trn <- predict(glmodel, traindata, type = "response")

validata <- predict(WOE, Entrenamiento50, replace=T)
pred.val <- predict(glmodel, validata, type = "response")
pred.val <- round(pred.val)
table(pred.val, Prueba50$Credibilidad)