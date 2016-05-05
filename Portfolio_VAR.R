################################################################################################################
##### Extracci?n de datos y evaluaci?n
################################################################################################################
library(quantmod)

Ibex35 <- c("SAN.MC", "TEF.MC", "BBVA.MC", "ITX.MC", "IBE.MC") # Valores ponderados por capitalizaci?n IBEX35
# SP500 = c("XOM", "MSFT", "BRK-B", "APPL", "WMT") # Valores ponderados por capitalizaci?n S&P500
inicio <- as.Date("2009-03-09") # Final crisis financiera segun chart S&P500 (ofrece 1515 obs)
fin <- as.Date("2014-12-31") # Final de 2014

# Crea objeto xts con el precio de cierre ajustado usando la funci?n getSymbols del paquete quantmod
CierreAj <- NULL
for (Ticker in Ibex35)
  CierreAj = cbind(CierreAj, getSymbols.yahoo(Ticker, from=inicio, to=fin, verbose=F, auto.assign=F)[,6])
colnames(CierreAj) <- Ibex35 # Renombra las columnas del objeto CierreAj
which(is.na(CierreAj)) # Comprueba n?mero de valores ausentes (fila 802 y 803 de la columna TEF.MC)

# Gr?fica de la cotizaci?n de los activos. ITX tiene un split el d?a 28 de Julio de 2014 ?Corregir atipico?
colores <- c("red", "green", "orange", "blue", "black")
plot(x = CierreAj[,"SAN.MC"], xlab = "Tiempo", ylab = "Precio",
  main = "Cotizaci?n", ylim = c(0, 25), major.ticks= F,
  minor.ticks = F, col = "red")
  lines(x = CierreAj[,"TEF.MC"], col = "green")
  lines(x = CierreAj[,"BBVA.MC"], col = "orange")
  lines(x = CierreAj[,"ITX.MC"], col = "blue")
  lines(x = CierreAj[,"IBE.MC"], col = "black")
  legend(x = 'topleft', legend = c("SAN", "TEF", "BBVA", "ITX", "IBE"), lty = 1, col = colores)      
      
### Retornos
# Retornos diarios
# Ret <- diff(log(CierreAj), lag = 1) # lag=1 para diario, lag=5 para semanal etc.
# Ret <- Ret[-1, ]  # Elimina la primera fila
# Retornos usando la funci?n ROC del paquete quantmod
lag <- 1 # Para retornos diarios=1, para semanales=5 etc.
Ret2 <- na.omit(ROC(CierreAj, lag, type = "discrete")) # Omite los valores ausentes (otra opci?n: na.spline)
# Retornos usando la funci?n periodReturn del paquete quantmod (mismo resultado que ROC) 
# Ret3 <- do.call(cbind, lapply(CierreAj, dailyReturn, type="arithmetic")) # Para valores continuos usar type="log"
# Ret3 <- Ret3[-1, ] 

# Grafica de los retornos para comprobar el at?pico en ITX
# plot(as.zoo(Ret2))

### Correlaci?n acepta m?todos pearson(por defecto), kendall y spearman. Conversi?n a porcentaje redondeando
Corr1 <- round(cor(Ret2, method="pearson") * 100)
print(Corr1)

### Genera mapa de calor
library(gplots)

generate_heat_map = function(correlationMatrix, title)
{
  heatmap.2(x = correlationMatrix,    
            cellnote = correlationMatrix, 	
            main = title,			
            symm = T,			
            dendrogram="none",		
            Rowv = F,			
            trace="none",			
            density.info="none",		
            notecol="black")		  
}

generate_heat_map(Corr1, "Correlaciones entre activos")

################################################################################################################
##### Cartera usando el paquete fPortfolio
################################################################################################################
library(fPortfolio)

espcartera <- portfolioSpec() # Especificaciones de la cartera
setRiskFreeRate(espcartera) <- -0.005 # Tasa libre de riesgo negativa a -0.005
setNFrontierPoints(espcartera) <- 20 # N?mero de puntos de la frontera eficiente
# setSolver(espcartera) <- "solveRquadprog" # Por defecto. Para otros valores ver solver-family en doc fPortfolio
# setSolver(espcartera) <- "solveRshortExact" # Solver para constraints = "short"

# Comprobar restricciones
Retornos <- as.timeSeries(Ret2) # Funci?n as.timeSeries del paquete timeSeries para convertir objeto
portfolioConstraints(Retornos, espcartera, constraints = "LongOnly")
portfolioConstraints(Retornos, espcartera, constraints = "Short")

# Crea la frontera eficiente
Frontera <- portfolioFrontier(Retornos, spec=espcartera)
# Frontera <- portfolioFrontier(Retornos, spec=espcartera, constraints = "Short") # Para venta de cortos
Puntos <- frontierPoints(Frontera) # Valores de los puntos de la frontera eficiente
PuntosAnual <- data.frame(Riesgo=Puntos[, "targetRisk"]*sqrt(252), Retorno=Puntos[, "targetReturn"]*252)
tangencyPoints(Frontera) # A?ade puntos tangentes
print(Frontera) # Imprime en consola un resumen del contenido del objeto Frontera

# Grafica la frontera eficiente
tailoredFrontierPlot(object = Frontera)
# Resultados anualizados
plot(PuntosAnual)
# Alternativamente, mediante el uso de men?
# plot(Frontera)
# Grafico de la ponderaci?n de la cartera
# weightsLinePlot(Frontera)
# weightedReturnsLinePlot(Frontera)
# Grafica la ponderaci?n de los 20 puntos de la frontera eficiente
Asignacion <- getWeights(Frontera@portfolio) # getWeights es uan funci?n del paquete fPortfolio
colnames(Asignacion) <- Ibex35
barplot(t(Asignacion), col=rainbow(ncol(Asignacion)+2), legend=colnames(Asignacion)) 
# Gr?fico interactivo de la ponderaci?n de los 20 puntos de la frontera eficiente
# weightsSlider(Frontera)
# Grafica ratio de Sharpe para cada punto de la frontera eficiente
TasaLibre <- -0.005
plot((PuntosAnual[,"Retorno"] - TasaLibre) / PuntosAnual[,"Riesgo"], xlab="cartera", ylab="ratio de Sharpe")

# C?lculo para la cartera eficiente para escenario de s?lo largos para tasa libre de riesgo a -0.005
SeficienteL <- efficientPortfolio(Retornos, spec=espcartera, constraints = "LongOnly")
SeficienteL # Ponderaci?n = 85% de TEF.MC + 14% de IBE.MC + 1% de ITX.MC
# VaR <- 0.0225
# C?lculo para la cartera eficiente para escenario de cortos para tasa libre de riesgo a -0.005
SeficienteC <- efficientPortfolio(Retornos, spec=espcartera, constraints = "Short")
SeficienteC # Ponderaci?n = -6% de SAN.MC +101% de TEF.MC -31% de BBVA.MC +1% de ITX.MC +35% de IBE.MC
# VaR = 0.0215

# Grafica
# portfolio_bar <- barplot(getWeights(StangenteC), main="Ponderaci?n cartera tangente", 
#                        xlab="Cortos permitidos", col="#6699cc")
# text(x= portfolio_bar, y= 0.5, round(getWeights(StangenteC),2)*100, xpd=T)

# C?lculo para la cartera tangente para escenario de s?lo largos para tasa libre de riesgo a -0.005
StangenteL <- tangencyPortfolio(Retornos, spec=espcartera, constraints = "LongOnly")
StangenteL # Ponderaci?n = 70% de TEF.MC + 28% de IBE.MC + 2% de ITX.MC
# VaR <- 0.0225
# C?lculo para la cartera tangente para escenario de cortos para tasa libre de riesgo a -0.005
StangenteC <- tangencyPortfolio(Retornos, spec=espcartera, constraints = "Short")
StangenteC # Ponderaci?n = +10% de SAN.MC +82% de TEF.MC -35% de BBVA.MC +2% de ITX.MC +41% de IBE.MC
# VaR = 0.0219

### VaR
# C?lculo del VaR con alpha por defecto. Para VaR de Basilea alpha = 0.01
pfolioVaR(CierreAj, weights = NULL, alpha = 0.05) # Asumiendo pesos iguales = 6.36
# C?lculo para cada activo individual
pfolioVaR(CierreAj$SAN.MC, weights = NULL, alpha = 0.05) # Para el activo SAN.MC = 3.78
pfolioVaR(CierreAj$TEF.MC, weights = NULL, alpha = 0.05) # Para el activo TEF.MC = 9.48 
pfolioVaR(CierreAj$BBVA.MC, weights = NULL, alpha = 0.05) # Para el activo BBVA.MC = 4.76
pfolioVaR(CierreAj$ITX.MC, weights = NULL, alpha = 0.05) # Para el activo ITX.MC = 4.25
pfolioVaR(CierreAj$IBE.MC, weights = NULL, alpha = 0.05) # Para el activo IBE.MC = 3.11
# C?lculo para las carteras eficientes
# PeficienteL <- c(0.0000, 0.8512,  0.0000,  0.0082,  0.1407)
# pfolioVaR(CierreAj, weights = PeficienteL, alpha = 0.05) # Para la cartera eficiente con s?lo largos = 
# PeficienteC <- c(-0.0561,  1.0130, -0.3139,  0.0078,  0.3491)
# pfolioVaR(CierreAj, weights = PeficienteC, alpha = 0.05) # Para la cartera eficiente con cortos = 
# C?lculo para las carteras tangentes
# PtangenteL <- c(0.0000,  0.7012,  0.0000,  0.0204,  0.2784)
# pfolioVaR(CierreAj, weights = PtangenteL, alpha = 0.05) # Para la cartera tangente con solo largos = 
# PtangenteC <- c(0.1017,  0.8158, -0.3459,  0.0190,  0.4094)
# pfolioVaR(CierreAj, weights = PtangenteL, alpha = 0.05) # Para la cartera tangente con cortos = 

################################################################################################################
##### Cartera usando el paquete tseries
################################################################################################################
# Portafolio usando el paquete tseries y la funci?n portfolio.optim para su optimizaci?n
library(tseries)
res <- portfolio.optim(Ret2, rf=-0.005) # S?lo acepta largos
# res <- portfolio.optim(Ret2, rf=-0.005, shorts=T) # Acepta largos y cortos

# Ponderaci?n de la cartera
portfolio <- res$pw # pw = portfolio weights (ponderaci?n)
prec <- 2
portfolio <- round(portfolio, prec)
print(portfolio) 
# Redondeando: 79% de la cartera a SAN.MC, 12% a ITX.MC y 9% a IBE.MC para largos
# Redondeando: +108% de SAN.MC -41% de TEF.MC -54% de BBVA.MC +9% de ITX.MC +78% de IBE.MC para cortos

# Gr?fico de barras
ylim_max <- max(ceiling(portfolio*10)/10)+0.1
ylim_min <- min(ceiling(portfolio*10)/10)+0.1
portfolio_bar <- barplot(portfolio, names.arg=names(Ret2), ylim=c(ylim_min, ylim_max),col="#6699cc")
text(x=portfolio_bar, y=portfolio+par("cxy"), round(portfolio,4)*100, xpd=T)

# Ratio de Sharpe
Ret_ts <- res$px #vector con los rendimientos generales de la cartera
sharpe(Ret_ts, r = -0.005, scale = sqrt(250)) #Para una tasa de riesgo de -0.005 = 2.4%  (demasiado alto??)

# Gr?ficos adicionales de la ponderaci?n de la cartera
# etiquetas=names(Ret2)
# etiquetas[which(portfolio == 0)] = NA
# pie(portfolio, labels=etiquetas, col = rainbow(5))
# Usando la funci?n pie3D del paquete plotrix
# library(plotrix)
# pie3D(portfolio, main="Portafolio ?ptimo", labels=etiquetas[which(portfolio > 0)]) 

################################################################################################################
##### Reporte
################################################################################################################
# Crea PDF con reporte de los activos usando la funci?n PerformanceSummary del paquete Performance Analytics
# library(PerformanceAnalytics)
# pdf("reporte.pdf")
# charts.PerformanceSummary(Ret2, colorset=rich6equal)
# Insertar otros gr?ficos para que aparezcan en el PDF
# dev.off()