library(tseries)

# Obtiene los precios a los que cotiza Air China en las plazas de Shanghai (ventas en corto ilegales) frente a Honk Kong (ventas en corto legales)
X <- get.hist.quote("0753.HK")
Y <- get.hist.quote("601111.SS")
plot(Y[,4],col="blue",ylim=c(0,30))
lines(X[,4],col="red")

# Como un valor está expresado en yuan renminbi y el otro en HK$ debemos igualar el precio
library(quantmod)
getFX("CNY/HKD", from="2004-12-15")
# Z <- as.data.frame(CNYHKD, row.names = c("date", "CNY.HKD"))
Z <- data.frame(date = index(dailyReturn(CNYHKD)), CNYHKD = CNYHKD, row.names=NULL, stringsAsFactors = FALSE)
Z$date <- as.Date(Z$date)
# Z <- read.table("http://freakonometrics.blog.free.fr/public/data/change-cny-hkd.csv",header=TRUE,sep=";",dec=",")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", lct)
# D <- as.Date(as.character(Z$date),"%d/%m/%y")
D <- as.Date(as.character(Z$date))
z <- as.numeric(Z$CNY.HKD)
plot(D,z,type="l")
X2 <- X[,4]
for(t in 1:length(X2)){
  X2[t]=X2[t]*z[D==time(X2[t])]
  } 
plot(Y[,4],col="blue",ylim=c(0,30))
lines(X2,col="red")

# lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
# xxx <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
# zzz <- as.Date(c("1jan1960", "2jan1960", "31mar1960", "30jul1960"), "%d%b%Y")

# modelo GARCH para comparar la volatilidad de los retornos
RX <- diff(log(X2))
RX <- RX[!is.na(RX)]
RY <- diff(log(Y[,4]))
RY <- window(RY, start=c("2011-04-01"), end=c("2016-03-30"))
Xgarch <-  garch(as.numeric(RX))
SIGMAX <- predict(Xgarch)
Ygarch  <-  garch(as.numeric(RY))
SIGMAY <- predict(Ygarch)
plot(time(RY),SIGMAY[,1],col="blue",type="l")
lines(time(RX),SIGMAX[,1],col="red")

# se usa exponentially-weighted moving averages para reducir la volatilidad del modelo GARCH
moy.ew <- function(x,r){ 
  m=rep(NA,length(x))
  for(i in 1:length(x)){ 
    m[i]=weighted.mean(x[1:i], 
                       rev(r^(0:(i-1))))}
    return(m)} 
sd.ew <- function(x,r,m){
  sd=rep(NA,length(x))
  for(i in 1:length(x)){
    sd[i]=weighted.mean((x[1:i]-m[i])^2,
                        rev(r^(0:(i-1))))}
    return(sd)} 
q <- .97
MX <- moy.ew(RX,q)
SX <- sd.ew(RX,q,MX)
MY <- moy.ew(RY,q)
SY <- sd.ew(RY,q,MY)
plot(time(RY),SY,col="blue",type="l")
lines(time(RX),SX,col="red")

# sombreado azul muestra Shangai (cortos prohibidos) los retornos son más volátiles que en HK.
# sombreado rojo muestra periodos donde es más volatil HK
a <- time(X2)[which(time(X2)%in%time(Y))]
b <- SY[which(time(Y)%in%time(X2))] - SX[which(time(X2)%in%time(Y))]
n <- length(a)
a <- a[-n];b <- b[-n]
plot(a,b,col="black",type="l")
polygon(c(a,rev(a)),c(pmax(b,0),rep(0,length(a))),col="blue",border=NA)
polygon(c(a,rev(a)),c(pmin(b,0),rep(0,length(a))),col="red",border=NA)

# idéntico a lo anterior pero atendiendo a los precios en vez de a la volatilidad de los retornos
a <- time(X2)[which(time(X2)%in%time(Y))]
b <- as.numeric(Y[which(time(Y)%in%time(X2)),4]) -  as.numeric(X2[which(time(X2)%in%time(Y))])
n <- length(a)
a <- a[-n];b <- b[-n]
plot(a,b,col="black",type="l")
polygon(c(a,rev(a)),c(pmax(b,0),rep(0,length(a))),col="blue",border=NA)
polygon(c(a,rev(a)),c(pmin(b,0),rep(0,length(a))),col="red",border=NA)
