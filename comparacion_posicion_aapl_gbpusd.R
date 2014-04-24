library(PerformanceAnalytics)
library(quantmod)
library(foreach)

##Modificar la ruta de acuerdo a donde están los dataset de AAPL y GBPUSD
ruta <- "C:/Users/luciano/Google Drive/time_series/simulacion_acciones"
GBPUSD <- read.table(paste0(ruta,"/GBPUSD.csv"), sep=",", header=T)
AAPL <- read.table(paste0(ruta,"/AAPL.csv"), sep=",", header=T)

GBPUSD$Date<-strptime(GBPUSD[,c(1)],format="%Y%m%d")
GBPUSD <- GBPUSD[,1:5]
GBPUSD <- as.xts(zoo(GBPUSD[,c(2:5)]), as.POSIXct(GBPUSD[,c(1)]))


AAPL$Date<-strptime(AAPL[,c(1)],format="%Y%m%d")
AAPL <- AAPL[,1:5]
AAPL <- as.xts(zoo(AAPL[,c(2:5)]), as.POSIXct(AAPL[,c(1)]))


.from <- '2013-04-01'
.to <- '2014-04-17'

#stock <- AAPL[,1:4][paste0(.from,"::",.to)]

symbols <- c("AAPL","GBPUSD")

par(mfrow = c(2,2), oma=c(0,1,0,0))
for (symbol in symbols) {
  stock <- get(symbol)
  stock <- stock[,1:4][paste0(.from,"::",.to)]
  names(stock) <- c("Open", "High", "Low", "Close")
  
  ####
  #
  #Calculo el retorno diario a partir del promedio de valor de Apertura, Cierre, Valor Mayor y Valor Menor para c/30 minutos
  
  stock$RetornoMedio <- ROC((stock$Open + stock$High + stock$Low + stock$Close) / 4 )
  
  #Precio promedio del periodo inmediato anterior
  stock$Lag <- Lag((stock$Open + stock$High + stock$Low + stock$Close) / 4)
  
  #Precio promedio
  stock$Promedio <- (stock$Open + stock$High + stock$Low + stock$Close) / 4
  
  #Media del Retorno Medio
  media<-mean(stock$RetornoMedio[-1])
  
  #Desvio estándar del Retorno Medio
  desvio<-sd(stock$RetornoMedio[-1])
  
  #Valor Aleatorio a partir de la media y desvio del retorno medio
  stock$Rnorm <- rnorm(nrow(stock),media, desvio)
  
  #Valor "techo"
  stock$Mayor <- stock$Lag * (1 + 5 * abs(stock$Rnorm))
  
  #Valor "Piso"
  stock$Menor <- stock$Lag * (1 - 0.01 * abs(stock$Rnorm))
  
  #Señal de salida ("stop loss"), cuyo valor es verdadera si el promedio es menor que el valor "piso"
  stock$Sig_piso <- stock$Promedio < stock$Menor
  
  #Señal de salida ("toma de ganancia"), cuyo valor es verdadero si el promedio es mayor que el valor "techo"
  stock$Sig_techo <- stock$Promedio > stock$Mayor
  
  #Señal de mantener la posicion, cuyo valor es verdadero si el promedio es mayor que el valor "piso" y menor que el valor "techo"
  stock$Sig_hold_in <- stock$Promedio < stock$Mayor & stock$Promedio > stock$Menor
  
  #Señal de mantenerse fuera, cuyo valor es verdadero si el promedio es menor que el valor "piso" o mayor que el valor "techo"
  stock$Sig_hold_out <- stock$Promedio > stock$Mayor | stock$Promedio < stock$Menor
  
  #Señal de "entrada" o compra de la accion, cuyo valor es verdadero si la señal de mantener la posicion es verdadera (sólo la 1era)
  stock$Sig_entrada <- !(Lag(stock$Sig_hold_in)) & stock$Sig_hold_in
  
  stock$Sig_salida <- !(Lag(stock$Sig_hold_out)) & stock$Sig_hold_out
  
  #Poner en "cero" el estado de compra
  estado <- 'no_compra'
  
  #
  stock$Posicion_dinero <- NA
  stock$Posicion_acciones <- NA
  stock$Decision <- NA
  
  #Dinero y acciones en la cuenta al inicio
  stock$Posicion_dinero[1] <- 10000
  stock$Posicion_acciones[1] <- 0
  stock$Decision[1] <- 'no_compra'
  costo <- 10
  
  #for (i in 2:nrow(GBPUSD)) {
  system.time(for (i in 2:nrow(stock)) {
    #Mantener la posicion anterior cuando haya nulos en Señal de entrada
    if (is.na(stock$Sig_entrada[i])) {
      stock$Posicion_dinero[i] <- as.numeric(stock$Posicion_dinero[i-1])
      stock$Posicion_acciones[i] <- as.numeric(stock$Posicion_acciones[i-1])
      estado <- 'no_compra'
      stock$Decision[i] <- 'no_compra'
    }
    
    #Se genera la compra de acciones
    if (stock$Sig_entrada[i]==1 & estado=='no_compra' & !(is.na(stock$Sig_entrada[i]))) {
      stock$Posicion_acciones[i] <- as.numeric(stock$Posicion_acciones[i-1]) + trunc((as.numeric(stock$Posicion_dinero[i-1])- costo) / as.numeric(stock$Promedio[i]) ,0)
      stock$Posicion_dinero[i] <- as.numeric(stock$Posicion_dinero[i-1])- costo - as.numeric(stock$Posicion_acciones[i]) * as.numeric(stock$Promedio[i]) 
      estado <- 'compra'
      stock$Decision[i] <- 'compra'
    }
    
    #Se genera la venta de acciones
    if (stock$Sig_salida[i]==1 & estado=='compra' & !(is.na(stock$Sig_entrada[i]))) {
      stock$Posicion_dinero[i] <- as.numeric(stock$Posicion_dinero[i-1]) - costo + as.numeric(stock$Posicion_acciones[i-1]) * as.numeric(stock$Promedio[i])
      stock$Posicion_acciones[i] <- as.numeric(0) 
      if (estado == 'compra' & stock$Sig_techo[i]==1) {stock$Decision[i] <- 'venta_techo'} 
      else if (estado == 'compra' & stock$Sig_piso[i]==1) {stock$Decision[i] <- 'venta_piso'} 
      #resetear el flag de compra
      estado <- 'no_compra'
    }
    
    if (is.na(stock$Decision[i])) {
      stock$Posicion_acciones[i] <- as.numeric(stock$Posicion_acciones[i-1])
      stock$Posicion_dinero[i] <- as.numeric(stock$Posicion_dinero[i-1])
      stock$Decision[i]<- 'no_compra'
    }
    #cat ( paste0(format(time(GBPUSD[i]), "%Y-%m-%d %H:%M")," Dinero:",stock$Posicion_dinero[i]," Acciones:",stock$Posicion_acciones[i],"\n"))
  })
  
  plot.xts(stock[stock$Posicion_acciones == 0]$Posicion_dinero, main=paste0("Posicion de ",symbol," entre ",.from," y ",.to))
  plot.xts(stock[stock$Decision=='compra']$Posicion_acciones, main=paste0("Posicion de ",symbol," entre ",.from," y ",.to))
  #chart.TimeSeries(GBPUSD[stock$Posicion_acciones == "    0"]$Posicion_dinero)
  #chart.TimeSeries(GBPUSD[stock$Posicion_acciones == 0]$Posicion_dinero, ylim=c(-10000000,10000000))
  
  print(table(stock$Decision))
}