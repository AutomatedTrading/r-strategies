library(PerformanceAnalytics)
library(quantmod)
library(foreach)

##Modificar la ruta de acuerdo a donde están los dataset de AAPL y GBPUSD
ruta <- "C:/Users/luciano/Google Drive/time_series/simulacion_acciones"
GBPUSD <- read.table(paste0(ruta,"/GBPUSD.csv"), sep=",", header=T)
AAPL <- read.table(paste0(ruta,"/AAPL.csv"), sep=",", header=T)
GOOGL <- read.table(paste0(ruta,"/GOOGL.csv"), sep=",", header=T)
NSRGY <- read.table(paste0(ruta,"/NSRGY.csv"), sep=",", header=T, stringsAsFactors = FALSE)

GBPUSD$Date<-strptime(GBPUSD[,c(1)],format="%Y%m%d")
GBPUSD <- GBPUSD[,1:5]
GBPUSD <- as.xts(zoo(GBPUSD[,c(2:5)]), as.POSIXct(GBPUSD[,c(1)]))


AAPL$Date<-strptime(AAPL[,c(1)],format="%Y%m%d")
AAPL <- AAPL[,1:5]
AAPL <- as.xts(zoo(AAPL[,c(2:5)]), as.POSIXct(AAPL[,c(1)]))


GOOGL$Date<-strptime(GOOGL[,c(1)],format="%Y%m%d")
GOOGL <- GOOGL[,1:5]
GOOGL <- as.xts(zoo(GOOGL[,c(2:5)]), as.POSIXct(GOOGL[,c(1)]))


NSRGY$Date<-strptime(NSRGY[,c(1)],"%d/%m/%Y")
#NSRGY$Date<-format(NSRGY$Date, format="%Y%m%d")
#NSRGY$Date <- as.Date(NSRGY$Date, "%Y-%m-%d")
NSRGY<-NSRGY[order(as.Date(NSRGY$Date)),]
NSRGY <- NSRGY[,1:5]
NSRGY <- as.xts(zoo(NSRGY[,c(2:5)]), as.POSIXct(NSRGY[,c(1)]))


.from <- '2010-01-01'
.to <- '2014-04-17'

#stock <- AAPL[,1:4][paste0(.from,"::",.to)]

symbols <- c("AAPL","GOOGL","GBPUSD","NSRGY")

par(mfrow = c(2,length(symbols)/2), oma=c(0,1,0,0))
#for (j in 1:3) {
for (symbol in symbols) {
  stock <- get(symbol)
  stock <- stock[,1:4][paste0(.from,"::",.to)]
  names(stock) <- c("Open", "High", "Low", "Close")
  
  ####
  #
  #Calculo el retorno diario a partir del promedio de valor de Apertura, Cierre, Valor Mayor y Valor Menor para c/30 minutos
  # La función ROC() es del package TTR
  stock$RetornoMedio <- ROC((stock$Open + stock$High + stock$Low + stock$Close) / 4 )
  
  # La función Lag() es del package quantmod
  #Precio promedio del periodo inmediato anterior
  stock$Lag <- Lag((stock$Open + stock$High + stock$Low + stock$Close) / 4)
  
  #Precio promedio
  stock$Promedio <- (stock$Open + stock$High + stock$Low + stock$Close) / 4
  
  ##
  ## Como acceder a un objeto xts
  ##
  # > class(stock$RetornoMedio)
  # [1] "xts" "zoo"
  
  # > class(coredata(stock$RetornoMedio))
  # [1] "matrix"
  
  # > class(index(stock$RetornoMedio))
  # [1] "POSIXct" "POSIXt"
  
  # > stock$RetornoMedio[2,1]
  # RetornoMedio
  # 2002-10-21 00:30:00 0.0004352978
  
  # > stock$RetornoMedio[[2]] es igual a coredata(stock$RetornoMedio)[2]
  # [1] 0.0004352978
  
  # > class(stock$RetornoMedio[[2,1]]) es igual a stock[[2,"RetornoMedio"]]
  # [1] "numeric"
  
  # > stock[2,"RetornoMedio"]
  # RetornoMedio
  # 2002-10-21 00:30:00 0.0004352978
  
  # > stock[[2,"RetornoMedio"]]
  # [1] 0.0004352978
  
  #Media del Retorno Medio
  #media<-mean(stock$RetornoMedio[-1])
  #lo hago explicito usando na.omit()
  media<-mean(na.omit(stock$RetornoMedio))
  
  #Desvio estándard del Retorno Medio
  #desvio<-sd(stock$RetornoMedio[-1])
  desvio<-sd(na.omit(stock$RetornoMedio))
  
  #Valor Aleatorio a partir de la media y desvio del retorno medio
  
  ## setear semillas si queremos que el algoritmo sea reproducible (ej.: unit testing)
  set.seed(123456)
  stock$Rnorm <- rnorm(nrow(stock),media, desvio)
  
  #Valor "techo"
  stock$Mayor <- stock$Lag * (1 + 10 * abs(stock$Rnorm))
  
  #Valor "Piso"
  stock$Menor <- stock$Lag * (1 - 0.01 * abs(stock$Rnorm))
  
  #Señal de salida ("stop loss"), cuyo valor es verdadera si el promedio es menor que el valor "piso"
  stock$Sig_piso <- stock$Promedio < stock$Menor
  
  #Señal de salida ("toma de ganancia"), cuyo valor es verdadero si el promedio es mayor que el valor "techo"
  stock$Sig_techo <- stock$Promedio > stock$Mayor
  
  #Señal de mantener la posicion, cuyo valor es verdadero si el promedio es mayor que el valor "piso" y menor que el valor "techo"
  stock$Sig_hold_in <- stock$Promedio <= stock$Mayor & stock$Promedio >= stock$Menor
  
  #Señal de mantenerse fuera, cuyo valor es verdadero si el promedio es menor que el valor "piso" o mayor que el valor "techo"
  stock$Sig_hold_out <- stock$Promedio > stock$Mayor | stock$Promedio < stock$Menor
  
  #Señal de "entrada" o compra de la accion, cuyo valor es verdadero si la señal de mantener la posición es verdadera (sólo la 1era)
  #también entro si ahora sig_hold_in == 1 y sig_hold_in anterior es desconocido (NA)
  stock$Sig_entrada <- (stock$Sig_hold_in & is.na(Lag(stock$Sig_hold_in))) | (!Lag(stock$Sig_hold_in) & stock$Sig_hold_in)
  
  #también salgo si ahora sig_hold_out == 1 y sig_hold_out anterior es desconocido (NA)
  stock$Sig_salida <- (stock$Sig_hold_out & is.na(Lag(stock$Sig_hold_out))) | (!Lag(stock$Sig_hold_out) & stock$Sig_hold_out)
  
  NO_COMPRA <- 0
  COMPRA <- 1
  VENTA_PISO <- -2
  VENTA_TECHO <- 2
  
  #Poner en "cero" el estado de compra
  estado <- NO_COMPRA
  
  #
  stock$Posicion_dinero <- 0
  stock$Posicion_acciones <- 0
  
  #Dinero y acciones en la cuenta al inicio
  stock$Posicion_dinero[[1]] <- 80000
  stock$Posicion_acciones[[1]] <- 0
  stock$Decision <- NO_COMPRA
  costo <- 35
  
  N <- nrow(stock)
  system.time(for (i in 2:N) {
    
    posicion_promedio <- stock[[i,"Promedio"]]
    posicion_acciones_antes <- stock[[i-1,"Posicion_acciones"]]
    posicion_dinero_antes <- stock[[i-1,"Posicion_dinero"]]
    
    #Se genera la compra de acciones
    if (stock[[i,"Sig_entrada"]]==1 & estado==NO_COMPRA) {
      stock[[i,"Posicion_acciones"]] <- posicion_acciones_antes + trunc((posicion_dinero_antes - costo) / posicion_promedio, 0)
      stock[[i,"Posicion_dinero"]] <- posicion_dinero_antes - costo - stock[[i,"Posicion_acciones"]] * posicion_promedio
      estado <- COMPRA
      stock[[i,"Decision"]] <- COMPRA
    }
    
    #Se genera la venta de acciones
    if (stock[[i,"Sig_salida"]]==1 & estado==COMPRA) {
      stock[[i,"Posicion_dinero"]] <- posicion_dinero_antes - costo + posicion_acciones_antes * posicion_promedio
      stock[[i,"Posicion_acciones"]] <- 0
      if (stock[[i,"Sig_techo"]]==1) {stock[[i,"Decision"]] <- VENTA_TECHO} 
      else if (stock[[i,"Sig_piso"]]==1) {stock[[i,"Decision"]] <- VENTA_PISO} 
      #resetear el flag de compra
      estado <- NO_COMPRA
    }
    
    if (stock[[i,"Decision"]] == NO_COMPRA) {
      stock[[i,"Posicion_acciones"]] <- posicion_acciones_antes
      stock[[i,"Posicion_dinero"]] <- posicion_dinero_antes
    }
  }) 
  
  # las siguientes 2 sentencias grafica el cash (posicion dinero) sin problemas:
  plot.xts(stock[stock$Posicion_acciones == 0]$Posicion_dinero, main=paste0("Posicion de ",symbol," entre ",.from," y ",.to))
  #plot.xts(stock[stock$Decision==COMPRA]$Posicion_acciones, main=paste0("Cant.Acciones negociadas de ",symbol," entre ",.from," y ",.to))
  #chart.TimeSeries(stock$Promedio)
  #chart.TimeSeries(GBPUSD[stock$Posicion_acciones == "    0"]$Posicion_dinero)
  #chart.TimeSeries(GBPUSD[stock$Posicion_acciones == 0]$Posicion_dinero, ylim=c(-10000000,10000000))
  
  print(table(stock$Decision))
 

}
#}