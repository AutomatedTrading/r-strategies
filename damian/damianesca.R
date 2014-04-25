library(quantstrat)

#suppressWarnings(try(rm(list=ls()),silent=TRUE))

###
# Poner la ruta donde se encuentran los datos
# path <- "C:/Users/riosp/Google Drive/time_series/simulacion_acciones"
# forex.30min<-read.csv(paste(path,"forex.30min.csv",sep="/"), sep=",")
# forex.30min<-as.xts(zoo(forex.30min[,c(2:5)]),as.POSIXct(forex.30min[,c(1)]))
# names(forex.30min)<-c("Open", "High", "Low", "Close")
# GBPUSD = align.time(forex.30min, 1800)

# cargo el archivo minuto a minuto para poder cambiar la escala (1 min, 5 min, 15 min, 30 min, 1 hora, etc.)
path <- "C:/Users/riosp/Downloads/GBPUSD"
#forex.1M<-read.csv(paste(path,"DAT_GBPUSD_M1_2002_2008.csv",sep="/"), sep=";", header=FALSE)
forex.1M<-read.csv(unz(paste(path,"DAT_GBPUSD_M1_2002_2008.zip",sep="/"),"DAT_GBPUSD_M1_2002_2008.csv"), sep=";", header=FALSE)
forex.1M<-as.xts(zoo(forex.1M[,c(2:5)]),as.POSIXct(forex.1M[,c(1)], "%Y%m%d %H%M%S", tz="UTC"))

#
#
#
currency(c('GBP', 'USD'))

exchange_rate('GBPUSD', tick_size=0.0001)

Sys.setenv(TZ="UTC")

NO_COMPRA <- 0
COMPRA <- 1
VENTA_PISO <- -2
VENTA_TECHO <- 2

applyStrategy <- function(.from, .to, .piso, .techo, .minutes) {
  
  GBPUSD = align.time(to.period(forex.1M, period='minutes', .minutes), .minutes*60)
  names(GBPUSD)<-c("Open", "High", "Low", "Close")
  
  GBPUSD<-GBPUSD[paste0(.from,'::',.to)]
  
  ####
  #
  #Precio promedio
  GBPUSD$Promedio <- (GBPUSD$Open + GBPUSD$High + GBPUSD$Low + GBPUSD$Close) / 4
  
  #Calculo el retorno diario a partir del promedio de valor de Apertura, Cierre, Valor Mayor y Valor Menor para c/30 minutos
  # La función ROC() es del package TTR. The ROC indicator provides the percentage difference
  # of a series over two observations
  GBPUSD$RetornoMedio <- ROC(GBPUSD$Promedio)
  
  # La función Lag() es del package quantmod
  #Precio promedio del periodo inmediato anterior
  GBPUSD$Lag <- Lag(GBPUSD$Promedio)
  
  ##
  ## Como acceder a un objeto xts
  ##
  # > class(GBPUSD$RetornoMedio)
  # [1] "xts" "zoo"
  
  # > class(coredata(GBPUSD$RetornoMedio))
  # [1] "matrix"
  
  # > class(index(GBPUSD$RetornoMedio))
  # [1] "POSIXct" "POSIXt"
  
  # > GBPUSD$RetornoMedio[2,1]
  # RetornoMedio
  # 2002-10-21 00:30:00 0.0004352978
  
  # > GBPUSD$RetornoMedio[[2]] es igual a coredata(GBPUSD$RetornoMedio)[2]
  # [1] 0.0004352978
  
  # > class(GBPUSD$RetornoMedio[[2,1]]) es igual a GBPUSD[[2,"RetornoMedio"]]
  # [1] "numeric"
  
  # > GBPUSD[2,"RetornoMedio"]
  # RetornoMedio
  # 2002-10-21 00:30:00 0.0004352978
  
  # > GBPUSD[[2,"RetornoMedio"]]
  # [1] 0.0004352978
  
  #Media del Retorno Medio
  #media<-mean(GBPUSD$RetornoMedio[-1])
  #lo hago explicito usando na.omit()
  media<-mean(na.omit(GBPUSD$RetornoMedio))
  
  #Desvio estándard del Retorno Medio
  #desvio<-sd(GBPUSD$RetornoMedio[-1])
  desvio<-sd(na.omit(GBPUSD$RetornoMedio))
  
  #Valor Aleatorio a partir de la media y desvio del retorno medio
  
  ## setear semillas si queremos que el algoritmo sea reproducible (ej.: unit testing)
  ## set.seed(123456)
  GBPUSD$rnorm <- rnorm(nrow(GBPUSD),media, desvio)
  
  #Valor "techo"
  GBPUSD$Mayor <- GBPUSD$Lag * (1 + .techo * abs(GBPUSD$rnorm))
  
  #Valor "Piso"
  GBPUSD$Menor <- GBPUSD$Lag * (1 - .piso * abs(GBPUSD$rnorm))
  
  #Señal de salida ("stop loss"), cuyo valor es verdadera si el promedio es menor que el valor "piso"
  GBPUSD$Sig_piso <- GBPUSD$Promedio < GBPUSD$Menor
  
  #Señal de salida ("toma de ganancia"), cuyo valor es verdadero si el promedio es mayor que el valor "techo"
  GBPUSD$Sig_techo <- GBPUSD$Promedio > GBPUSD$Mayor
  
  #Señal de mantener la posicion, cuyo valor es verdadero si el promedio es mayor que el valor "piso" y menor que el valor "techo"
  GBPUSD$Sig_hold_in <- GBPUSD$Promedio <= GBPUSD$Mayor & GBPUSD$Promedio >= GBPUSD$Menor
  
  #Señal de mantenerse fuera, cuyo valor es verdadero si el promedio es menor que el valor "piso" o mayor que el valor "techo"
  GBPUSD$Sig_hold_out <- GBPUSD$Promedio > GBPUSD$Mayor | GBPUSD$Promedio < GBPUSD$Menor
  
  #Señal de "entrada" o compra de la accion, cuyo valor es verdadero si la señal de mantener la posición es verdadera (sólo la 1era)
  #también entro si ahora sig_hold_in == 1 y sig_hold_in anterior es desconocido (NA)
  GBPUSD$Sig_entrada <- (GBPUSD$Sig_hold_in & is.na(Lag(GBPUSD$Sig_hold_in))) | (!Lag(GBPUSD$Sig_hold_in) & GBPUSD$Sig_hold_in)
  
  #también salgo si ahora sig_hold_out == 1 y sig_hold_out anterior es desconocido (NA)
  GBPUSD$Sig_salida <- (GBPUSD$Sig_hold_out & is.na(Lag(GBPUSD$Sig_hold_out))) | (!Lag(GBPUSD$Sig_hold_out) & GBPUSD$Sig_hold_out)
  
  #Poner en "cero" el estado de compra
  estado <- NO_COMPRA
  
  #
  GBPUSD$Posicion_dinero <- 0
  GBPUSD$Posicion_acciones <- 0
  
  #Dinero y acciones en la cuenta al inicio
  GBPUSD$Posicion_dinero[[1]] <- 100000
  GBPUSD$Posicion_acciones[[1]] <- 0
  GBPUSD$Decision <- NO_COMPRA
  costo <- 10
  
  N <- nrow(GBPUSD)
  for (i in 2:N) {
  
    posicion_promedio <- GBPUSD[[i,"Promedio"]]
    posicion_acciones_antes <- GBPUSD[[i-1,"Posicion_acciones"]]
    posicion_dinero_antes <- GBPUSD[[i-1,"Posicion_dinero"]]
  
    #Se genera la compra de acciones
  	if (GBPUSD[[i,"Sig_entrada"]]==1 & estado==NO_COMPRA) {
      GBPUSD[[i,"Posicion_acciones"]] <- posicion_acciones_antes + trunc((posicion_dinero_antes - costo) / posicion_promedio, 0)
      GBPUSD[[i,"Posicion_dinero"]] <- posicion_dinero_antes - costo - GBPUSD[[i,"Posicion_acciones"]] * posicion_promedio
      estado <- COMPRA
      GBPUSD[[i,"Decision"]] <- COMPRA
  	}
  
    #Se genera la venta de acciones
    if (GBPUSD[[i,"Sig_salida"]]==1 & estado==COMPRA) {
      GBPUSD[[i,"Posicion_dinero"]] <- posicion_dinero_antes - costo + posicion_acciones_antes * posicion_promedio
      GBPUSD[[i,"Posicion_acciones"]] <- 0
      if (GBPUSD[[i,"Sig_techo"]]==1)
        {GBPUSD[[i,"Decision"]] <- VENTA_TECHO} 
      else if (GBPUSD[[i,"Sig_piso"]]==1)
        {GBPUSD[[i,"Decision"]] <- VENTA_PISO} 
      estado <- NO_COMPRA
  	}
  
  	if (GBPUSD[[i,"Decision"]] == NO_COMPRA) {
      GBPUSD[[i,"Posicion_acciones"]] <- posicion_acciones_antes
      GBPUSD[[i,"Posicion_dinero"]] <- posicion_dinero_antes
  	}
  }
  
  return(GBPUSD)
}

graficarPosicionDinero <- function(ts, n) {
  # las siguientes 2 sentencias grafica el cash (posicion dinero) sin problemas:
  # plot.xts(time.series[time.series$Posicion_acciones == 0]$Posicion_dinero)
  column_posicion_acciones = "Posicion_acciones"
  column_posicion_dinero = "Posicion_dinero"
  for(i in (0:n-1)) {
    if (i > 0) {
      column_posicion_acciones = paste0("Posicion_acciones.",i)
      column_posicion_dinero = paste0("Posicion_dinero.",i)
    }
    chart.TimeSeries(ts[ts[,column_posicion_acciones] == 0,column_posicion_dinero])
  }
}

.from='2002-10-21'
.to='2008-07-04'
.piso=1
.techo=4
.minutes=30
N_RANDOM_WALKS <- 3

ts <- NULL
results.df <- data.frame()
for(i in (1:N_RANDOM_WALKS)) {
  system.time(
    GBPUSD <- applyStrategy(.from, .to, .piso, .techo, .minutes)
  )
  N <- nrow(GBPUSD)
  ts <- cbind(ts, GBPUSD[,c("Posicion_acciones", "Posicion_dinero", "Decision")])
  posicion_final <- GBPUSD[[N,"Posicion_dinero"]] + GBPUSD[[N,"Posicion_acciones"]] * GBPUSD[[N,"Promedio"]]
  rbind(results.df, c(nrow(GBPUSD[GBPUSD$Decision == COMPRA]),
                      nrow(GBPUSD[GBPUSD$Decision == VENTA_PISO]),
                      nrow(GBPUSD[GBPUSD$Decision == VENTA_TECHO]),
                      posicion_final)) -> results.df
}
names(results.df) <- c("compras","ventas piso","ventas techo","posicion final")
View(results.df)

graficarPosicionDinero(ts, N_RANDOM_WALKS)
charts.PerformanceSummary(GBPUSD$RetornoMedio)

#write.zoo(GBPUSD, file="resultado.csv", na = "", sep=",")
