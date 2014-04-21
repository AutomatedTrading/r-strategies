library(quantstrat)

suppressWarnings(try(rm(list=ls()),silent=TRUE))

###
# Poner la ruta donde se encuentran los datos
path <- "C:/Users/riosp/Google Drive/time_series/simulacion_acciones"

forex.30min<-read.csv(paste(path,"forex.30min.csv",sep="/"), sep=",")

forex.30min<-as.xts(zoo(forex.30min[,c(2:5)]),as.POSIXct(forex.30min[,c(1)]))

names(forex.30min)<-c("GBPUSD.Open", "GBPUSD.High", "GBPUSD.Low", "GBPUSD.Close")

GBPUSD = align.time(forex.30min, 1800)

#
#
#
currency(c('GBP', 'USD'))

exchange_rate('GBPUSD', tick_size=0.0001)

Sys.setenv(TZ="UTC")
#
# Defino el día de comienzo
#
initDate = '2002-10-21'

.from=initDate
#.to='2003-10-26'
.to='2008-07-04'
#.to='2003-12-31'

GBPUSD<-GBPUSD[paste0(.from,'::',.to)]


####
#
#Calculo el retorno diario a partir del promedio de valor de Apertura, Cierre, Valor Mayor y Valor Menor para c/30 minutos
# La función ROC() es del package TTR
GBPUSD$RetornoMedio <- ROC((GBPUSD$GBPUSD.Open + GBPUSD$GBPUSD.High + GBPUSD$GBPUSD.Low + GBPUSD$GBPUSD.Close) / 4 )

# La función Lag() es del package quantmod
#Precio promedio del periodo inmediato anterior
GBPUSD$Lag <- Lag((GBPUSD$GBPUSD.Open + GBPUSD$GBPUSD.High + GBPUSD$GBPUSD.Low + GBPUSD$GBPUSD.Close) / 4)

#Precio promedio
GBPUSD$Promedio <- (GBPUSD$GBPUSD.Open + GBPUSD$GBPUSD.High + GBPUSD$GBPUSD.Low + GBPUSD$GBPUSD.Close) / 4

# > class(GBPUSD$RetornoMedio)
# [1] "xts" "zoo"

# > class(coredata(GBPUSD$RetornoMedio))
# [1] "matrix"

# > class(index(GBPUSD$RetornoMedio))
# [1] "POSIXct" "POSIXt"

# > GBPUSD$RetornoMedio[2,1]
# RetornoMedio
# 2002-10-21 00:30:00 0.0004352978
# > GBPUSD$RetornoMedio[[2]] == coredata(GBPUSD$RetornoMedio)[2]
# [1] 0.0004352978

# > class(GBPUSD$RetornoMedio[[2,1]])
# [1] "numeric"
# > GBPUSD[2,"RetornoMedio"]
# RetornoMedio
# 2002-10-21 00:30:00 0.0004352978


#Media del Retorno Medio
#media<-mean(GBPUSD$RetornoMedio[-1])
#lo hago explicito usando na.omit()
media<-mean(na.omit(GBPUSD$RetornoMedio))

#Desvio estándard del Retorno Medio
#desvio<-sd(GBPUSD$RetornoMedio[-1])
desvio<-sd(na.omit(GBPUSD$RetornoMedio))

#Valor Aleatorio a partir de la media y desvio del retorno medio
GBPUSD$Rnorm <- rnorm(nrow(GBPUSD),media, desvio)

#Valor "techo"
GBPUSD$Mayor <- GBPUSD$Lag * (1 + 4 * abs(GBPUSD$Rnorm))

#Valor "Piso"
GBPUSD$Menor <- GBPUSD$Lag * (1 - 1 * abs(GBPUSD$Rnorm))

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

NO_COMPRA <- 0
COMPRA <- 1
VENTA_PISO <- -2
VENTA_TECHO <- 2

#Poner en "cero" el estado de compra
estado <- NO_COMPRA

#
GBPUSD$Posicion_dinero <- 0
GBPUSD$Posicion_acciones <- 0
GBPUSD$Decision <- NA

#Dinero y acciones en la cuenta al inicio
GBPUSD$Posicion_dinero[[1]] <- 100000
GBPUSD$Posicion_acciones[[1]] <- 0
GBPUSD$Decision <- NO_COMPRA
costo <- 10

system.time(for (i in 2:nrow(GBPUSD)) {
  #Se genera la compra de acciones
	if (GBPUSD$Sig_entrada[[i]]==1 & estado==NO_COMPRA) {
	  GBPUSD$Posicion_acciones[[i]] <- GBPUSD$Posicion_acciones[[i-1]] + trunc((GBPUSD$Posicion_dinero[[i-1]] - costo) / GBPUSD$Promedio[[i]], 0)
	  GBPUSD$Posicion_dinero[[i]] <- GBPUSD$Posicion_dinero[[i-1]] - costo - GBPUSD$Posicion_acciones[[i]] * GBPUSD$Promedio[[i]]
	  estado <- COMPRA
	  GBPUSD$Decision[[i]] <- COMPRA
	}
  
	#Se genera la venta de acciones
	if (GBPUSD$Sig_salida[[i]]==1 & estado==COMPRA) {
	  GBPUSD$Posicion_dinero[[i]] <- GBPUSD$Posicion_dinero[[i-1]] - costo + GBPUSD$Posicion_acciones[[i-1]] * GBPUSD$Promedio[[i]]
	  GBPUSD$Posicion_acciones[[i]] <- 0
	  if (GBPUSD$Sig_techo[[i]]==1) {GBPUSD$Decision[[i]] <- VENTA_TECHO} 
	  else if (GBPUSD$Sig_piso[[i]]==1) {GBPUSD$Decision[[i]] <- VENTA_PISO} 
	  #resetear el flag de compra
	  estado <- NO_COMPRA
	}
	
	if (GBPUSD$Decision[[i]] == NO_COMPRA) {
	  GBPUSD$Posicion_acciones[[i]] <- GBPUSD$Posicion_acciones[[i-1]]
	  GBPUSD$Posicion_dinero[[i]] <- GBPUSD$Posicion_dinero[[i-1]]
	}
})

# las siguientes 2 sentencias grafica el cash (posicion dinero) sin problemas:
plot.xts(GBPUSD[GBPUSD$Posicion_acciones == 0]$Posicion_dinero)
charts.TimeSeries(GBPUSD[GBPUSD$Posicion_acciones == 0]$Posicion_dinero)

write.zoo(GBPUSD, file="resultado.csv", na = "", sep=",")
