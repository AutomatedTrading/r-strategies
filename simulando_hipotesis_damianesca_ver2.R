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
.to='2002-10-26'
#.to='2008-07-04'
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

#Media del Retorno Medio
media<-mean(GBPUSD$RetornoMedio[-1])

#Desvio estándard del Retorno Medio
desvio<-sd(GBPUSD$RetornoMedio[-1])

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
GBPUSD$Sig_hold_in <- GBPUSD$Promedio < GBPUSD$Mayor & GBPUSD$Promedio > GBPUSD$Menor

#Señal de mantenerse fuera, cuyo valor es verdadero si el promedio es menor que el valor "piso" o mayor que el valor "techo"
GBPUSD$Sig_hold_out <- GBPUSD$Promedio > GBPUSD$Mayor | GBPUSD$Promedio < GBPUSD$Menor

#Señal de "entrada" o compra de la accion, cuyo valor es verdadero si la señal de mantener la posición es verdadera (sólo la 1era)
GBPUSD$Sig_entrada <- !(Lag(GBPUSD$Sig_hold_in)) & GBPUSD$Sig_hold_in

GBPUSD$Sig_salida <- !(Lag(GBPUSD$Sig_hold_out)) & GBPUSD$Sig_hold_out

#Poner en "cero" el estado de compra
estado <- 'no_compra'

#
GBPUSD$Posicion_dinero <- NA
GBPUSD$Posicion_acciones <- NA
GBPUSD$Decision <- NA

#Dinero y acciones en la cuenta al inicio
GBPUSD$Posicion_dinero[1] <- 100000
GBPUSD$Posicion_acciones[1] <- 0
GBPUSD$Decision[1] <- 'no_compra'
costo <- 10

#for (i in 2:nrow(GBPUSD)) {
system.time(for (i in 2:nrow(GBPUSD)) {
	#Mantener la posicion anterior cuando haya nulos en Señal de entrada
	if (is.na(GBPUSD$Sig_entrada[i])) {
		GBPUSD$Posicion_dinero[i] <- as.numeric(GBPUSD$Posicion_dinero[i-1])
		GBPUSD$Posicion_acciones[i] <- as.numeric(GBPUSD$Posicion_acciones[i-1])
		estado <- 'no_compra'
		GBPUSD$Decision[i] <- 'no_compra'
		}
	
	#Se genera la compra de acciones
	if (GBPUSD$Sig_entrada[i]==1 & estado=='no_compra' & !(is.na(GBPUSD$Sig_entrada[i]))) {
		GBPUSD$Posicion_acciones[i] <- as.numeric(GBPUSD$Posicion_acciones[i-1]) + trunc((as.numeric(GBPUSD$Posicion_dinero[i-1])- costo) / as.numeric(GBPUSD$Promedio[i]) ,0)
		GBPUSD$Posicion_dinero[i] <- as.numeric(GBPUSD$Posicion_dinero[i-1])- costo - as.numeric(GBPUSD$Posicion_acciones[i]) * as.numeric(GBPUSD$Promedio[i]) 
		estado <- 'compra'
		GBPUSD$Decision[i] <- 'compra'
  		}
	
	#Se genera la venta de acciones
	if (GBPUSD$Sig_salida[i]==1 & estado=='compra' & !(is.na(GBPUSD$Sig_entrada[i]))) {
		GBPUSD$Posicion_dinero[i] <- as.numeric(GBPUSD$Posicion_dinero[i-1]) - costo + as.numeric(GBPUSD$Posicion_acciones[i-1]) * as.numeric(GBPUSD$Promedio[i])
		GBPUSD$Posicion_acciones[i] <- as.numeric(0) 
		if (estado == 'compra' & GBPUSD$Sig_techo[i]==1) {GBPUSD$Decision[i] <- 'venta_techo'} 
		else if (estado == 'compra' & GBPUSD$Sig_piso[i]==1) {GBPUSD$Decision[i] <- 'venta_piso'} 
		#resetear el flag de compra
		estado <- 'no_compra'
		}
	
	if (is.na(GBPUSD$Decision[i])) {
		GBPUSD$Posicion_acciones[i] <- as.numeric(GBPUSD$Posicion_acciones[i-1])
		GBPUSD$Posicion_dinero[i] <- as.numeric(GBPUSD$Posicion_dinero[i-1])
		GBPUSD$Decision[i]<- 'no_compra'
		}
#cat ( paste0(format(time(GBPUSD[i]), "%Y-%m-%d %H:%M")," Dinero:",GBPUSD$Posicion_dinero[i]," Acciones:",GBPUSD$Posicion_acciones[i],"\n"))
})


write.zoo(GBPUSD, file="resultado.csv", na = "", sep=",")
