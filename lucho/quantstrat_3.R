library(quantstrat)

suppressWarnings(try(rm(list=ls()),silent=TRUE))
options(width=82,continue=" ")

path <- "C:/Users/luciano/Google Drive/time_series/simulacion_acciones"

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
#.to='2002-10-26'
.to='2008-07-04'
#.to='2003-12-31'

GBPUSD<-GBPUSD[paste0(.from,'::',.to)]

###
#
# bautizando la estrategia, el portfolio y la cuenta
#

strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'invertironline'

###
#
#Defino la cantidad que tiene la cuenta al inicio
#Defino el valor para el umbral que usaré en la aplicación de las reglas
#Defino el costo de la transacción (simpre valores negativos)
#

.orderqty = 100000
.threshold = 0.0005
.txnfees = -6

###
#
#Defino las medias corta y larga segun 1er ejemplo del libro
#

.fast = 10
.slow = 30

###
#
# Inicializando el portfolio y la cuenta
#

###
#
# Si quiero volver a correr el algoritmo borro el portfolio y la cuenta
#
rm.strat(portfolio.st)
rm.strat(account.st)


initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

###
#
# Inicializando las ordenes
#

initOrders(portfolio.st, initDate=initDate)

### 
#
# Definiendo la estrategia
#

strategy(strategy.st, store=TRUE)


###
#
# agregando indicadores a la estrategia
#

add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)


###
#
# agregando señales a la estrategia
#
# lt = "less than"; lte="less than or equal"; gte="greater than or equal" ;gt="greater than"
#

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)



###
#
# Agregando reglas a la estrategia
#

#Regla de salir cuando estoy 'long'

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

#Regla de salir cuando estoy 'short'

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

#Regla de entrar en 'long'

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', 
                        prefer='High', 
                        threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

#Regla de entrar en 'short'

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', 
                        prefer='Low', 
                        threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

###
#
# Antes de ver el portfolio podemos queree ver cómo fue la distribución de precio en el periodo a estudiar
#
#chartSeries(GBPUSD, theme='white')
#
#En este caso, si quieren graficar las medias largas y cortas a lo largo de todo el período veran que se solapan mucho con las barras
#
#addSMA(.fast)
#addSMA(.slow)

###############################################################################
#
# Aplicando la estrategia al portfolio
#
applyStrategy(strategy.st, portfolio.st)

View(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD)

###############################################################################

updatePortf(portfolio.st, Symbols='GBPUSD', Dates=paste('::',as.Date(Sys.time()),sep=''))

chart.Posn(portfolio.st, "GBPUSD", TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)")
###############################################################################

View(t(tradeStats(portfolio.st, 'GBPUSD')))

###############################################################################

# Grabar la estrategia en un objeto .RData

#save.strategy(strategy.st)

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################




#Perlitas:
# getPortfolio(nombre_portfolio$symbols$txn)
# getPortfolio(nombre_portfolio$symbols$posPL)

# cumsum(posPL$Net.Trading.PL)

# p = getPortfolio(portfolio.st)

# head(p$symbols$GBPUSD$txn)

# head(p$symbols$GBPUSD$posPL)

# min(p$symbols$GBPUSD$posPL[,c(11)])
# -2386

# time(p$symbols$GBPUSD$posPL[,c(11)][p$symbols$GBPUSD$posPL[,c(11)]==min(p$symbols$GBPUSD$posPL[,c(11)])])
# "2006-04-27 13:30:00 UTC"

# max(p$symbols$GBPUSD$posPL[,c(11)])

# 2570

# chart.TimeSeries(cumsum(p$symbols$GBPUSD$posPL[,c(11)]))

GBPUSD$RetornoMedio <- ROC((GBPUSD$GBPUSD.Open + GBPUSD$GBPUSD.High + GBPUSD$GBPUSD.Low + GBPUSD$GBPUSD.Close) / 4 )
GBPUSD$Lag <- Lag((GBPUSD$GBPUSD.Open + GBPUSD$GBPUSD.High + GBPUSD$GBPUSD.Low + GBPUSD$GBPUSD.Close) / 4)
GBPUSD$Promedio <- (GBPUSD$GBPUSD.Open + GBPUSD$GBPUSD.High + GBPUSD$GBPUSD.Low + GBPUSD$GBPUSD.Close) / 4
media<-mean(GBPUSD$RetornoMedio[-1])
desvio<-sd(GBPUSD$RetornoMedio[-1])

GBPUSD$Rnorm <- rnorm(nrow(GBPUSD),media, desvio)

GBPUSD$Mayor <- GBPUSD$Lag * (1 + 4 * abs(GBPUSD$Rnorm))
GBPUSD$Menor <- GBPUSD$Lag * (1 - 1 * abs(GBPUSD$Rnorm))


GBPUSD$Sig_piso <- GBPUSD$GBPUSD.Close < GBPUSD$Menor
GBPUSD$Sig_techo <- GBPUSD$GBPUSD.Close > GBPUSD$Mayor
GBPUSD$Sig_hold <- GBPUSD$GBPUSD.Close < GBPUSD$Mayor & GBPUSD$GBPUSD.Close > GBPUSD$Menor
GBPUSD$Sig_entrada <- !(Lag(GBPUSD$Sig_hold)) & GBPUSD$Sig_hold

GBPUSD$Sig_piso <- GBPUSD$Promedio < GBPUSD$Menor
GBPUSD$Sig_techo <- GBPUSD$Promedio > GBPUSD$Mayor
GBPUSD$Sig_hold <- GBPUSD$Promedio < GBPUSD$Mayor & GBPUSD$Promedio > GBPUSD$Menor
GBPUSD$Sig_entrada <- !(Lag(GBPUSD$Sig_hold)) & GBPUSD$Sig_hold

estado <- 'no_compra'

GBPUSD$Posicion <- NA
for (i in 3:nrow(GBPUSD)) {
	if (GBPUSD$Sig_entrada[i] & estado == 'no_compra') {
		GBPUSD$Posicion[i] <- GBPUSD$Promedio[i] * (-100000)
		estado <- 'compra'
  		}
	if ((GBPUSD$Sig_techo[i] | GBPUSD$Sig_piso[i]) &  (estado=='compra')) { 
		GBPUSD$Posicion[i] <- GBPUSD$Promedio[i] * 100000
		estado <- 'no_compra'
		}
}

Posicion <- GBPUSD$Posicion
for (i in 1:nrow(Posicion)) {
	if(is.na(Posicion[i])) {Posicion[i] <- 0}
}

sum(Posicion)

par(mfrow=c(2,2))

plot(density(GBPUSD[,c('Medio')][-1]))
abline(v=mean(GBPUSD[,c('Medio')][-1]))
abline(v=mean(GBPUSD[,c('Medio')][-1]+desvio),col="green")
abline(v=mean(GBPUSD[,c('Medio')][-1]-desvio),col="red")


plot(density(GBPUSD[,c('GBPUSD.Close')]))
abline(v=mean(GBPUSD[,c('GBPUSD.Close')]))
abline(v=mean(GBPUSD[,c('GBPUSD.Close')])+sd(GBPUSD[,c('GBPUSD.Close')]), col="green")
abline(v=mean(GBPUSD[,c('GBPUSD.Close')])-sd(GBPUSD[,c('GBPUSD.Close')]), col="red")


plot(density(GBPUSD[,c('GBPUSD.High')]))
abline(v=mean(GBPUSD[,c('GBPUSD.High')]))
abline(v=mean(GBPUSD[,c('GBPUSD.High')])+sd(GBPUSD[,c('GBPUSD.High')]), col="green")
abline(v=mean(GBPUSD[,c('GBPUSD.High')])-sd(GBPUSD[,c('GBPUSD.High')]), col="red")


plot(density(GBPUSD[,c('GBPUSD.Low')]))
abline(v=mean(GBPUSD[,c('GBPUSD.Low')]))
abline(v=mean(GBPUSD[,c('GBPUSD.Low')])+sd(GBPUSD[,c('GBPUSD.Low')]), col="green")
abline(v=mean(GBPUSD[,c('GBPUSD.Low')])-sd(GBPUSD[,c('GBPUSD.Low')]), col="red")

###
acciones_random_walk[i,j] <- acciones_random_walk[i-1,j] * (1 + rnorm(1, mean=medias[1,1], sd=desvios[1,1]))



###Otra prueba: comparar Retorno medio c/30 min vs. Random
GBPUSD$RetornoMedio <- ROC((GBPUSD$GBPUSD.Open + GBPUSD$GBPUSD.High + GBPUSD$GBPUSD.Low + GBPUSD$GBPUSD.Close) / 4 ) 
media<-mean(GBPUSD$RetornoMedio[-1])
desvio<-sd(GBPUSD$RetornoMedio[-1])
GBPUSD$Rnorm <- rnorm(nrow(GBPUSD),media, desvio)

par(mfrow=c(1,2))
plot(density(GBPUSD[,c("Rnorm")]), main="Densidad Retorno Random", xlim=c(-0.006,0.006), ylim=c(0,1200))
text(0.0035, 700, paste0("media:",round(media,7)))
text(0.0035, 625, paste0("desvio:",round(desvio,5)))
text(0.0035, 550, paste0("media + desvio:",round(media+desvio,6)), col = "black")
text(0.0035, 475, paste0("media - desvio:",round(media-desvio,6)), col = "black")
text(0.0035, 400, paste0("media + 2*desvio:",round(media+2*desvio,6)), col = "black")
text(0.0035, 325, paste0("media - 2*desvio:",round(media-2*desvio,6)), col = "black")
abline(v=media+desvio, col="green")
abline(v=media-desvio, col="red")
abline(v=media+2*desvio, col="blue")
abline(v=media-2*desvio, col="orange")

plot(density(GBPUSD[,c("RetornoMedio")][-1]), main="Densidad Retorno", xlim=c(-0.006,0.006), ylim=c(0,1200))
text(0.0035, 700, paste0("media:",round(media,7)))
text(0.0035, 625, paste0("desvio:",round(desvio,5)))
text(0.0035, 550, paste0("media + desvio:",round(media+desvio,6)), col = "black")
text(0.0035, 475, paste0("media - desvio:",round(media-desvio,6)), col = "black")
text(0.0035, 400, paste0("media + 2*desvio:",round(media+2*desvio,6)), col = "black")
text(0.0035, 325, paste0("media - 2*desvio:",round(media-2*desvio,6)), col = "black")
abline(v=media+desvio, col="green")
abline(v=media-desvio, col="red")
abline(v=media+2*desvio, col="blue")
abline(v=media-2*desvio, col="orange")

