library(quantstrat)
suppressWarnings(try(rm(list=ls()),silent=TRUE))

oldTZ <- Sys.timezone()
Sys.setenv(TZ="UTC")

.minutes <- 30

### cambiar path ###
path <- "C:/Users/riosp/Google Drive/time_series/simulacion_acciones"
forex.1M<-read.csv(unz(paste(path,"DAT_GBPUSD_M1_2002_2008.zip",sep="/"),"DAT_GBPUSD_M1_2002_2008.csv"), sep=";", header=FALSE)
forex.1M<-as.xts(zoo(forex.1M[,c(2:5)]),as.POSIXct(forex.1M[,c(1)], "%Y%m%d %H%M%S", tz="UTC"))
GBPUSD = align.time(to.period(forex.1M, period='minutes', .minutes), .minutes*60)
names(GBPUSD)<-c("Open", "High", "Low", "Close")

currency(c('GBP', 'USD'))

exchange_rate('GBPUSD', tick_size=0.0001)

Sys.setenv(TZ="UTC")

#
# Defino el día de comienzo
#
initDate = '2002-10-21'
.from=initDate
.to='2008-07-04'

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
# Luxor en el libro Trading Systems de Tomasini & Jaekle usa slippage+commission=30
.txnfees = 0

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

#Regla de salir cuando estoy 'short'

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

#Regla de salir cuando estoy 'long'

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
# Antes de ver el portfolio podemos queree ver c?mo fue la distribuci?n de precio en el periodo a estudiar
#
#chartSeries(GBPUSD, theme='white')
#
#En este caso, si quieren graficar las medias largas y cortas a lo largo de todo el per?odo veran que se solapan mucho con las barras
#
#addSMA(.fast)
#addSMA(.slow)

###############################################################################
#
# Aplicando la estrategia al portfolio
#
applyStrategy(strategy.st, portfolio.st)

View(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD)
# View(getOrderBook(portfolio.st)$forex$GBPUSD)

###############################################################################

updatePortf(portfolio.st, Symbols='GBPUSD', Dates=paste('::',as.Date(Sys.time()),sep=''))

### grafico la serie, los 2 SMAs, las señales, order fill (hay un bug ???), el Profit/Loss acumulado, y el max drawdown
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart.Posn(portfolio.st, "GBPUSD", Dates="2002-12-19::2002-12-20", TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)", theme=myTheme)

View(t(tradeStats(portfolio.st, 'GBPUSD')))

### otras tablas de interes
pf <- getPortfolio(portfolio.st)
View(perTradeStats(portfolio.st, 'GBPUSD'))
# View(pf$symbols$GBPUSD$txn)
# View(pf$symbols$GBPUSD$posPL)
# mk <- mktdata['2002-10-21 15:00::2002-10-31 03:00']
# View(mk)

### grafico el Profit/Loss acumulado
chart.TimeSeries(cumsum(pf$symbols$GBPUSD$posPL$Net.Trading.PL))

Sys.setenv(TZ=oldTZ)
