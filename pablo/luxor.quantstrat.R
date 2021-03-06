library(quantstrat)
suppressWarnings(try(rm(list=ls()),silent=TRUE))

oldTZ <- Sys.timezone()
Sys.setenv(TZ="UTC")

# path <- "C:/Users/riosp/Google Drive/time_series/simulacion_acciones"
# forex.30min<-read.csv(paste(path,"forex.30min.csv",sep="/"), sep=",")
# forex.30min<-as.xts(zoo(forex.30min[,c(2:5)]),as.POSIXct(forex.30min[,c(1)]))
# names(forex.30min)<-c("GBPUSD.Open", "GBPUSD.High", "GBPUSD.Low", "GBPUSD.Close")
# GBPUSD = to.minutes30(forex.30min)
# GBPUSD = align.time(GBPUSD, 1800)

.minutes <- 30

path <- "C:/Users/riosp/Google Drive/time_series/simulacion_acciones"
forex.1M<-read.csv(unz(paste(path,"DAT_GBPUSD_M1_2002_2008.zip",sep="/"),"DAT_GBPUSD_M1_2002_2008.csv"), sep=";", header=FALSE)
forex.1M<-as.xts(zoo(forex.1M[,c(2:5)]),as.POSIXct(forex.1M[,c(1)], "%Y%m%d %H%M%S", tz="UTC"))
GBPUSD = align.time(to.period(forex.1M, period='minutes', .minutes), .minutes*60)
names(GBPUSD)<-c("Open", "High", "Low", "Close")

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
#.to='2002-10-31' # fecha que usa el demo de quantstrat

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

.orderqty = 30000
.threshold = 0.0005
# Luxor en el libro Trading Systems de Tomasini & Jaekle usa slippage+commission=30
.txnfees = 0

###
#
#Defino las medias corta y larga segun 1er ejemplo del libro
#

# .fast = 10
# .slow = 30

# Segun libro Trading Systems de Tomasini & Jaekle, los parametros .fast = 1, .slow = 44
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
# agregando se?ales a la estrategia
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

myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

chart.Posn(portfolio.st, "GBPUSD", TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)", theme=myTheme)
chart.Posn(portfolio.st, "GBPUSD", Dates='2002-10-29 12:00:00::2002-10-31 0:00:00', TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)")

View(t(tradeStats(portfolio.st, 'GBPUSD')), "Summary")
View(perTradeStats(portfolio.st, 'GBPUSD'))
pf <- getPortfolio(portfolio.st)
View(pf$symbols$GBPUSD$txn)
View(pf$symbols$GBPUSD$posPL)
mk <- mktdata['2002-10-21 15:00::2002-10-31 03:00']
View(mk)

chart.TimeSeries(cumsum(pf$symbols$GBPUSD$posPL$Net.Trading.PL))

# See Also
# perTradeStats for the calculations used by this chart, and
# tradeStats for a summary view of the performance
chart.ME(portfolio.st,'GBPUSD',type='MAE',scale='percent')

###############################################################################

# ob <- getOrderBook(portfolio.st)$forex$GBPUSD
# ob.df <- data.frame(Date=time(ob), ob)
# write.csv(ob.df,"ob.csv")
# PerformanceAnalytics:::textplot(ob.df, show.rownames=F)
# 
# View(t(tradeStats(portfolio.st, 'GBPUSD')))
# write.csv(tradeStats(portfolio.st, 'GBPUSD'),"tradeStats.csv")
# PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st, 'GBPUSD')))
# 
# View(perTradeStats(portfolio.st, 'GBPUSD'))
# write.csv(perTradeStats(portfolio.st, 'GBPUSD'),'perTradeStats.csv')
# 
# mk<-data.frame(index(mktdata), coredata(mktdata))
# write.csv(mk, 'mktdata.csv')
# 
# View(pf$symbols$GBPUSD$txn)
# df<-data.frame(check.names=FALSE, date=time(pf$symbols$GBPUSD$txn), pf$symbols$GBPUSD$txn)
# write.csv(df, 'txn.csv')
# 
# # pf$symbols$GBPUSD$posPL contains same info as pf$symbols$GBPUSD$posPL.USD, in this case
# df<-data.frame(check.names=FALSE, date=time(pf$symbols$GBPUSD$posPL), pf$symbols$GBPUSD$posPL)
# write.csv(df, 'posPL.csv')
# 
# # no hace falta agregar la columna date al data frame
# df<-data.frame(check.names=FALSE, pf$summary)
# write.csv(df, 'summary.csv')
# 
# mk <- mktdata['2002-10-23 15:00::2002-10-24 03:00']
# # no tiene ningun efecto llamar a coredata()
# mk.df <- data.frame(Date=time(mk), coredata(mk))
# PerformanceAnalytics:::textplot(mk.df,show.rownames=F)
# 
# # listar los objetos de los R environments .blotter y .strategy 
# ls(.blotter)
# ls(.strategy)
# 
# # obtener la estrategia
# st<-getStrategy(strategy.st)

Sys.setenv(TZ=oldTZ)
###############################################################################

# Grabar la estrategia en un objeto .RData

#save.strategy(strategy.st)

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################

### 
### 2da parte (optimización en quantstrat)
### 
### Distributions for paramset analysis

.nsamples=80

.FastSMA = (1:20)
.SlowSMA = (30:80)

.FastWFA = c(1, 3, 5, 7, 9)
.SlowWFA = c(42, 44, 46)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)

# it is not add.constraint()
add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA'
)

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,
         initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)

library(parallel)
detectCores()

if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
  registerDoParallel(cores=detectCores())
}

#library ("doRedis")
#registerDoRedis ("jobs")
#startLocalWorkers (n=4, queue = "jobs")
#foreach ( icount (10) ,. combine = sum ,
#            + . multicombine = TRUE ,. inorder = FALSE ) % dopar %
#   + 4* sum (( runif (1000000)^2 + runif (1000000)^2) <1)/10000000
#system.time(foreach(i=1:80, .combine=c) % dopar % sqrt(i))
#removeQueue ("jobs")



system.time(foreach(i=1:80, .combine=c) %dopar% sqrt(i))

if( Sys.info()['sysname'] == "Windows" )
{
  registerDoSEQ()
}

results <- apply.paramset(strategy.st, paramset.label='SMA', portfolio.st=portfolio.st, account.st=account.st, nsamples=.nsamples, verbose=TRUE)

stats <- results$tradeStats
#print(stats)
idx <- order(stats[,1],stats[,2])
stats <- stats[idx,]
View(stats)
View(stats[stats$End.Equity > 0,])
View(t(stats)[,1:10])

# los parámetros .fast y .slow que terminaron con ganancia > 0 (End.Equity) y el menor DrawDown son:
# .fast/.slow = 3/68, 5/61 y 20/32

# net profit
z <- tapply(X=stats[,"End.Equity"],INDEX=list(Fast=stats[,1],Slow=stats[,2]),FUN=sum)
z[1:5,1:10]
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Net Profit")

# idem anterior para las siguientes variables
# "Max.Drawdown"
# "Profit.Factor"
# "Avg.Trade.PL"
# "Profit.To.Max.Draw"

rmdd <- stats$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(stats$nFAST[idx],stats$nSLOW[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")

###
### parte 3) - Stoploss orders
###
### To implement stop-loss or trailing-stop orders, quantstrat utilizes the concept of ordersets and order chains
rm.strat(strategy.st)
strategy(strategy.st, store=TRUE)

# stop loss amount
.stoploss <- 0.30/100

# trading window
.timespan = 'T00:00/T23:59'

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

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long' ,
                        ordertype='stoplimit',
                        prefer='High',
                        threshold=.threshold,
                        TxnFees=0,
                        orderqty=+.orderqty,
                        osFUN=osMaxPos,
                        orderset='ocolong'
         ),
         type='enter',
         timespan = .timespan,
         label='EnterLONG'
)

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoplimit',
                        prefer='Low',
                        threshold=.threshold,
                        TxnFees=0,
                        orderqty=-.orderqty,
                        osFUN=osMaxPos,
                        orderset='ocoshort'
         ),
         type='enter',
         timespan = .timespan,
         label='EnterSHORT'
)

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        replace=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='exit',
         timespan = .timespan,
         label='Exit2SHORT'
)

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=TRUE,
                        orderside='short',
                        ordertype='market',
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='exit',
         timespan = .timespan,
         label='Exit2LONG'
)

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoplimit',
                        tmult=TRUE,
                        threshold=quote(.stoploss),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='StopLossLONG',
         enabled=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short' , sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoplimit',
                        tmult=TRUE,
                        threshold=quote(.stoploss),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='StopLossSHORT',
         enabled=FALSE
)

# rm.strat(portfolio.st)
# rm.strat(account.st)

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
addPosLimit(
   portfolio=portfolio.st,
   symbol='GBPUSD',
   timestamp=initDate,
   maxpos=.orderqty)

initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)

enable.rule('luxor', 'chain', 'StopLoss')

out <- applyStrategy(strategy.st, portfolio.st)

updatePortf(portfolio.st, Symbols='GBPUSD',
            Dates=paste('::',as.Date(Sys.time()),sep=''))

ob <- getOrderBook(portfolio.st)$forex$GBPUSD
View(ob)

myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

chart.Posn(portfolio.st, "GBPUSD", TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)", theme=myTheme)

View(t(tradeStats(portfolio.st, 'GBPUSD')))
View(perTradeStats(portfolio.st, 'GBPUSD'))
pf <- getPortfolio(portfolio.st)
View(pf$symbols$GBPUSD$txn)
View(pf$symbols$GBPUSD$posPL)
mk <- mktdata['2002-10-23 15:00::2002-10-24 03:00']
View(mk)

chart.TimeSeries(cumsum(pf$symbols$GBPUSD$posPL$Net.Trading.PL))

chart.ME(portfolio.st,'GBPUSD',type='MAE',scale='percent')
