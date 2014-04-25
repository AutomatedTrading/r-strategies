library(quantstrat)

suppressWarnings(try(rm(list=ls()),silent=TRUE))
options(width=82,continue=" ")

path <- "C:/Users/riosp/Google Drive/time_series/simulacion_acciones"

forex.30min<-read.csv(paste(path,"forex.30min.csv",sep="/"), sep=",")

oldTZ <- Sys.timezone()
Sys.setenv(TZ="UTC")

forex.30min<-as.xts(zoo(forex.30min[,c(2:5)]),as.POSIXct(forex.30min[,c(1)]))

names(forex.30min)<-c("GBPUSD.Open", "GBPUSD.High", "GBPUSD.Low", "GBPUSD.Close")

GBPUSD = to.minutes30(GBPUSD)
GBPUSD = align.time(forex.30min, 1800)

#
#
#
currency(c('GBP', 'USD'))

exchange_rate('GBPUSD', tick_size=0.0001)

Sys.setenv(TZ="UTC")
#
# Defino el d?a de comienzo
#
initDate = '2002-10-21'

.from=initDate
#.to='2002-10-26'
#.to='2008-07-04'
.to='2002-10-31' # fecha que usa el demo de quantstrat

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
#Defino el valor para el umbral que usar? en la aplicaci?n de las reglas
#Defino el costo de la transacci?n (simpre valores negativos)
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

###############################################################################

updatePortf(portfolio.st, Symbols='GBPUSD', Dates=paste('::',as.Date(Sys.time()),sep=''))

myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

chart.Posn(portfolio.st, "GBPUSD", TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)", theme=myTheme)
#chart.Posn(portfolio.st, "GBPUSD", Dates='2002-11-08::2002-11-11', TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)")

# See Also
# perTradeStats for the calculations used by this chart, and
# tradeStats for a summary view of the performance
chart.ME(portfolio.st,'GBPUSD',type='MAE',scale='percent')

###############################################################################

ob <- getOrderBook(portfolio.st)$forex$GBPUSD
ob.df <- data.frame(Date=time(ob), ob)
write.csv(ob.df,"ob.csv")
PerformanceAnalytics:::textplot(ob.df, show.rownames=F)

View(t(tradeStats(portfolio.st, 'GBPUSD')))
write.csv(tradeStats(portfolio.st, 'GBPUSD'),"tradeStats.csv")
PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st, 'GBPUSD')))

View(perTradeStats(portfolio.st, 'GBPUSD'))
write.csv(perTradeStats(portfolio.st, 'GBPUSD'),'perTradeStats.csv')

mk<-data.frame(index(mktdata), coredata(mktdata))
write.csv(mk, 'mktdata.csv')

View(pf$symbols$GBPUSD$txn)
df<-data.frame(check.names=FALSE, date=time(pf$symbols$GBPUSD$txn), pf$symbols$GBPUSD$txn)
write.csv(df, 'txn.csv')

# pf$symbols$GBPUSD$posPL contains same info as pf$symbols$GBPUSD$posPL.USD, in this case
df<-data.frame(check.names=FALSE, date=time(pf$symbols$GBPUSD$posPL), pf$symbols$GBPUSD$posPL)
write.csv(df, 'posPL.csv')

# no hace falta agregar la columna date al data frame
df<-data.frame(check.names=FALSE, pf$summary)
write.csv(df, 'summary.csv')

mk <- mktdata['2002-10-23 15:00::2002-10-24 03:00']
# no tiene ningun efecto llamar a coredata()
mk.df <- data.frame(Date=time(mk), coredata(mk))
PerformanceAnalytics:::textplot(mk.df,show.rownames=F)

# listar los objetos de los R environments .blotter y .strategy 
ls(.blotter)
ls(.strategy)

# obtener la estrategia
st<-getStrategy(strategy.st)

Sys.setenv(TZ=oldTZ)
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

# 
# 2da parte
# 
### Distributions for paramset analysis

.nsamples=80

.FastSMA = (1:20)
.SlowSMA = (30:80)

.StopLoss = seq(0.05, 2.4, length.out=48)/100
.StopTrailing = seq(0.05, 2.4, length.out=48)/100
.TakeProfit = seq(0.1, 4.8, length.out=48)/100

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

foreach(i=1:8, .combine=c) %dopar% sqrt(i)

if( Sys.info()['sysname'] == "Windows" )
{
  registerDoSEQ()
}

if( Sys.info()['sysname'] == "Windows" )
{
  if(file.exists("resultsMAOpt.RData"))
  {
    load("resultsMAOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='SMA', portfolio.st=portfolio.st, account.st=account.st, nsamples=.nsamples, verbose=TRUE)
  }
}

stats <- results$tradeStats
#print(stats)
idx <- order(stats[,1],stats[,2])
stats <- stats[idx,]
View(stats)
View(t(stats)[,1:10])

