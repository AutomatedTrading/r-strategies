# getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD es un 'xts' object
# index(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD) retorna un vector de POSIXct con las fecha/hora de los precios
# indexTZ(<xts object>) retorna el timezone
ob<-getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD
indexTZ(ob)<-"America/Buenos_Aires"
# mejor es
indexTZ(mktdata)<-"UTC"