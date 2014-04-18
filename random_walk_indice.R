symbol <- 'TS.BA'

.from <- '2005-01-01'
.to <- '2014-04-17'

getSymbols(symbol, from=.from, to=.to)

stock <- get(symbol)
stock <- stock[,1:4]
names(stock) <- c("Open", "High", "Low", "Close")
stock$Lowest <- NA
stock$Highest <- NA
stock$ATR <- NA

k <- 14
columnas <- ncol(stock)
for (i in 1:(k-1)) {
  stock$Baja <- NA
  stock$Baja <- Lag(stock$Low, i)
  names(stock)[columnas+i] <- paste0('Low_',i)
}

stock$Lowest <- pmin(stock[,'Low'],stock[,columnas+1],stock[,columnas+2],stock[,columnas+3],stock[,columnas+4],stock[,columnas+5],stock[,columnas+6],stock[,columnas+7],stock[,columnas+8],stock[,columnas+9],stock[,columnas+10],stock[,columnas+11],stock[,columnas+12],stock[,columnas+13])

stock <- stock[,1:7]



k <- 14
columnas <- ncol(stock)
for (i in 1:(k-1)) {
  stock$Alta <- NA
  stock$Alta <- Lag(stock$High, i)
  names(stock)[columnas+i] <- paste0('High_',i)
}

stock$Highest <- pmax(stock[,'High'],stock[,columnas+1],stock[,columnas+2],stock[,columnas+3],stock[,columnas+4],stock[,columnas+5],stock[,columnas+6],stock[,columnas+7],stock[,columnas+8],stock[,columnas+9],stock[,columnas+10],stock[,columnas+11],stock[,columnas+12],stock[,columnas+13])

stock <- stock[,1:7]

stock$ATR <- ATR(stock)$atr
stock$RWI.High <- (1/sqrt(14))*(stock$High - stock$Lowest) / stock$ATR
stock$RWI.Low <- (1/sqrt(14))*(stock$Highest - stock$Low) / stock$ATR

chart.TimeSeries(stock[,8:9]['2014'], legend="topleft")
