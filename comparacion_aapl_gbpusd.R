library(PerformanceAnalytics)
library(quantmod)
library(foreach)

##Modificar la ruta de acuerdo a donde están los dataset de AAPL y GBPUSD
ruta <- "C:/Users/luciano/Google Drive/time_series/simulacion_acciones"
GBPUSD <- read.table(paste0(ruta,"/GBPUSD.csv"), sep=",", header=T)
AAPL <- read.table(paste0(ruta,"/AAPL.csv"), sep=",", header=T)

GBPUSD$Date<-strptime(GBPUSD[,c(1)],format="%Y%m%d")
GBPUSD <- GBPUSD[,1:5]
GBPUSD <- as.xts(zoo(GBPUSD[,c(2:5)]), as.POSIXct(GBPUSD[,c(1)]))


AAPL$Date<-strptime(AAPL[,c(1)],format="%Y%m%d")
AAPL <- AAPL[,1:5]
AAPL <- as.xts(zoo(AAPL[,c(2:5)]), as.POSIXct(AAPL[,c(1)]))

.from <- '2010-12-01'
.to <- '2014-04-17'

#stock <- AAPL[,1:4][paste0(.from,"::",.to)]

symbols <- c("AAPL","GBPUSD")

par(mfrow = c(2,1))
for (symbol in symbols) {
stock <- get(symbol)
stock <- stock[,1:4][paste0(.from,"::",.to)]

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

chart.TimeSeries(stock[,8:9], legend="topleft", col=c("green","red"), main=paste0("Random Walk Index - ",symbol))
}

barChart(AAPL[paste0(.from,"::",.to)], theme="white")
barChart(GBPUSD[paste0(.from,"::",.to)], theme="white")
