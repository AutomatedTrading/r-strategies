# instalar quantmod con dependencias
#
install.packages("quantmod", repos = "http://cran.fhcrc.org",lib=.Library, depend=T)
#
# instalar PerformanceAnalytics con dependencias
#
install.packages("PerformanceAnalytics", repos = "http://cran.fhcrc.org",lib=.Library, depend=T)
#
# instalar quantstrat, FinancialInstrument y blotter
#
install.packages("FinancialInstrument", repos = "http://R-Forge.R-project.org", lib=.Library)
install.packages("blotter", repos = "http://R-Forge.R-project.org", lib=.Library)
install.packages("quantstrat", repos = "http://R-Forge.R-project.org", lib=.Library)
install.packages("TTR", repos = "http://R-Forge.R-project.org", lib=.Library)


library(quantstrat)

path <- "C:/Users/luciano/Google Drive/time_series/simulacion_acciones"

forex.30min<-read.csv(paste(path,"forex.30min.csv",sep="/"), sep=",")

forex.30min<-as.xts(zoo(forex.30min[,c(2:5)]),as.POSIXct(forex.30min[,c(1)]))

names(forex.30min)<-c("Open", "High", "Low", "Close")

GBPUSD = align.time(forex.30min, 1800)

#Ejemplo del Gráfico que aparece en el libro
chartSeries(GBPUSD['2007-12-23::2007-12-26'], theme='white')
addSMA(3)
addSMA(30)



# Otra forma de graficar, poniendo color rojo a la media de 7 barras (fastSMA) y azul a la media de 44 barras (slowSMA)
#
chartSeries(GBPUSD['2007-12-23::2007-12-26'],theme='white',name="GBPUSD",TA=c(addSMA(n=7,col="red"),addSMA(n=44,col="blue")))
#
# buscando la barra de cruce 
#
# calculo el vector de la media movil 'corta' de 7 periodos o barras 
fastMA <- SMA(Cl(GBPUSD),n=7)
#
# calculo el vector de la media movil 'larga' de 44 periodos o barras 
#
slowMA <- SMA(Cl(GBPUSD),n=44)
#
# genero un vector booleano que para periodo o barra tendrá 'TRUE' o 'FALSE' segun la media movil corta sea mayor que la media movil larga
#
co <- fastMA > slowMA
#
# ¿Cuantos períodos o barras hay entre el 24-dic-2007 y el 26-dic-2007
#
nrow(co['2007-12-24/2007-12-26'])
#
# ¿Cual es el 1er periodo o barra donde se cumple que la media movil corta es mayor que la larga?
#
x <- which(co['2007-12-24/2007-12-26'])[1]
#
# identificando la barra de cruce
#
ss <- GBPUSD['2007-12-24/2007-12-26']
#
# Al gráfico anterior le agrego una señal técnica 
# Esa señal la voy a poner en la 1er barra donde la media movil corta es mayor que la larga 
# La señal va a tener color 'rojo" y la coloco debajo del pto más bajo de la barra
#
addTA(ss[x,"Low"]-0.0005,pch=17,type="p",col="red", on=1,cex=2)
#
text(x=x,y=ss[x,"Low"]-0.0005,"Barra Cruce",pos=2)
