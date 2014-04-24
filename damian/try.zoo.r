zoo.data <- zoo(rnorm(31)+10,as.Date(13514:13744,origin="1970-01-01"))
ep <- endpoints(zoo.data,'days',k=2)
myfunction<-function(x) {str(x)}
period.apply(zoo.data, INDEX=ep, FUN=myfunction)


N <- nrow(GBPUSD)
system.time(for (i in 2:N) {
  GBPUSD[[i,"Posicion_acciones"]] <- GBPUSD[[i,"Posicion_acciones"]]
  GBPUSD[[i,"Posicion_dinero"]] <- GBPUSD[[i,"Posicion_dinero"]]
#   GBPUSD[i, "Decision"]
#   GBPUSD[i, "Sig_entrada"]
})
