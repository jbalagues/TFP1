ordenarPorDiaSemana <- function(diasSemana,tablaDias){
  vector.valores <- numeric(0)
  for(d in 1:length(diasSemana)) {
    diaSemana<-diasSemana[d]
    for(i in 1:length(tablaDias)) {
     
      fila <- names(tablaDias)[[i]]
      valor <- tablaDias[i]
      if(diaSemana == fila){
        vector.valores <- c(vector.valores,valor)
      }
    }
  }
  return( vector.valores)
}


ordenarPorDiaSemanafreq <- function(diasSemana, datos, nsemanas){
  vector.valores <- numeric(0)
  tablaDias<- table(diasSemana)
  for(d in 1:length(diasSemana)) {
    diaSemana<-diasSemana[d]
    datosDia<-datos[datos$dia == diaSemana,]
    for(i in 1:length(tablaDias)) {
      fila <- names(tablaDias)[[i]]
      valor <- tablaDias[i]
      if(diaSemana == fila){
        suma<-sum(datosDia$frecuencia)/nsemanas
        valor[[1]]<-suma
        vector.valores <- c(vector.valores,valor)
      }
    }
  }
  return( vector.valores)
}


