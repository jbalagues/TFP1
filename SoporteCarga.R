library(stringr)
library(mongolite)
library(jsonlite)

options(stringsAsFactors = FALSE)

#Retorna el listado de ficheros de logs que van a ser cargados
listLogFiles <- function(pathLogs, patternFiles){
  filenames <- list.files(path=pathLogs, pattern=patternFiles, full.names=TRUE)
  return(filenames)
} 

#Lee el contenido de un fichero
readLogFile <- function(filename){
  con <- file(filename,open="r")
  res <- readLines(con, n = -1,encoding = "UTF-8", skipNul = FALSE)
  close(con) 
  return(res)
}

cargarLinea <- function(vectorValores, dfFiltro){
  for (i in 1:nrow(dfFiltro)) {
    #if(dfFiltro[i,1]==vectorValores[1] & grepl(dfFiltro[i,3], vectorValores[6])){
    if(grepl(dfFiltro[i,2], vectorValores[6])){
      return(dfFiltro[i,1])
    }
  }
  return ("")
}

#Funciona para cortar una linea de log en elementos de una lista
cutLine <- function(x){
  pos1 <- regexpr(")]",x)
  
  #Zona datos de control
  txtControl <- substr(x, 1, pos1+1)
  
  txtControl <-gsub("\\[", "", txtControl)
  txtControl <-gsub("\\]", "", txtControl)
  valores <-strsplit(txtControl, " ")
  
  #Zona mensaje del log
  txtDesc <- substr(x, pos1+2, nchar(iconv(enc2utf8(x), sub = "byte")))
  #Borrar espacios a la izquierda
  valores[[1]][length(valores[[1]])+1]<- trimws(txtDesc)
  
  return (unlist(valores))
}



#Se cera un dataframe vacio con las diferentes partes 
clearDataFrameLogs <- function(){
  df.logs <- data.frame(  "application"= character(), 
                          "level"= character(), 
                          "date"= character(),
                          "time" = character(),
                          "class" = character(),
                          "text" = character(),
                          "tag" = character(),
                          "idObject" = character(),
                          stringsAsFactors=F)  
  return(df.logs)
}




#Filtra y selecciona las lineas que deben guardarse
#y cada una se guarda en una fila de una dataframe
filterLogsFile <- function(res,dfFiltro){
  df.logs<-clearDataFrameLogs()
  counter<- 1
  l <- list();
  
  for (i in 1:length(res)){ #length(res)) {
    vectorValores <- cutLine(res[i])
    tag <- ""
    regex.idObject <-""
    if(length(vectorValores) >= 6){
      if(vectorValores[2]=="ERROR"){
        tag <- vectorValores[6]
      }else{
        for (j in 1:nrow(df.Filtro)) {   
          
          if(grepl(df.Filtro[j,2], vectorValores[6])){
            tag <- df.Filtro[j,1]
            regex.idObject <-df.Filtro[j,"idObject"]
            break
          }
        }
      }
      if( tag!= ""){
        vectorValores <-c(vectorValores,tag)
        print(i)
        if(nchar(regex.idObject)>1){
          idObject <- regmatches(vectorValores[6], gregexpr(regex.idObject, vectorValores[6]))
          idObject <-gsub("\\(", "", idObject)
          idObject <-gsub("\\)", "", idObject)
          vectorValores <-c(vectorValores,idObject[1])
        }else{
          vectorValores <-c(vectorValores,"")
        }
        df.logs[counter,] <- vectorValores
        counter<- (counter+1)
      } 
    }
    
    #df.logs$date <- paste(df.logs$date,"T",df.logs$time,"Z")
    #df.logs$date <- str_replace_all(df.logs$date, fixed(" "), "")
  }  
  drop <- c("application","text","class")
  
  df.logs <- df.logs[,!(names(df.logs) %in% drop)]
  return(df.logs)
}


#Convierte las lineas a objetos JSON
# y los guarda en la base de datso
storeMongoDB <- function(mongoDBconn,df.logs ){
  for (i in 1:nrow(df.logs)){ 
    datos <-toJSON(df.logs[i,])
    mongoDBconn$insert(datos)
  }
  return(mongoDBconn$count())
}
