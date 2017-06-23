library(stringr)

source("SoporteCarga.R")

options(stringsAsFactors = FALSE)

# Se carga un fichero que contiene el filtro para saber que lineas de los ficheros de logs se van
# a cargar y cuales no
df.Filtro <- read.csv("config/lineas.csv", header = TRUE, sep=";",encoding="utf-8",stringsAsFactors =FALSE )  
head(df.Filtro)


# Connect to MongoDB database.
urlMongo <-"mongodb://localhost:27017/TFP"
#collection <- "logs"
collection <- "etpv"
mongoDBconn<- mongo(collection, url = urlMongo )

filenames <- listLogFiles("./datos", "*.log")
numfiles <- length(filenames)

for (i in 1:numfiles){ 
  res <- readLogFile(filenames[i])
  df.logs <- filterLogsFile(res, df.Filtro)
}

print(paste("Numero de registros a guardar en la base de datos:",nrow(df.logs)))


#Creo 2 nuevos campos: dia de la semana y hora de 0 a 24h
df.logs$weekday<-weekdays(as.Date(df.logs$date))
df.logs$hora<-substr(df.logs$time, 1, 2)

#Elimino los milisegundos de la hora
df.logs$time<-substr(df.logs$time, 1, 8)

#Se guardan los datos en formato JSOn en la base de datos
mongoDBconn$insert(df.logs)
rm(df.logs)


#Cierro la conexion con la base de daos
rm(mongoDBconn)
