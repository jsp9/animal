rm(list = ls() )
library(data.table)
library(stringr)
setwd("C:/Users/Rebeca/Downloads")


#unzip("enigh2016_ns_viviendas_csv.zip")

data<-as.data.frame(fread('viviendas.csv', colClasses = 'character'))

data$edo<- substr(data$ubica_geo, 1,2)

table(data$edo)

data<-data[data$edo=="09",]

numeric<-names(data)[!names(data) %in% c("folioviv", "ubica_geo", "ageb", "edo")]

data[, numeric]<-apply(data[, numeric],2, as.numeric)

data$focos<-data$focos_inca+data$focos_ahor

library(ggplot2)

ggplot(data, aes(x=focos, fill=factor(escrituras),group=factor(escrituras)))+
  geom_histogram(, position = "identity", alpha=0.5)

