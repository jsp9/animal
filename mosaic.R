rm(list=ls())
setwd("~/git/ANIMAL_POLITICO/hogares/data/in")
library(data.table)

hog<-fread('hogares.csv')

library(survey)
library(stringr)
hog$ubica_geo<-str_pad(hog$ubica_geo, 5, side='left', pad= '0')
hog$edo<-substr(hog$ubica_geo, 1, 2)
hog<-hog[hog$edo=='09',]

hog$alim1<-factor(hog$alim1)
hog$pisos<-factor(hog$pisos)
hog$escri[hog$escri==2]<-1
hog$dormi[hog$dormi %in% c(3:22)]<-3


hog$escri<-factor(hog$escri)
hog$dormi<-factor(hog$dormi)

levels(hog$dormi)<-c('Uno', 'Dos', 'Tres+')
levels(hog$alim1)<-c('Sí', 'No')
levels(hog$pisos)<-c('Tierra', 'Cemento', 'Madera/mosaico', 'NE')
levels(hog$escri)<-c('C/escrituras', 'S/escrituras', 'NS')


des<-svydesign(ids=~upm, strata=~est_dis, data=hog, weights = ~factor)



#round(prop.table(table(hog$dormi))*100,1)
#round(prop.table(table(hog$alim1))*100,1)

tabla10<-svytable( ~dormi+alim1+escri, design=des)

library(vcd)

#mosaic(~escri+ alim1+dormi, data=tabla10, highlighting="escri", highlighting_fill=c("lightblue", "pink", 'grey'),
#       direction=c("v","h","h"))





#rm(list=ls())
library(data.table)
library(stringr)
library(car)
library(cluster)
library(questionr)
setwd("~/git/ANIMAL_POLITICO/hogares/data/in")

viviendas<-fread("viviendas.csv", sep = ",", encoding = "UTF-8")
socdem<-fread("concentradohogar.csv", sep = ",", encoding = "UTF-8")
loc<-fread("localidades.csv", sep = ",", encoding = "UTF-8", colClasses = c("c", "c", "c", "c", "c", "num", "num", "num", "c"))
loc$CVE_ENT <- str_pad(loc$CVE_ENT, 2, pad = "0")
loc$CVE_MUN <- str_pad(loc$CVE_MUN, 3, pad = "0")
loc$CVE_LOC <- str_pad(loc$CVE_LOC, 4, pad = "0")

loc$LATITUD<-as.numeric(loc$LATITUD)
loc$LONGITUD<-as.numeric(loc$LONGITUD)

loc$ubica_geo<-paste0(loc$CVE_ENT, loc$CVE_MUN, loc$CVE_LOC)

data <- merge(viviendas, socdem, by=c("tam_loc", "ubica_geo", "ageb",  "folioviv"))

data$ubica_geo<-str_pad(as.character(data$ubica_geo), 9, pad = "0")
data <- merge(data, loc, by= "ubica_geo", all.x=T)


rm(socdem, viviendas, loc)


names(data)

vars<-c("cuart_dorm",   "escrituras", "AMBITO", 'upm.y', 'est_dis.y', 'factor.y')

data<-data[, vars, with=F]

data<-data[data$CVE_ENT=='09',]


des16<-svydesign(ids=~upm.y, strata=~est_dis.y, data=data, weights = ~factor.y)





