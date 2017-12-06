rm(list = ls() )
library(data.table)
library(stringr)
library(foreign)

setwd("~/git/animal")

data_10<-fread('hogares.csv', colClasses = c('character', rep('numeric', 167)))


data_10$edo<-substr(data_10$folioviv, 1,2)

table(data_10$edo)

data_10<-data_10[data_10$edo=="09",]


data_16<-as.data.frame(fread('viviendas.csv', colClasses = 'character'))

data_16$edo<- substr(data_16$ubica_geo, 1,2)

table(data_16$edo)

data_16<-data_16[data_16$edo=="09",]

numeric<-names(data_16)[!names(data_16) %in% c("folioviv", "ubica_geo", "ageb", "edo")]

data_16[, numeric]<-apply(data_16[, numeric],2, as.numeric)

data_16$focos<-data_16$focos_inca+data_16$focos_ahor

data_16<-data_16[!is.na(data_16$escrituras), c('focos', 'escrituras',  "factor" )]

data_16$escrituras[data_16$escrituras==2]<-1

data_10<-data_10[data_10$edo=="09",]

data_10<-data_10[!is.na(data_10$escri), c('focos', 'escri',  "factor" )]

data_10$escri[data_10$escri==2]<-1

data_10$yr<-2010
data_16$yr<-2016
names(data_16)<-names(data_10)

data<-rbind(data_10, data_16)
data$escri[data$escri==1]<-'Con escrituras'
data$escri[data$escri=='3']<-'Sin escrituras'
data$escri[data$escri=='4']<-'NS'

data$escrituras<-data$escri

library(questionr)
round(prop.table(wtd.table(data$escrituras, data$yr, weights = data$factor))*100, 1)

library(ggridges)



c_2010<-data$focos[data$escrituras=='Con escrituras' & data$yr==2010]
fac_c_2010<-data$factor[data$escrituras=='Con escrituras' & data$yr==2010]

s_2010<-data$focos[data$escrituras=='Sin escrituras' & data$yr==2010]
fac_s_2010<-data$factor[data$escrituras=='Sin escrituras' & data$yr==2010]


c_2016<-data$focos[data$escrituras=='Con escrituras' & data$yr==2016]
fac_c_2016<-data$factor[data$escrituras=='Con escrituras' & data$yr==2016]

s_2016<-data$focos[data$escrituras=='Sin escrituras' & data$yr==2016]
fac_s_2016<-data$factor[data$escrituras=='Sin escrituras' & data$yr==2016]


sum(c_2010*fac_c_2010, na.rm = T)/sum(fac_c_2010, na.rm = T)

sum(c_2016*fac_c_2016, na.rm = T)/sum(fac_c_2016, na.rm = T)

sum(s_2010*fac_s_2010, na.rm = T)/sum(fac_s_2010, na.rm = T)

sum(s_2016*fac_s_2016, na.rm = T)/sum(fac_s_2016, na.rm = T)

ggplot(data=data[data$escri!='NS',], aes(x=focos, y=factor(yr), group=factor(yr), fill=escrituras))+
  geom_density_ridges2()+
  facet_wrap(~escri)+
  theme_ridges()+xlab('Número de focos en la vivienda')+
  ylab('')+
  ggtitle('Distribución de número de focos en viviendas de la CDMX', subtitle = 'Según condición de escrituración y año \n Media de focos en viviendas con escrituras 2010=9.9; 2016=9.9 \n Media de focos en viviendas sin escrituras 2010=7.3; 2016=7.8')