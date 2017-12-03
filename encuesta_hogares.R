rm(list=ls())
setwd("~")

##############################
# Israel Piña                #
#                            #
# ENCUESTA HHOGARES          #
##############################

#Instalar paquetes
require(plyr)  # el papá de dplyr
library(dplyr) # manipular data
library(tidyr) # "tidy data" o el paquete que SIEMPRE van a prender
library(ggplot2)
require(foreign)
require(readxl)
require(readstata13)
require(tidyverse)
require(RColorBrewer)
library(scales)
require(doBy)
require(reshape2)
require(gridExtra)
require(Hmisc)
require(ggmap)
require(rgdal)
require(rgeos)
require(sp)
require(maptools)
require(DeducerSpatial)
require(mapproj)
require(RJSONIO)
library(ggmap)
require(rjson)
require(ggthemes)
require(readxl)
require(foreign)
require(plyr)
require(tidyverse)
require(readstata13)
require(doBy)
require(reshape2)
require(gridExtra)
require(Hmisc)
require(ggmap)
require(rgdal)
require(rgeos)
require(sp)
require(maptools)
require(readxl)
require(ggplot2)
require(DeducerSpatial)
require(mapproj)
library(foreign)
library(rio)
library(questionr)
library(survey)
require(StatMeasures)
require(plotly)
require(htmlwidgets)
library(stringr)

#Ubicar datos
dir1 <- "/Users/macbook/Desktop/hogares/data/in"  # poner ruta 
dir2 <- "/Users/macbook/Desktop/hogares/graphs"  # poner ruta
dir3 <- "/Users/macbook/Desktop/hogares/maps"
dir4 <- "/Users/macbook/Desktop/hogares/data/out"


######### ENCUESTA GASTO EN HOGARES ###########################################################

### http://www.beta.inegi.org.mx/proyectos/enchogares/regulares/enigh/nc/2016/default.html ###

###############################################################################################



#PASO 00: LLAMAR ENCUESTA Y DATOS DE UBICACIÓN GEOGRÁFICA
data <-  read.csv(paste(dir1, "viviendas.csv", sep="/"), fileEncoding = "Windows-1252")
data2 <-  read.csv(paste(dir1, "localidades.csv", sep="/"), fileEncoding = "Windows-1252")
data3 <-  read.csv(paste(dir1, "municipios.csv", sep="/"), fileEncoding = "Windows-1252")
data4 <-  read.csv(paste(dir1, "concentradohogar.csv", sep="/"), fileEncoding = "Windows-1252")



######### INICIA BASE DE DATOS ESTADO Y MUNICIPIO ###########

#PASO 01: ELIMINAR VARIABLES INSERVIBLES 
data3 <- data3[,-(5:6)] 

#PASO 02: COLCAR UN 0 A CLAVES GEOGRAFICAS PARA TENERLAS COMPLETAS
data3$CVE_ENT <- str_pad(data3$CVE_ENT, 2, pad = "0")
data3$CVE_MUN <- str_pad(data3$CVE_MUN, 3, pad = "0")

#PASO 03: UNIR VARIABLES ESTADO Y MUNICIPIO
data3$cve_muni = paste(data3$CVE_ENT, data3$CVE_MUN, sep="")

#PASO 04: ORDENAR VARIABLES
data3 <- data3[c(5,1,2,3,4)]

#PASO 05: PONER EN MINÚSCULAS NOMBRES DE VARIABLES
names(data3) <- tolower(names(data3))

######### TERMINA BASE DE DATOS ESTADO Y MUNICIPIO ###########

##

######### INICIA BASE DE DATOS LOCALIDADES ###########

#PASO 01: ELIMINAR VARIABLES INSERVIBLES 
data2 <- data2[,-(8:9)] 

#PASO 02: COLOCAR 0 A CLAVES GEOGRÁFICAS DE LISTA PARA HACER EL MATCH
data2$CVE_ENT <- str_pad(data2$CVE_ENT, 2, pad = "0")
data2$CVE_MUN <- str_pad(data2$CVE_MUN, 3, pad = "0")
data2$CVE_LOC <- str_pad(data2$CVE_LOC, 4, pad = "0")

#PASO 03: UNIR VARIABLES DE LA LISTA DE CÓDIGOS
data2$ubica_geo = paste(data2$CVE_ENT, data2$CVE_MUN, data2$CVE_LOC, sep="")

data2$cve_muni = paste(data2$CVE_ENT, data2$CVE_MUN, sep="")

#PASO 04: UNIR ESTADO, MUNICIPIO Y LOCALIDAD
data2 <- merge(x = data2, y = data3, by = "cve_muni", all.x = TRUE)

#PASO 05: ORDENAR VARIABLES
data2 <- data2[c(9,4,5,12,13,10,11,6,7,8)]

#PASO 06: PONER EN MINÚSCULAS NOMBRES DE VARIABLES
names(data2) <- tolower(names(data2))

#PASO 07: GUARDAR BASE
write.csv(data, paste(dir4, "localidades_mx.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)

######### TERMINA BASE DE DATOS LOCALIDADES ###########



######### INICIA BASE DE DATOS CONCENTRADO ###########

#PASO 01: ELIMINAR VARIABLES INSERVIBLES 

data4 <- data4[c(1,10:26,101,102)] 

######### TERMINA BASE DE DATOS CONCENTRADO ###########


##

 
######### INICIA BASE DE DATOS HOGARES ###########

#PASO 01: COLCAR UN 0 A CLAVES GEOGRAFICAS PARA TENERLAS COMPLETAS
data$ubica_geo <- str_pad(data$ubica_geo, 9, pad = "0")

#PASO 02: UNIR BASES DE DATOS SOBRE LA BASE PRINCIPAL

#Poner ubicación geográfica
data <- merge(x = data, y = data2, by = "ubica_geo", all.x = TRUE)

#Poner datos socioeconómicos
data  <- cbind(data, data4[match(data$ï..folioviv, data4$ï..folioviv),1:20])

#PASO 03: REMOVER BASES QUE NO SIRVEN
rm(data3)
rm(data2)
rm(data4)

#PASO 04 RENOMBRAR VARIABLE FOLIO VIVIENDA
names(data)[2] = "folio_viv"

#PASO 05 BORRAR VARIABLE REPETIDA
data$ï..folioviv <- NULL

#PASO 06: REORDENAR VARIABLES
data <- data[c(2,3,74,60,71,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
               31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,
               64,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,
               91,92,59,1,65,66,67,68,69,70,72,73,61,62,63)]


#PASO 06: COLOCAR RESPUESTAS A PREGUNTAS A USAR

data$escrituras <- as.character(data$escrituras)
data$escrituras <- as.numeric(data$escrituras)

data$escrituras <- replace(data$escrituras, data$escrituras=="1", "A nombre del dueño")
data$escrituras <- replace(data$escrituras, data$escrituras=="2", "A nombre de otra persona")
data$escrituras <- replace(data$escrituras, data$escrituras=="3", "Sin escrituras")
data$escrituras <- replace(data$escrituras, data$escrituras=="4", "No sabe")

###
data$drenaje  <- as.character(data$drenaje)
data$drenaje  <- as.numeric(data$drenaje)

data$drenaje <- replace(data$drenaje, data$drenaje=="1", "Red pública")
data$drenaje <- replace(data$drenaje, data$drenaje=="2", "Tanque séptico")
data$drenaje <- replace(data$drenaje, data$drenaje=="3", "Tubería a una barranca")
data$drenaje <- replace(data$drenaje, data$drenaje=="4", "Tubería a un río")
data$drenaje <- replace(data$drenaje, data$drenaje=="5", "Sin drenaje")

###
data$disp_elect  <- as.character(data$disp_elect)
data$disp_elect  <- as.numeric(data$disp_elect)

data$disp_elect <- replace(data$disp_elect, data$disp_elect=="1", "Servicio público")
data$disp_elect <- replace(data$disp_elect, data$disp_elect=="2", "Planta particular")
data$disp_elect <- replace(data$disp_elect, data$disp_elect=="3", "Panel solar")
data$disp_elect <- replace(data$disp_elect, data$disp_elect=="4", "Otra fuente")
data$disp_elect <- replace(data$disp_elect, data$disp_elect=="5", "Sin luz electrica")

###
data$disp_agua  <- as.character(data$disp_agua)
data$disp_agua  <- as.numeric(data$disp_agua)

data$disp_agua <- replace(data$disp_agua , data$disp_agua =="1", "Entubada dentro de la viv")
data$disp_agua <- replace(data$disp_agua , data$disp_agua =="2", "Entubada dentro del terreno")
data$disp_agua <- replace(data$disp_agua , data$disp_agua =="3", "De llave pública")
data$disp_agua <- replace(data$disp_agua , data$disp_agua =="4", "Captadores de lluvia")
data$disp_agua <- replace(data$disp_agua , data$disp_agua =="5", "Acarreo de otra viv")
data$disp_agua <- replace(data$disp_agua , data$disp_agua =="6", "De pipa")
data$disp_agua <- replace(data$disp_agua , data$disp_agua =="7", "De pozo")

###
data$mat_techos  <- as.character(data$mat_techos)
data$mat_techos  <- as.numeric(data$mat_techos)

data$mat_techos <- replace(data$mat_techos , data$mat_techos =="1", "Desecho")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="2", "Lámina de cartón")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="3", "Lámina metálica")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="4", "Lámina de asbesto")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="5", "Lámina de fibrocemento")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="6", "Palma o paja")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="7", "Madera o tejamanil")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="8", "Terrado con viguería")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="9", "Teja")
data$mat_techos <- replace(data$mat_techos , data$mat_techos =="10", "Losa de concreto")

###
data$mat_pared  <- as.character(data$mat_pared)
data$mat_pared  <- as.numeric(data$mat_pared)

data$mat_pared <- replace(data$mat_pared , data$mat_pared =="1", "Desecho")
data$mat_pared <- replace(data$mat_pared , data$mat_pared =="2", "Lámina de cartón")
data$mat_pared <- replace(data$mat_pared , data$mat_pared =="3", "Lámina de asbesto")
data$mat_pared <- replace(data$mat_pared , data$mat_pared =="4", "Carrizo o banbú")
data$mat_pared <- replace(data$mat_pared , data$mat_pared =="5", "Embarro")
data$mat_pared <- replace(data$mat_pared , data$mat_pared =="6", "Madera")
data$mat_pared <- replace(data$mat_pared , data$mat_pared =="7", "Adobe")
data$mat_pared <- replace(data$mat_pared , data$mat_pared =="8", "Tabique o ladrillo")

###
data$mat_pisos  <- as.character(data$mat_pisos)
data$mat_pisos  <- as.numeric(data$mat_pisos)

data$mat_pisos <- replace(data$mat_pisos , data$mat_pisos =="1", "Tierra")
data$mat_pisos <- replace(data$mat_pisos , data$mat_pisos =="2", "Cemento o firme")
data$mat_pisos <- replace(data$mat_pisos , data$mat_pisos =="3", "Madera o mosaico")

###
data$tipo_viv  <- as.character(data$tipo_viv)
data$tipo_viv  <- as.numeric(data$tipo_viv)

data$tipo_viv  <- replace(data$tipo_viv , data$tipo_viv =="1", "Casa independiente")
data$tipo_viv  <- replace(data$tipo_viv , data$tipo_viv =="2", "Departamento en edificio")
data$tipo_viv  <- replace(data$tipo_viv , data$tipo_viv =="3", "Vivienda en vecindad")
data$tipo_viv  <- replace(data$tipo_viv , data$tipo_viv =="4", "Vivienda en cuarto de azotea")
data$tipo_viv  <- replace(data$tipo_viv , data$tipo_viv =="5", "No construida para habitación")

###
data$educa_jefe  <- as.character(data$educa_jefe)
data$educa_jefe  <- as.numeric(data$educa_jefe)

data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="1", "Sin instrucción")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="2", "Preescolar")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="3", "Primaria incompleta")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="4", "Primaria completa")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="5", "Secundaria incompleta")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="6", "Secundaria completa")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="7", "Preparatoria incompleta")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="8", "Preparatoria completa")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="9", "Profesional incompleta")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="10", "Profesional completa")
data$educa_jefe  <- replace(data$educa_jefe , data$educa_jefe =="11", "Posgrado")

###
data$tam_loc  <- as.character(data$tam_loc)
data$tam_loc  <- as.numeric(data$tam_loc)

data$tam_loc  <- replace(data$tam_loc , data$tam_loc =="1", "100000 y más")
data$tam_loc  <- replace(data$tam_loc , data$tam_loc =="2", "15000 y 99999")
data$tam_loc  <- replace(data$tam_loc , data$tam_loc =="3", "2500 y 14999")
data$tam_loc  <- replace(data$tam_loc , data$tam_loc =="4", "Menos de 2500")

###
data$ambito  <- as.character(data$ambito)

data$ambito  <- replace(data$ambito , data$ambito =="R", "Rural")
data$ambito  <- replace(data$ambito , data$ambito =="U", "Urbano")

###
data$sexo_jefe  <- as.character(data$sexo_jefe)
data$sexo_jefe  <- as.numeric(data$sexo_jefe)

data$sexo_jefe  <- replace(data$sexo_jefe , data$sexo_jefe =="1", "Hombre")
data$sexo_jefe  <- replace(data$sexo_jefe , data$sexo_jefe =="2", "Mujer")


###
data$est_socio  <- as.character(data$est_socio)
data$est_socio  <- as.numeric(data$est_socio)

data$est_socio  <- replace(data$est_socio , data$est_socio =="1", "Bajo")
data$est_socio  <- replace(data$est_socio , data$est_socio =="2", "Medio bajo")
data$est_socio  <- replace(data$est_socio , data$est_socio =="3", "Medio alto")
data$est_socio  <- replace(data$est_socio , data$est_socio =="4", "Alto")


#PASO 08: GUARDAR BASE
write.csv(data, paste(dir4, "encuesta_hogares.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)


################################################################################################

######### GRAFICAS


#ESCRITURAS NACIONALES

#Ponderador
tempo <- wtd.table(data$escrituras, weights=data$factor)
round(prop.table(tempo)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje)) +
  geom_bar(stat="identity", color="#FF0000", fill="#FF0000") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=.9),  colour="black", family="Tahoma") +
  labs(title="Títulos de propiedad en México", 
       x="", y="Viviendas", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold", size = 12), 
        axis.line = element_line(size=.5, colour = "black"),
        legend.position="none")

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_nacional.png", sep="/"), plot=gr, width=20, height=12)

###########

#ESCRITURAS POR ESTADO

#Ponderador
tempo <- wtd.table(data$escrituras, data$nom_ent, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "entidad"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(entidad,escrituras) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=entidad, y=porcentaje, fill=escrituras)) +
  geom_bar(stat="identity", width=.9) +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=.5),  colour="black", family="Tahoma", size=3.5) +
  labs(title="Títulos de propiedad por estado", 
       x="", y="Viviendas", fill=" ", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold", size = 12), 
        axis.line = element_line(size=.5, colour = "black"),
        plot.title = element_text(size = 13, face = "bold"),
        legend.position="bottom",
        legend.direction = "horizontal",
        legend.text=element_text(size=12),
        legend.title=element_text(size=10),
        legend.key=element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"))

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_estado.png", sep="/"), plot=gr, width=20, height=12)


###########

#ESCRITURAS POR TIPO DE VIVIENDA
#Ponderador
tempo <- wtd.table(data$escrituras, data$tipo_viv, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "tipo_de_vivienda"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,tipo_de_vivienda) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=tipo_de_vivienda)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=360, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")) +
  labs(title="Títulos de propiedad por tipo de vivienda", 
       x="", y="Viviendas", fill="Tipo de vivienda", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()
           
#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_tipo_viv.png", sep="/"), plot=gr, width=20, height=12)


###########

#ESCRITURAS POR ÁMBITO
#Ponderador
tempo <- wtd.table(data$escrituras, data$ambito, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "ambito"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,ambito) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=ambito)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=360, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")) +
  labs(title="Títulos de propiedad por ámbito", 
       x="", y="Viviendas", fill="Ámbito", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_ambito.png", sep="/"), plot=gr, width=20, height=12)


###########

#ESCRITURAS POR TAMAÑO DE POBLACIÓN
#Ponderador
tempo <- wtd.table(data$escrituras, data$tam_loc, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "tamano"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,tamano) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=tamano)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")) +
  labs(title="Títulos de propiedad por tamaño de población", 
       x="", y="Viviendas", fill="Tamaño de población", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_tamano_pob.png", sep="/"), plot=gr, width=20, height=12)

###########

#ESCRITURAS POR TAMAÑO POR MATERIAL DE PAREDES
#Ponderador
tempo <- wtd.table(data$escrituras, data$mat_pared, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "mat_pared"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,mat_pared) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=mat_pared)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="gray", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#fafad2","#ffffcc","#eee8aa","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")) +
  labs(title="Títulos de propiedad \npor tipo de material de las paredes", 
       x="", y="Viviendas", fill="Material de las paredes", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_material_paredes.png", sep="/"), plot=gr, width=20, height=12)

###########

#ESCRITURAS POR TAMAÑO POR MATERIAL DE TECHOS
#Ponderador
tempo <- wtd.table(data$escrituras, data$mat_techos, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "mat_techo"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,mat_techo) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=mat_techo)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#eee8aa","#ffdead","#cd853f","#c7e9b4","#7fcdbb","#41b6c4","#008080","#2c7fb8","#253494")) +
  labs(title="Títulos de propiedad \npor tipo de material de los techos", 
       x="", y="Viviendas", fill="Material de los techos", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_material_techos.png", sep="/"), plot=gr, width=20, height=12)

###########

#ESCRITURAS POR TAMAÑO POR MATERIAL DE PISOS
#Ponderador
tempo <- wtd.table(data$escrituras, data$mat_pisos, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "piso"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,piso) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=piso)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#c7e9b4","#7fcdbb","#41b6c4","#008080","#2c7fb8","#253494")) +
  labs(title="Títulos de propiedad \npor tipo de material del piso", 
       x="", y="Viviendas", fill="Material del piso", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_material_piso.png", sep="/"), plot=gr, width=20, height=12)


###########

#ESCRITURAS POR TAMAÑO POR DISPONIBILIDAD DE AGUA
#Ponderador
tempo <- wtd.table(data$escrituras, data$disp_agua, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "agua"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,agua) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=agua)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#fafad2","#eee8aa","#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8")) +
  labs(title="Títulos de propiedad \npor disponibilidad de agua", 
       x="", y="Viviendas", fill="Disponibilidad de agua", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_agua.png", sep="/"), plot=gr, width=20, height=12)

#################

#ESCRITURAS POR TAMAÑO POR ACCESO AL DRENAJE
#Ponderador
tempo <- wtd.table(data$escrituras, data$drenaje, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "drenaje"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,drenaje) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=drenaje)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8")) +
  labs(title="Títulos de propiedad por conexión al drenaje", 
       x="", y="Viviendas", fill="Conexión del drenaje", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_drenaje.png", sep="/"), plot=gr, width=20, height=12)

##############

#ESCRITURAS POR TAMAÑO POR ACCESO A LA LUZ ELECTRICA
#Ponderador
tempo <- wtd.table(data$escrituras, data$disp_elect, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "electricidad"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,electricidad) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=electricidad)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8")) +
  labs(title="Títulos de propiedad por conexión eléctrica", 
       x="", y="Viviendas", fill="Electricidad", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_electricidad.png", sep="/"), plot=gr, width=20, height=12)

##############

#ESCRITURAS POR TAMAÑO POR ACCESO A LA LUZ ELECTRICA
#Ponderador
tempo <- wtd.table(data$escrituras, data$sexo_jefe, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "sexo"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,sexo) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=sexo)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#7fcdbb","#41b6c4","#2c7fb8")) +
  labs(title="Títulos de propiedad por jefe del hogar", 
       x="", y="Viviendas", fill="Sexo", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_sexo.png", sep="/"), plot=gr, width=20, height=12)


###########

#ESCRITURAS POR ESTRATO SOCIAL
#Ponderador
tempo <- wtd.table(data$escrituras, data$est_socio, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "estrato"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,estrato) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=estrato)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#7fcdbb","#41b6c4","#008080","#2c7fb8")) +
  labs(title="Títulos de propiedad \npor estrato socioeconómico", 
       x="", y="Viviendas", fill="Estrato socioeconómico", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_estrato_social.png", sep="/"), plot=gr, width=20, height=12)


##############

#ESCRITURAS POR NIVEL EDUCATIVO DEL JEFE DEL HOGAR  
#Ponderador
tempo <- wtd.table(data$escrituras, data$educa_jefe, weights=data$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "educacion"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,educacion) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=educacion)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=45, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#eee8aa","#ffdead","#cd853f","#c7e9b4","#7fcdbb","#87ceeb","#41b6c4","#008080","#2c7fb8","#253494")) +
  labs(title="Títulos de propiedad \npor educación del jefe del hogar", 
       x="", y="Viviendas", fill="Nivel educativo", caption="INEGI: ENIGH") +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_nivel_educativo.png", sep="/"), plot=gr, width=20, height=12)

#Hacer gráfica 2
gr <- ggplot(tempo, aes(x=escrituras, y=educacion, fill=porcentaje)) +
  geom_tile() +
  geom_text(aes(label=paste0(porcentaje,"%")), colour="black", family="Tahoma", size=3.5) +
  scale_fill_continuous(low="#ffeda0", high="#f03b20") +
  labs(title="Títulos de propiedad \npor educación del jefe del hogar", 
       x="", y="Viviendas", fill="Nivel educativo", caption="INEGI: ENIGH") +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica 2
gr

#Salvar gráfica 2
ggsave(paste(dir2, "escrituras_nivel_educativo2.png", sep="/"), plot=gr, width=20, height=12)


################################################################################################

######### GRAFICAS ESCRITURAS

#PASO 01: EXTRAER VIVIENDAS SIN ESCRITURA
data01 <- data

data01 <- subset(data01, escrituras=="Sin escrituras")

#PASO 02: GUARDAR DATOS VIVIENDAS SIN ESCRITURAS
write.csv(data01, paste(dir4, "viviendas_sin_escrituras.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)

#PASO 03: LLAMAR TREEMAPS
require(treemap)

####

#NO ESCRITURAS POR TAMAÑO DE LAS LOCALIDADES  
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$tam_loc, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "localidad"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,localidad) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Salvar gráfica
png(paste(dir2, "noescrituras_localidad.png", sep="/"), width=12, height=12, units="in", res=300)

#Hacer gráfica
treemap(tempo, index="localidad", vSize=c("porcentaje"), vColor="index", type="index", 
        title="Tamaño de las localidades donde se ubican las viviendas sin escrituras", palette="Reds", 
        title.legend="", border.col="grey", border.lwd=0.5)

#Borrar gráfica
dev.off()


####

#NO ESCRITURAS POR MATERIAL DE PARED
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$mat_pared, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "pared"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,pared) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Salvar gráfica
png(paste(dir2, "noescrituras_pared.png", sep="/"), width=12, height=12, units="in", res=300)

#Hacer gráfica
treemap(tempo, index="pared", vSize=c("porcentaje"), vColor="index", type="index", 
        title="Material de las paredes de las viviendas sin escrituras", palette="Reds", 
        title.legend="", border.col="grey", border.lwd=0.5)

#Borrar gráfica
dev.off()

####

#NO ESCRITURAS POR MATERIAL DEL TECHO
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$mat_techos, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "techo"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,techo) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Salvar gráfica
png(paste(dir2, "noescrituras_techo.png", sep="/"), width=12, height=12, units="in", res=300)

#Hacer gráfica
treemap(tempo, index="techo", vSize=c("porcentaje"), vColor="index", type="index", 
        title="Material del techo de las viviendas sin escrituras", palette="Reds", 
        title.legend="", border.col="grey", border.lwd=0.5)

#Borrar gráfica
dev.off()

####

#NO ESCRITURAS POR MATERIAL DEL PISO
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$mat_pisos, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "piso"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,piso) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Salvar gráfica
png(paste(dir2, "noescrituras_piso.png", sep="/"), width=12, height=12, units="in", res=300)

#Hacer gráfica
treemap(tempo, index="piso", vSize=c("porcentaje"), vColor="index", type="index", 
        title="Material del techo de las viviendas sin escrituras", palette="Reds", 
        title.legend="", border.col="grey", border.lwd=0.5)

#Borrar gráfica
dev.off()

####

#NO ESCRITURAS POR DRENAJE
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$drenaje, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "drenaje"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,drenaje) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Salvar gráfica
png(paste(dir2, "noescrituras_drenaje.png", sep="/"), width=12, height=12, units="in", res=300)

#Hacer gráfica
treemap(tempo, index="drenaje", vSize=c("porcentaje"), vColor="index", type="index", 
        title="Drenaje en las viviendas sin escrituras", palette="Reds", 
        title.legend="", border.col="grey", border.lwd=0.5)

#Borrar gráfica
dev.off()

####

#NO ESCRITURAS POR ELECTRICIDAD
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$educa_jefe, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "educacion_jefe"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,educacion_jefe) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Salvar gráfica
png(paste(dir2, "noescrituras_educacion_jefe.png", sep="/"), width=12, height=12, units="in", res=300)

#Hacer gráfica
treemap(tempo, index="educacion_jefe", vSize=c("porcentaje"), vColor="index", type="index", 
        title="Educación de los jefes de las viviendas sin escrituras", palette="Reds", 
        title.legend="", border.col="grey", border.lwd=0.5)

#Borrar gráfica
dev.off()


###########

#SIN ESCRITURAS POR ÁMBITO 

#PASO 01: PREPARAR BASE
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$ambito, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

data01$ambito <- as.character(data01$ambito)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "ambito"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,ambito) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=ambito)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=360, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")) +
  labs(title="Viviendas sin escrituras por ámbito", 
       x="", y="Viviendas", fill="Ámbito", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "noescrituras_ambito.png", sep="/"), plot=gr, width=20, height=12)

###########

#SIN ESCRITURAS POR ESTRATO SOCIOECONÓMICO

#PASO 01: PREPARAR BASE
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$est_socio, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

tempo$est_socio <- as.character(tempo$est_socio)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "estrato"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,estrato) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Salvar gráfica
png(paste(dir2, "noescrituras_estrato.png", sep="/"), width=12, height=12, units="in", res=300)

#Hacer gráfica
treemap(tempo, index="estrato", vSize=c("porcentaje"), vColor="index", type="index", 
        title="Estrato socioeconómico de las viviendas sin escrituras", palette="Reds", 
        title.legend="", border.col="grey", border.lwd=0.5)

#Borrar gráfica
dev.off()
###########


###########

#SIN ESCRITURAS POR ÁMBITO 
#Ponderador
tempo <- wtd.table(data01$escrituras, data01$sexo_jefe, weights=data01$factor)
round(prop.table(tempo, 2)*100, 1)

#Data frame
tempo <- as.data.frame(tempo)

#Renombrar variables
names(tempo)[1] = "escrituras"
names(tempo)[2] = "sexo"
names(tempo)[3] = "frecuencia"

#Sacar porcentajes
tempo <- tempo %>%
  group_by(escrituras,sexo) %>%
  summarise(total = sum(frecuencia)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 1) )

#Hacer gráfica
gr <- ggplot(tempo, aes(x=escrituras, y=porcentaje, fill=sexo)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=360, colour="black", family="Tahoma", size=3.5) +
  scale_fill_manual(values=c("#7fcdbb","#41b6c4")) +
  labs(title="Viviendas sin escrituras por sexo del jefe de familia", 
       x="", y="Viviendas", fill="Ámbito", caption="INEGI: ENIGH") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme_bw()

#Imprimir gráfica
gr

#Salvar gráfica
ggsave(paste(dir2, "noescrituras_sexo_jefe.png", sep="/"), plot=gr, width=20, height=12)

################################################################################################

###########################

#BOXPLOT CON BASE EXPANDIDA

#COPIA DE LA BASE
tempo <- data

#SELECCIONAR VARIABLES
tempo <- tempo[,c("folio_viv", "escrituras", "tot_integ", "ing_cor", "educacion", "upm", "est_dis", "factor")] 

#VOLVER CARACTER
tempo$escrituras <- as.character(tempo$escrituras)

#CAMBIAR NOMBRES
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="A nombre del dueño", "1")
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="A nombre de otra persona", "2")
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="Sin escrituras", "3")
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="No sabe", "4")

#VOLVER NÚMERO
tempo$escrituras <- as.numeric(tempo$escrituras)

#OBTENER INGRESO PERCAPITA
tempo <- tempo %>%
  mutate(ing_perca = (ing_cor / tot_integ)) %>%
  round(digits=2) 

#OBTENER GASTO MENSUAL EN EDUCACIÓN
tempo  <-  tempo  %>%
  mutate(educacion_mes = (educacion / 3)) %>%
  round(digits=2) 

#ORDENAR VARIABLES
tempo  <- tempo [,c("folio_viv", "escrituras", "tot_integ", "ing_perca", "ing_cor", 
                    "educacion", "educacion_mes", "upm", "est_dis", "factor")] 

#CAMBIAR NOMBRES
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="1", "A nombre del dueño")
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="2", "A nombre de otra persona")
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="3", "Sin escrituras")
tempo$escrituras <- replace(tempo$escrituras, tempo$escrituras=="4", "No sabe")

#VOLVER FACTOR
tempo$escrituras <- as.factor(tempo$escrituras)

#EXPANDIR BASE
tempo <- tempo[rep(seq_len(nrow(tempo)), tempo$factor), 1:10]


#PASO 02: CREAR GRÁFICO
#Agrupar
tempo <- tempo %>%
  group_by(escrituras,educacion_mes)%>%
  filter(escrituras!="NA") 

gr <- ggplot(tempo, aes(x=escrituras, y=educacion_mes)) + 
  geom_boxplot(fill="#3E588F", color= "#3E588F") +
  labs(title="Gasto mensual en educación \npor tipo de tenencia de vivienda", 
       x="", y="Pesos", caption="INEGI: ENIGH") +
  scale_y_continuous(labels = scales::comma)+
  theme_fivethirtyeight() +
  theme(legend.position="none",
        panel.spacing = unit(0.5, "lines"))

gr

#Salvar gráfica
ggsave(paste(dir2, "escrituras_gasto_educacion.png", sep="/"), plot=gr, width=20, height=12)

