rm(list=ls())
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

vars<-c("CVE_ENT", "CVE_MUN", "CVE_LOC", "NOM_LOC", "ubica_geo", "LATITUD", "LONGITUD", "factor.y", "tipo_viv" , "antiguedad",   "mat_pared",   "mat_techos","mat_pisos" , "mat_pisos",  "cocina" , "cuart_dorm",  "num_cuarto",  "disp_agua",     "disp_elect" , "tenencia",    "renta",       "estim_pago", "escrituras", "tot_resid",   "tot_hom",     "tot_muj",   "tam_loc", "clase_hog",   "sexo_jefe",   "edad_jefe"  , "educa_jefe",  "tot_integ", "mayores", "menores", "ing_cor", "AMBITO")

for_ns<-data[, vars, with=F]

for_ns<-with(for_ns, data.frame(ubica_geo=ubica_geo,
                                CVE_ENT=CVE_ENT,
                                CVE_MUN=CVE_MUN,
                                CVE_LOC=CVE_LOC,
                                NOM_LOC=NOM_LOC,
                                lat=LATITUD,
                                long=LONGITUD,
                                FAC=factor.y,
                tipo_viv=factor(car::recode(tipo_viv, "'1'='Casa independiente';
                                                   '2'='Departamento en edificio';
                                                   '3'='Vivienda en vecindad';
                                                   '4'='Vivienda en cuarto de azotea';
                                                   '5'='No construida para habitacion';
                                                    NA='No respuesta'")), 
                        mat_techos=factor(car::recode(mat_techos, "1='Deshecho';
                                                   2:5='Lamina';
                                                   6='Palma o paja';
                                                   7='Madera o tejamanil';
                                                   8='Terrado con vigueria';
                                                   9='Teja';
                                                   10='Losa de concreto';
                                                   NA='No respuesta'")), 
                        mat_pared=factor(car::recode(mat_pared, "1='Deshecho';
                                                   2:3='Lamina';
                                                   4='Carrizo o bambu';
                                                   5='Embarro';
                                                   6='Madera';
                                                   7='Adobe';
                                                   8='Tabique o ladrillo';
                                                   NA='No respuesta'")),
                        mat_pisos=factor(car::recode(mat_pisos, "1='Tierra';
                                                   2='Cemento o firme';
                                                   3='Mosaico o madera'")),
                        antiguedad=factor(car::recode(antiguedad, "0:10='q1';
                                                                   11:16='q2';
                                                                   17:30='q3';
                                                                   31:99='q4';
                                                      NA='No respuesta'")),
                        num_cuarto=as.numeric(num_cuarto),
                        disp_agua=factor(car::recode(disp_agua, "1='Entubada en viv';
                                                   2='Entubada en terr';
                                                   3='Llave publica';
                                                   4='Captadores de lluvia';
                                                   5='Acarreo otra viv';
                                                   6='Pipa';
                                                   7='Pozo'")),
                        disp_elect=factor(car::recode(disp_elect, "1='Servicio publico';
                                                   2='Planta particular';
                                                    3='Panel solar';
                                                    4='Otra fuente';
                                                    5='Sin electricidad'")),
                        renta=factor(car::recode(renta, "0:900='q1';
                                                      901:1300='q2';
                                                      1301:2000='q3';
                                                      2000:33000='q4';
                                                      NA='No rentan'")),
                        tenencia=factor(car::recode(tenencia, "1='Rentada';
                                                   2='Prestada';
                                                      3:4='Propia';
                                                      5:6='Otra'")),
                        escrituras=factor(car::recode(escrituras, "1='A nombre del dueno';
                                                   2='A nombre de otra persona';
                                                    3='Sin escrituras';
                                                    4='No sabe';
                                                    NA='No respuesta'")),
                        tot_integ=as.numeric(tot_integ),
                        porc_muj=as.numeric(tot_muj/tot_resid),
                        porc_mayores=as.numeric(mayores/tot_integ),
                        ing_cor=as.numeric(ing_cor),
                        ambito=factor(car::recode(AMBITO, "'R'='R';
                                                   'U'='U';
                                                  NA='No especifica'"))))

########MODELO########

sample<-for_ns[sample(nrow(for_ns), 20000),]
gower_dist <- daisy(sample[,-c(1:8)],
                    metric = "gower")

index_sample<-sample(length(gower_dist), 1000)

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam( dist(gower_dist[index_sample]),
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam( gower_dist,
                diss = TRUE,
                k = 7)

sample$cluster<-pam_fit$clustering
setwd("~/git/ANIMAL_POLITICO/hogares/data/out")

write.csv(sample, "clusters.csv", row.names = F)


indep_categ<-names(sample)[-c(1:8, 14, 20, 21, 22, 23, 25)]
indep_cont<-c("num_cuarto", "tot_integ",  "porc_muj", "porc_mayores",  "ing_cor")

indep_categ<-lapply(indep_categ, function(x) round(prop.table(wtd.table(sample[, x] , sample$cluster, weights = sample$FAC),2)*100,1))


indep_categ<-as.data.frame(do.call(rbind, indep_categ))
indep_categ$var<-rownames(indep_categ)


cont<-list()
for(i in 1:length(indep_cont)){
cont[[i]]<-t(do.call(rbind, lapply(split(sample[,c(indep_cont[i], "FAC")], sample$cluster) , function(x) sum(x[,1]*x$FAC)/sum(x$FAC))))}

cont<-as.data.frame(do.call(rbind, cont))
cont$var<-indep_cont



out<-rbind(indep_categ, cont)

options("scipen"=100, "digits"=2)

prop.table(wtd.table(sample$cluster, weights = sample$FAC))

library(ggplot2)

ggplot(data=sample, aes(x=-long, y=lat, group=factor(cluster), color=factor(cluster)))+
  geom_point(,  alpha=0.3)

ggplot(data=sample, aes(x=-long, y=lat, group=escrituras, color=escrituras))+
  geom_point(,  alpha=0.3)

#REGIONES

sur<-c("31", "23", "04", "27", "07", "20")
bajio<-c("12", "16", "06", "14", "18", "25")
centro<-c("30", "21", "17", "29", "13", "22", "24", "11", "01")
vmx<-c("09", "15")
norte<-c("28", "19", "05", "32", "10", "26", "03", "02", "08")

sample$CVE_ENT[is.na(sample$CVE_ENT)]<-substr(sample$ubica_geo[is.na(sample$CVE_ENT)], 1, 2)

sample$region<-ifelse(sample$CVE_ENT %in% sur, "SUR",
       ifelse(sample$CVE_ENT %in% bajio, "BAJIO",
              ifelse(sample$CVE_ENT %in% centro, "CENTRO",
                     ifelse(sample$CVE_ENT %in% vmx, "VALLE MX", 
                            ifelse(sample$CVE_ENT %in% norte, "NORTE", F)))))
table(sample$CVE_ENT, sample$region)

ent<-round(prop.table(wtd.table(sample[, "region"] , sample$cluster, weights = sample$FAC),2)*100,1)
out<-rbind(out, cbind(ent, var=rownames(ent)))


write.csv(out, "sum_clusters.csv", row.names = F)

                    