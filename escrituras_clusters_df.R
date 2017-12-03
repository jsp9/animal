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

for_ns<-for_ns[for_ns$CVE_ENT=='09' & for_ns$AMBITO=='U',]

for_ns<-for_ns[, -c(1, 36)]

for_ns<-with(for_ns, data.frame(ubica_geo=ubica_geo,
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
                        mat_pisos=factor(car::recode(mat_pisos, "1='Tierra';
                                                   2='Cemento o firme';
                                                   3='Mosaico o madera'")),
                        antiguedad=factor(car::recode(antiguedad, "0:15='q1';
                                                                   14:28='q2';
                                                                   28:40='q3';
                                                                   41:99='q4';
                                                      NA='No respuesta'")),
                        num_cuarto=as.numeric(num_cuarto),
                        disp_agua=factor(car::recode(disp_agua, "1='Entubada en viv';
                                                   2='Entubada en terr';
                                                   3='Llave publica';
                                                   4='Captadores de lluvia';
                                                   5='Acarreo otra viv';
                                                   6='Pipa';
                                                   7='Pozo'")),
                        renta=factor(car::recode(renta, "0:2000='q1';
                                                      2001:3000='q2';
                                                      3001:5000='q3';
                                                      5001:33000='q4';
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
                        ing_cor=as.numeric(ing_cor)))

wt<-rep(1, length(names(for_ns[,-c(1:7)])))
names(wt)<-names(for_ns[,-c(1:7)])
wt['escrituras']<-2


gower_dist <- daisy(for_ns[,-c(1:7)],
                    metric = "gower", weights =wt )



sil_width <- NA

for(i in 2:10){
  
  pam_fit <- pam( dist(gower_dist),
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  print(i)
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam( gower_dist,
                diss = TRUE,
                k = 5)

for_ns$cluster<-pam_fit$clustering
setwd("~/git/ANIMAL_POLITICO/hogares/data/out")

write.csv(for_ns, "clusters.csv", row.names = F)


indep_categ<-c("CVE_MUN",    "tipo_viv",  "mat_pisos",  "antiguedad", "disp_agua",  "renta", "tenencia", "escrituras")
indep_cont<-c("num_cuarto", "tot_integ",  "porc_muj", "porc_mayores",  "ing_cor")

indep_categ<-lapply(indep_categ, function(x) round(prop.table(wtd.table(for_ns[, x] , for_ns$cluster, weights = for_ns$FAC),2)*100,1))


indep_categ<-as.data.frame(do.call(rbind, indep_categ))
indep_categ$var<-rownames(indep_categ)


cont<-list()
for(i in 1:length(indep_cont)){
cont[[i]]<-t(do.call(rbind, lapply(split(for_ns[,c(indep_cont[i], "FAC")], for_ns$cluster) , function(x) sum(x[,1]*x$FAC)/sum(x$FAC))))}

cont<-as.data.frame(do.call(rbind, cont))
cont$var<-indep_cont

tamano<-t.data.frame(cbind.data.frame(clust=as.numeric(prop.table(wtd.table(for_ns$cluster, weights = for_ns$FAC)))))
tamano<-cbind.data.frame(tamano, var='tamano')

out<-rbind.data.frame(tamano, indep_categ, cont)

options("scipen"=100, "digits"=2)


write.csv(out, "sum_clusters.csv", row.names = F)

                    