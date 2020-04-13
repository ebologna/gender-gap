library(questionr)
library(foreign)
library(ggplot2)
library(ggthemes)
library(doBy)
library(scales)

ubicaciones.bases.dbf=list(
  "archivosINDEC\\Ind_t104.dbf","archivosINDEC\\Ind_t105.dbf",
  "archivosINDEC\\Ind_t106.dbf", "archivosINDEC\\Ind_t107.dbf",
  "archivosINDEC\\Ind_t108.dbf", "archivosINDEC\\Ind_t109.dbf",
  "archivosINDEC\\Ind_t110.dbf", "archivosINDEC\\Ind_t111.dbf",
  "archivosINDEC\\Ind_t112.dbf", "archivosINDEC\\Ind_t113.dbf",
  "archivosINDEC\\Ind_t114.dbf", "archivosINDEC\\Ind_t115.dbf")

ubicaciones.bases.txt=list(
  "archivosINDEC\\usu_individual_t316.txt",
  "archivosINDEC\\usu_individual_t117.txt",
  "archivosINDEC\\usu_individual_t118.txt")

eph.dbf=vector("list",12)
for(i  in 1:12) eph.dbf[[i]]=read.dbf(ubicaciones.bases.dbf[[i]])

eph.txt=vector("list",3)
for(i  in 1:3) eph.txt[[i]]=read.csv(ubicaciones.bases.txt[[i]], sep = ";")

for(i  in 1:3) eph.txt[[i]]$PP08D1[is.na(eph.txt[[i]]$PP08D1)]=0

eph=vector("list",15)
eph=c(eph.dbf,eph.txt)

originales<-vector("list",15)
originales<-eph

for (i in 1:15) {
  originales[[i]]$sexo=as.factor(originales[[i]]$CH04)
  levels(originales[[i]]$sexo)=c("varones", "mujeres")
  originales[[i]]$educacion=factor(originales[[i]]$NIVEL_ED,
                            levels(factor(originales[[i]]$NIVEL_ED))[c(7,1:6)])
  levels(originales[[i]]$educacion)=c("Sin instrucción",
                               "Primaria Incompleta",
                               "Primaria Completa","Secundaria Incompleta", "Secundaria Completa",
                               "Universitaria Incompleta","Universitaria Completa")
}

# por la definición INDEC, la tasa es sobre la población
# total, por eso quedan los menores de 10.
# pero en "sin instrucción", hay muchos y la tasa da muy baja
# habrá que aclararlo
#cálculo de las tasas:
# definir los u como as.data.frame de las wtd.table de estado
# calcular las tasas operando sobre los elementos de u
# y servirán también para desocupación
actividad<-(u$Freq[u$Var1==1]+u$Freq[u$Var1==2])/u$Freq[u$Var1=="Sum"]

# los subconjuntos están perfectos
# los subsets de originales
varones<-vector("list",15)
mujeres<-vector("list",15)

varones_Sin_instrucción<-vector("list",15)
varones_Primaria_Incompleta<-vector("list",15)
varones_Primaria_Completa<-vector("list",15)
varones_Secundaria_Incompleta<-vector("list",15)
varones_Secundaria_Completa<-vector("list",15)
varones_Universitaria_Incompleta<-vector("list",15)
varones_Universitaria_Completa<-vector("list",15)
mujeres_Sin_instrucción<-vector("list",15)
mujeres_Primaria_Incompleta<-vector("list",15)
mujeres_Primaria_Completa<-vector("list",15)
mujeres_Secundaria_Incompleta<-vector("list",15)
mujeres_Secundaria_Completa<-vector("list",15)
mujeres_Universitaria_Incompleta<-vector("list",15)
mujeres_Universitaria_Completa<-vector("list",15)

for (i in 1:15) {
  varones[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones")
}
for (i in 1:15) {
  mujeres[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres")
}

for (i in 1:15) {
  varones_Sin_instrucción[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Sin instrucción")
}

for (i in 1:15) {
  mujeres_Sin_instrucción[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Sin instrucción")
}

for (i in 1:15) {
  varones_Primaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Primaria Incompleta")
}

for (i in 1:15) {
  mujeres_Primaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Primaria Incompleta")
}

for (i in 1:15) {
  varones_Primaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Primaria Completa")
}

for (i in 1:15) {
  mujeres_Primaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Primaria Completa")
}

for (i in 1:15) {
  varones_Secundaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Secundaria Incompleta")
}

for (i in 1:15) {
  mujeres_Secundaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Secundaria Incompleta")
}

for (i in 1:15) {
  varones_Secundaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Secundaria Completa")
}

for (i in 1:15) {
  mujeres_Secundaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Secundaria Completa")
}

for (i in 1:15) {
  varones_Universitaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Universitaria Incompleta")
}

for (i in 1:15) {
  mujeres_Universitaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Universitaria Incompleta")
}

for (i in 1:15) {
  varones_Universitaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Universitaria Completa")
}

for (i in 1:15) {
  mujeres_Universitaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Universitaria Completa")
}



# Se definen las 17 listas cada una con 15 tablas de estado

# todos
tabla_estado<-vector("list",15)
for (i in 1:15) {
      tabla_estado[[i]]<-addmargins(
      wtd.table(
        originales[[i]]$ESTADO, weights = originales[[i]]$PONDERA))
      tabla_estado[[i]]<-as.data.frame(tabla_estado[[i]])
}

# varones
tabla_estado_varones<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones[[i]]<-addmargins(
    wtd.table(
      varones[[i]]$ESTADO, weights = varones[[i]]$PONDERA))
  tabla_estado_varones[[i]]<-as.data.frame(tabla_estado_varones[[i]])
}

# mujeres
tabla_estado_mujeres<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres[[i]]<-addmargins(
    wtd.table(
      mujeres[[i]]$ESTADO, weights = mujeres[[i]]$PONDERA))
  tabla_estado_mujeres[[i]]<-as.data.frame(tabla_estado_mujeres[[i]])
}

# varones_Sin_instrucción
tabla_estado_varones_Sin_instrucción<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Sin_instrucción[[i]]<-addmargins(
    wtd.table(
      varones_Sin_instrucción[[i]]$ESTADO, weights = varones_Sin_instrucción[[i]]$PONDERA))
  tabla_estado_varones_Sin_instrucción[[i]]<-as.data.frame(tabla_estado_varones_Sin_instrucción[[i]])
}

# mujeres_Sin_instrucción
tabla_estado_mujeres_Sin_instrucción<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Sin_instrucción[[i]]<-addmargins(
    wtd.table(
      mujeres_Sin_instrucción[[i]]$ESTADO, weights = mujeres_Sin_instrucción[[i]]$PONDERA))
  tabla_estado_mujeres_Sin_instrucción[[i]]<-as.data.frame(tabla_estado_mujeres_Sin_instrucción[[i]])
}

# varones_Primaria_Incompleta
tabla_estado_varones_Primaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Primaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      varones_Primaria_Incompleta[[i]]$ESTADO, weights = varones_Primaria_Incompleta[[i]]$PONDERA))
  tabla_estado_varones_Primaria_Incompleta[[i]]<-as.data.frame(tabla_estado_varones_Primaria_Incompleta[[i]])
}

# mujeres_Primaria_Incompleta
tabla_estado_mujeres_Primaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Primaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      mujeres_Primaria_Incompleta[[i]]$ESTADO, weights = mujeres_Primaria_Incompleta[[i]]$PONDERA))
  tabla_estado_mujeres_Primaria_Incompleta[[i]]<-as.data.frame(tabla_estado_mujeres_Primaria_Incompleta[[i]])
}

# varones_Primaria_Completa
tabla_estado_varones_Primaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Primaria_Completa[[i]]<-addmargins(
    wtd.table(
      varones_Primaria_Completa[[i]]$ESTADO, weights = varones_Primaria_Completa[[i]]$PONDERA))
  tabla_estado_varones_Primaria_Completa[[i]]<-as.data.frame(tabla_estado_varones_Primaria_Completa[[i]])
}

# mujeres_Primaria_Completa
tabla_estado_mujeres_Primaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Primaria_Completa[[i]]<-addmargins(
    wtd.table(
      mujeres_Primaria_Completa[[i]]$ESTADO, weights = mujeres_Primaria_Completa[[i]]$PONDERA))
  tabla_estado_mujeres_Primaria_Completa[[i]]<-as.data.frame(tabla_estado_mujeres_Primaria_Completa[[i]])
}

# varones_Secundaria_Incompleta
tabla_estado_varones_Secundaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Secundaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      varones_Secundaria_Incompleta[[i]]$ESTADO, weights = varones_Secundaria_Incompleta[[i]]$PONDERA))
  tabla_estado_varones_Secundaria_Incompleta[[i]]<-as.data.frame(tabla_estado_varones_Secundaria_Incompleta[[i]])
}

# mujeres_Secundaria_Incompleta
tabla_estado_mujeres_Secundaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Secundaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      mujeres_Secundaria_Incompleta[[i]]$ESTADO, weights = mujeres_Secundaria_Incompleta[[i]]$PONDERA))
  tabla_estado_mujeres_Secundaria_Incompleta[[i]]<-as.data.frame(tabla_estado_mujeres_Secundaria_Incompleta[[i]])
}

# varones_Secundaria_Completa
tabla_estado_varones_Secundaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Secundaria_Completa[[i]]<-addmargins(
    wtd.table(
      varones_Secundaria_Completa[[i]]$ESTADO, weights = varones_Secundaria_Completa[[i]]$PONDERA))
  tabla_estado_varones_Secundaria_Completa[[i]]<-as.data.frame(tabla_estado_varones_Secundaria_Completa[[i]])
}

# mujeres_Secundaria_Completa
tabla_estado_mujeres_Secundaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Secundaria_Completa[[i]]<-addmargins(
    wtd.table(
      mujeres_Secundaria_Completa[[i]]$ESTADO, weights = mujeres_Secundaria_Completa[[i]]$PONDERA))
  tabla_estado_mujeres_Secundaria_Completa[[i]]<-as.data.frame(tabla_estado_mujeres_Secundaria_Completa[[i]])
}

# varones_Universitaria_Incompleta
tabla_estado_varones_Universitaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Universitaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      varones_Universitaria_Incompleta[[i]]$ESTADO, weights = varones_Universitaria_Incompleta[[i]]$PONDERA))
  tabla_estado_varones_Universitaria_Incompleta[[i]]<-as.data.frame(tabla_estado_varones_Universitaria_Incompleta[[i]])
}

# mujeres_Universitaria_Incompleta
tabla_estado_mujeres_Universitaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Universitaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      mujeres_Universitaria_Incompleta[[i]]$ESTADO, weights = mujeres_Universitaria_Incompleta[[i]]$PONDERA))
  tabla_estado_mujeres_Universitaria_Incompleta[[i]]<-as.data.frame(tabla_estado_mujeres_Universitaria_Incompleta[[i]])
}

# varones_Universitaria_Completa
tabla_estado_varones_Universitaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Universitaria_Completa[[i]]<-addmargins(
    wtd.table(
      varones_Universitaria_Completa[[i]]$ESTADO, weights = varones_Universitaria_Completa[[i]]$PONDERA))
  tabla_estado_varones_Universitaria_Completa[[i]]<-as.data.frame(tabla_estado_varones_Universitaria_Completa[[i]])
}

# mujeres_Universitaria_Completa
tabla_estado_mujeres_Universitaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Universitaria_Completa[[i]]<-addmargins(
    wtd.table(
      mujeres_Universitaria_Completa[[i]]$ESTADO, weights = mujeres_Universitaria_Completa[[i]]$PONDERA))
  tabla_estado_mujeres_Universitaria_Completa[[i]]<-as.data.frame(tabla_estado_mujeres_Universitaria_Completa[[i]])
}

# Vectores con las 15 tasas cada uno
# todos
tasas_actividad<-vector()
for (i in 1:15) {
  tasas_actividad[i]<-
    (tabla_estado[[i]]$Freq[tabla_estado[[i]]$Var1==1]+
       tabla_estado[[i]]$Freq[tabla_estado[[i]]$Var1==2])/
    tabla_estado[[i]]$Freq[tabla_estado[[i]]$Var1=="Sum"]
    }

# varones
tasas_actividad_varones<-vector()
for (i in 1:15) {
  tasas_actividad_varones[i]<-
    (tabla_estado_varones[[i]]$Freq[tabla_estado_varones[[i]]$Var1==1]+
       tabla_estado_varones[[i]]$Freq[tabla_estado_varones[[i]]$Var1==2])/
    tabla_estado_varones[[i]]$Freq[tabla_estado_varones[[i]]$Var1=="Sum"]
}

# mujeres
tasas_actividad_mujeres<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres[i]<-
    (tabla_estado_mujeres[[i]]$Freq[tabla_estado_mujeres[[i]]$Var1==1]+
       tabla_estado_mujeres[[i]]$Freq[tabla_estado_mujeres[[i]]$Var1==2])/
    tabla_estado_mujeres[[i]]$Freq[tabla_estado_mujeres[[i]]$Var1=="Sum"]
}

# varones_Sin_instrucción
tasas_actividad_varones_Sin_instrucción<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Sin_instrucción[i]<-
    (tabla_estado_varones_Sin_instrucción[[i]]$Freq[tabla_estado_varones_Sin_instrucción[[i]]$Var1==1]+
       tabla_estado_varones_Sin_instrucción[[i]]$Freq[tabla_estado_varones_Sin_instrucción[[i]]$Var1==2])/
    tabla_estado_varones_Sin_instrucción[[i]]$Freq[tabla_estado_varones_Sin_instrucción[[i]]$Var1=="Sum"]
}

# mujeres_Sin_instrucción
# se construyen individualmente, porque hay cuatro años en los que, de las
# mujeres sin instrucciónn, ninguna trabajó
tasas_actividad_mujeres_Sin_instrucción<-vector()

tasas_actividad_mujeres_Sin_instrucción[1]<-
  (tabla_estado_mujeres_Sin_instrucción[[1]]$Freq[tabla_estado_mujeres_Sin_instrucción[[1]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[1]]$Freq[tabla_estado_mujeres_Sin_instrucción[[1]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[1]]$Freq[tabla_estado_mujeres_Sin_instrucción[[1]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[2]<-
  (tabla_estado_mujeres_Sin_instrucción[[2]]$Freq[tabla_estado_mujeres_Sin_instrucción[[2]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[2]]$Freq[tabla_estado_mujeres_Sin_instrucción[[2]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[2]]$Freq[tabla_estado_mujeres_Sin_instrucción[[2]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[3]<-
  (tabla_estado_mujeres_Sin_instrucción[[3]]$Freq[tabla_estado_mujeres_Sin_instrucción[[3]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[3]]$Freq[tabla_estado_mujeres_Sin_instrucción[[3]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[3]]$Freq[tabla_estado_mujeres_Sin_instrucción[[3]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[4]<-
  (tabla_estado_mujeres_Sin_instrucción[[4]]$Freq[tabla_estado_mujeres_Sin_instrucción[[4]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[4]]$Freq[tabla_estado_mujeres_Sin_instrucción[[4]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[4]]$Freq[tabla_estado_mujeres_Sin_instrucción[[4]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[5]<-
  (tabla_estado_mujeres_Sin_instrucción[[5]]$Freq[tabla_estado_mujeres_Sin_instrucción[[5]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[5]]$Freq[tabla_estado_mujeres_Sin_instrucción[[5]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[5]]$Freq[tabla_estado_mujeres_Sin_instrucción[[5]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[6]<-
  (tabla_estado_mujeres_Sin_instrucción[[6]]$Freq[tabla_estado_mujeres_Sin_instrucción[[6]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[6]]$Freq[tabla_estado_mujeres_Sin_instrucción[[6]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[6]]$Freq[tabla_estado_mujeres_Sin_instrucción[[6]]$Var1=="Sum"]

## alerta en 7      
tasas_actividad_mujeres_Sin_instrucción[7]<-
  (tabla_estado_mujeres_Sin_instrucción[[7]]$Freq[tabla_estado_mujeres_Sin_instrucción[[7]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[7]]$Freq[tabla_estado_mujeres_Sin_instrucción[[7]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[7]]$Freq[tabla_estado_mujeres_Sin_instrucción[[7]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[8]<-
  (tabla_estado_mujeres_Sin_instrucción[[8]]$Freq[tabla_estado_mujeres_Sin_instrucción[[8]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[8]]$Freq[tabla_estado_mujeres_Sin_instrucción[[8]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[8]]$Freq[tabla_estado_mujeres_Sin_instrucción[[8]]$Var1=="Sum"]

# otro en 9
tasas_actividad_mujeres_Sin_instrucción[9]<-
  (tabla_estado_mujeres_Sin_instrucción[[9]]$Freq[tabla_estado_mujeres_Sin_instrucción[[9]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[9]]$Freq[tabla_estado_mujeres_Sin_instrucción[[9]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[9]]$Freq[tabla_estado_mujeres_Sin_instrucción[[9]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[10]<-
  (tabla_estado_mujeres_Sin_instrucción[[10]]$Freq[tabla_estado_mujeres_Sin_instrucción[[10]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[10]]$Freq[tabla_estado_mujeres_Sin_instrucción[[10]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[10]]$Freq[tabla_estado_mujeres_Sin_instrucción[[10]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[11]<-
  (tabla_estado_mujeres_Sin_instrucción[[11]]$Freq[tabla_estado_mujeres_Sin_instrucción[[11]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[11]]$Freq[tabla_estado_mujeres_Sin_instrucción[[11]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[11]]$Freq[tabla_estado_mujeres_Sin_instrucción[[11]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[12]<-
  (tabla_estado_mujeres_Sin_instrucción[[12]]$Freq[tabla_estado_mujeres_Sin_instrucción[[12]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[12]]$Freq[tabla_estado_mujeres_Sin_instrucción[[12]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[12]]$Freq[tabla_estado_mujeres_Sin_instrucción[[12]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[13]<-
  (tabla_estado_mujeres_Sin_instrucción[[13]]$Freq[tabla_estado_mujeres_Sin_instrucción[[13]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[13]]$Freq[tabla_estado_mujeres_Sin_instrucción[[13]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[13]]$Freq[tabla_estado_mujeres_Sin_instrucción[[13]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[14]<-
  (tabla_estado_mujeres_Sin_instrucción[[14]]$Freq[tabla_estado_mujeres_Sin_instrucción[[14]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[14]]$Freq[tabla_estado_mujeres_Sin_instrucción[[14]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[14]]$Freq[tabla_estado_mujeres_Sin_instrucción[[14]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[15]<-
  (tabla_estado_mujeres_Sin_instrucción[[15]]$Freq[tabla_estado_mujeres_Sin_instrucción[[15]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[15]]$Freq[tabla_estado_mujeres_Sin_instrucción[[15]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[15]]$Freq[tabla_estado_mujeres_Sin_instrucción[[15]]$Var1=="Sum"]


# en los años 7, 9, 10 y 13 (2010, 2012, 2013, 2016) no hay mujeres sin instrucción
#  se los vuelve cero
tasas_actividad_mujeres_Sin_instrucción[
  is.na(tasas_actividad_mujeres_Sin_instrucción)==TRUE]<-0

# varones_Primaria_Incompleta
tasas_actividad_varones_Primaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Primaria_Incompleta[i]<-
    (tabla_estado_varones_Primaria_Incompleta[[i]]$Freq[tabla_estado_varones_Primaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_varones_Primaria_Incompleta[[i]]$Freq[tabla_estado_varones_Primaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_varones_Primaria_Incompleta[[i]]$Freq[tabla_estado_varones_Primaria_Incompleta[[i]]$Var1=="Sum"]
}

# mujeres_Primaria_Incompleta
tasas_actividad_mujeres_Primaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Primaria_Incompleta[i]<-
    (tabla_estado_mujeres_Primaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Primaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_mujeres_Primaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Primaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_mujeres_Primaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Primaria_Incompleta[[i]]$Var1=="Sum"]
}


# varones_Primaria_Completa
tasas_actividad_varones_Primaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Primaria_Completa[i]<-
    (tabla_estado_varones_Primaria_Completa[[i]]$Freq[tabla_estado_varones_Primaria_Completa[[i]]$Var1==1]+
       tabla_estado_varones_Primaria_Completa[[i]]$Freq[tabla_estado_varones_Primaria_Completa[[i]]$Var1==2])/
    tabla_estado_varones_Primaria_Completa[[i]]$Freq[tabla_estado_varones_Primaria_Completa[[i]]$Var1=="Sum"]
}

# mujeres_Primaria_Completa
tasas_actividad_mujeres_Primaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Primaria_Completa[i]<-
    (tabla_estado_mujeres_Primaria_Completa[[i]]$Freq[tabla_estado_mujeres_Primaria_Completa[[i]]$Var1==1]+
       tabla_estado_mujeres_Primaria_Completa[[i]]$Freq[tabla_estado_mujeres_Primaria_Completa[[i]]$Var1==2])/
    tabla_estado_mujeres_Primaria_Completa[[i]]$Freq[tabla_estado_mujeres_Primaria_Completa[[i]]$Var1=="Sum"]
}

# varones_Secundaria_Incompleta
tasas_actividad_varones_Secundaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Secundaria_Incompleta[i]<-
    (tabla_estado_varones_Secundaria_Incompleta[[i]]$Freq[tabla_estado_varones_Secundaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_varones_Secundaria_Incompleta[[i]]$Freq[tabla_estado_varones_Secundaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_varones_Secundaria_Incompleta[[i]]$Freq[tabla_estado_varones_Secundaria_Incompleta[[i]]$Var1=="Sum"]
}


# mujeres_Secundaria_Incompleta
tasas_actividad_mujeres_Secundaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Secundaria_Incompleta[i]<-
    (tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Var1=="Sum"]
}


# varones_Secundaria_Completa
tasas_actividad_varones_Secundaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Secundaria_Completa[i]<-
    (tabla_estado_varones_Secundaria_Completa[[i]]$Freq[tabla_estado_varones_Secundaria_Completa[[i]]$Var1==1]+
       tabla_estado_varones_Secundaria_Completa[[i]]$Freq[tabla_estado_varones_Secundaria_Completa[[i]]$Var1==2])/
    tabla_estado_varones_Secundaria_Completa[[i]]$Freq[tabla_estado_varones_Secundaria_Completa[[i]]$Var1=="Sum"]
}

# mujeres_Secundaria_Completa
tasas_actividad_mujeres_Secundaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Secundaria_Completa[i]<-
    (tabla_estado_mujeres_Secundaria_Completa[[i]]$Freq[tabla_estado_mujeres_Secundaria_Completa[[i]]$Var1==1]+
       tabla_estado_mujeres_Secundaria_Completa[[i]]$Freq[tabla_estado_mujeres_Secundaria_Completa[[i]]$Var1==2])/
    tabla_estado_mujeres_Secundaria_Completa[[i]]$Freq[tabla_estado_mujeres_Secundaria_Completa[[i]]$Var1=="Sum"]
}

# varones_Universitaria_Incompleta
tasas_actividad_varones_Universitaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Universitaria_Incompleta[i]<-
    (tabla_estado_varones_Universitaria_Incompleta[[i]]$Freq[tabla_estado_varones_Universitaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_varones_Universitaria_Incompleta[[i]]$Freq[tabla_estado_varones_Universitaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_varones_Universitaria_Incompleta[[i]]$Freq[tabla_estado_varones_Universitaria_Incompleta[[i]]$Var1=="Sum"]
}

# mujeres_Universitaria_Incompleta
tasas_actividad_mujeres_Universitaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Universitaria_Incompleta[i]<-
    (tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Var1=="Sum"]
}

# varones_Universitaria_Completa
tasas_actividad_varones_Universitaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Universitaria_Completa[i]<-
    (tabla_estado_varones_Universitaria_Completa[[i]]$Freq[tabla_estado_varones_Universitaria_Completa[[i]]$Var1==1]+
       tabla_estado_varones_Universitaria_Completa[[i]]$Freq[tabla_estado_varones_Universitaria_Completa[[i]]$Var1==2])/
    tabla_estado_varones_Universitaria_Completa[[i]]$Freq[tabla_estado_varones_Universitaria_Completa[[i]]$Var1=="Sum"]
}

# mujeres_Universitaria_Completa
tasas_actividad_mujeres_Universitaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Universitaria_Completa[i]<-
    (tabla_estado_mujeres_Universitaria_Completa[[i]]$Freq[tabla_estado_mujeres_Universitaria_Completa[[i]]$Var1==1]+
       tabla_estado_mujeres_Universitaria_Completa[[i]]$Freq[tabla_estado_mujeres_Universitaria_Completa[[i]]$Var1==2])/
    tabla_estado_mujeres_Universitaria_Completa[[i]]$Freq[tabla_estado_mujeres_Universitaria_Completa[[i]]$Var1=="Sum"]
}

anio<-2004:2018
# Se construye la matriz
matriz_tasas_actividad<-data.frame(anio, tasas_actividad, tasas_actividad_varones, tasas_actividad_mujeres,
                                   tasas_actividad_varones_Sin_instrucción, tasas_actividad_mujeres_Sin_instrucción,
                                   tasas_actividad_varones_Primaria_Incompleta, tasas_actividad_mujeres_Primaria_Incompleta,
                                   tasas_actividad_varones_Primaria_Completa,
                                   tasas_actividad_mujeres_Primaria_Completa,
                                   tasas_actividad_varones_Secundaria_Incompleta,
                                   tasas_actividad_mujeres_Secundaria_Incompleta,
                                   tasas_actividad_varones_Secundaria_Completa,
                                   tasas_actividad_mujeres_Secundaria_Completa,
                                   tasas_actividad_varones_Universitaria_Incompleta,
                                   tasas_actividad_mujeres_Universitaria_Incompleta,
                                   tasas_actividad_varones_Universitaria_Completa,
                                   tasas_actividad_mujeres_Universitaria_Completa)

ggplot(matriz_tasas_actividad)+
  geom_point(aes(anio, tasas_actividad))+ylim(0.25,.75)


cols <- c("varones"="green","mujeres"="red")
ggplot(matriz_tasas_actividad)+
  geom_point(aes(anio, tasas_actividad_varones), col="green")+
  geom_point(aes(anio, tasas_actividad_mujeres), col="red")+ylim(0.3,.6)+
  scale_color_manual(name="sexo",values=cols,
                     guide = guide_legend())

# es un formato incómodo, cambia el formato
tasas<-c(tasas_actividad, tasas_actividad_varones, tasas_actividad_mujeres,
         tasas_actividad_varones_Sin_instrucción, tasas_actividad_mujeres_Sin_instrucción,
         tasas_actividad_varones_Primaria_Incompleta, tasas_actividad_mujeres_Primaria_Incompleta,
         tasas_actividad_varones_Primaria_Completa,
         tasas_actividad_mujeres_Primaria_Completa,
         tasas_actividad_varones_Secundaria_Incompleta,
         tasas_actividad_mujeres_Secundaria_Incompleta,
         tasas_actividad_varones_Secundaria_Completa,
         tasas_actividad_mujeres_Secundaria_Completa,
         tasas_actividad_varones_Universitaria_Incompleta,
         tasas_actividad_mujeres_Universitaria_Incompleta,
         tasas_actividad_varones_Universitaria_Completa,
         tasas_actividad_mujeres_Universitaria_Completa)

sexo<-c(rep("ambos",15),rep(c(rep("varones",15), rep("mujeres",15)),8))

educacion<-c(rep("todos",45), rep("sin instrucción",30),
             rep("primaria incompleta",30), rep("primaria completa",30),
             rep("secundaria incompleta",30), rep("secundaria completa",30),
             rep("universitaria incompleta",30), rep("universitaria completa",30))

anio<-c(rep(2004:2018, 17))

tasas_de_actividad<-data.frame(anio, sexo, educacion,tasas)


class(tasas_de_actividad$educacion)
levels(tasas_de_actividad$educacion)

tasas_de_actividad$educacion_ordenada<-tasas_de_actividad$educacion

tasas_de_actividad$educacion_ordenada <- 
  factor(tasas_de_actividad$educacion_ordenada,
         levels = c("sin instrucción", "primaria incompleta", "primaria completa",
                    "secundaria incompleta", "secundaria completa",
                    "universitaria incompleta", "universitaria completa", "todos"))

table(tasas_de_actividad$educacion, tasas_de_actividad$educacion_ordenada)

## gráficos

ggplot(subset(tasas_de_actividad, tasas_de_actividad$sexo!="ambos"))+
  geom_bar(aes(sexo, tasas), stat = "identity")+
  facet_grid(anio~educacion_ordenada)


ggplot(subset(tasas_de_actividad,
              tasas_de_actividad$sexo=="ambos" &
                tasas_de_actividad$educacion=="todos"))+
  geom_point(aes(anio, tasas))+ylim(.3,.6)

ggplot(subset(tasas_de_actividad,
              tasas_de_actividad$sexo!="ambos" &
                tasas_de_actividad$educacion=="todos"))+
  geom_point(aes(anio, tasas, col=sexo))+ylim(.1,.9)

# este está bueno:
ggplot(subset(tasas_de_actividad,
              tasas_de_actividad$sexo!="ambos"))+
  geom_point(aes(anio, tasas, col=sexo))+
  geom_line(aes(anio, tasas, col=sexo))+
  facet_grid(educacion_ordenada~.)+theme_tufte()


## todo se repite con edad 18+ ### desde acá referenciar

eph_mayor17<-vector("list",15)

for (i in 1:15) {
  eph_mayor17[[i]]=subset(eph[[i]], eph[[i]]$CH06>17)
}

class(eph_mayor17[[14]]$CH06)
summary(eph_mayor17[[14]]$CH06)

originales<-vector("list",15)
originales<-eph_mayor17
class(eph_mayor17[[13]])
for (i in 1:15) {
  originales[[i]]$sexo=as.factor(originales[[i]]$CH04)
  levels(originales[[i]]$sexo)=c("varones", "mujeres")
  originales[[i]]$educacion=factor(originales[[i]]$NIVEL_ED,
                                   levels(factor(originales[[i]]$NIVEL_ED))[c(7,1:6)])
  levels(originales[[i]]$educacion)=c("Sin instrucción",
                                      "Primaria Incompleta",
                                      "Primaria Completa","Secundaria Incompleta", "Secundaria Completa",
                                      "Universitaria Incompleta","Universitaria Completa")
}

# por la definición INDEC, la tasa es sobre la población
# total, por eso quedan los menores de 10.
# pero en "sin instrucción", hay muchos y la tasa da muy baja
# habrá que aclararlo
#cálculo de las tasas:
# definir los u como as.data.frame de las wtd.table de estado
# calcular las tasas operando sobre los elementos de u
# y servirán también para desocupación
actividad<-(u$Freq[u$Var1==1]+u$Freq[u$Var1==2])/u$Freq[u$Var1=="Sum"]

# los subconjuntos están perfectos
# los subsets de originales
varones<-vector("list",15)
mujeres<-vector("list",15)

varones_Sin_instrucción<-vector("list",15)
varones_Primaria_Incompleta<-vector("list",15)
varones_Primaria_Completa<-vector("list",15)
varones_Secundaria_Incompleta<-vector("list",15)
varones_Secundaria_Completa<-vector("list",15)
varones_Universitaria_Incompleta<-vector("list",15)
varones_Universitaria_Completa<-vector("list",15)
mujeres_Sin_instrucción<-vector("list",15)
mujeres_Primaria_Incompleta<-vector("list",15)
mujeres_Primaria_Completa<-vector("list",15)
mujeres_Secundaria_Incompleta<-vector("list",15)
mujeres_Secundaria_Completa<-vector("list",15)
mujeres_Universitaria_Incompleta<-vector("list",15)
mujeres_Universitaria_Completa<-vector("list",15)

for (i in 1:15) {
  varones[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones")
}
for (i in 1:15) {
  mujeres[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres")
}

for (i in 1:15) {
  varones_Sin_instrucción[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Sin instrucción")
}

for (i in 1:15) {
  mujeres_Sin_instrucción[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Sin instrucción")
}

for (i in 1:15) {
  varones_Primaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Primaria Incompleta")
}

for (i in 1:15) {
  mujeres_Primaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Primaria Incompleta")
}

for (i in 1:15) {
  varones_Primaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Primaria Completa")
}

for (i in 1:15) {
  mujeres_Primaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Primaria Completa")
}

for (i in 1:15) {
  varones_Secundaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Secundaria Incompleta")
}

for (i in 1:15) {
  mujeres_Secundaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Secundaria Incompleta")
}

for (i in 1:15) {
  varones_Secundaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Secundaria Completa")
}

for (i in 1:15) {
  mujeres_Secundaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Secundaria Completa")
}

for (i in 1:15) {
  varones_Universitaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Universitaria Incompleta")
}

for (i in 1:15) {
  mujeres_Universitaria_Incompleta[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Universitaria Incompleta")
}

for (i in 1:15) {
  varones_Universitaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="varones" &
             originales[[i]]$educacion=="Universitaria Completa")
}

for (i in 1:15) {
  mujeres_Universitaria_Completa[[i]]<-
    subset(originales[[i]],
           originales[[i]]$sexo=="mujeres" &
             originales[[i]]$educacion=="Universitaria Completa")
}



# Se definen las 17 listas cada una con 15 tablas de estado

# todos
tabla_estado<-vector("list",15)
for (i in 1:15) {
  tabla_estado[[i]]<-addmargins(
    wtd.table(
      originales[[i]]$ESTADO, weights = originales[[i]]$PONDERA))
  tabla_estado[[i]]<-as.data.frame(tabla_estado[[i]])
}

# varones
tabla_estado_varones<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones[[i]]<-addmargins(
    wtd.table(
      varones[[i]]$ESTADO, weights = varones[[i]]$PONDERA))
  tabla_estado_varones[[i]]<-as.data.frame(tabla_estado_varones[[i]])
}

# mujeres
tabla_estado_mujeres<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres[[i]]<-addmargins(
    wtd.table(
      mujeres[[i]]$ESTADO, weights = mujeres[[i]]$PONDERA))
  tabla_estado_mujeres[[i]]<-as.data.frame(tabla_estado_mujeres[[i]])
}

# varones_Sin_instrucción
tabla_estado_varones_Sin_instrucción<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Sin_instrucción[[i]]<-addmargins(
    wtd.table(
      varones_Sin_instrucción[[i]]$ESTADO, weights = varones_Sin_instrucción[[i]]$PONDERA))
  tabla_estado_varones_Sin_instrucción[[i]]<-as.data.frame(tabla_estado_varones_Sin_instrucción[[i]])
}

# mujeres_Sin_instrucción
tabla_estado_mujeres_Sin_instrucción<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Sin_instrucción[[i]]<-addmargins(
    wtd.table(
      mujeres_Sin_instrucción[[i]]$ESTADO, weights = mujeres_Sin_instrucción[[i]]$PONDERA))
  tabla_estado_mujeres_Sin_instrucción[[i]]<-as.data.frame(tabla_estado_mujeres_Sin_instrucción[[i]])
}

# varones_Primaria_Incompleta
tabla_estado_varones_Primaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Primaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      varones_Primaria_Incompleta[[i]]$ESTADO, weights = varones_Primaria_Incompleta[[i]]$PONDERA))
  tabla_estado_varones_Primaria_Incompleta[[i]]<-as.data.frame(tabla_estado_varones_Primaria_Incompleta[[i]])
}

# mujeres_Primaria_Incompleta
tabla_estado_mujeres_Primaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Primaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      mujeres_Primaria_Incompleta[[i]]$ESTADO, weights = mujeres_Primaria_Incompleta[[i]]$PONDERA))
  tabla_estado_mujeres_Primaria_Incompleta[[i]]<-as.data.frame(tabla_estado_mujeres_Primaria_Incompleta[[i]])
}

# varones_Primaria_Completa
tabla_estado_varones_Primaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Primaria_Completa[[i]]<-addmargins(
    wtd.table(
      varones_Primaria_Completa[[i]]$ESTADO, weights = varones_Primaria_Completa[[i]]$PONDERA))
  tabla_estado_varones_Primaria_Completa[[i]]<-as.data.frame(tabla_estado_varones_Primaria_Completa[[i]])
}

# mujeres_Primaria_Completa
tabla_estado_mujeres_Primaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Primaria_Completa[[i]]<-addmargins(
    wtd.table(
      mujeres_Primaria_Completa[[i]]$ESTADO, weights = mujeres_Primaria_Completa[[i]]$PONDERA))
  tabla_estado_mujeres_Primaria_Completa[[i]]<-as.data.frame(tabla_estado_mujeres_Primaria_Completa[[i]])
}

# varones_Secundaria_Incompleta
tabla_estado_varones_Secundaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Secundaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      varones_Secundaria_Incompleta[[i]]$ESTADO, weights = varones_Secundaria_Incompleta[[i]]$PONDERA))
  tabla_estado_varones_Secundaria_Incompleta[[i]]<-as.data.frame(tabla_estado_varones_Secundaria_Incompleta[[i]])
}

# mujeres_Secundaria_Incompleta
tabla_estado_mujeres_Secundaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Secundaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      mujeres_Secundaria_Incompleta[[i]]$ESTADO, weights = mujeres_Secundaria_Incompleta[[i]]$PONDERA))
  tabla_estado_mujeres_Secundaria_Incompleta[[i]]<-as.data.frame(tabla_estado_mujeres_Secundaria_Incompleta[[i]])
}

# varones_Secundaria_Completa
tabla_estado_varones_Secundaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Secundaria_Completa[[i]]<-addmargins(
    wtd.table(
      varones_Secundaria_Completa[[i]]$ESTADO, weights = varones_Secundaria_Completa[[i]]$PONDERA))
  tabla_estado_varones_Secundaria_Completa[[i]]<-as.data.frame(tabla_estado_varones_Secundaria_Completa[[i]])
}

# mujeres_Secundaria_Completa
tabla_estado_mujeres_Secundaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Secundaria_Completa[[i]]<-addmargins(
    wtd.table(
      mujeres_Secundaria_Completa[[i]]$ESTADO, weights = mujeres_Secundaria_Completa[[i]]$PONDERA))
  tabla_estado_mujeres_Secundaria_Completa[[i]]<-as.data.frame(tabla_estado_mujeres_Secundaria_Completa[[i]])
}

# varones_Universitaria_Incompleta
tabla_estado_varones_Universitaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Universitaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      varones_Universitaria_Incompleta[[i]]$ESTADO, weights = varones_Universitaria_Incompleta[[i]]$PONDERA))
  tabla_estado_varones_Universitaria_Incompleta[[i]]<-as.data.frame(tabla_estado_varones_Universitaria_Incompleta[[i]])
}

# mujeres_Universitaria_Incompleta
tabla_estado_mujeres_Universitaria_Incompleta<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Universitaria_Incompleta[[i]]<-addmargins(
    wtd.table(
      mujeres_Universitaria_Incompleta[[i]]$ESTADO, weights = mujeres_Universitaria_Incompleta[[i]]$PONDERA))
  tabla_estado_mujeres_Universitaria_Incompleta[[i]]<-as.data.frame(tabla_estado_mujeres_Universitaria_Incompleta[[i]])
}

# varones_Universitaria_Completa
tabla_estado_varones_Universitaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_varones_Universitaria_Completa[[i]]<-addmargins(
    wtd.table(
      varones_Universitaria_Completa[[i]]$ESTADO, weights = varones_Universitaria_Completa[[i]]$PONDERA))
  tabla_estado_varones_Universitaria_Completa[[i]]<-as.data.frame(tabla_estado_varones_Universitaria_Completa[[i]])
}

# mujeres_Universitaria_Completa
tabla_estado_mujeres_Universitaria_Completa<-vector("list",15)
for (i in 1:15) {
  tabla_estado_mujeres_Universitaria_Completa[[i]]<-addmargins(
    wtd.table(
      mujeres_Universitaria_Completa[[i]]$ESTADO, weights = mujeres_Universitaria_Completa[[i]]$PONDERA))
  tabla_estado_mujeres_Universitaria_Completa[[i]]<-as.data.frame(tabla_estado_mujeres_Universitaria_Completa[[i]])
}

# Vectores con las 15 tasas cada uno
# todos
tasas_actividad<-vector()
for (i in 1:15) {
  tasas_actividad[i]<-
    (tabla_estado[[i]]$Freq[tabla_estado[[i]]$Var1==1]+
       tabla_estado[[i]]$Freq[tabla_estado[[i]]$Var1==2])/
    tabla_estado[[i]]$Freq[tabla_estado[[i]]$Var1=="Sum"]
}

# varones
tasas_actividad_varones<-vector()
for (i in 1:15) {
  tasas_actividad_varones[i]<-
    (tabla_estado_varones[[i]]$Freq[tabla_estado_varones[[i]]$Var1==1]+
       tabla_estado_varones[[i]]$Freq[tabla_estado_varones[[i]]$Var1==2])/
    tabla_estado_varones[[i]]$Freq[tabla_estado_varones[[i]]$Var1=="Sum"]
}

# mujeres
tasas_actividad_mujeres<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres[i]<-
    (tabla_estado_mujeres[[i]]$Freq[tabla_estado_mujeres[[i]]$Var1==1]+
       tabla_estado_mujeres[[i]]$Freq[tabla_estado_mujeres[[i]]$Var1==2])/
    tabla_estado_mujeres[[i]]$Freq[tabla_estado_mujeres[[i]]$Var1=="Sum"]
}

# varones_Sin_instrucción
tasas_actividad_varones_Sin_instrucción<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Sin_instrucción[i]<-
    (tabla_estado_varones_Sin_instrucción[[i]]$Freq[tabla_estado_varones_Sin_instrucción[[i]]$Var1==1]+
       tabla_estado_varones_Sin_instrucción[[i]]$Freq[tabla_estado_varones_Sin_instrucción[[i]]$Var1==2])/
    tabla_estado_varones_Sin_instrucción[[i]]$Freq[tabla_estado_varones_Sin_instrucción[[i]]$Var1=="Sum"]
}

# mujeres_Sin_instrucción
# se construyen individualmente, porque hay cuatro años en los que, de las
# mujeres sin instrucciónn, ninguna trabajó
tasas_actividad_mujeres_Sin_instrucción<-vector()

tasas_actividad_mujeres_Sin_instrucción[1]<-
  (tabla_estado_mujeres_Sin_instrucción[[1]]$Freq[tabla_estado_mujeres_Sin_instrucción[[1]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[1]]$Freq[tabla_estado_mujeres_Sin_instrucción[[1]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[1]]$Freq[tabla_estado_mujeres_Sin_instrucción[[1]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[2]<-
  (tabla_estado_mujeres_Sin_instrucción[[2]]$Freq[tabla_estado_mujeres_Sin_instrucción[[2]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[2]]$Freq[tabla_estado_mujeres_Sin_instrucción[[2]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[2]]$Freq[tabla_estado_mujeres_Sin_instrucción[[2]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[3]<-
  (tabla_estado_mujeres_Sin_instrucción[[3]]$Freq[tabla_estado_mujeres_Sin_instrucción[[3]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[3]]$Freq[tabla_estado_mujeres_Sin_instrucción[[3]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[3]]$Freq[tabla_estado_mujeres_Sin_instrucción[[3]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[4]<-
  (tabla_estado_mujeres_Sin_instrucción[[4]]$Freq[tabla_estado_mujeres_Sin_instrucción[[4]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[4]]$Freq[tabla_estado_mujeres_Sin_instrucción[[4]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[4]]$Freq[tabla_estado_mujeres_Sin_instrucción[[4]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[5]<-
  (tabla_estado_mujeres_Sin_instrucción[[5]]$Freq[tabla_estado_mujeres_Sin_instrucción[[5]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[5]]$Freq[tabla_estado_mujeres_Sin_instrucción[[5]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[5]]$Freq[tabla_estado_mujeres_Sin_instrucción[[5]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[6]<-
  (tabla_estado_mujeres_Sin_instrucción[[6]]$Freq[tabla_estado_mujeres_Sin_instrucción[[6]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[6]]$Freq[tabla_estado_mujeres_Sin_instrucción[[6]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[6]]$Freq[tabla_estado_mujeres_Sin_instrucción[[6]]$Var1=="Sum"]

## alerta en 7      
tasas_actividad_mujeres_Sin_instrucción[7]<-
  (tabla_estado_mujeres_Sin_instrucción[[7]]$Freq[tabla_estado_mujeres_Sin_instrucción[[7]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[7]]$Freq[tabla_estado_mujeres_Sin_instrucción[[7]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[7]]$Freq[tabla_estado_mujeres_Sin_instrucción[[7]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[8]<-
  (tabla_estado_mujeres_Sin_instrucción[[8]]$Freq[tabla_estado_mujeres_Sin_instrucción[[8]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[8]]$Freq[tabla_estado_mujeres_Sin_instrucción[[8]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[8]]$Freq[tabla_estado_mujeres_Sin_instrucción[[8]]$Var1=="Sum"]

# otro en 9
tasas_actividad_mujeres_Sin_instrucción[9]<-
  (tabla_estado_mujeres_Sin_instrucción[[9]]$Freq[tabla_estado_mujeres_Sin_instrucción[[9]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[9]]$Freq[tabla_estado_mujeres_Sin_instrucción[[9]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[9]]$Freq[tabla_estado_mujeres_Sin_instrucción[[9]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[10]<-
  (tabla_estado_mujeres_Sin_instrucción[[10]]$Freq[tabla_estado_mujeres_Sin_instrucción[[10]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[10]]$Freq[tabla_estado_mujeres_Sin_instrucción[[10]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[10]]$Freq[tabla_estado_mujeres_Sin_instrucción[[10]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[11]<-
  (tabla_estado_mujeres_Sin_instrucción[[11]]$Freq[tabla_estado_mujeres_Sin_instrucción[[11]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[11]]$Freq[tabla_estado_mujeres_Sin_instrucción[[11]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[11]]$Freq[tabla_estado_mujeres_Sin_instrucción[[11]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[12]<-
  (tabla_estado_mujeres_Sin_instrucción[[12]]$Freq[tabla_estado_mujeres_Sin_instrucción[[12]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[12]]$Freq[tabla_estado_mujeres_Sin_instrucción[[12]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[12]]$Freq[tabla_estado_mujeres_Sin_instrucción[[12]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[13]<-
  (tabla_estado_mujeres_Sin_instrucción[[13]]$Freq[tabla_estado_mujeres_Sin_instrucción[[13]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[13]]$Freq[tabla_estado_mujeres_Sin_instrucción[[13]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[13]]$Freq[tabla_estado_mujeres_Sin_instrucción[[13]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[14]<-
  (tabla_estado_mujeres_Sin_instrucción[[14]]$Freq[tabla_estado_mujeres_Sin_instrucción[[14]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[14]]$Freq[tabla_estado_mujeres_Sin_instrucción[[14]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[14]]$Freq[tabla_estado_mujeres_Sin_instrucción[[14]]$Var1=="Sum"]

tasas_actividad_mujeres_Sin_instrucción[15]<-
  (tabla_estado_mujeres_Sin_instrucción[[15]]$Freq[tabla_estado_mujeres_Sin_instrucción[[15]]$Var1==1]+
     tabla_estado_mujeres_Sin_instrucción[[15]]$Freq[tabla_estado_mujeres_Sin_instrucción[[15]]$Var1==2])/
  tabla_estado_mujeres_Sin_instrucción[[15]]$Freq[tabla_estado_mujeres_Sin_instrucción[[15]]$Var1=="Sum"]


# en los años 7, 9, 10 y 13 (2010, 2012, 2013, 2016) no hay mujeres sin instrucción
#  se los vuelve cero
tasas_actividad_mujeres_Sin_instrucción[
  is.na(tasas_actividad_mujeres_Sin_instrucción)==TRUE]<-0

# varones_Primaria_Incompleta
tasas_actividad_varones_Primaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Primaria_Incompleta[i]<-
    (tabla_estado_varones_Primaria_Incompleta[[i]]$Freq[tabla_estado_varones_Primaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_varones_Primaria_Incompleta[[i]]$Freq[tabla_estado_varones_Primaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_varones_Primaria_Incompleta[[i]]$Freq[tabla_estado_varones_Primaria_Incompleta[[i]]$Var1=="Sum"]
}

# mujeres_Primaria_Incompleta
tasas_actividad_mujeres_Primaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Primaria_Incompleta[i]<-
    (tabla_estado_mujeres_Primaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Primaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_mujeres_Primaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Primaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_mujeres_Primaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Primaria_Incompleta[[i]]$Var1=="Sum"]
}


# varones_Primaria_Completa
tasas_actividad_varones_Primaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Primaria_Completa[i]<-
    (tabla_estado_varones_Primaria_Completa[[i]]$Freq[tabla_estado_varones_Primaria_Completa[[i]]$Var1==1]+
       tabla_estado_varones_Primaria_Completa[[i]]$Freq[tabla_estado_varones_Primaria_Completa[[i]]$Var1==2])/
    tabla_estado_varones_Primaria_Completa[[i]]$Freq[tabla_estado_varones_Primaria_Completa[[i]]$Var1=="Sum"]
}

# mujeres_Primaria_Completa
tasas_actividad_mujeres_Primaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Primaria_Completa[i]<-
    (tabla_estado_mujeres_Primaria_Completa[[i]]$Freq[tabla_estado_mujeres_Primaria_Completa[[i]]$Var1==1]+
       tabla_estado_mujeres_Primaria_Completa[[i]]$Freq[tabla_estado_mujeres_Primaria_Completa[[i]]$Var1==2])/
    tabla_estado_mujeres_Primaria_Completa[[i]]$Freq[tabla_estado_mujeres_Primaria_Completa[[i]]$Var1=="Sum"]
}

# varones_Secundaria_Incompleta
tasas_actividad_varones_Secundaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Secundaria_Incompleta[i]<-
    (tabla_estado_varones_Secundaria_Incompleta[[i]]$Freq[tabla_estado_varones_Secundaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_varones_Secundaria_Incompleta[[i]]$Freq[tabla_estado_varones_Secundaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_varones_Secundaria_Incompleta[[i]]$Freq[tabla_estado_varones_Secundaria_Incompleta[[i]]$Var1=="Sum"]
}


# mujeres_Secundaria_Incompleta
tasas_actividad_mujeres_Secundaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Secundaria_Incompleta[i]<-
    (tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Secundaria_Incompleta[[i]]$Var1=="Sum"]
}


# varones_Secundaria_Completa
tasas_actividad_varones_Secundaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Secundaria_Completa[i]<-
    (tabla_estado_varones_Secundaria_Completa[[i]]$Freq[tabla_estado_varones_Secundaria_Completa[[i]]$Var1==1]+
       tabla_estado_varones_Secundaria_Completa[[i]]$Freq[tabla_estado_varones_Secundaria_Completa[[i]]$Var1==2])/
    tabla_estado_varones_Secundaria_Completa[[i]]$Freq[tabla_estado_varones_Secundaria_Completa[[i]]$Var1=="Sum"]
}

# mujeres_Secundaria_Completa
tasas_actividad_mujeres_Secundaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Secundaria_Completa[i]<-
    (tabla_estado_mujeres_Secundaria_Completa[[i]]$Freq[tabla_estado_mujeres_Secundaria_Completa[[i]]$Var1==1]+
       tabla_estado_mujeres_Secundaria_Completa[[i]]$Freq[tabla_estado_mujeres_Secundaria_Completa[[i]]$Var1==2])/
    tabla_estado_mujeres_Secundaria_Completa[[i]]$Freq[tabla_estado_mujeres_Secundaria_Completa[[i]]$Var1=="Sum"]
}

# varones_Universitaria_Incompleta
tasas_actividad_varones_Universitaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Universitaria_Incompleta[i]<-
    (tabla_estado_varones_Universitaria_Incompleta[[i]]$Freq[tabla_estado_varones_Universitaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_varones_Universitaria_Incompleta[[i]]$Freq[tabla_estado_varones_Universitaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_varones_Universitaria_Incompleta[[i]]$Freq[tabla_estado_varones_Universitaria_Incompleta[[i]]$Var1=="Sum"]
}

# mujeres_Universitaria_Incompleta
tasas_actividad_mujeres_Universitaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Universitaria_Incompleta[i]<-
    (tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Var1==1]+
       tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Var1==2])/
    tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Freq[tabla_estado_mujeres_Universitaria_Incompleta[[i]]$Var1=="Sum"]
}

# varones_Universitaria_Completa
tasas_actividad_varones_Universitaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Universitaria_Completa[i]<-
    (tabla_estado_varones_Universitaria_Completa[[i]]$Freq[tabla_estado_varones_Universitaria_Completa[[i]]$Var1==1]+
       tabla_estado_varones_Universitaria_Completa[[i]]$Freq[tabla_estado_varones_Universitaria_Completa[[i]]$Var1==2])/
    tabla_estado_varones_Universitaria_Completa[[i]]$Freq[tabla_estado_varones_Universitaria_Completa[[i]]$Var1=="Sum"]
}

# mujeres_Universitaria_Completa
tasas_actividad_mujeres_Universitaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Universitaria_Completa[i]<-
    (tabla_estado_mujeres_Universitaria_Completa[[i]]$Freq[tabla_estado_mujeres_Universitaria_Completa[[i]]$Var1==1]+
       tabla_estado_mujeres_Universitaria_Completa[[i]]$Freq[tabla_estado_mujeres_Universitaria_Completa[[i]]$Var1==2])/
    tabla_estado_mujeres_Universitaria_Completa[[i]]$Freq[tabla_estado_mujeres_Universitaria_Completa[[i]]$Var1=="Sum"]
}

anio<-2004:2018
# Se construye la matriz
matriz_tasas_actividad<-data.frame(anio, tasas_actividad, tasas_actividad_varones, tasas_actividad_mujeres,
                                   tasas_actividad_varones_Sin_instrucción, tasas_actividad_mujeres_Sin_instrucción,
                                   tasas_actividad_varones_Primaria_Incompleta, tasas_actividad_mujeres_Primaria_Incompleta,
                                   tasas_actividad_varones_Primaria_Completa,
                                   tasas_actividad_mujeres_Primaria_Completa,
                                   tasas_actividad_varones_Secundaria_Incompleta,
                                   tasas_actividad_mujeres_Secundaria_Incompleta,
                                   tasas_actividad_varones_Secundaria_Completa,
                                   tasas_actividad_mujeres_Secundaria_Completa,
                                   tasas_actividad_varones_Universitaria_Incompleta,
                                   tasas_actividad_mujeres_Universitaria_Incompleta,
                                   tasas_actividad_varones_Universitaria_Completa,
                                   tasas_actividad_mujeres_Universitaria_Completa)

ggplot(matriz_tasas_actividad)+
  geom_point(aes(anio, tasas_actividad))+ylim(0.25,.75)


cols <- c("varones"="green","mujeres"="red")
ggplot(matriz_tasas_actividad)+
  geom_point(aes(anio, tasas_actividad_varones), col="green")+
  geom_point(aes(anio, tasas_actividad_mujeres), col="red")+ylim(0.3,.6)+
  scale_color_manual(name="sexo",values=cols,
                     guide = guide_legend())

# es un formato incómodo, cambia el formato
tasas<-c(tasas_actividad, tasas_actividad_varones, tasas_actividad_mujeres,
         tasas_actividad_varones_Sin_instrucción, tasas_actividad_mujeres_Sin_instrucción,
         tasas_actividad_varones_Primaria_Incompleta, tasas_actividad_mujeres_Primaria_Incompleta,
         tasas_actividad_varones_Primaria_Completa,
         tasas_actividad_mujeres_Primaria_Completa,
         tasas_actividad_varones_Secundaria_Incompleta,
         tasas_actividad_mujeres_Secundaria_Incompleta,
         tasas_actividad_varones_Secundaria_Completa,
         tasas_actividad_mujeres_Secundaria_Completa,
         tasas_actividad_varones_Universitaria_Incompleta,
         tasas_actividad_mujeres_Universitaria_Incompleta,
         tasas_actividad_varones_Universitaria_Completa,
         tasas_actividad_mujeres_Universitaria_Completa)

sexo<-c(rep("ambos",15),rep(c(rep("varones",15), rep("mujeres",15)),8))

educacion<-c(rep("todos",45), rep("sin instrucción",30),
             rep("primaria incompleta",30), rep("primaria completa",30),
             rep("secundaria incompleta",30), rep("secundaria completa",30),
             rep("universitaria incompleta",30), rep("universitaria completa",30))

anio<-c(rep(2004:2018, 17))

tasas_de_actividad<-data.frame(anio, sexo, educacion,tasas)


class(tasas_de_actividad$educacion)
levels(tasas_de_actividad$educacion)

tasas_de_actividad$educacion_ordenada<-tasas_de_actividad$educacion

tasas_de_actividad$educacion_ordenada <- 
  factor(tasas_de_actividad$educacion_ordenada,
         levels = c("sin instrucción", "primaria incompleta", "primaria completa",
                    "secundaria incompleta", "secundaria completa",
                    "universitaria incompleta", "universitaria completa", "todos"))

table(tasas_de_actividad$educacion, tasas_de_actividad$educacion_ordenada)

ggplot(subset(tasas_de_actividad,
              tasas_de_actividad$sexo!="ambos"))+
  geom_point(aes(anio, tasas, col=sexo))+
  geom_line(aes(anio, tasas, col=sexo))+
  facet_grid(educacion_ordenada~.)+theme_tufte()

# sin todos
ggplot(subset(tasas_de_actividad,
              tasas_de_actividad$sexo!="ambos" &
                tasas_de_actividad$educacion_ordenada!="todos"))+
  geom_point(aes(anio, tasas, col=sexo))+
  geom_line(aes(anio, tasas, col=sexo))+
  facet_grid(educacion_ordenada~.)+theme_tufte()
