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
tasas_actividad<-vector()
for (i in 1:15) {
  tasas_actividad[i]<-
    (addmargins(
      wtd.table(
        originales[[i]]$ESTADO, weights = originales[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           originales[[i]]$ESTADO, weights = originales[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        originales[[i]]$ESTADO, weights = originales[[i]]$PONDERA))[6]
}

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

# ahora se calculan las tasas de actividad de cada año para cada
# subset y más tarde se reunirán en una sola matriz

# varones
tasas_actividad_varones<-vector()
for (i in 1:15) {
  tasas_actividad_varones[i]<-
    (addmargins(
      wtd.table(
        varones[[i]]$ESTADO, weights = varones[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones[[i]]$ESTADO, weights = varones[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones[[i]]$ESTADO, weights = varones[[i]]$PONDERA))[6]
}
# mujeres
tasas_actividad_mujeres<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres[i]<-
    (addmargins(
      wtd.table(
        mujeres[[i]]$ESTADO, weights = mujeres[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres[[i]]$ESTADO, weights = mujeres[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres[[i]]$ESTADO, weights = mujeres[[i]]$PONDERA))[6]
}

# varones_Sin_instrucción
tasas_actividad_varones_Sin_instrucción<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Sin_instrucción[i]<-
    (addmargins(
      wtd.table(
        varones_Sin_instrucción[[i]]$ESTADO, weights = varones_Sin_instrucción[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones_Sin_instrucción[[i]]$ESTADO, weights = varones_Sin_instrucción[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones_Sin_instrucción[[i]]$ESTADO, weights = varones_Sin_instrucción[[i]]$PONDERA))[6]
}

# mujeres_Sin_intrucción
tasas_actividad_mujeres_Sin_instrucción<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Sin_instrucción[i]<-
    (addmargins(
      wtd.table(
        mujeres_Sin_instrucción[[i]]$ESTADO, weights = mujeres_Sin_instrucción[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres_Sin_instrucción[[i]]$ESTADO, weights = mujeres_Sin_instrucción[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres_Sin_instrucción[[i]]$ESTADO, weights = mujeres_Sin_instrucción[[i]]$PONDERA))[6]
}

# varones_Primaria_Incompleta
tasas_actividad_varones_Primaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Primaria_Incompleta[i]<-
    (addmargins(
      wtd.table(
        varones_Primaria_Incompleta[[i]]$ESTADO, weights = varones_Primaria_Incompleta[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones_Primaria_Incompleta[[i]]$ESTADO, weights = varones_Primaria_Incompleta[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones_Primaria_Incompleta[[i]]$ESTADO, weights = varones_Primaria_Incompleta[[i]]$PONDERA))[6]
}
# mujeres_Primaria_Incompleta
tasas_actividad_mujeres_Primaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Primaria_Incompleta[i]<-
    (addmargins(
      wtd.table(
        mujeres_Primaria_Incompleta[[i]]$ESTADO, weights = mujeres_Primaria_Incompleta[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres_Primaria_Incompleta[[i]]$ESTADO, weights = mujeres_Primaria_Incompleta[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres_Primaria_Incompleta[[i]]$ESTADO, weights = mujeres_Primaria_Incompleta[[i]]$PONDERA))[6]
}

# varones_Primaria_Completa
tasas_actividad_varones_Primaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Primaria_Completa[i]<-
    (addmargins(
      wtd.table(
        varones_Primaria_Completa[[i]]$ESTADO, weights = varones_Primaria_Completa[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones_Primaria_Completa[[i]]$ESTADO, weights = varones_Primaria_Completa[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones_Primaria_Completa[[i]]$ESTADO, weights = varones_Primaria_Completa[[i]]$PONDERA))[6]
}
# mujeres_Primaria_Completa
tasas_actividad_mujeres_Primaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Primaria_Completa[i]<-
    (addmargins(
      wtd.table(
        mujeres_Primaria_Completa[[i]]$ESTADO, weights = mujeres_Primaria_Completa[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres_Primaria_Completa[[i]]$ESTADO, weights = mujeres_Primaria_Completa[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres_Primaria_Completa[[i]]$ESTADO, weights = mujeres_Primaria_Completa[[i]]$PONDERA))[6]
}

# varones_Secundaria_Incompleta
tasas_actividad_varones_Secundaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Secundaria_Incompleta[i]<-
    (addmargins(
      wtd.table(
        varones_Secundaria_Incompleta[[i]]$ESTADO, weights = varones_Secundaria_Incompleta[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones_Secundaria_Incompleta[[i]]$ESTADO, weights = varones_Secundaria_Incompleta[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones_Secundaria_Incompleta[[i]]$ESTADO, weights = varones_Secundaria_Incompleta[[i]]$PONDERA))[6]
}
# mujeres_Secundaria_Incompleta
tasas_actividad_mujeres_Secundaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Secundaria_Incompleta[i]<-
    (addmargins(
      wtd.table(
        mujeres_Secundaria_Incompleta[[i]]$ESTADO, weights = mujeres_Secundaria_Incompleta[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres_Secundaria_Incompleta[[i]]$ESTADO, weights = mujeres_Secundaria_Incompleta[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres_Secundaria_Incompleta[[i]]$ESTADO, weights = mujeres_Secundaria_Incompleta[[i]]$PONDERA))[6]
}

# varones_Secundaria_Completa
tasas_actividad_varones_Secundaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Secundaria_Completa[i]<-
    (addmargins(
      wtd.table(
        varones_Secundaria_Completa[[i]]$ESTADO, weights = varones_Secundaria_Completa[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones_Secundaria_Completa[[i]]$ESTADO, weights = varones_Secundaria_Completa[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones_Secundaria_Completa[[i]]$ESTADO, weights = varones_Secundaria_Completa[[i]]$PONDERA))[6]
}
# mujeres_Secundaria_Completa
tasas_actividad_mujeres_Secundaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Secundaria_Completa[i]<-
    (addmargins(
      wtd.table(
        mujeres_Secundaria_Completa[[i]]$ESTADO, weights = mujeres_Secundaria_Completa[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres_Secundaria_Completa[[i]]$ESTADO, weights = mujeres_Secundaria_Completa[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres_Secundaria_Completa[[i]]$ESTADO, weights = mujeres_Secundaria_Completa[[i]]$PONDERA))[6]
}

# varones_Universitaria_Incompleta
tasas_actividad_varones_Universitaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Universitaria_Incompleta[i]<-
    (addmargins(
      wtd.table(
        varones_Universitaria_Incompleta[[i]]$ESTADO, weights = varones_Universitaria_Incompleta[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones_Universitaria_Incompleta[[i]]$ESTADO, weights = varones_Universitaria_Incompleta[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones_Universitaria_Incompleta[[i]]$ESTADO, weights = varones_Universitaria_Incompleta[[i]]$PONDERA))[6]
}
# mujeres_Universitaria_Incompleta
tasas_actividad_mujeres_Universitaria_Incompleta<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Universitaria_Incompleta[i]<-
    (addmargins(
      wtd.table(
        mujeres_Universitaria_Incompleta[[i]]$ESTADO, weights = mujeres_Universitaria_Incompleta[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres_Universitaria_Incompleta[[i]]$ESTADO, weights = mujeres_Universitaria_Incompleta[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres_Universitaria_Incompleta[[i]]$ESTADO, weights = mujeres_Universitaria_Incompleta[[i]]$PONDERA))[6]
}

# varones_Universitaria_Completa
tasas_actividad_varones_Universitaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_varones_Universitaria_Completa[i]<-
    (addmargins(
      wtd.table(
        varones_Universitaria_Completa[[i]]$ESTADO, weights = varones_Universitaria_Completa[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           varones_Universitaria_Completa[[i]]$ESTADO, weights = varones_Universitaria_Completa[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        varones_Universitaria_Completa[[i]]$ESTADO, weights = varones_Universitaria_Completa[[i]]$PONDERA))[6]
}
# mujeres_Universitaria_Completa
tasas_actividad_mujeres_Universitaria_Completa<-vector()
for (i in 1:15) {
  tasas_actividad_mujeres_Universitaria_Completa[i]<-
    (addmargins(
      wtd.table(
        mujeres_Universitaria_Completa[[i]]$ESTADO, weights = mujeres_Universitaria_Completa[[i]]$PONDERA))[2]+
       addmargins(
         wtd.table(
           mujeres_Universitaria_Completa[[i]]$ESTADO, weights = mujeres_Universitaria_Completa[[i]]$PONDERA))[3])/
    addmargins(
      wtd.table(
        mujeres_Universitaria_Completa[[i]]$ESTADO, weights = mujeres_Universitaria_Completa[[i]]$PONDERA))[6]
}