# los data frame está armados por año, hay una lista para cada indicador
# que contiene las tablas por sexo y educación
class(prop.activos[[1]])
rm(actividad)
actividad<-list()
for (i in 1:15) {
  actividad[[i]]<-as.data.frame(prop.activos[[i]])
}

anio<-2004:2018
anio<-as.factor(anio)
anio[15]
for (i in 1:15) {
  actividad[[i]]$year<-rep(anio[i],28)
}

tasa_actividad_sexo_educacion<-list()
for (i in 1:15) {
  tasa_actividad_sexo_educacion[[i]]<-
    subset(actividad[[i]], actividad[[i]]$condicion=="activos")
}
tasa_actividad_sexo_educacion[[1]]

tasa_actividad_sexo_educacion_df<-as.data.frame(rbind(
  tasa_actividad_sexo_educacion[[1]],tasa_actividad_sexo_educacion[[2]],
  tasa_actividad_sexo_educacion[[3]],tasa_actividad_sexo_educacion[[4]],
  tasa_actividad_sexo_educacion[[5]],tasa_actividad_sexo_educacion[[6]],
  tasa_actividad_sexo_educacion[[7]],tasa_actividad_sexo_educacion[[8]],
  tasa_actividad_sexo_educacion[[9]],tasa_actividad_sexo_educacion[[10]],
  tasa_actividad_sexo_educacion[[11]],tasa_actividad_sexo_educacion[[12]],
  tasa_actividad_sexo_educacion[[13]],
  tasa_actividad_sexo_educacion[[14]],tasa_actividad_sexo_educacion[[15]]
))

library(ggplot2)
ggplot(tasa_actividad_sexo_educacion_df)+
  geom_point(aes(year, proporcion, col=sexo))+
  facet_grid(.~educacion)

ggplot(tasa_actividad_sexo_educacion_df)+
  geom_point(aes(year, proporcion, shape=sexo, col=educacion))

library(questionr)
wtd.table(eph[[1]]$ESTADO, eph[[1]]$sexo, eph[[1]]$educacion, weights = eph[[1]]$PONDERA)
wtd.table(eph[[1]]$ESTADO, eph[[1]]$sexo, weights = eph[[1]]$PONDERA)

1-((wtd.table(eph[[13]]$ESTADO, weights = eph[[13]]$PONDERA))[3])/
  (addmargins(wtd.table(eph[[13]]$ESTADO, weights = eph[[13]]$PONDERA))[4])

1-((wtd.table(eph[[14]]$ESTADO, weights = eph[[14]]$PONDERA))[3])/
  (addmargins(wtd.table(eph[[14]]$ESTADO, weights = eph[[14]]$PONDERA))[4])

1-((wtd.table(eph[[15]]$ESTADO, weights = eph[[15]]$PONDERA))[3])/
  (addmargins(wtd.table(eph[[15]]$ESTADO, weights = eph[[15]]$PONDERA))[4])

# sobre población total??
original_2016<-read.table("archivosINDEC/usu_individual_t316.txt",
                           header=TRUE, sep=";")

addmargins(wtd.table(original_2016$ESTADO, weights = original_2016$PONDERA))

1-addmargins(
  wtd.table(
    original_2016$ESTADO, weights = original_2016$PONDERA))[4]/
  addmargins(
  wtd.table(
    original_2016$ESTADO, weights = original_2016$PONDERA))[6]
## del modo que calcula INDEC
(addmargins(
  wtd.table(
    original_2016$ESTADO, weights = original_2016$PONDERA))[2]+
  addmargins(
    wtd.table(
      original_2016$ESTADO, weights = original_2016$PONDERA))[3])/
  addmargins(
    wtd.table(
      original_2016$ESTADO, weights = original_2016$PONDERA))[6]
###############XXXXXXXXXX###############

# Hasta la linea 63 de gender_gap queda igual
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
# subset y se reunen en una sola matriz
table(varones_Universitaria_Completa[[15]]$sexo,
      varones_Universitaria_Completa[[15]]$educacion)
