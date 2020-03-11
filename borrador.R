library(foreign)
library(ggplot2)
library(ggthemes)
library(doBy)
library(scales)
##condicion de actividad por educacion y sexo

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

##ordena niveles, rotula, elimina 0 y 4 de estado, edad>14
for (i in 1:15) {
  eph[[i]]$sexo=as.factor(eph[[i]]$CH04)
  levels(eph[[i]]$sexo)=c("varones", "mujeres")
  eph[[i]]=subset(eph[[i]], eph[[i]]$ESTADO!=0 & eph[[i]]$ESTADO!=4 &
                    eph[[i]]$CH06>14)
  eph[[i]]$actividad=as.factor(eph[[i]]$ESTADO)
  levels(eph[[i]]$actividad)=c("ocupado", "desocupado", "inactivo")
  eph[[i]]$educacion=factor(eph[[i]]$NIVEL_ED,
                            levels(factor(eph[[i]]$NIVEL_ED))[c(7,1:6)])
  levels(eph[[i]]$educacion)=c("Sin instrucción",
                               "Primaria Incompleta",
                               "Primaria Completa","Secundaria Incompleta", "Secundaria Completa",
                               "Universitaria Incompleta","Universitaria Completa")
}

##se definen los activos
for (i in 1:15) {
  eph[[i]]$condact=ifelse(eph[[i]]$ESTADO==3, 0, 1)  
}
eph[[15]]$condact=factor(eph[[15]]$condact)
levels(eph[[15]]$condact)

for (i in 1:15) {
  eph[[i]]$condact=factor(eph[[i]]$condact)
  levels(eph[[i]]$condact)=c("inactivos", "activos")
}


levels(eph[[15]]$condact)

prop.activos=vector("list",15)
for (i in 1:15) {
  prop.activos[[i]]=as.data.frame(ftable(prop.table(
    table(eph[[i]]$condact, eph[[i]]$educacion, eph[[i]]$sexo),
    c(3,2))))
  names(prop.activos[[i]])=c("condicion", "educacion","sexo", "proporcion")
  
}
prop.activos[[15]]
graficos.condact=vector("list",15)
for (i in 1:15) {
  graficos.condact[[i]]=ggplot(eph[[i]])+
    geom_bar(aes(x=educacion, fill=factor(condact,
                                          levels=c("activos","inactivos"))), position = "fill")+
    scale_fill_brewer(type = "div", palette = "Set2", direction = 1,
                      aesthetics = "fill")+
    guides(fill=guide_legend(title=NULL))+
    geom_text(data=prop.activos[[i]],position=position_stack(vjust=.5),
              size=3, aes(x= educacion, 
                          y=proporcion,label =paste(100*round(proporcion,3),"%")))+
    labs(
      title="Condición de actividad según nivel de educación y sexo",
      subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",eph[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    facet_grid(sexo~.)
}
graficos.condact[[1]]

for(i in 1:15){ 
  ggsave(filename = paste("condiciondeactividad", eph[[i]]$ANO4,
                          ".png"),
         path = "graficos\\condicion_actividad",
         plot = graficos.condact[[i]], device = "png",
         scale = 1.7)
}




##XXXXXXXXXXXXXXXXXXX

#################
###desocupacion
solo.activos=vector("list", 15)
for (i in 1:15) {
  solo.activos[[i]]=subset(eph[[i]],
                           eph[[i]]$ESTADO==1 | eph[[i]]$ESTADO==2)
  solo.activos[[i]]$actividad=factor(solo.activos[[i]]$actividad) 
  # para descartar el nivel no usado
}

props.d=vector("list",15)
for (i in 1:15) {
  props.d[[i]]=as.data.frame(ftable(prop.table(
    table(solo.activos[[i]]$actividad, solo.activos[[i]]$educacion,
          solo.activos[[i]]$sexo),
    c(3,2))))
  names(props.d[[i]])=
    c("ocupacion", "educacion","sexo", "proporcion")
}  

graficos.desocupacion=vector("list", 15)
for (i in 1:15) {
  graficos.desocupacion[[i]]=ggplot(solo.activos[[i]])+
    geom_bar(aes(x=educacion, fill=factor(actividad,
                                          levels=c("desocupado",
                                                   "ocupado"))),
             position = "fill")+
    scale_fill_brewer(type = "div", palette = "Set2", direction = 1,
                      aesthetics = "fill")+
    guides(fill=guide_legend(title=NULL))+
    geom_text(data=props.d[[i]],
              position=position_stack(vjust=0.5), size=3,
              aes(x= educacion,y=proporcion,
                  label =paste(100*round(proporcion,3),"%")))+
    labs(
      title="Condición de ocupación de la población económicamente activa
      según nivel de educación y sexo",
      subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",eph[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    facet_grid(sexo~.)
}


for(i in 1:15){ 
  ggsave(filename = paste("desocupación", eph[[i]]$ANO4,
                          ".png"),
         plot = graficos.desocupacion[[i]], device = "png",
         scale = 1.7)
}

################
###categoría ocupacional
##ordena niveles, rotula y quita trabajador familiar sin remuneracion
solo.activos.3.cats=vector("list", 15)
for (i in 1:15) {
  solo.activos.3.cats[[i]]=subset(solo.activos[[i]], solo.activos[[i]]$CAT_OCUP==1 |
                                         solo.activos[[i]]$CAT_OCUP==2 |
                                         solo.activos[[i]]$CAT_OCUP==3)
  solo.activos.3.cats[[i]]$categoria=
    as.factor(as.character(solo.activos.3.cats[[i]]$CAT_OCUP))
  levels(solo.activos.3.cats[[i]]$categoria)=c("Patrón", "Cuenta propia",
                                                    "Obrero o empleado")
}

###graficos categoria ocupacional por sexo y educacion

##otra vez las anotaciones
props.cats=vector("list",15)
for (i in 1:15) {
  props.cats[[i]]=as.data.frame(ftable(prop.table(
    table(solo.activos.3.cats[[i]]$categoria,
          solo.activos.3.cats[[i]]$educacion,
          solo.activos.3.cats[[i]]$sexo),
    c(3,2))))
  names(props.cats[[i]])=
    c("ocupacion", "educacion","sexo", "proporcion")
}  

graficos.categoria=vector("list", 15)
for (i in 1:15) {
  graficos.categoria[[i]]=ggplot(solo.activos.3.cats[[i]])+
    geom_bar(aes(x=educacion, fill=factor(categoria,
                                          levels=c("Patrón", "Cuenta propia",
                                                   "Obrero o empleado"))),
             position = "fill")+
    scale_fill_brewer(type = "div", palette = "Set2", direction = 1,
                      aesthetics = "fill")+
    guides(fill=guide_legend(title=NULL))+
    geom_text(data=props.cats[[i]],
              position=position_stack(vjust=0.5), size=3,
              aes(x= educacion,y=proporcion,group=ocupacion,
                  label =paste(100*round(proporcion,3),"%")))+
    labs(
      title="Categoría ocupacional según nivel de educación y sexo",
      subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",eph[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    facet_grid(sexo~.)
}

for(i in 1:15){ 
  ggsave(filename = paste("categoria", eph[[i]]$ANO4,
                          ".png"),
         plot = graficos.categoria[[i]], device = "png",
         scale = 1.7)
}

###precariedad
precariedad.sexo.educacion=vector("list",15)
for (i in 1:15) {
  precariedad.sexo.educacion[[i]]=
    summaryBy(PP07G_59 ~ sexo+educacion , data = solo.activos[[i]], 
              FUN = function(x) { c(m = mean(x, na.rm = TRUE)/5) })  
}

precariedad.sexo.educacion[[15]]

precariedad.graficos=vector("list",15)
for (i in 1:15) {
precariedad.graficos[[i]] =ggplot(precariedad.sexo.educacion[[i]])+
  geom_point(aes(
    educacion, sexo, size=100*PP07G_59.m), show.legend = FALSE)+
  geom_text(aes(educacion, sexo,
                label=paste(
                  round(100*PP07G_59.m,0),"%")), vjust=-3.7)+
  scale_size_continuous(
    range = c(3, 27))+xlab("Máximo nivel de educación")+
  ylab("Sexo")+
  labs(
    title="Proporción de asalariados precarios según sexo y nivel de educación",
    subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",eph[[i]]$ANO4),
    caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar")+
  theme_economist(base_family = "serif", base_size = 9)
}

precariedad.graficos[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("precariedad", eph[[i]]$ANO4,
                          ".png"),
         plot = precariedad.graficos[[i]], device = "png",
         scale = 1.7)
}
precariedad.graficos[[15]]
ggsave(filename = "precariedad2018.png",
       plot = graf.precariedad.2018, device = "png",
       scale = 1.7)


#############
##reelaboracion a partir de cuadro  de INDEC
decil=1:10
varones=c(0.277128302,0.415868806,0.355728829,0.485605242,0.49141086,0.513458412,0.553143809,0.589526257,0.611492607,0.634385757)
mujeres=c(0.722871698,0.584131194,0.644271171,0.514394758,0.50858914,0.486541588,0.446856191,0.410473743,0.388507393,0.365614243)
de.cuadro.4=data.frame(decil,varones,mujeres)
varones.col=rep("varones",10)
mujeres.col=rep("mujeres",10)
sexo=c(varones.col,mujeres.col)
proporción=c(varones,mujeres)
decil.col=c(decil,decil)
de.cuadro.4.1=data.frame(decil.col,sexo,proporción)

composicion.deciles=ggplot(de.cuadro.4.1)+
  geom_bar(stat = "identity",aes(x=factor(decil.col), y=proporción, 
                                 fill=sexo), position = "fill")+
  scale_fill_brewer(type = "div", palette = "Set1", direction = 1,
                    aesthetics = "fill")+
  labs(
    title="Composición por sexos de los deciles de ingreso",
    subtitle = "Argentina. Aglomerados Urbanos primer trimestre 2018",
    caption="Fuente: elaboración a partir del Cuadro 4 de INDEC, 2018",
    x="Deciles de ingreso"
  )+theme_economist(base_family = "serif", base_size = 9)+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))
composicion.deciles

ggsave(filename = "composiciondeciles.png",
       plot = composicion.deciles, device = "png",
       scale = 1.7)

##### hasta acá pasado al rmd

##diferencias brutas
asalariados=vector("list",15)
for(i  in 1:15) asalariados[[i]]=subset(eph[[i]], eph[[i]]$PP08D1>0)

res.ing.educa.sexo=vector("list",15)
for(i  in 1:15) {
  res.ing.educa.sexo[[i]]=summaryBy(PP08D1 ~ sexo + 
                                      educacion, data = asalariados[[i]], 
                                    FUN = function(x) { c(n=length(x),
                                                          m = mean(x), s = sd(x)) } )
  
}

error=vector("list",15)
for(i in 1:15) {
  error[[i]] =1.96*res.ing.educa.sexo[[i]]$PP08D1.s/
    sqrt(res.ing.educa.sexo[[i]]$PP08D1.n)
}

p=vector("list",15)
for(i in 1:15) { 
  p[[i]]=ggplot(res.ing.educa.sexo[[i]], aes(educacion, PP08D1.m))+
    geom_point(aes(col=sexo), size=4)+
    geom_errorbar(aes(ymin=PP08D1.m-error[[i]],
                      ymax=PP08D1.m+error[[i]],
                      col=sexo), width=.1)+
    scale_y_continuous(c(0,2000))+
    geom_line(aes(group=sexo, col=sexo))+
    scale_color_manual(values = c("orange","green"))+labs(
      title="Ingresos salariales mensuales promedio según nivel de educación para varones y mujeres",
      subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",asalariados[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    geom_text(aes(label = paste("$",round(PP08D1.m,0))),
              size = 4, hjust = 0.5, vjust = -2)+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
for(i in 1:15){ 
  ggsave(filename = paste("diferenciasalariobruto", asalariados[[i]]$ANO4, ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}

##box plots
bp=vector("list",15)
for (i in 1:15) {
  bp[[i]]=ggplot(asalariados[[i]])+geom_boxplot(
    aes(educacion,fill=sexo, PP08D1))+
    xlab("Máximo nivel de educación alcanzado")+
    ylab("Ingresos salariales")+
    ggtitle("Ingresos salariales mensuales promedio según nivel
            de educación para varones y mujeres",
            subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",
                             asalariados[[i]]$ANO4))+
    labs(
      caption="Fuente: Encuesta Permanente de Hogares,
      INDEC www.indec.gob.ar")+
    scale_fill_manual(values = c("orange","green")) +
    theme_economist(base_family = "serif",base_size = 9)+theme(
      legend.title=element_blank(),plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size = 7.5)
    )
} 
bp[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("boxplots", asalariados[[i]]$ANO4, ".png"),
         plot = bp[[i]], device = "png",
         scale = 1.7)
}
##########hasta acá controlado y pasados los plots al documento 13/10

########se definenn los sectores
asalariados.E=vector("list",15)
for (i in 1:15) asalariados.E[[i]]=subset(asalariados[[i]],asalariados[[i]]$PP04A==1)

asalariados.P=vector("list",15)
for (i in 1:15) asalariados.P[[i]]=subset(asalariados[[i]],asalariados[[i]]$PP04A==2)

##ESTATAL
res.ing.educa.sexo.E=vector("list",15)
for(i  in 1:15) res.ing.educa.sexo.E[[i]]=summaryBy(PP08D1 ~ sexo + educacion, data = asalariados.E[[i]], 
                                                    FUN = function(x) { c(n=length(x),m = mean(x), s = sd(x)) } )

error=vector("list",15)
for(i in 1:15) {
  error[[i]] =1.96*res.ing.educa.sexo.E[[i]]$PP08D1.s/
    sqrt(res.ing.educa.sexo.E[[i]]$PP08D1.n)
}

p=vector("list",15)
for(i in 1:15) { 
  p[[i]]=ggplot(res.ing.educa.sexo.E[[i]], aes(educacion, PP08D1.m))+
    geom_point(aes(col=sexo), size=4)+
    geom_errorbar(aes(ymin=PP08D1.m-error[[i]],
                      ymax=PP08D1.m+error[[i]],
                      col=sexo), width=.1)+
    scale_y_continuous(c(0,2000))+
    geom_line(aes(group=sexo, col=sexo))+
    scale_color_manual(values = c("orange","green"))+labs(
      title="Ingresos salariales mensuales promedio según nivel de educación para varones y mujeres, sector estatal",
      subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",asalariados[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    geom_text(aes(label = paste("$",round(PP08D1.m,0))),
              size = 4, hjust = 0.5, vjust = -2)+
    ylab("")+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
p[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("difsalestatal", asalariados[[i]]$ANO4, ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}

##PRIVADO
res.ing.educa.sexo.P=vector("list",15)
for(i  in 1:15) res.ing.educa.sexo.P[[i]]=summaryBy(PP08D1 ~ sexo + educacion, data = asalariados.P[[i]], 
                                                    FUN = function(x) { c(n=length(x),m = mean(x), s = sd(x)) } )


error=vector("list",15)
for(i in 1:15) {
  error[[i]] =1.96*res.ing.educa.sexo.P[[i]]$PP08D1.s/
    sqrt(res.ing.educa.sexo.P[[i]]$PP08D1.n)
}

p=vector("list",15)
for(i in 1:15) { 
  p[[i]]=ggplot(res.ing.educa.sexo.P[[i]], aes(educacion, PP08D1.m))+
    geom_point(aes(col=sexo), size=4)+
    geom_errorbar(aes(ymin=PP08D1.m-error[[i]],
                      ymax=PP08D1.m+error[[i]],
                      col=sexo), width=.1)+
    scale_y_continuous(c(0,2000))+
    geom_line(aes(group=sexo, col=sexo))+
    scale_color_manual(values = c("orange","green"))+labs(
      title="Ingresos salariales mensuales promedio según nivel de educación para varones y mujeres, sector privado",
      subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",asalariados[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    geom_text(aes(label = paste("$",round(PP08D1.m,0))),
              size = 4, hjust = 0.5, vjust = -2)+
    ylab("")+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
p[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("difsalprivado", asalariados[[i]]$ANO4, ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}

#####################
##por hora horas>0
for(i  in 1:15) asalariados[[i]]=subset(asalariados[[i]],
                                          asalariados[[i]]$PP3E_TOT>0
                                        & asalariados[[i]]$PP3E_TOT<999)

for (i in 1:15) {
  asalariados[[i]]$ing.hora=asalariados[[i]]$PP08D1/(4*asalariados[[i]]$PP3E_TOT)
}

res.ing.h.educa.sexo=vector("list",15)
for(i  in 1:15) res.ing.h.educa.sexo[[i]]=summaryBy(ing.hora ~ sexo + educacion, data = asalariados[[i]], 
                                                    FUN = function(x) { c(n=length(x),m = mean(x), s = sd(x)) } )
res.ing.h.educa.sexo[[15]]
p=vector("list",15)
for(i in 1:15) { 
  p[[i]]=ggplot(res.ing.h.educa.sexo[[i]], aes(educacion, ing.hora.m))+
    geom_point(aes(col=sexo), size=4)+
    geom_line(aes(group=sexo, col=sexo))+
    scale_color_manual(values = c("orange","green"))+labs(
      title="Ingresos salariales promedio por hora según nivel de educación para varones y mujeres",
      subtitle = paste("Argentina. Aglomerados Urbanos segundo trimestre",asalariados[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    geom_text(aes(label = paste("$",round(ing.hora.m,0))),
              size = 4, hjust = 0.5, vjust = -2)+
    ylab("")+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))
}
p[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("difsalhoramedias", asalariados[[i]]$ANO4, ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}

###POR SECTOR
for (i in 1:15) {
  asalariados[[i]]=subset(asalariados[[i]],asalariados[[i]]$PP04A<3)
  asalariados[[i]]$sector=as.factor(asalariados[[i]]$PP04A)
  asalariados[[i]]$sector=factor(asalariados[[i]]$sector)
  levels(asalariados[[i]]$sector)=c("estatal","privado", NA)
}

res.ing.h.educa.sexo.sector=vector("list",15)
for(i  in 1:15) res.ing.h.educa.sexo.sector[[i]]=summaryBy(ing.hora ~ sexo + educacion+sector, data = asalariados[[i]], 
                                                           FUN = function(x) { c(n=length(x),m = mean(x), s = sd(x), mdn=median(x)) } )



res.ing.h.educa.sexo.sector[[15]]

p=vector("list",15)
for(i in 1:15) { 
  p[[i]]=ggplot(res.ing.h.educa.sexo.sector[[i]], aes(educacion, ing.hora.m))+
    geom_point(aes(col=sexo), size=4)+
    geom_line(aes(group=sexo, col=sexo))+
    scale_color_manual(values = c("orange","green"))+labs(
      title="Ingresos salariales promedio por hora según nivel de educación para varones y mujeres",
      subtitle = paste("Argentina. Aglomerados Urbanos segundo trimestre",asalariados[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif", base_size = 9)+
    geom_text(aes(label = paste("$",round(ing.hora.m,0))),
              size = 4, hjust = 0.5, vjust = -2)+
    ylab("")+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    facet_grid(.~sector)
}
p[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("difsalhoramediassector", asalariados[[i]]$ANO4, ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}

##con medianas
p=vector("list",15)
for(i in 1:15) { 
  p[[i]]=ggplot(res.ing.h.educa.sexo.sector[[i]], aes(educacion, ing.hora.mdn))+
    geom_point(aes(col=sexo), size=4)+
    geom_line(aes(group=sexo, col=sexo))+
    scale_color_manual(values = c("orange","green"))+labs(
      title="Mediana de los ingresos salariales por hora según nivel de educación para varones y mujeres",
      subtitle = paste("Argentina. Aglomerados Urbanos segundo trimestre",asalariados[[i]]$ANO4),
      caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
      x="Máximo nivel de educación alcanzado"
    )+theme_economist(base_family = "serif",base_size = 9)+
    geom_text(aes(label = paste("$",round(ing.hora.mdn,0))),
              size = 2, hjust = 0.5, vjust = -2)+
    ylab("")+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    facet_grid(.~sector)
}
p[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("difsalhoramedianassector", asalariados[[i]]$ANO4, ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}

##mujeres como proporcion de varones
library(dplyr)
library(tidyr)

res.para.prop=vector("list",15)
for(i  in 1:15) {
  res.para.prop[[i]]=
    summaryBy(ing.hora ~ sexo + educacion, data = asalariados[[i]], 
              FUN = function(x) { c(m = mean(x), mdn=median(x)) } )
}

mujeres.a.varones=vector("list",15)
for (i in 1:15) {
  mujeres.a.varones[[i]]=res.para.prop[[i]] %>% select(sexo, educacion, ing.hora.m) %>%
    spread(sexo, ing.hora.m) %>%
    mutate(M.sobre.V = mujeres/varones)
}
p=vector("list",15)
for (i in 1:15) {
  p[[i]]=ggplot(subset(mujeres.a.varones[[i]], educacion!="Sin instrucción"))+
    geom_point(aes(x=educacion, y=M.sobre.V, size=M.sobre.V))+
    geom_hline(yintercept = 1)+
    scale_y_continuous(limits = c(.7,1.3))+
    geom_text(size=3.5,hjust=0,nudge_x = .1,
              aes(x= educacion, y=M.sobre.V, label=paste(100*round(M.sobre.V,3),"%")))+
    labs(title="Porcentaje del salario masculino percibido por mujeres según nivel de educación",
         subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",asalariados[[i]]$ANO4),
         caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
         x="Máximo nivel de educación alcanzado")+
    theme_economist(base_family = "serif", base_size = 9)+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_size(guide = "none")
}
p[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("prop.M.a.V", asalariados[[i]]$ANO4,
                          ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}

##para gap OCDE
para.gap=vector("list",15)
for (i in 1:15) {
  para.gap[[i]]=res.para.prop[[i]] %>% select(sexo, educacion, ing.hora.mdn) %>%
    spread(sexo, ing.hora.mdn) %>%
    mutate(gap=round(100*(varones-mujeres)/varones,1))
}

p=vector("list",15)
for (i in 1:15) {
  p[[i]]=ggplot(subset(para.gap[[i]], educacion!="Sin instrucción"))+
    geom_point(aes(x=educacion, y=gap, size=gap))+
    geom_text(size=3.5,hjust=0,nudge_x = .1,
              aes(x= educacion, y=gap, label=paste(gap,"%")))+
    labs(title="Brecha salarial entre varones y mujeres, según definición de OCDE",
         subtitle = paste("Argentina. Aglomerados Urbanos primer trimestre",asalariados[[i]]$ANO4),
         caption="Fuente: Encuesta Permanente de Hogares, INDEC www.indec.gob.ar",
         x="Máximo nivel de educación alcanzado")+
    theme_economist(base_family = "serif", base_size = 9)+
    theme(axis.line=element_blank(),axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_size(guide = "none")
}
p[[15]]
for(i in 1:15){ 
  ggsave(filename = paste("gap.OCDE", asalariados[[i]]$ANO4,
                          ".png"),
         plot = p[[i]], device = "png",
         scale = 1.7)
}
