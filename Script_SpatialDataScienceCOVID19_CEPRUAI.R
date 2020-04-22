### Charla: Spatial Data Science
# Qué podemos decir (y qué no) con los datos actuales Covid19?
# Fuente de Datos: https://github.com/MinCiencia/Datos-COVID19 
# Autor: Esteban López Ochoa
# Cargo1: Profesor Asistente Escuela de Negocios
# Cargo2: Director Ejecutivo CEPR-UAI (www.cepr.uai.cl)
# Cargo3: Director Académico Diplomado en Big Data para políticas Públicas


#---- Parte 1: Intro al Mapeo de datos agregados -------------------
#install.packages("chilemapas")
#install.packages("rgdal")
#install.packages('sf')
library(rgdal)
library(sp)
library(chilemapas)
library(data.table)


covid<-fread(input ="output/producto2/2020-04-20-CasosConfirmados.csv")
str(covid)
sapply(covid,FUN = class)

covid[,`Casos Confirmados`:=as.numeric(`Casos Confirmados`)]

# Choropleth maps
library(sf)
library(chilemapas)

help(package='chilemapas')

comunas_rm<-mapa_comunas[mapa_comunas$codigo_region==13,]

comunas_rm<-merge(comunas_rm,covid,by.x="codigo_comuna",by.y="Codigo comuna",all.x=TRUE,sort=F)


# Choropleth plot (continuos scale)

library(RColorBrewer)
paleta <- rev(brewer.pal(n = 5,name = "Reds"))

p_cont<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = `Casos Confirmados`, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "No. Casos") +
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
p_cont

# Choropleth plot (Discrete scale)
## Fixed
library(classInt)
help(package='classInt')

breaks_fixed <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "fixed", fixedBreaks=c(min(comunas_rm$`Casos Confirmados`,na.rm = T),5,20,50,100,max(comunas_rm$`Casos Confirmados`,na.rm = T)))

comunas_rm$casos_fixed<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_fixed$brks)

p_fixed<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_fixed, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
p_fixed

# Equal interval
breaks_equal <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "equal")

comunas_rm$casos_equal<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_equal$brks)

p_equal<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_equal, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
p_equal


# Natural Breaks (Jenks)
breaks_jenks <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "jenks")

comunas_rm$casos_jenks<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_jenks$brks)

p_jenks<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_jenks, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
p_jenks

# Quantile (Equal share)
breaks_quantile <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "quantile")

comunas_rm$casos_quantile<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_quantile$brks)

p_quantile<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_quantile, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)

# Standard Deviation 
breaks_sd <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "sd")

comunas_rm$casos_sd<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_sd$brks)

p_sd<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_sd, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
p_sd

### looking at everything together
library(gridExtra)
help(package='gridExtra')

grid.arrange(p_cont,p_fixed,p_equal,p_jenks,p_quantile,p_sd)


##### leaflet
library(leaflet)
library(sp)

st_crs(comunas_rm)
comunas_rm<-st_transform(comunas_rm,crs = "+proj=longlat +datum=WGS84")

bins <- c(min(comunas_rm$`Casos Confirmados`,na.rm = T),5,20,50,100,max(comunas_rm$`Casos Confirmados`,na.rm = T))
pal <- colorBin("Reds", domain = comunas_rm$`Casos Confirmados`, bins = bins,right = T)

pal_quantile <- colorQuantile("Reds", domain = comunas_rm$`Casos Confirmados`, n = 5)
pal_quantile2<-colorFactor("Reds",domain = comunas_rm$casos_quantile)

names(comunas_rm)

leaflet(comunas_rm) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal(`Casos Confirmados`), weight = 2, fillOpacity = 0.7,group = "Fixed",color=~pal(`Casos Confirmados`)) %>%
  addLegend(pal = pal, values = ~`Casos Confirmados`, opacity = 1,position = "bottomright",title = "Fixed",group = "Fixed") %>%
  addPolygons(stroke = FALSE, color = ~pal_quantile2(casos_quantile), weight = 2, fillOpacity = 0.7,group = "Quantile") %>%
  addLegend(pal = pal_quantile2, values = ~casos_quantile, opacity = 1,position = "bottomright",title = "Quantile",group = "Quantile") %>%
  addLayersControl(
    overlayGroups = c("Fixed", "Quantile"),
    options = layersControlOptions(collapsed = FALSE)
  )

#---- Parte 2: Mapping Rates  -------------------

#concepts
# O_i: Observed Events at spatial unit i  (`Casos Confirmados`)
# P_i: Population at risk at spatial unit i (Poblacion)
# r_i:  Risk rate at spatial unit i (O_i/P_i)
# AR:  Average Risk Rate (sum(O_i)/sum(P_i))
# E_i: Expected Events at spatial unit i (r*P_i)
# R_i: Relative Risk at spatial unit i (Excess Risk)  (O_i/E_i)

comunas_rm$r_i<-comunas_rm$`Casos Confirmados`/comunas_rm$Poblacion

div_pal<-brewer.pal(name =  "RdBu",n = 5)

r_cont<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = r_i, geometry = geometry)) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                       high = div_pal[1],name = "Tasa Riesgo") +
  labs(title = "Tasa Cruda de Riesgo", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
r_cont

comunas_rm$AR<-sum(comunas_rm$`Casos Confirmados`,na.rm = T)/sum(comunas_rm$Poblacion,na.rm = T)

comunas_rm$E_i<-comunas_rm$Poblacion*comunas_rm$AR

comunas_rm$R_i<-comunas_rm$`Casos Confirmados`/comunas_rm$E_i

R_cont<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = R_i, geometry = geometry)) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                        high = div_pal[1],name = "Tasa Relativa",midpoint=1) +
  labs(title = "Tasa Relativa de Riesgo", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)

grid.arrange(r_cont,R_cont,nrow=1)


#---- Parte 3: Mapping Rates - Changing the population at Risk  -----------

# Visualización de la Commutación

ODRM<-readRDS("otros_datos/ConmutacionRM.rds")
class(ODRM)
View(ODRM)
View(comunas_rm)
names(ODRM)
names(ODRM)<-c("destino",'origen','Total')

library(circlize)
library(chorddiag)
# Creamos matriz de conmutación, donde las filas representan en qué comuna viven y las columna en que columna trabajan:

MATRIZ_OD_RM <- with(ODRM, tapply(Total, list(destino,origen), FUN=sum))

MATRIZ_OD_RM[is.na(MATRIZ_OD_RM)] <- 0 # Cambiamos NAs de las matrices por 0s.

chorddiag(MATRIZ_OD_RM[-44,], type = "directional")


# Diferenciando población trabajadora por origen y destino

Trabajadores_origen<-ODRM[,.(Trab_origen=sum(Total,na.rm = T)),by=.(origen)]

Trabajadores_destino<-ODRM[,.(Trab_destino=sum(Total,na.rm = T)),by=.(destino)]

comunas_rm<-merge(comunas_rm,Trabajadores_origen,by.x='codigo_comuna',by.y='origen',all.x=T,sort=F)

comunas_rm<-merge(comunas_rm,Trabajadores_destino,by.x='codigo_comuna',by.y='destino',all.x=T,sort=F)

# Diferencias Poblacionales

# Natural Breaks (Jenks) - Poblacion total
breaks_jenks_t <- classIntervals(comunas_rm$Poblacion, n = 5, style = "jenks")

comunas_rm$pop_jenks<-cut(comunas_rm$Poblacion,breaks = breaks_jenks_t$brks)

pop_jenks<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = pop_jenks, geometry = geometry)) +
  scale_fill_brewer(palette = "YlOrBr")+
  labs(title = "Población", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)

# Natural Breaks (Jenks) - Trabajadores por Origen
breaks_jenks_o <- classIntervals(comunas_rm$Trab_origen, n = 5, style = "jenks")

comunas_rm$trab_jenks_o<-cut(comunas_rm$Trab_origen,breaks = breaks_jenks_o$brks)

trab_jenks_o<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = trab_jenks_o, geometry = geometry)) +
  scale_fill_brewer(palette = "YlOrBr")+
  labs(title = "Trabajadores por Origen", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
trab_jenks_o

# Natural Breaks (Jenks) - Trabajadores por Destino
breaks_jenks_d <- classIntervals(comunas_rm$Trab_destino, n = 5, style = "jenks")

comunas_rm$trab_jenks_d<-cut(comunas_rm$Trab_destino,breaks = breaks_jenks_d$brks)

trab_jenks_d<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = trab_jenks_d, geometry = geometry)) +
  scale_fill_brewer(palette = "YlOrBr")+
  labs(title = "Trabajadores por Destino", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)
trab_jenks_d

# Risk by origen

comunas_rm$r_i_TO<-comunas_rm$`Casos Confirmados`/comunas_rm$Trab_origen

comunas_rm$AR_TO<-sum(comunas_rm$`Casos Confirmados`,na.rm = T)/sum(comunas_rm$Trab_origen,na.rm = T)

comunas_rm$E_i_TO<-comunas_rm$Trab_origen*comunas_rm$AR_TO

comunas_rm$R_i_TO<-comunas_rm$`Casos Confirmados`/comunas_rm$E_i_TO

R_cont_TO<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = R_i_TO, geometry = geometry)) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                       high = div_pal[1],name = "Tasa Relativa",midpoint=1) +
  labs(title = "Tasa Relativa de Riesgo - Trabajadores Origen", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)
R_cont_TO

# Risk by destino

comunas_rm$r_i_TD<-comunas_rm$`Casos Confirmados`/comunas_rm$Trab_destino

comunas_rm$AR_TD<-sum(comunas_rm$`Casos Confirmados`,na.rm = T)/sum(comunas_rm$Trab_destino,na.rm = T)

comunas_rm$E_i_TD<-comunas_rm$Trab_destino*comunas_rm$AR_TD

comunas_rm$R_i_TD<-comunas_rm$`Casos Confirmados`/comunas_rm$E_i_TD

R_cont_TD<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = R_i_TD, geometry = geometry)) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                       high = div_pal[1],name = "Tasa Relativa",midpoint=1) +
  labs(title = "Tasa Relativa de Riesgo - Trabajadores Destino", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

R_cont_TD


#---- Parte 4: Time correlations  -------------------

#Covid
library(data.table)

archivos<-dir(path = "output/producto2/")
COVID<-fread(input =paste0("output/producto2/",archivos[1]))
names(COVID)[6]<-paste0("Confirmados_",substr(archivos[1],start = 1,stop = 10))

for(i in 2:length(archivos)){
  aa<-fread(input =paste0("output/producto2/",archivos[i]))
  aa<-aa[,.(`Codigo comuna`,`Casos Confirmados`)]
  names(aa)[2]<-paste0("Confirmados_",substr(archivos[i],start = 1,stop = 10))
  COVID<-merge(COVID,aa,by="Codigo comuna",all.x=T,sort=F)
}
View(COVID)

COVID[is.na(`Confirmados_2020-03-30`),`Confirmados_2020-03-30`:=0]

library(ggplot2)
ggplot(COVID,aes(x=`Confirmados_2020-03-30`,y=`Confirmados_2020-04-20`))+geom_point()+geom_smooth(method = lm)

ggplot(COVID,aes(x=`Confirmados_2020-04-10`,y=`Confirmados_2020-04-20`))+geom_point()+geom_smooth(method = lm)

ggplot(COVID,aes(x=`Confirmados_2020-04-17`,y=`Confirmados_2020-04-20`))+geom_point()+geom_smooth(method = lm)


#---- Parte 5: Intro 2 Spatial Autocorrelation  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

comunas_rm<-mapa_comunas[mapa_comunas$codigo_region==13,]

comunas_rm<-merge(x = comunas_rm,y = COVID[`Codigo region`==13,],by.x="codigo_comuna",by.y="Codigo comuna",all.x=TRUE,sort=F)

comunas_rm<-as_Spatial(comunas_rm)

library(spdep)

nbs<-poly2nb(comunas_rm,queen = T)

w_rm<-nb2listw(nbs,style = "W")

plot(comunas_rm)
plot(nbs,coordinates(comunas_rm),add=T,col='blue',pch=".")

comunas_rm@data$sl_Confirmados_2020.04.20<-lag.listw(w_rm,comunas_rm$Confirmados_2020.04.20)

comunas_rm<-as(comunas_rm,'sf')

ggplot(comunas_rm,aes(x=Confirmados_2020.04.20, y=sl_Confirmados_2020.04.20))+ geom_point()+geom_smooth(method = 'loess')


# Quantile (Equal share)
breaks_quantile <- classIntervals(comunas_rm$Confirmados_2020.04.20, n = 5, style = "quantile")

comunas_rm$casos_quantile<-cut(comunas_rm$Confirmados_2020.04.20,breaks = breaks_quantile$brks)

p_quantile<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_quantile, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)

#Spatial Lag
breaks_quantile_sl <- classIntervals(comunas_rm$sl_Confirmados_2020.04.20, n = 5, style = "quantile")

comunas_rm$casos_quantile_sl<-cut(comunas_rm$sl_Confirmados_2020.04.20,breaks = breaks_quantile$brks)

p_quantile_sl<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_quantile_sl, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Promedio Casos Confirmados Comunas Vecinas", subtitle = "Región Metropolitana - 2020.04.20") +
  theme_minimal(base_size = 11)

