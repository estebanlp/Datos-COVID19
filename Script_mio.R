### Script Clase 05
# EDSA - Exploratory Spatial Data Analysis
# Basado en Datos COVID19 CHILE
# Fuente: https://github.com/MinCiencia/Datos-COVID19 

install.packages('sf')
library(data.table)
library(ggplot2)

covid<-fread(input ="2020-04-08-CasosConfirmados.csv")
str(covid)
sapply(covid,FUN = class)

covid[,`Casos Confirmados`:=as.numeric(`Casos Confirmados`)]

ggplot(covid, aes(x=`Casos Confirmados`)) +geom_histogram(stat = 'count')
p_hist<-ggplot(covid, aes(x=`Casos Confirmados`)) +geom_histogram(stat = 'count')+ geom_density(stat = 'count')


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
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

# Choropleth plot (Discrete scale)
## Fixed
library(classInt)
help(package='classInt')

breaks_fixed <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "fixed", fixedBreaks=c(min(comunas_rm$`Casos Confirmados`,na.rm = T),5,20,50,100,max(comunas_rm$`Casos Confirmados`,na.rm = T)))

comunas_rm$casos_fixed<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_fixed$brks)

p_fixed<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_fixed, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

# Equal interval
breaks_equal <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "equal")

comunas_rm$casos_equal<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_equal$brks)

p_equal<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_equal, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

# Natural Breaks (Jenks)
breaks_jenks <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "jenks")

comunas_rm$casos_jenks<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_jenks$brks)

p_jenks<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_jenks, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)


# Quantile (Equal share)
breaks_quantile <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "quantile")

comunas_rm$casos_quantile<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_quantile$brks)

p_quantile<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_quantile, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

# Standard Deviation 
breaks_sd <- classIntervals(comunas_rm$`Casos Confirmados`, n = 5, style = "sd")

comunas_rm$casos_sd<-cut(comunas_rm$`Casos Confirmados`,breaks = breaks_sd$brks)

p_sd<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = casos_sd, geometry = geometry)) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

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


# Mapping Rates
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
  labs(title = "Tasa Cruda de Riesgo", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

comunas_rm$AR<-sum(comunas_rm$`Casos Confirmados`,na.rm = T)/sum(comunas_rm$Poblacion,na.rm = T)

comunas_rm$E_i<-comunas_rm$Poblacion*comunas_rm$AR

comunas_rm$R_i<-comunas_rm$`Casos Confirmados`/comunas_rm$E_i

R_cont<-ggplot(comunas_rm) + 
  geom_sf(aes(fill = R_i, geometry = geometry)) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                        high = div_pal[1],name = "Tasa Relativa",midpoint=1) +
  labs(title = "Tasa Relativa de Riesgo", subtitle = "Región Metropolitana - 2020-04-08") +
  theme_minimal(base_size = 11)

grid.arrange(r_cont,R_cont,nrow=1)

# Mapping Rates - Changing the population at Risk

ODRM<-data.table(readRDS(""))
