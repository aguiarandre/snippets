# Carregando os pacotes----
  
library(maptools)     
library(spdep)          
library(cartography)    
library(tmap)           
library(leaflet)        
library(dplyr)
library(rgdal)
library(dplyr)
library(RColorBrewer) 


setwd("C:/Users/andreaguiar/Desktop/usr/dev/maps-plotting/mapabrasil-master/")
# Importando shapefile (mapa do Brasil)----

shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

class(shp)

# Importando dataset----

pg <- read.csv("Dados\\ClassificacaoPontosCorridos.csv", header=T,sep=";")

pg <- pg %>% group_by(Estado) %>% mutate(cumsum = cumsum(PG))

pg <- pg %>%
  group_by(Estado) %>%
  summarise(Score= max(cumsum))

pg <- as.data.frame(pg)

class(pg)

# Importando cÃ³digos do IBGE e adicionando ao dataset----

ibge <- read.csv("Dados\\estadosibge.csv", header=T,sep=",")

pg <- merge(pg,ibge, by.x = "Estado", by.y = "UF")

# Fazendo a junÃ§Ã£o entre o dataset e o shapefile----

names(pg) <- c("Estado", "Score", "CD_GEOCUF", "UF")
brasileiropg <- merge(shp,pg, by = "CD_GEOCUF")



#Tratamento e transformaÃ§Ã£o dos dados----

proj4string(brasileiropg) <- CRS("+proj=longlat +datum=WGS84 +no_defs") #adicionando coordenadas geogrÃ¡ficas

Encoding(brasileiropg$NM_ESTADO) <- "UTF-8"

brasileiropg$Score[is.na(brasileiropg$Score)] <- 0 #substituindo NA por 0

brum_lat <-  -20.1515
brum_long <-   -44.2011


# reading dataframe
df_vale <- read.csv("./Dados/fake_data.csv", header = TRUE, sep=";")

# Gerando o mapa----

display.brewer.all()

#pal <- colorBin("Blues",domain = NULL,n=10) #cores do mapa
#pal <- brewer.pal(10, "YlOrRd")

pal <- colorBin("YlOrRd",domain = 0:100)
risco_normalizado <- df_vale$risco_tomado/max(df_vale$risco_tomado)
colors <- pal(as.integer(risco_normalizado*100))


state_popup <- paste0("<strong>Estado: </strong>", 
                      brasileiropg$NM_ESTADO, 
                      "<br><strong>Pontos: </strong>", 
                      brasileiropg$Score)

df_vale$size <- 20*exp((df_vale$dbl_porc_depend_vale+1)**3)
#df_vale$size <- 10000*10^df_vale$dbl_porc_depend_vale/5
#outline <- df_vale[chull(df_vale$longitude, df_vale$latitude),]

mask_emp4 <- which(df_vale$cod_segmento=="K")
mask_emp3 <- which(df_vale$cod_segmento=="2")
mask_emp2 <- which(df_vale$cod_segmento=="D")


leaflet(data = brasileiropg) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white",#~pal(1), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1 
              #popup = state_popup
              ) %>%
  addLegend("bottomright", pal = pal, values = as.numeric(df_vale$risco_tomado),
            title = "Risco Tomado",
            opacity = 1) %>%
  addCircles(lng = brum_long, lat = brum_lat, 
             radius=~50000 , 
             color = 'black', 
             fillColor = "Green",
             stroke = TRUE, 
             weight = 1,
             fillOpacity = 0.1,
             popup = "Brumadinho ~50km ") %>%
  # add emp2
  addCircles(lng = df_vale$longitude[mask_emp2], lat = df_vale$latitude[mask_emp2], 
             radius=~ df_vale$size[mask_emp2], 
             color= colors[mask_emp2], #"blue",
             stroke = TRUE, 
             group = "EMP2",
             weight = 1,
             fillOpacity = 0.5,
             popup = paste("Empresa", df_vale$nome[mask_emp2], "<br>",
                           "Segmento:", df_vale$cod_segmento[mask_emp2], "<br>")) %>%
  addCircles(lng = df_vale$longitude[mask_emp3], lat = df_vale$latitude[mask_emp3], 
             radius=~ df_vale$size[mask_emp3], 
             color= colors[mask_emp3],#"red",
             stroke = TRUE, 
             group = "EMP3",
             weight = 1,
             fillOpacity = 0.5,
             popup = paste("Empresa", df_vale$nome[mask_emp3], "<br>",
                           "Segmento:", df_vale$cod_segmento[mask_emp3], "<br>")) %>%
  addCircles(lng = df_vale$longitude[mask_emp4], lat = df_vale$latitude[mask_emp4], 
             radius=~ df_vale$size[mask_emp4], 
             color= colors[mask_emp4],#"purple",
             stroke = TRUE, 
             group = "EMP4",
             weight = 1,
             fillOpacity = 0.2,
             popup = paste("Empresa", df_vale$nome[mask_emp4], "<br>",
                           "Segmento:", df_vale$cod_segmento[mask_emp4], "<br>")) %>%
  addLayersControl(
    overlayGroups = c("EMP2", "EMP3", "EMP4"),
    options = layersControlOptions(collapsed = FALSE))



  
  
  





