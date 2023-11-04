
library(NbClust)
library(FactoMineR)
library(tidyverse)

# Crear un vector con los nombres de las DIM
dimensiones <- DF11 %>% 
  select(social:valores) %>% 
  colnames()



# Clusterización

clustering <- NbClust(
  data = DF11 %>% select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn"
)

clustering$All.index


## Creación de 5 clusters

clustering.5 <- NbClust(
  data = DF11 %>% select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn",
  
  # Limitar la iteración
  min.nc = 5,
  max.nc = 5
)



# tabla que muestra el peso de cada segmento

clustering.5
table(clustering.5$Best.partition)

# Analizando los segmentos

## Creando el DF12

DF12 <- DF11 %>% 
  mutate(segmento = as.factor(clustering.5$Best.partition))

summary(DF12$segmento)



# Convirtiendo a rango 0-1
library(scales)

DF13 <- DF12 %>%
  mutate_at(.vars = dimensiones,
            .funs = rescale)


## Resumen

DF14 <- DF13 %>% 
  group_by(segmento) %>% 
  summarise(social = mean(social),
            introversion = mean(introversion),
            exito = mean(exito),
            riesgo = mean(riesgo),
            valores = mean(valores)) %>% 
  column_to_rownames(var = "segmento")

DF14


## Mapa perceptual

FactoMineR::CA(DF14)























