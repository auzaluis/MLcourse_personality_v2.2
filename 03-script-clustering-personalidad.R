
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


## Cruzar por el consumo de apps

### Convertir las horas en números
### Creando función para convertir

funct_conversion <- function(app) {
  sapply(strsplit(app, split = ":"),
         function(x) {
           x <- as.numeric(x)
           x[1] + x[2]/60 + x[3]/60^2
         })
}



### Aplicar la función a las columnas apps

DF15 <- DF13 %>% 
  mutate_at(.vars = all_of(apps),
            .funs = funct_conversion) %>% 
  
  # nombrar a los segmentos
  mutate(segmento = case_when(
    segmento == 1 ~ "ganadores",
    segmento == 2 ~ "sociables",
    segmento == 3 ~ "precavidos",
    segmento == 4 ~ "tímidos",
    segmento == 5 ~ "reservados",
    .default = NA
    
  ))



# Consumo de apps por segmento

DF16 <- DF15 %>% 
  group_by(segmento) %>% 
  summarise_at(.vars = apps,
               .funs = mean) %>% 
  column_to_rownames("segmento")

FactoMineR::CA(DF16)



prop.table(
  base::table(DF15$segmento, DF15$Sexo),
  margin = 1
)


prop.table(
  base::table(DF15$segmento, DF15$edad.gr),
  margin = 1
)








