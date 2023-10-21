
library(ggcorrplot)
library(FactoMineR)
library(tidyverse)

# Matriz de correlaciones ----
frases3 <- as.vector(frases2)

r <- cor(
  x = DF6 %>% select(all_of(frases3)),
  method = "spearman"
)


## Gráfico

ggplotly(
  ggcorrplot(corr = r,
             # type = "upper",
             colors = c("red", "white", "blue"),
             show.legend = F,
             tl.cex = 10) +
    
    theme(axis.text.x = element_blank(),
          panel.grid.major = element_blank())
)



# PCA: Principal Component Analysis

## Dimension 1: social
social <- frases3[c(20,23)]


## Creando la dimensión
PCA.social <- FactoMineR::PCA(
  DF6 %>% select(all_of(social)),
  ncp = 1
)



## Eigenvalue y variación explicada
PCA.social$eig


## Correlacion entre dimension y var orig
PCA.social$var$cor



## valores de la dimensión
head(PCA.social$ind$coord*-1)



## compara valores orig con valores de la dim
tibble(
  dim = PCA.social$ind$coord*-1,
  DF6 %>% select(all_of(social))
) %>% View()



## Dimension 2: introversion
introversion <- frases3[c(11,21,22)]


## Creando la dimensión
PCA.introversion <- FactoMineR::PCA(
  DF6 %>% select(all_of(introversion)),
  ncp = 1
)



## Eigenvalue y variación explicada
PCA.introversion$eig


## Correlacion entre dimension y var orig
PCA.introversion$var$cor



## valores de la dimensión
head(PCA.introversion$ind$coord)



## compara valores orig con valores de la dim
tibble(
  dim = PCA.introversion$ind$coord,
  DF6 %>% select(all_of(introversion))
) %>% View()



## Dimension 3: exito
exito <- frases3[c(2,5,10,16)]


## Creando la dimensión
PCA.exito <- FactoMineR::PCA(
  DF6 %>% select(all_of(exito)),
  ncp = 1
)



## Eigenvalue y variación explicada
PCA.exito$eig


## Correlacion entre dimension y var orig
PCA.exito$var$cor



## valores de la dimensión
head(PCA.exito$ind$coord)



## compara valores orig con valores de la dim
tibble(
  dim = PCA.exito$ind$coord,
  DF6 %>% select(all_of(exito))
) %>% View()



## Dimension 4: riesgo
riesgo <- frases3[c(7,8)]


## Creando la dimensión
PCA.riesgo <- FactoMineR::PCA(
  DF6 %>% select(all_of(riesgo)),
  ncp = 1
)



## Eigenvalue y variación explicada
PCA.riesgo$eig


## Correlacion entre dimension y var orig
PCA.riesgo$var$cor



## valores de la dimensión
head(PCA.riesgo$ind$coord*-1)



## compara valores orig con valores de la dim
tibble(
  dim = PCA.riesgo$ind$coord*-1,
  DF6 %>% select(all_of(riesgo))
) %>% View()



## Dimension 5: valores
valores <- frases3[c(3,18, 14, 1)]


## Creando la dimensión
PCA.valores <- FactoMineR::PCA(
  DF6 %>% select(all_of(valores)),
  ncp = 1
)



## Eigenvalue y variación explicada
PCA.valores$eig


## Correlacion entre dimension y var orig
PCA.valores$var$cor



## valores de la dimensión
head(PCA.valores$ind$coord)



## compara valores orig con valores de la dim
tibble(
  dim = PCA.valores$ind$coord,
  DF6 %>% select(all_of(valores))
) %>% View()



# Agregando las dim al DF

DF11 <- DF6 %>% 
  
  mutate(social = PCA.social$ind$coord*-1,
         )






















