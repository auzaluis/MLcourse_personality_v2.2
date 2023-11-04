
library(NbClust)
library(FactoMineR)

# Crear un vector con los nombres de las DIM
dimensiones <- DF11 %>% 
  select(social:valores) %>% 
  colnames()



# Clusterización

clustering <- NbClust(
  
)