
library(ggcorrplot)
library(FactoMineR)

# Matriz de correlaciones ----
frases3 <- as.vector(frases2)

r <- cor(
  x = DF6 %>% select(all_of(frases3)),
  method = "spearman"
)
