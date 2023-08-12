
# Tema 01: Carga de datos ----

## Carga local
DF <- read.csv(file = "Personalidad y uso de apps (respuestas) - Respuestas de formulario 1.csv",
               check.names = FALSE)


## Carga en línea
install.packages("gsheet")
library(gsheet)

url_google <-"https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

DF <- read.csv(text = gsheet2text(url = url_google),
               check.names = F)



# Estructura del data frame
class(DF)
nrow(DF)
ncol(DF)

class(DF$`Escribe tu edad exacta`)
class(DF$Sexo)

install.packages("tidyverse")
library(tidyverse)

glimpse(DF)



# Tema 02: Transformación de datos ----
DF$`Escribe tu edad exacta`
is.na(DF$`Escribe tu edad exacta`)
summary(is.na(DF$`Escribe tu edad exacta`))

## Casos perdidos ----

### Reemplazo con la media

ifelse(test = is.na(DF$`Escribe tu edad exacta`),
       yes = mean(DF$`Escribe tu edad exacta`, na.rm = T),
       no = DF$`Escribe tu edad exacta`)

DF2 <- DF %>% 
  mutate(edad2 = ifelse(test = is.na(`Escribe tu edad exacta`),
                        yes = mean(`Escribe tu edad exacta`, na.rm = T),
                        no = `Escribe tu edad exacta`)) %>% 
  relocate(edad2, .after = `Escribe tu edad exacta`)



### Eliminar las filas completas
DF2 <- DF2 %>% na.omit()



## Estandarizaciones ----

### Normalización

scale(DF2$`Escribe tu edad exacta`)

mean(DF2$`Escribe tu edad exacta`)

tibble(
  original    = DF2$`Escribe tu edad exacta`,
  normalizada = scale(DF2$`Escribe tu edad exacta`)
) %>%
  print(n = 20)


# (convirtiendo el DF en tibble)
DF2 <- DF2 %>% as_tibble()



### Rango
library(scales)

tibble(
  original    = DF2$`Escribe tu edad exacta`,
  rango       = rescale(DF2$`Escribe tu edad exacta`)
) %>%
  print(n = 10)

min(DF2$`Escribe tu edad exacta`)
max(DF2$`Escribe tu edad exacta`)



## Agrupaciones ----

### Variables numéricas

cut(DF2$`Escribe tu edad exacta`,
    breaks = c(-Inf, 18, 23, Inf),
    labels = c("18 o menos", "19 a 23", "Más de 23"))

tibble(
  edad    = DF2$`Escribe tu edad exacta`,
  edad.gr = cut(DF2$`Escribe tu edad exacta`,
                breaks = c(-Inf, 18, 23, Inf),
                labels = c("18 o menos", "19 a 23", "Más de 23"))
) %>% 
  print(n = 20)



DF4 <- DF2 %>% 
  mutate(edad.gr = cut(`Escribe tu edad exacta`,
                       breaks = c(-Inf, 18, 23, Inf),
                       labels = c("18 o menos", "19 a 23", "Más de 23"))) %>% 
  relocate(edad.gr, .after = edad2)


### Variables categóricas

unique(DF4[,9])

tibble(
  
  original     = DF4[,9],
  
  categorizada = ifelse(
    test = DF4[,9] == "Totalmente verdadero" | DF4[,9] == "Un poco verdadero",
    yes = "SI",
    no  = "NO"
  )
  
)



## (bucles)

### Paso 1: Crear un vector que contenga lo nombres de las variables
### donde se aplicará el buble

frases <- colnames(DF4)[8:31]
frases

### Como el bucle sobreescribe, creamos un data.frame nuevo (DF5)
DF5 <- DF4



### Paso 2: Crear el bucle

for (elemento in frases) {
  
  DF5[,elemento] <- ifelse(
    
    test = DF5[,elemento] == "Totalmente verdadero" |
      DF5[,elemento] == "Un poco verdadero",
    yes = 1,
    no = 0
    
  )
  
}



















