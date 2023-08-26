
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



## Manipulación de data frames ----

### Función select: columnas ----
DF5 %>% select(Sexo)  #Usando tidyvere
DF5[,"Sexo"]          #Usando sintaxis base

DF5 %>% select(Sexo, `Escribe tu edad exacta`)
DF5[,c("Sexo", "Escribe tu edad exacta")]

DF5 %>% select(-`Marca temporal`)
DF5 %>% select(!c("Marca temporal", "Sexo"))

DF5 %>% select(starts_with("edad"))
DF5 %>% select(ends_with("00"))
DF5 %>% select(contains("edad"))



### Función filter: filas ----
DF5[DF5["Sexo"]=="Mujer", "Sexo"]
DF5 %>%
  filter(Sexo == "Mujer") %>% 
  select(Sexo)


DF5 %>%
  filter(Sexo != "Hombre") %>% 
  select(Sexo)

DF5 %>%
  filter(`Escribe tu edad exacta` >= 20) %>% 
  select(`Escribe tu edad exacta`)

DF5 %>%
  filter(`Escribe tu edad exacta` >= 20,
         Sexo == "Hombre") %>% 
  select(`Escribe tu edad exacta`, Sexo)


#Forma1: usando coma
DF5 %>%
  filter(`Escribe tu edad exacta` >= 18,
         `Escribe tu edad exacta` <= 21) %>% 
  select(`Escribe tu edad exacta`)

#Forma2: usando &
DF5 %>%
  filter(`Escribe tu edad exacta` >= 18 &
           `Escribe tu edad exacta` <= 21) %>% 
  select(`Escribe tu edad exacta`)

#Forma3: usando %in%
DF5 %>%
  filter(`Escribe tu edad exacta` %in% 18:21) %>% 
  select(`Escribe tu edad exacta`)


#Forma4: usando la sintaxis
DF5[DF5["Escribe tu edad exacta"] >= 18 &
      DF5["Escribe tu edad exacta"] <= 21,
    "Escribe tu edad exacta"] 



### Renombrado de columnas ----
DF6 <- DF5

#### APPS
## Paso 1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

## Paso 2: Renombrar
colnames(DF6)[33:36] <- apps



#### Frases
## Paso 1: Crear un vector con los nuevos nombres
frases2 <- frases %>% 
  as_tibble() %>% 
  separate(col = value,
           into = c("NoSirve", "Sirve"),
           sep = "\\[") %>% 
  select("Sirve") %>% 
  separate(col = Sirve,
           into = c("Sirve", "NoSirve"),
           sep = "\\]") %>% 
  select("Sirve") %>% 
  as_vector()

## Paso 2: Renombrar
colnames(DF6)[8:31] <- frases2


### Pivotado ----
#### Pivot longer
DF7 <- DF6 %>%
  select(`Marca temporal`, Sexo, apps) %>% 
  pivot_longer(cols = apps, 
               names_to = "app",
               values_to = "time")



#### Pivot wider
DF8 <- DF7 %>% 
  pivot_wider(names_from = "app",
              values_from = "time")



# (transformación de horas a num)

# strsplit separa los textos
strsplit(x = DF7$time, split = ":") %>% 
  head()

# transformación
DF7$time <- sapply(X = strsplit(x = DF7$time, split = ":"),
                   
                   function(x) {
                     x <- as.numeric(x)
                     x[1] + x[2]/60 + x[3]/60^2
                   })



## Graficas con GGPLOT ----

### Boxplots

#boxplot feo
boxplot(DF7$time, range = 2.5)



#boxplot bonito
library(plotly)

ggplotly(
  
  DF7 %>% 
    
    ggplot(mapping = aes(x = app,
                         y = time,
                         fill = app)) +
    geom_boxplot() +
    
    theme_classic() +
    
    labs(title = "Uso de apps en horas/semana",
         y = "Horas/semana",
         x = "") +
    
    theme(legend.position = "none")
  
)



# Apertura por sexo

ggplotly(
  
  DF7 %>% 
    
    ggplot(mapping = aes(x = app,
                         y = time,
                         fill = app)) +
    geom_boxplot() +
    
    facet_wrap(~ Sexo) +
    
    theme_minimal() +
    
    labs(title = "Uso de apps en horas/semana",
         y = "Horas/semana",
         x = "") +
    
    theme(legend.position = "none",
          panel.grid.major.x = element_blank())
  
)



ggplotly(
  
  DF7 %>% 
    
    ggplot(mapping = aes(x = Sexo,
                         y = time,
                         fill = Sexo)) +
    geom_boxplot() +
    
    facet_wrap(~ app, nrow = 1) +
    
    theme_minimal() +
    
    labs(title = "Uso de apps en horas/semana",
         y = "Horas/semana",
         x = "") +
    
    theme(legend.position = "none",
          panel.grid.major.x = element_blank())
  
)



# Gráfico de violin

ggplotly(
  
  DF7 %>% 
    
    ggplot(mapping = aes(x = app,
                         y = time,
                         fill = app)) +
    geom_violin() +
    
    theme_classic() +
    
    labs(title = "Uso de apps en horas/semana",
         y = "Horas/semana",
         x = "") +
    
    theme(legend.position = "none")
  
)



















