
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
