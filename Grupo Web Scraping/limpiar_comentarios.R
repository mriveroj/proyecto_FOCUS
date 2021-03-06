## Limpieza de scripts

# Cargamos las librerías necesarias
library(tidyverse)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(rtweet)
library(wordcloud)
library(stringr)
library(tm)
library(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)
ls("package:sentiment")

#### Cargamos los datos ####
data <- readRDS("restaurant_trip_advisor.rds")
str(data)
class(data)


#### Hacemos un pre-procesamiento inicial ####

# Quitamos la úlitma columna que parece estar de más
data <- data[, -10]

# Cambiamos nombres de columna para hacerlos más legibles
colnames(data) <- c("id", "cocina", "rango_precios", "calificacion", "numero_calificaciones", 
                    "direccion", "horario", "comentarios", "ranking")

# Pasamos de character a string la columna Calificación
data$calificacion <- gsub(",", ".", data$calificacion)
data$calificacion <- as.double(data$calificacion)
str(data)

# Corregimos número de calificaciones para dejarlas como numéricas
unique(data$numero_calificaciones)
data$numero_calificaciones <- gsub("opiniones", "", data$numero_calificaciones)
data$numero_calificaciones <- gsub("opinión", "", data$numero_calificaciones)
data$numero_calificaciones <- str_trim(data$numero_calificaciones)
unique(data$numero_calificaciones)
data$numero_calificaciones[c(331, 602, 723)] <- "1371"
data$numero_calificaciones[c(422, 452, 511)] <- "1588"
data$numero_calificaciones <- as.integer(data$numero_calificaciones)
str(data)

# Corregimos rangos de precios para hacerlos más interpretables
data$rango_precios <- as.factor(data$rango_precios)
data$rango_precios <- fct_collapse(data$rango_precios, "Bajo"= "$", "Medio"="$$ - $$$", "Alto"="$$$$", 
                                   "No disponible"=c("0", "Asiática", "Bar", "Café", "Caribeña", "China",
                                                     "Churrasquería", "Comida rápida", "Española", 
                                                     "Estadounidense", "Internacional", "Italiana",
                                                     "Japonesa", "Mariscos", "Mediterránea",
                                                     "Mercado de alimentos especializados", "Mexicana",
                                                     "Parrilla", "Peruana", "Pizzería", "Saludable", 
                                                     "Sudamericana", "Sushi", NA))
fct_count(data$rango_precios)

# Corregimos columna de cocina
data$cocina <- as.factor(data$cocina)
data$cocina <- fct_collapse(data$cocina, "No disponible"="0")
fct_count(data$cocina)


### Corregimos la columna de comentarios
data$comentarios <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data$comentarios) # Quitamos Retweets
head(data$comentario, 10)
data$comentarios <- gsub("@\\w+", "", data$comentarios) # Quitamos mencion a personas
data$comentarios <-  gsub("\\bhttp[a-zA-Z0-9]*\\b", "", data$comentarios) # Quitamos los links html
data$comentarios <- gsub("[^a-zA-Z0-9 ]", "", data$comentarios) # Se quietan carácteres no alfanuméricos
data$comentarios <- gsub("[[:punct:]]", "", data$comentarios) # Quitamos puntuación
data$comentarios <- gsub("amp ", "", data$comentarios)# Quitamos la palabra amp
data$comentarios <-  gsub("\\btco[a-zA-Z0-9]*\\b", "", data$comentarios) # Quitamos los Toc
data$comentarios <- iconv(data$comentarios, 'UTF-8', 'ASCII') # Quitamos los emojis
data$comentarios <- gsub("[ \t]{2,}", "", data$comentarios) # Quitamos tabuladores
data$comentarios <- gsub("^\\s+|\\s+$", "", data$comentarios) # Quitamos tabuladores x2
data$comentarios <- tolower(data$comentarios) # Pasamos todo a minúsculas
head(data$comentarios, 3)

#write.csv(data, "restaurant_tripAdvisor_corregido.csv")



##### Procedemos con el análisis de los textos #####

comentarios <- read.csv("comentarios_separados.csv") %>% select(-1)

### Corregimos los comentarios
comentarios <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",comentarios) # Quitamos Retweets
comentarios<- gsub("@\\w+", "", comentarios) # Quitamos mencion a personas
comentarios<-  gsub("\\bhttp[a-zA-Z0-9]*\\b", "", comentarios) # Quitamos los links html
comentarios <- gsub("[^a-zA-Z0-9 ]", "", comentarios) # Se quietan carácteres no alfanuméricos
comentarios<- gsub("[[:punct:]]", "", comentarios) # Quitamos puntuación
comentarios <- gsub("amp ", "", comentarios)# Quitamos la palabra amp
comentarios <-  gsub("\\btco[a-zA-Z0-9]*\\b", "", comentarios) # Quitamos los Toc
comentarios <- iconv(comentarios, 'UTF-8', 'ASCII') # Quitamos los emojis
comentarios <- gsub("[ \t]{2,}", "", comentarios) # Quitamos tabuladores
comentarios <- gsub("^\\s+|\\s+$", "", comentarios) # Quitamos tabuladores x2
comentarios <- tolower(comentarios) # Pasamos todo a minúsculas

corpus_restaurantes <- Corpus(VectorSource(comentarios))
inspect(corpus_restaurantes[1:3])
corpus_restaurantes_limpio <- tm_map(corpus_restaurantes, removeWords, stopwords(kind = "es"))

# Hacemos un Term-document-matrix (tdm) y un Document-Term-Matrix (dtm)
restaurantes_tdm <- TermDocumentMatrix(corpus_restaurantes_limpio, control = list(stopwords = TRUE)) %>% as.matrix()
restaurantes_dtm <- DocumentTermMatrix(corpus_restaurantes_limpio, control = list(minWordLength = 1, stopwords = TRUE))
inspect(restaurantes_dtm) # Permite ver cuantas veces aparecen las palabras en el documento

# Hacemos el stem
restaurantes_stem <- tm_map(corpus_restaurantes_limpio, stemDocument)
inspect(restaurantes_stem[1:5])
head(findFreqTerms(restaurantes_dtm, lowfreq=10), 40)
findAssocs(restaurantes_dtm, 'comida', 0.90)



## Clasificación de emociones
restaurantes_emotions <- classify_emotion(comentarios, algorithm="bayes", prior=1.0)
