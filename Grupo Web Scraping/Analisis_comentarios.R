## ANÁLISIS DE SENTIMIENTO

library(RColorBrewer)
library(plyr)
library(rtweet)
library(wordcloud)
library(stringr)
library(tm)
library(SnowballC)




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
