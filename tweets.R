library(rtweet)
library(wordcloud2)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(igraph)
library(ggraph)


# #----Datos de la App----
# api_key             <- "BiOJhEwMOXBafRjoLE67Qh6MV"
# api_secret          <- "vKNW6kkGMTUB3hT0IVYbYhQkHGhq3Rrl91SrLo4taILwRhsAGw"
# access_token        <- "1247606882-QIvdfavoGJscykfujLabQoIHBATWVmb3j0vZ2Bl"
# access_token_secret <- "68xzFWNNOHT4YYWSIpB4AMrtZ05nXjBGka6xmTDUtvhjE"
# twitter_app         <- "viviana990320"
# 
# 
# #---- Accedemos a Twitter a traves de los datos del token----
# create_token(
#   app             = twitter_app,
#   consumer_key    = api_key,
#   consumer_secret = api_secret,
#   access_token    = access_token,
#   access_secret   = access_token_secret)
# 
# #---- Analisis de tweets ----
# #trabajaremos con estos por ahora
# datos<- search_tweets("#comida OR comida OR delicia OR sabor OR
# Rica OR saludable OR cocina OR #almuerzo OR #desayunos OR #cena OR comida típica OR
# #MenuDelDia OR #Menu OR pollo OR ceviche OR hamburguesa OR servicio 
#                       OR italiana OR vegano OR libanesa OR
#                       parrillada OR mariscos OR china OR
#                       postres OR postre OR vino OR jugos
#                       OR comidacaribeña OR cerveza",n = 3000, include_rts = FALSE, lang="es", geocode =  '10.99904,-74.863,200km')
# 
# 
# saveRDS(datos,file="tweets.Rda")
tweets<-readRDS(file="tweets.Rda")
# str(tweets)
# tweets$text[1:5]

#----Limpieza de datos ----
#Convertimos el texto a mayusculas
tweets$text <- toupper(tweets$text)
#Eliminar la tildes
tweets$text <- chartr(old = "ÁÉÍÓÚ",new = "AEIOU",x = tweets$text)
# head(tweets$text, 5)
#Convertimos a minusculas
tweets$text<- tolower(tweets$text)
# head(tweets$text, 5)

# Sacamos el texto y lo definimos con la siguiente variable
tweet_txt <- tweets$text
# Mostramos los 10 primeros
#head(tweet_txt, 10)

#Limpieza de datos
tweet_txt<- gsub("â","(RT|via)((?:\\b\\W*@\\w+)+)",tweet_txt)

# Veamos los 10 primeros tweets
#head(tweet_txt, 10)

#Eliminacion de usuarios
tweet_txt<- gsub("@\\w+", "", tweet_txt)

#Eliminacion de links
tweet_txt <-  gsub("\\bhttp[a-zA-Z0-9]*\\b", "", tweet_txt)
#head(tweet_txt, 5)

#Eliminacion de caracteres NO alfanumericos
tweet_txt <- gsub("[^a-zA-Z0-9 ]", "", tweet_txt)
#head(tweet_txt, 5)
#Eliminando la puntuacion
tweet_txt <- gsub("[[:punct:]]", "", tweet_txt)
#head(tweet_txt, 5)

#Quitamos los tco. 
#Los tco son un servicio que se usan para cortar los links
#Ejemplo:tcowUfSXy00Qq tcouE1uBIKakb
tweet_txt <-  gsub("\\btco[a-zA-Z0-9]*\\b", "",tweet_txt)
#head(tweet_txt, 5)

#Eliminacion de NAs 
tweet_txt <- tweet_txt[!is.na(tweet_txt)]
#head(tweet_txt, 5)

#Eliminamos emojis
tweet_txt <- iconv(tweet_txt, 'UTF-8', 'ASCII')
#head(tweet_txt, 5)

#Limpieza de datos tabuladores
tweet_txt <- gsub("[ \t]{2,}", "",tweet_txt)
tweet_txt<- gsub("^\\s+|\\s+$", "", tweet_txt)
head(tweet_txt, 5)

#----Construccion corpus----
#Construimos el corpus
library(tm)
tweets_corpus<-iconv(tweet_txt)
tweets_corpus<-Corpus(VectorSource(tweet_txt))
inspect(tweets_corpus[1:5])

#Pasamos todo a mayusculas
tweets_corpus<-tm_map(tweets_corpus,tolower)
inspect(tweets_corpus[1:5])

#Eliminamos la puntuacion
tweets_corpus<-tm_map(tweets_corpus,removePunctuation)
inspect(tweets_corpus[1:5])
#Eliminamos los numeros
tweets_corpus<-tm_map(tweets_corpus,removeNumbers)

# Eliminamos los espacios en blanco
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)

#Eliminando palabras comunes
#Ejemplo: de, la , que, el, etc
tweets_cleanset<-tm_map(tweets_corpus,removeWords,stopwords("es"))
tweets_cleanset<-tm_map(tweets_cleanset,removeWords,c("mas","dia","tan","vez"))

inspect(tweets_cleanset[1:5])

tweets_tdm<-TermDocumentMatrix(tweets_cleanset)
tweets_tdm
tweets_tdm<-as.matrix(tweets_tdm)
tweets_tdm[1:10,1:20]

#---- Grafico con ggplot2 ----
#Construccion de diagram,a de barras
#Diagrama de barras
t1<-rowSums(tweets_tdm)
t1<-subset(t1,t1>=20)
t1 <- as.data.frame(t1)
t1 = data.frame("palabras"=rownames(t1), "frecuencia"=t1$t1) %>% arrange(-t1) %>%  head(10)
ggplotly(ggplot(t1, aes(x=reorder(palabras, frecuencia), y=frecuencia)) + geom_bar(stat="identity", fill="dodgerblue2", alpha=0.7) + 
  theme_classic() + 
  coord_flip() + 
  labs(x="Palabras mas frecuentes", y="Frecuencia",
  title="Frecuencia de palabras"))

#---- Nube de palabras ----
t<-rowSums(tweets_tdm)
t<-subset(t,t>=20)
library(wordcloud2)
t<-data.frame(names(t),t)
colnames(t)<-c("word","freq")
wordcloud2(t,
           size = 1,
           shape = "cicle",
           rotateRatio = 0.5,
           minSize = 1)

#----Analisis de sentimiento----
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#obtain sentiment scores
sentiment<-iconv(tweets$text,to="UTF-8")
View(sentiment)
s<-get_nrc_sentiment(sentiment,language = "spanish")
head(s)
colnames(s)<-c("Enojo", "Expectativa", "Disgusto", "Preocupacion", "Alegria", "Tristeza", "Sorpresa", "Confianza", 
               "Negativo", "Positivo")

s_graphT <- as.data.frame(colSums(s))
s_graphT$`colSums(s)` <- as.integer(s_graphT$`colSums(s)`)
s_graphT <- data.frame("palabras"=rownames(s_graphT), "frecuencia"=s_graphT$`colSums(s)`) %>% arrange(-frecuencia)

#Barplot de emociones
ggplotly(ggplot(s_graphT, aes(x=reorder(palabras, -frecuencia), y=frecuencia)) + geom_bar(stat="identity", fill="firebrick", alpha=0.7) + 
  theme_classic()+
  labs(x="Emociones", y="Frecuencia", title="Analisis de sentimientos Tweets") + 
  theme(axis.text.x = element_text(angle=50, hjust=1, size=10.5)))

#Para cuantificar los sentimientos
k<-get_sentiment(tweets$text,language = "spanish")
s$porcentaje<-k
View(s)
s$percepcion[s$porcentaje>0]<-"positivo"
s$percepcion[s$porcentaje<0]<-"negativo"
s$percepcion[s$porcentaje<=0 & s$porcentaje>=0]<-"neutral"
#Diagrama de barras de la percepcion
ggplotly(ggplot(data=s, aes(x=percepcion))+
  geom_bar(aes(fill=percepcion))+
  theme_classic()+
  labs(y="Numero de tweets", x="Percepción",
       title="Percepción de los tweets"))





####Bigrams
#Para analizar las palabras seguidas 
A <- as.data.frame(tweet_txt)
bigramsTweets <- A %>%
  unnest_tokens(bigram,A, token = "ngrams", n = 2)

bigramsTweets <- bigramsTweets[complete.cases(bigramsTweets), ]
bigramsTweets <- as.data.frame(bigramsTweets)

#Contamos cuantas veces se repiten
conteoT <-bigramsTweets %>%
  count(bigram, sort = TRUE)

#Las separamos
bigrams_separatedT <- bigramsTweets %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filteredT <- bigrams_separatedT %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#Eliminamos palabras que no dicen mucho
bigrams_filteredT <- bigrams_filteredT %>%
  filter(!word1 %in% c("este","a","y","se","con"))


#Las volvemos a contar
bigram_countsT <- bigrams_filteredT %>% 
  count(word1, word2, sort = TRUE)

#Nos quedamos con las que tienen un n>20
bigram_graphT <- bigram_countsT %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2020)
b <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graphT, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = b, end_cap = circle(.05, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



