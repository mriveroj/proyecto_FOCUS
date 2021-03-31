install.packages("RColorBrewer")
install.packages("plyr")
install.packages("wordcloud")
install.packages("stringr")
install.packages("tm")
install.packages("SnowballC")
install.packages("reshape2")
install.packages("syuzhet")
install.packages("lubridate")

library(RColorBrewer)
library(plyr)
library(rtweet)
library(wordcloud)
library(stringr)
library(tm)
library(SnowballC)
library(reshape2)
library(dplyr)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(stringi)

comentarios <- read.csv(file="Comentarios.csv",sep=";", na.strings=c("","NA"))

comentarios <- comentarios[-1,]
comentarios_tripadvisor <- readRDS("comentarios_separados.rds") 

i=2
mat_comentarios <- c()
for(i in 1:dim(comentarios_tripadvisor)[2]){
        com_temp <- comentarios_tripadvisor[, i]
        com_temp <- com_temp[complete.cases(com_temp)]
        mat_comentarios <- c(mat_comentarios, com_temp)
        
}
for(i in 1:dim(comentarios)[2]){
        com_temp2 <- comentarios[, i]
        com_temp2 <- com_temp2[complete.cases(com_temp2)]
        mat_comentarios <- c(mat_comentarios, com_temp2)
        
}


mat_comentarios <- as.data.frame(mat_comentarios)
mat_comentarios <- mat_comentarios[!duplicated(mat_comentarios), ]
mat_comentarios <- stri_trans_general(mat_comentarios,"Latin-ASCII")
rm(com_temp, com_temp2)

corpus_restaurantes <- Corpus(VectorSource(mat_comentarios))

#inspect(corpus_restaurantes[1:3])

corpus_restaurantes <-tm_map(corpus_restaurantes, str_replace_all, pattern="[[:punct:]]", replacement = " ")
corpus_restaurantes <-tm_map(corpus_restaurantes, str_replace_all, pattern="[^a-zA-Z0-9 ]", replacement = "")
#corpus_restaurantes <-tm_map(corpus_restaurantes, str_replace_all, pattern="^\\s+|\\s+$", replacement = "")
corpus_restaurantes <-tm_map(corpus_restaurantes, tolower)
corpus_restaurantes <-tm_map(corpus_restaurantes, removeNumbers)
corpus_restaurantes <-tm_map(corpus_restaurantes, stemDocument)
corpus_restaurantes<-tm_map(corpus_restaurantes,stripWhitespace)
corpus_restaurantes <- tm_map(corpus_restaurantes, removeWords, stopwords(kind = "es"))


inspect(corpus_restaurantes[1:3])

restaurantes_tdm <- TermDocumentMatrix(corpus_restaurantes, control = list(stopwords = TRUE)) %>% as.matrix()
restaurantes_dtm <- DocumentTermMatrix(corpus_restaurantes, control = list(minWordLength = 1, stopwords = TRUE))
inspect(restaurantes_dtm) # Permite ver cuantas veces aparecen las palabras en el documento


restaurantes_tdm<-as.matrix(restaurantes_tdm)
restaurantes_tdm[1:10,1:20]

head(findFreqTerms(restaurantes_dtm, lowfreq=20), 40)

w<-rowSums(restaurantes_tdm)
#w<-subset(w,w>=500)
w <- as.data.frame(w)
w = data.frame("palabras"=rownames(w), "frecuencia"=w$w) %>% arrange(-w) %>%  head(10)

ggplot(w, aes(x=reorder(palabras, frecuencia), y=frecuencia)) + geom_bar(stat="identity", fill="dodgerblue2", alpha=0.7) + 
        theme_bw() + 
        coord_flip() + 
        labs(x="Palabras más frecuentes", y="Frecuencia")


library(wordcloud2)
wordcloud2(w,
           size = 0.5,
           shape = "star",
           rotateRatio = 0.5,
           minSize = 1)


#SENTIMENT ANALYSIS
str(comentarios)
mat_comentarios
mat_comentarios<-iconv(mat_comentarios,from = "", to="latin1")

View(comentarios)
s<-get_nrc_sentiment(mat_comentarios,language = "spanish")
colSums(s)
head(s)

#Bar Plot
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab="count",
        main="Analisis de sentimiento de los restaurantes del Atl?ntico")


#para cuantificar los sentimientos
k<-get_sentiment(mat_comentarios,language = "spanish")
s$porcentaje<-k
View(s)
s$percepcion[s$porcentaje>0]<-"positivo"
s$percepcion[s$porcentaje<0]<-"negativo"
s$percepcion[s$porcentaje<=0 & s$porcentaje>=0]<-"neutral"
fct_count(s$percepcion)

ggplot(s, aes(x = percepcion)) + geom_bar()+ xlab("percepcion") + ylab ("N?mero de tweets") #diagrama de barras de la percepcion



