
datos <- readRDS("C:/Users/USUARIO_PC/Documents/GitHub/proyecto_FOCUS/Grupo Web Scraping/restaurant_trip_advisor.rds") 
datos <- readRDS("restaurant_trip_advisor.rds")
library(tidyverse)
dat<- data.frame(id=datos$id, comentarios= datos$Comentarios)

dat<-dat %>%
  separate(comentarios,c("coment1","coment2","coment3","coment4","coment5","coment6",
                     "coment7","coment8","coment9","coment10","coment11",
                     "coment12","coment13","coment14","coment15","coment16",
                     "coment17","coment18","coment19","coment20","coment21","coment22",
                     "coment23","coment24","coment25","coment26","coment27"
                     ,"coment29","coment30","coment31"),
           sep= "~")

seq <- seq(from=2, to=30, by=2)
dat <- dat %>% select(-seq)

#write.csv(dat, "comentarios_separados.csv")
