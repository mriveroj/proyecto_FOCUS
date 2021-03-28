

library(tidyverse)
dat_vivi<- readxl::read_excel("Restaurantes por municipios.xlsx")
dat_vivi2 <- dat_vivi[dat_vivi$puntuacion!="",c("nombre","puntuacion")]

dat_vivi2$rank_num <- 0
dat_vivi2$ciudad <- "Barranquilla"


for (i in 1:NROW(dat_vivi2)) {
  cadena <- unlist(strsplit(dat_vivi2$puntuacion[i],split = " "))
  dat_vivi2$posrank[i]<- as.double(gsub(x = cadena[2],pattern = "#",""))
  dat_vivi2$posrankde[i]<- as.double(gsub(x = cadena[4],pattern = "#",""))
  dat_vivi2$rank_num[i] <- (as.double(gsub(x = cadena[2],pattern = "#",""))) / (as.double(cadena[4]))
  dat_vivi2$tipo_rank[i] <- paste0(cadena[-1:-4],collapse = " ")
  cadena2 <- unlist(strsplit(dat_vivi2$puntuacion[i],split = "en"))
  dat_vivi2$ciudad[i] <- paste0(cadena2[-1],collapse = " ")
  
}


