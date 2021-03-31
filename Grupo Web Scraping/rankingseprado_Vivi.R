

library(tidyverse)
dat_vivi<- readxl::read_excel("Restaurantes por municipios.xlsx")
dat_vivi2 <- dat_vivi[dat_vivi$puntuacion!="",c("nombre","puntuacion")]
dat_vivi2<-rename(dat_vivi2, Nombre= "nombre", Ranking= "puntuacion")

dat_vivi2$rank_num <- 0
dat_vivi2$ciudad <- "Barranquilla"
dat_vivi2$posrank<-0
dat_vivi2$posrankde<-0
dat_vivi2$tipo_rank<- "ninguno"
for (i in 1:NROW(dat_vivi2)) {
  cadena <- unlist(strsplit(dat_vivi2$Ranking[i],split = " "))
  dat_vivi2$posrank[i]<- as.double(gsub(x = cadena[2],pattern = "#",""))
  dat_vivi2$posrankde[i]<- as.double(gsub(x = cadena[4],pattern = "#",""))
  dat_vivi2$rank_num[i] <- (as.double(gsub(x = cadena[2],pattern = "#",""))) / (as.double(cadena[4]))
  dat_vivi2$tipo_rank[i] <- paste0(cadena[-1:-4],collapse = " ")
  cadena2 <- unlist(strsplit(dat_vivi2$Ranking[i],split = "en"))
  dat_vivi2$ciudad[i] <- paste0(cadena2[-1],collapse = " ")
  
}

tripad_rest<- readRDS("restaurant_trip_advisor.rds")

tripad_rest2 <- tripad_rest[tripad_rest$Ranking!="",c("Nombre","Ranking")]

tripad_rest2$rank_num <- 0
tripad_rest2$ciudad <- "Barranquilla"
tripad_rest2$posrank<-0
tripad_rest2$posrankde <-0

for (i in 1:NROW(tripad_rest2)) {
  cadena <- unlist(strsplit(tripad_rest2$Ranking[i],split = " "))
  tripad_rest2$posrank[i]<- as.double(gsub(x = cadena[1],pattern = "#",""))
  tripad_rest2$posrankde[i]<- as.double(gsub(x = cadena[3],pattern = "#",""))
  tripad_rest2$rank_num[i] <- (as.double(gsub(x = cadena[1],pattern = "#",""))) / (as.double(cadena[3]))
  tripad_rest2$tipo_rank[i] <- paste0(cadena[-1:-3],collapse = " ")
}

dat_rank<- rbind(dat_vivi2,tripad_rest2)
dat_rank$tipo_rank<-str_replace(dat_rank$tipo_rank, pattern = "ent",replacement = "int" )
dat_rank$tipo_rank<-str_replace(dat_rank$tipo_rank, pattern = "xedas",replacement = "ia" )
dat_rank$tipo_rank<-str_replace(dat_rank$tipo_rank, pattern = "xe1s",replacement = "as" )
dat_rank$ciudad <- str_replace(dat_rank$ciudad, pattern = "xe1s",replacement = "as" )

dat_rank$tipo_res<-"Ninguno"

for (i in 1:NROW(dat_rank)) {
  cadena3 <- unlist(strsplit(dat_rank$tipo_rank[i],split = "en"))
  dat_rank$tipo_res[i] <- paste0(cadena3[-2],collapse = " ")
  
}
unique(dat_rank$ciudad)

library(plotly)
dat_bar<- table(dat_rank$tipo_res)
dat_bar<- as.data.frame(dat_bar)
fig <- plot_ly(
  x = dat_bar$Var1,
  y = dat_bar$Freq,
  name = "SF Zoo",
  type = "bar"
)

fig
dat_bar2<- table(dat_rank$ciudad)
dat_bar2<- as.data.frame(dat_bar2)
fig2 <- plot_ly(
  x = dat_bar2$Var1,
  y = dat_bar2$Freq,
  name = "SF Zoo",
  type = "bar"
)

fig2






