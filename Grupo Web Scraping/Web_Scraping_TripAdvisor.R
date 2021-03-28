
# Cargar los paquetes:
library(rvest)
library(robotstxt)
library(dplyr)
library(stringr)
library(forcats)
library(tidyr)
library(ggplot2)
library(purrr)
require(XML)


#-------------------------TripAdvisor----------------------------

# BASE DE DATOS

# Aqui se lee el archivo rds con la base de datos, recuerden cambiar al directorio
#  donde se encuentra el archivo rds
data <- readRDS(file = "restaurant_trip_advisor.rds")


# El resto es el procedimiento

# Esto nada mas son pruebas
url <- 'https://www.tripadvisor.com/Restaurants-g297473-c33-Barranquilla_Atlantico_Department.html'
html_web <- read_html(url)
(html_nodes(html_web, "._15_ydu6b") %>% html_attr('href'))[]
html_nodes(html_web, "#component_2 > div > div:nth-child(2) > span > div._1kNOY9zw > div._2Q7zqOgW > div._2kbTRHSI > div > span > a") %>%
html_attr('href')


# Tipos de cocina
cocina <-
c('Saludable', 'De la india', 'Cafeterías', 'Pizza', 'Tailandesa', 'Española',
 'Americana', 'Francesa', 'Peruana', 'Japonesa', 'China', 'Cubana', 'Steakhouses (Carnes)', 'Argentina',
 'Vegetariana', 'Vegana', 'Mediterránea', 'Italiana', 'Mexicana',
 'Bar y Pub', 'Caribeña', 'Árabe', 'Comida Rápida', 'Sudamericana',
 'Mariscos', 'Venezolana', 'Africana', 'Asiática', 'Griega',
 'Fusión', 'Internacional')

# Links de los tipos de cocina, hay 31
link_res <-c(
"https://www.tripadvisor.co/Restaurants-g297473-c10679-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c24-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.es/Restaurants-g297473-c8-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c31-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c39-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c36-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c2-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c20-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c10631-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.es/Restaurants-g297473-c27-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c11-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c10744-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.com/Restaurants-g297473-c37-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c10698-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-zfz10665-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-zfz10697-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c28-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c26-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c29-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.es/Restaurants-g297473-zfg11776-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c10-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.es/Restaurants-g297473-c11744-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c10646-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.es/Restaurants-g297473-c35-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c33-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c10715-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c1-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c3-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c23-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c17-Barranquilla_Atlantico_Department.html"
,"https://www.tripadvisor.co/Restaurants-g297473-c22-Barranquilla_Atlantico_Department.html")


# Estas son las categorías, tecnicamente para confirmar, no sirve de a mucho
fill_res <- data.frame(name = 1:NROW(link_res))
for (i in 1:NROW(link_res)) {
  
  fill_res[i,1] <- read_html(link_res[i]) %>% html_nodes("#HEADING") %>% html_text()
  
}

# Campos a extraer - Ejemplo
read_html(url) %>% html_nodes("#HEADING") %>% html_text()
read_html('https://www.tripadvisor.co/Restaurants-g297473-c10631-Barranquilla_Atlantico_Department.html') %>%
  html_nodes("._15_ydu6b") %>% html_attr('href')


# Extraer los links de cada tipo de cocina por lo general son 30 restaurantes por tipo de cocina
url_rest <- list()
for (i in 1:NROW(link_res)) {
  
  url_rest[[i]] <- paste0("https://www.tripadvisor.co",
                          read_html(link_res[i]) %>%
    html_nodes("._15_ydu6b") %>% html_attr('href') )
    
  
}


# Campos a extraer - Ejemplo
f <- "https://www.tripadvisor.co/Restaurant_Review-g297473-d2534018-Reviews-Salvator_s_Pizza_Pasta-Barranquilla_Atlantico_Department.html"
f <- read_html(f) 
f %>% html_nodes("._2mn01bsa:nth-child(1)") %>% html_text() # Money
f %>% html_nodes(".r2Cf69qf") %>%  html_text() # Calificacion
f %>% html_nodes("._10Iv7dOs") %>%  html_text() # Numero de calificaciones
f %>% html_nodes("._2vbD36Hr ._27M8V6YV") %>% html_text() # Direccion
f %>% html_nodes(".ct1ZIzQ6") %>% html_text() # Horario
f %>% html_nodes(".noQuotes , .partial_entry") %>% html_text() # Comentarios
f %>% html_nodes("._1hkogt_o+ ._1ud-0ITN ._2VxaSjVD") %>% html_text()  #Ranking
f %>% html_nodes('._3UjHBXYa div:nth-child(1) ._1XLfiSsv') %>% html_text() # Price Range

# Campos a extraer - Corre esto
tet <- c("._3a1XQ88S","._2mn01bsa:nth-child(1)",".r2Cf69qf","._10Iv7dOs",
         "._2vbD36Hr ._27M8V6YV",".ct1ZIzQ6",".noQuotes , .partial_entry",
         "._1hkogt_o+ ._1ud-0ITN ._2VxaSjVD",'._3UjHBXYa div:nth-child(1) ._1XLfiSsv')

# Aqui se extraen todos los campos de cada pagina web
# Tiempo de Corrida Largo por lo menos 10 minutos, hasta 15 min me duró

time_1 <- Sys.time()  # Esto es solo para mostrar el tiempo que demoró en correr

rest_data <- list()
for (i in 1:NROW(link_res)) {
  for (k in 1:NROW(url_rest[[i]])) {
    
    a <- read_html((url_rest[[i]])[k]) ; b <- list()
    for (j in 1:NROW(tet)) {
      
      b[[j]] <- a %>%  html_nodes(tet[j]) %>% html_text() 
    }
    rest_data[[k]] <- b
  }
  all_data[[i]] <- rest_data
  rest_data <- list()
}

time <- Sys.time() # Esto es solo para mostrar el tiempo que demoró en correr
remove(a) ; remove(b) 


# DATAFRAME
var <- c('Cocina','Nombre','Money','Calificacion', 'Numero de calificaciones', 'Direccion', 'Horario', 'Comentarios', 'Ranking','Price Range')

tripad_rest <-as.data.frame.matrix(matrix(0,1000,NROW(var)+1))
names(tripad_rest) <- c("id",var)

id <- 0
for (i in 1:NROW(link_res)) {
  
  for (j in 1:length(all_data[[i]])) {
    id <- id + 1
    list_ita <- list()
    list_ita <- all_data[[i]][[j]]
    list_ita[lengths(list_ita)==0] <- NA_character_
    for (z in 1:11) {
      #Para cada dimension o variable por ej "id" o "Cocina"
      if (z == 1){ 
        tripad_rest[id,"id"] <- id
      }else{ 
        
        if (z == 2) {
          tripad_rest[id,"Cocina"] <- cocina[i]
        }else{
          
          if(z == 9){
            tripad_rest[id,"Comentarios"] <- paste(list_ita[[(z-2)]],
                                                   collapse = "~")
          }else{
            tripad_rest[id,(names(tripad_rest))[z] ] <- list_ita[[(z-2)]]
          }
          
        }
        
        
      }
      
    }
    
  }
}

tripad_rest <- tripad_rest[tripad_rest$id!=0,]
View(tripad_rest)

## Save an object to a file
# saveRDS(tripad_rest, file = "restaurant_trip_advisor.rds")
## Restore the object
# tripad_rest2 <- readRDS(file = "restaurant_trip_advisor.rds")









#------------------ IGNORAR DE ACA A ABAJO --------------------------------------




# 
# # Rappi
# 
# borrar <- read_html("https://www.rappi.com.co/barranquilla/restaurantes") 
# 
# 
# borrar %>%  html_nodes(".score , .store-tags , .store-name") %>% html_text()
# 
# # ".star-icon , .score , .store-tags , .store-name"
# 
# borrar %>%  html_nodes(".star-icon") %>% html_text()
# 
# 
# # iFood
# 
# borrar2 <- read_html("https://www.ifood.com.co/delivery/barranquilla-atl/mcdonalds-portal-del-prado-viejo-prado/522d7b60-b971-4900-a5cd-532ebef2e454")
# 
# borrar2 %>% html_nodes("#__NEXT_DATA__") %>% html_text()
# 
# borrar3 <-htmlTreeParse('https://www.ifood.com.co/delivery/barranquilla-atl/sandiwch-qbano-villa-country-villa-country/8e494f72-8354-4855-a15d-7a6e0dfb7434', useInternal = TRUE)
# 
# xpathSApply(borrar3, '//*[@id="__NEXT_DATA__"]/text()')
# 
# 
# borrar %>%  html_nodes(".info-name") %>% html_text()
# borrar %>%  html_nodes(".info-name") %>% 
# 
# # Scrape 1: Título
# titulo <- html_nodes(html_web, ".lister-item-header a")
# titulo <- html_text(titulo)
# # Equivalente:
# titulo <- html_web %>% html_nodes(".lister-item-header a") %>% html_text()
# 
# # Scrape 2: Año de estreno
# año_estreno <- html_web %>% 
#   html_nodes(".lister-item-year.text-muted.unbold") %>% 
#   html_text() %>% 
#   str_sub(start = 2, end = 5) %>%
#   as.numeric()
# 
# # Scrape 3: Minutos que dura
# duracion <- html_web %>% 
#   html_nodes(".runtime") %>% 
#   html_text() %>%
#   str_split(" ") %>%
#   map_chr(1) %>%
#   as.numeric()
# 
# # Scrape 4: Género
# genero <- html_web %>%
#   html_nodes(".genre") %>%
#   html_text() %>%
#   str_trim()
#   
# # Scrape 5: Rating
# rating <- html_web %>%
#   html_nodes(".inline-block.ratings-imdb-rating strong") %>%
#   html_text()
# 
# # Se juntan todas las variables creadas en un data set:
# top_50 <- tibble(Pelicula = titulo,
#                      Año = año_estreno,
#                      Duración_min = duracion,
#                      Genero = genero,
#                      Rating = rating)
# 
# # Exportar a excel:
# write.csv2(top_50, "top_50.csv")
# 








