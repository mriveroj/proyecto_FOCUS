#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(jsonlite)
library(rgdal)
library(tidyverse)
library(plotly)
library(networkD3)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(factoextra)
library(readxl)
library(knitr)
library(kableExtra)
library(formattable)
library(shinydashboard)

Dane_paramodelos <- read.csv("Dane_paramodelos.csv", header = T, stringsAsFactors = T, fileEncoding = "Latin1")
Dane_paramodelos$Departamento <- as.factor(Dane_paramodelos$Departamento)
Dane_paramodelos$Departamento <- fct_collapse(Dane_paramodelos$Departamento, 
                                              "Region Central 1"=c("11", "76"),
                                              "Region Central 2"=c("15", "25", "73", "50"),
                                              "Region Suroccidente"=c("19", "52", "18", "41"),
                                              "Region Nororiente"="54",
                                              "Region Cafetera 1"=c("68", "5"),
                                              "Region Cafetera 2"=c("17", "63", "66"),
                                              "Region Norte 1"="8",
                                              "Region Norte 2"=c("13", "20"),
                                              "Region Norte 3"=c("23", "47", "70"),
                                              "Region Cluster 4"=c("27", "44"))
Depto <-  unique(Dane_paramodelos$Departamento)

#### Codigo para primera página
#Mapa
mapa <- readOGR("map_2.geojson", verbose = F)
#Barplot
small_dane <- Dane_paramodelos %>% group_by(Departamento) %>% tally() %>% rename("Frecuencia"=n)
small_dane$Departamento <- factor(small_dane$Departamento, levels = unique(small_dane$Departamento)[order(small_dane$Frecuencia, decreasing = F)])
# Sankey plot
data_matching <- read.csv("asignacion_matching.csv", header=T, stringsAsFactors = T)
data_matching <- data_matching[, -1]
data_DANE <- read.csv("Dane_etapa_mapeo.csv", header = T, stringsAsFactors = T, fileEncoding = "Latin1")
data_DANE$Departamento <- fct_collapse(data_DANE$Departamento, "Region Cafetera 1"="Region cafetera 1")

table(data_DANE$Departamento)
Depto <-  unique(data_DANE$Departamento)



# Creamos vector con los nombres de las regiones finales

region_asignada <- apply(data_matching, 1, which.max) %>% as.tibble()
region_asignada$value <- as.factor(region_asignada$value)
region_asignada$value <- fct_collapse(region_asignada$value, "Region.Central.1"="1",
                                      "Region.Central.2"="2",
                                      "Region.Suroccidente"="3",
                                      "Region.Nororiente"="4",
                                      "Region.Cafetera.1"="5",
                                      "Region.Cafetera.2"="6",
                                      "Region.Cluster.4"="7",
                                      "Region.Norte.1"="8",
                                      "Region.Norte.2"="9",
                                      "Region.Norte.3"="10")

### Construimos la matriz de flujo
tabla_flujo <-  data_DANE %>% group_by(Familia, Departamento) %>% tally() # Estas son las regiones iniciales
tabla_flujo <- rename(tabla_flujo, "Region_inicial"=Departamento, "Miembros"=n) 
tabla_flujo <- tabla_flujo %>% select(1,3,2)
tabla_flujo <- cbind(tabla_flujo, region_asignada) %>% rename("Region_final"="value")
flux_matrix <- tabla_flujo %>% group_by(Region_inicial, Region_final) %>% tally() %>% rename("Familias"="n")
nodos <- data.frame(name=c(as.character(tabla_flujo$Region_inicial),as.character(tabla_flujo$Region_final))) %>% unique()

flux_matrix$ID_Region_inicial=match(flux_matrix$Region_inicial, nodos$name)-1 
flux_matrix$ID_Region_final=match(flux_matrix$Region_final, nodos$name)-1

ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'


#### Codigo para segunda página
# Clusters
##Cargar archivo caso 4
Metricas <- read_excel("Metricas.xlsx", sheet = "caso 4")

## PREPARACION DE DATOS
caso4 <- Metricas
caso4[caso4 == " " ] <- NA
caso4[caso4== "NA" ] <- NA
caso4[caso4== "N.A" ] <- NA
caso4 <- caso4[stats::complete.cases(caso4),]
Metricas <- caso4
caso4$Departamento <-NULL
## etiqueta de departamentos
rownames(caso4) <- Metricas$Departamento

## distancia entre departamentos
caso4 <- scale(caso4)
caso4 <- as.data.frame(caso4)
res.dist <- get_dist(caso4, method = "euclidean")


#### Codigo para tercera página
#Diagrama de influencia de variables
coeficientes <- data.frame()
for ( Dep in Depto ){
    data <- read.csv(paste0("variable_weights_", Dep, ".csv"))
    coeficientes <- rbind(coeficientes, data)
}
coeficientes$Nivel <- paste(coeficientes$Variable, coeficientes$Nivel)
coeficientes <- coeficientes[, -c(1,2)] %>% select(Departamento, everything())
influencia <- coeficientes %>% group_by(Departamento, Nivel) %>% summarise("Influencia"=mean(Valor))

# Diagrma de Demanda/Oferta
data_barplot <- data.frame("Departamento"=Metricas$Departamento, "Oferta_Demanda"=Metricas$`OFERTA/DEMANDA`)
data_barplot$Oferta_Demanda <- (1/data_barplot$Oferta_Demanda)





#####################################################################################################################
##################################   AQUÍ ARRANCA EL DASHBOARD     ##################################################
#####################################################################################################################



# Define UI for application that draws a histogram
ui <- navbarPage("Dashboard", theme = shinytheme("simplex"),
    tabPanel("Regiones",
             
        # Application title
        titlePanel("Mapa agrupación por Regiones"),
        
        leafletOutput("mapa_regiones"),
        hr(),

        # Sidebar 
        fluidRow(
            column(3,
                shinydashboard::box(title = "Seleccione las regiones", background = "orange", height = 10, width = 8,
                   checkboxGroupInput("regiones","Grupo", choices= unique(small_dane$Departamento), selected= unique(small_dane$Departamento))
                   )
            ),
            
            column(5,
                   shinydashboard::box( width = 11, height = 10, title="Inmigrantes observados por región",
                   plotlyOutput("barplot_regiones")
                   )
            ),
            column(4,
                   shinydashboard::box(background = "orange", width = 11, height = 10, title="Flujo de inmigrantes propuesto",
                   sankeyNetworkOutput("sankey_plot")
                   )
            )
        )
    ),
    tabPanel("Clusters",
        # Application title
        titlePanel("Análisis de Clusters de acuerdo a condiciones socioeconómicas"),
        
        fluidRow(
            column(8,
                   plotOutput("clusters")
            ),
            column(4,
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(), h4("Análisis de clusters"),
                   sliderInput("clusters", "Introduzca el número de clusters a construir", min=1, max=10, value=4)
            )
        ),
        fluidRow(
            column(12,
                  tableOutput("stats") 
            )
        )
        
    ),
    tabPanel("Demanda laboral",
             titlePanel("Análisis de factores influyentes en ocupación"),
             fluidRow(
                 column(4,
                        box(title = "Selección de región para analizar", primary= T, solidHeader = T,
                            selectInput("Region", "Seleccione la región", choices = Depto, selected = "Region Central 1")
                        )
                 )
             ),
             
             fluidRow(
                 column(6,
                        plotOutput("barplot_influencia")
                 ),
                 column(6,
                        h2("Relación de Demanda y Oferta de plazas de trabajo por departamento"),
                        plotOutput("barplot_demanda")
                 
                 )
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$mapa_regiones <- renderLeaflet({
        leaflet(mapa, options = leafletOptions(zoomControl = TRUE,
                                              minZoom = 3, maxZoom = 19,
                                              dragging = TRUE)) %>%  addTiles() %>% 
            addTiles(group = "Base") %>% addPolygons(color = mapa$stroke, fillColor = mapa$fill, weight = mapa$stroke.width, opacity = mapa$stroke.opacity,
                        fillOpacity = mapa$fill.opacity, label = mapa$name, 
                        popup = paste0("<b>", "Observaciones:", "</b>", " ", mapa$observations),
                        labelOptions = labelOptions(noHide = F, direction = "auto",
                                                    style = list(
                                                        "color" = "black",
                                                        "font-family" = "serif",
                                                        "box-shadow" = "4px 4px",
                                                        "font-size" = "14px",
                                                        "border-color" = "rgba(0,0,0,0.5)")))%>%
            setView(lng =-72.817549 , lat = 4.467326, zoom = 6)
    })
    
    output$barplot_regiones <- renderPlotly({
        data = small_dane %>% filter(Departamento %in% input$regiones)
        plot_ly(data, y = ~Departamento, x = ~Frecuencia, type = 'bar', orientation = "h",
                marker = list(color = 'rgb(158,202,225)',
                              line = list(color = 'rgb(8,48,107)',width = 1.5))) 
    })
    

    output$sankey_plot <- renderSankeyNetwork({
        sankeyNetwork(Links = as.data.frame(flux_matrix), Nodes = nodos,
                      Source = "ID_Region_inicial", Target = "ID_Region_final",
                      Value = "Familias", NodeID = "name", 
                      sinksRight=TRUE,  nodeWidth=20, fontSize=13, nodePadding = 20)
    })
    
    modelo_kmeans <- reactive({
        ## modelo
        set.seed(123)
        kmeans(x = caso4, centers = input$clusters, nstart = 50)
    })
    
    output$clusters <- renderPlot({

        fviz_cluster(object = modelo_kmeans(), data = caso4, show.clust.cent = TRUE,
                     ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
            labs(title = "Resultados clustering K-means") +
            theme_bw() +
            theme(legend.position = "bottom") 
    })
 
    
    output$stats <- function(){
        
        clusters_table <- as.data.frame(modelo_kmeans()$centers)
        clusters_table$cluster <- seq(from=1, to=input$clusters)
        clusters_table <- select(clusters_table, cluster, everything())
        clusters_table$cluster <- cell_spec(clusters_table$cluster, bold = T)
        kbl(clusters_table, escape = F) %>% kable_paper() %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
        
    }
    
    
    output$barplot_influencia <- renderPlot({
        
        influencia_depto <- filter(influencia, Departamento == input$Region, Influencia >0) %>% head(10)
        ggplot(influencia_depto, aes(x=reorder(Nivel, Influencia), y=Influencia)) + 
            geom_col(fill="dodgerblue", color="dodgerblue2", alpha=0.7) + coord_flip() + 
            theme_bw() + labs(x="Variable", y="Influencia")
    })
    
    output$barplot_demanda <- renderPlot({
        ggplot(data_barplot, aes(x=reorder(Departamento, Oferta_Demanda), y= Oferta_Demanda)) + geom_col(fill="orangered", col="orangered3", alpha=0.7) + 
            theme_bw() + labs(x= "Departamentos", y="Ratio Demanda/Oferta") +
            theme(axis.text.x = element_text(angle=50, h=1))
    })
}

# Run the application 
shinyApp(ui, server)

