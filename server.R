library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(plotly)
library(readr)
library(rsconnect)
library(repmis)





source_data("https://github.com/brianlli2000/Accident_UK/blob/main/datos_limpios.Rdata?raw=false")
#fit <- readRDS("modelo.rds")



datos$Accident_Severity=as.factor(datos$Accident_Severity)
datos$Day_of_Week=as.factor(datos$Day_of_Week)
datos$Road_Type=as.factor(datos$Road_Type)
datos$Speed_limit=as.factor(datos$Speed_limit)
datos$Pedestrian_Crossing.Human_Control=as.factor(datos$Pedestrian_Crossing.Human_Control )
datos$Pedestrian_Crossing.Physical_Facilities=as.factor(datos$Pedestrian_Crossing.Physical_Facilities)
datos$Light_Conditions=as.factor(datos$Light_Conditions)
datos$Weather_Conditions=as.factor(datos$Weather_Conditions)
datos$Road_Surface_Conditions=as.factor(datos$Road_Surface_Conditions)
datos$Special_Conditions_at_Site=as.factor(datos$Special_Conditions_at_Site)
datos$Carriageway_Hazards=as.factor(datos$Carriageway_Hazards)
datos$Urban_or_Rural_Area=as.factor(datos$Urban_or_Rural_Area)

shinyServer(function(input,output){
    
    #Pestaña 1
    output$barra <- renderPlotly({
        mbarra <- datos %>% filter(Year==input$year1) %>% group_by(Road_Type,Accident_Severity) %>% 
            summarise("Casualties"=sum(Number_of_Casualties))
        brt <- mbarra %>% filter(Road_Type==input$roadtype)
        ggplotly(ggplot(brt,aes(x=Road_Type,y=Casualties))+
                     geom_bar(aes(fill=as.factor(Accident_Severity)),stat = "identity",position = "dodge"))                
    })
    
    output$muertes <- renderText({
        muertes <- datos %>% filter(Year==input$year1)  %>% summarise("Total Muertes"=sum(Number_of_Casualties))
        
        as.character(paste(muertes$`Total Muertes`))
    })
    
    output$mean <- renderText({
        mean <- datos %>% filter(Year==input$year1)  %>% tally()
        
        
        as.character(paste(mean$n))
    })
    
    output$mapa <- renderLeaflet({
        mapa <- datos %>% filter(Year==input$year1) %>% 
            select(c(Longitude,Latitude,Road_Type,Number_of_Vehicles,Number_of_Casualties,Accident_Severity))  
        mapa <- mapa %>% filter(Road_Type==input$roadtype)
        
        leaflet(mapa) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=mapa$Longitude, lat=mapa$Latitude, clusterOptions = markerClusterOptions(),
                       popup=paste(sep = "<br/>",paste0("Road Type: ",as.character(mapa$Road_Type)),
                                   paste0("Number of vehicle: ",as.character(mapa$Number_of_Vehicles)),
                                   paste0("Accident Severity: ",as.character(mapa$Accident_Severity))),
                       label = paste0("Casualties: ",as.character(mapa$Number_of_Casualties)),
                       labelOptions = labelOptions(style=list("color"="blue"))
            )
        
    })
    
    
    #Pestaña 2
    output$humo <- renderPlot({
        ubicacion <- datos %>% filter(Year==input$year2)
        ggplot(ubicacion,aes(x=Longitude,y=Latitude,color=Accident_Severity))+
            geom_jitter(alpha=1/100)
    })
    
    output$latitud <- renderPlotly({
        ubicacion <- datos %>% filter(Year==input$year2)
        ggplotly(ggplot(ubicacion,aes(x=Latitude))+
                     geom_density()+
                     geom_hline(yintercept = 0.6974679,color="red")
        )
    })
    
    output$longitud <- renderPlotly({
        ubicacion <- datos %>% filter(Year==input$year2)
        ggplotly(ggplot(ubicacion,aes(x=Longitude))+
                     geom_density()+
                     geom_hline(yintercept = 0.4345078,color="red")
        )
    })
    
    
    #Pestaña 3
    
    output$prediction <- renderText({
       
      datos$Accident_SeverityC <- ifelse(datos$Accident_Severity=="1" |datos$Accident_Severity=="2",0,1)
      datos$Accident_SeverityC <- as.factor(datos$Accident_SeverityC)
      
      datos <- datos[complete.cases(datos),]
      
      fit <- glm(Accident_SeverityC~Number_of_Vehicles+Number_of_Casualties+Day_of_Week+
                   Road_Type+Pedestrian_Crossing.Human_Control+
                   Pedestrian_Crossing.Physical_Facilities+Light_Conditions+
                   Weather_Conditions+Road_Surface_Conditions+
                   Carriageway_Hazards, family=binomial,
                 data = datos)
      
        newd <- data.frame(Number_of_Vehicles=as.integer(input$m1),Number_of_Casualties=as.integer(input$m2),
                           Day_of_Week=input$m3,
                           Road_Type=input$m4,Pedestrian_Crossing.Human_Control=input$m5,
                           Pedestrian_Crossing.Physical_Facilities=input$m6,
                           Light_Conditions=input$m7,Weather_Conditions=input$m8,
                           Road_Surface_Conditions=input$m9,Carriageway_Hazards=input$m10)
       
         pred <- predict(fit,newdata = newd,type = "response")
        
        as.character(paste("Under these conditions, the probability of a serious accident is",round(pred,3)))
        
    })
    
    
})
