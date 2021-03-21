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


shinyUI(dashboardPage(skin="purple",
                      dashboardHeader(title = "Road Accidents in UK",
                                      titleWidth = 300),
                      
                      dashboardSidebar(
                          sidebarMenu(
                              menuItem("General", tabName = "General", icon = icon("dashboard")),
                              menuItem("Space", tabName = "Space", icon = icon("dashboard")),
                              menuItem("Model", tabName = "Model", icon = icon("dashboard"))
                          )
                      ),
                      
                      dashboardBody(
                          tabItems(
                              
                              tabItem(tabName = "General",
                                      h1("General Information"),
                                      fluidRow(
                                          box(title = "Bar Diagram by Year and Road Type", status = "info", solidHeader = TRUE,
                                              collapsible = TRUE,
                                              selectInput(inputId = "year1",
                                                          label = "Year",choices = c(2005,2006,2007,2009,2010,2011,2012)),
                                              selectInput(inputId = "roadtype",
                                                          label = "Road Type",choices = levels(datos$Road_Type)),
                                              plotlyOutput(outputId = "barra")
                                          ),
                                          infoBox(#color = "teal",
                                              "Casualties Report",textOutput(outputId = "muertes")
                                          ),
                                          infoBox(#color = "teal",
                                              "Accidents report",textOutput(outputId = "mean")
                                          ),
                                          box(title = "Map of United Kingdom", status = "warning", solidHeader = TRUE,
                                              collapsible = TRUE,
                                              leafletOutput(outputId = "mapa")
                                          )
                                      )
                              ),
                              
                              tabItem(tabName = "Space", 
                                      h2("Spatial Distribution of Accidents"),
                                      fluidRow(
                                          box(title = "United Kingdom Road Accident by Year", status = "primary", solidHeader = TRUE,
                                              collapsible = TRUE,
                                              selectInput(inputId = "year2",
                                                          label = "Year",choices = c(2005,2006,2007,2009,2010,2011,2012)),
                                              plotOutput(outputId = "humo")
                                          ),
                                          tabBox(title = "Yearwise trend of accident location",
                                                 tabPanel("Latitude",
                                                          plotlyOutput(outputId = "latitud")
                                                 ),
                                                 tabPanel("Longitude",
                                                          plotlyOutput(outputId = "longitud")
                                                 )
                                          )
                                      )
                              ),
                              
                              
                              tabItem(tabName = "Model",
                                      fluidRow(
                                          box(title = "Predicion of Accident Severity ", status = "info", solidHeader = TRUE,
                                              collapsible = TRUE,
                                              numericInput(inputId = "m1",
                                                           label = "Number of Vehicles",min=1,value = 1),
                                              numericInput(inputId = "m2",
                                                            label = "Number of Casualties",min=1,value = 1),
                                              selectInput(inputId = "m3",
                                                          label = "Day of Week",choices = c("1","2","3","4","5","6","7")),
                                              selectInput(inputId = "m4",
                                                          label = "Road Type",choices = levels(datos$Road_Type)),
                                              selectInput(inputId = "m5",
                                                          label = "Pedestrian Crossing Human Control",choices = levels(datos$Pedestrian_Crossing.Human_Control)),
                                              selectInput(inputId = "m6",
                                                          label = "Pedestrian Crossing Physical Facilities",choices = levels(datos$Pedestrian_Crossing.Physical_Facilities)),
                                              selectInput(inputId = "m7",
                                                          label = "Light Conditions",choices = levels(datos$Light_Conditions)),
                                              selectInput(inputId = "m8",
                                                          label = "Weather Conditions",choices = levels(datos$Weather_Conditions)),
                                              selectInput(inputId = "m9",
                                                          label = "Road Surface Conditions",choices = levels(datos$Road_Surface_Conditions)),
                                              selectInput(inputId = "m10",
                                                          label = "Carriageway Hazards",choices = levels(datos$Carriageway_Hazards))
                                              
                                          ),
                                          
                                          box(
                                              textOutput(outputId = "prediction")
                                          )
                                          
                                          
                                          
                                          
                                      ) 
                              )
                          )
                      )
))
