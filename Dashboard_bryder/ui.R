
library(shiny)
library(tidyverse)
library(shinyjs)
library(data.table)
library(lubridate)
library(shinyalert)
library(DT)
library (dplyr)
library(readxl)
library(xlsx)
library(scales)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme = shinytheme("flatly"),
  tabsetPanel(
    
    
    ##############################################TEORIA#############################################################################
    tabPanel("Información Teórica", titlePanel(strong("Liquidación de Nomina", style="color:coral")),
             h2(strong("Resumen"), style="color:darkslateblue"),
             div("La liquidación de nómina es un factor clave en todo negocio; esta debe
ser procurada de tal forma que tanto el empleado como el empleador
reciban y den los porcentajes estipulados por la legislación para evitar
inconvenientes legales."),
             br(),
             div("Los valores estipulados deberán ser actualizados anualmente por las empresas,
a fin de garantizar que el total devengado por los trabajadores se
ajuste a los costos de la canasta familiar del momento."),
             br(),
             div("En la mayoría de los casos, todo tipo de esfuerzo cuenta con una remuneración
adicional, generalmente de tipo monetaria, y se ve reflejado en
el pago de horas extras, comisiones y bonificaciones, a las cuales se ha
hecho referencia a lo largo de este capítulo. La correcta liquidación de la
nómina representa un compromiso con los trabajadores y la sociedad a
la que pertenecen."),
             br(),
             br(),
             div(strong("Para acceder a la teoría completa del tema, dar click en el siguiente enlace:")),
             br(),
             
             downloadLink("downloadLiquidacion", "Descargar teoría completa"),
             br(),
             br(),
             div(strong("Para mayor entendimiento de la teoría, puede acceder al siguiente video:")),
             br(),
             tags$iframe(width="640", height="375", src="https://youtu.be/yXptob8qaKk", frameborder="0"),
             br(),
             div(strong("En caso de error con el video, copie el siguiente enlace en su navegador:"), style="color:darkred"),
             br(),
             a("https://youtu.be/yXptob8qaKk"),
             br(),
             br(),
             ####ESPERAR POR LOS CUESTIONARIOS HECHOS POR EL PROFESOR
             hr(),
             
             h2(strong("Cuestionario y Evaluaciones", style="color:darkslateblue")),
             
             h4("Sección en espera por cuestionarios, y evaluaciones", style="color:darkcyan"),
             
             
             
             ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###########################################################################################################################################################
    tabPanel("Calculo de Nómina", titlePanel("Ingreso de Datos Nomina"),
             
             # Sidebar with a slider input for number of bins
             fluidRow(
               ##Info para la nomina
               column(width = 3, wellPanel(
                 dateInput("Date_nom","Fecha", value = Sys.Date()),    
                 textInput("Cod_emp_nom","Número de Identificación del Empleado"),  hr(),  
                 numericInput("Salario_min", "Salario Mínimo Legal Vigente", 877802 ),
                 numericInput("Aux_tranporte", "Auxilio de Transporte", 97032 ), hr(),
                 numericInput("Sueldo_nom", "Sueldo Básico",0 ),
                 numericInput("Dias_nom", "Días Reales Trabajados",30 ), br(), hr(), br(),
                 
                 
                 
                 actionButton("Calcular_nom", "Calcular Nómina", class="btn btn-primary" ), hr(),
                 actionButton("Save_Nomina"," Guardar Nómina")
                 
                 
               )),
               
               
               
               
               ##Horas Extras
               column(width = 3, wellPanel(
                 
                 
                 numericInput("H_diurnas_ext_nom", "Horas Diurnas Extras",0 ),
                 numericInput("H_nocturnas_ext_nom", "Horas Nocturnas Extras",0 ),
                 numericInput("H_diurnas_ext_dominical_nom", "Horas Diurnas Extras Dominicales",0 ),
                 numericInput("H_nocturnas_ext_dominical_nom", "Horas Nocturnas Extras Dominicales",0 ),
                 numericInput("H_recargo_dom_fest_nom", "Horas de Recargo Domigos o Festivos",0 ),
                 numericInput("H_recargo_noc_nom", "Horas de Recargo Nocturno",0),
                 numericInput("H_recargo_noc_dom_fest_nom", "Horas de Recargo Nocturno en Domigos o Festivos",0 ),
                 
                 numericInput("Otr_ingresos_nom", "Otros Ingresos",0 ),
                 numericInput("d_prestamos", "Prestamos",0 ),
                 numericInput("d_abonos", "Abonos",0 ),
                 numericInput("d_otr_descuentos", "Otros Descuentos",0 )
                 
                 
               )
               
               
               ),
               
               column(width = 6, wellPanel(
                 h3("Compensaciones"),
                 tableOutput("res_tab_com"),
                 
                 
                 h3("Deducciones"),
                 tableOutput("res_tab_ded"),
                 
                 
                 
                 
                 h3("Salario Neto:"),
                 verbatimTextOutput("res_Salario_Neto")
                 
                 
                 
               ))
             )),
    
    
    tabPanel("Vista de Nómina", titlePanel("Nomina"),
             ### This is to adjust the width of pop up "showmodal()" for DT modify table 
            
             fluidRow(
               column(width = 3, wellPanel(
                 
                 h4("Pagado"),
                 tableOutput("res_empleador_nomina_pagado"),hr(),
                 
                 h4("Deducciones Empleador"),
                 tableOutput("res_empleador_nomina_deducciones"),hr(),
                 
                 h4("TOTALES"),
                 tableOutput("res_empleador_nomina_total"),
                 
                 actionButton("Calcular_emp","Actualizar")
               )),
               
               
              
               column(width = 8, wellPanel( tags$head(tags$style(HTML('
                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
             helpText("Note: Remember to save any updates!"),
             br(),
             ### tags$head() is to customize the download button
             tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
             downloadButton("Download_xlsx", "Download in Excel", class="butt"),
             useShinyalert(), # Set up shinyalert
             uiOutput("MainBody_nomina"),actionButton(inputId = "Save_Nomina_cambios",label = "Save")
               ))
             )

            )
    
       
        
    )
)

#####
)
