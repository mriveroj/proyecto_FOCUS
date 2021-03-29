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

useShinyalert()
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
   
   Nomina <- reactiveValues()
   
   
   
   Nomina$Data <- readRDS("Nomina.rds")
   
 
   Nomina$Calculo_nom<-readRDS("Col_nomina.rds")
   Nomina$Empleador_nom <- data.frame()
   
   Compensacion_per<-data.frame(  p_diurnas_ext = 1.25,
                     p_nocturnas_ext = 1.75,
                     p_diurnas_ext_dominical= 1.25,
                     p_nocturnas_ext_dominical= 2.5,
                     p_recargo_dom_fest =0.75 ,
                     p_recargo_noc = 0.35 ,
                     p_recargo_noc_dom_fest=1.1
      
               )
   Deducciones_per <- data.frame(
      
                     p_pension_staff = 0.04,
                     p_salud_staff = 0.04,
                     p_sol_pension_4_16 = 0.01,
                     p_sol_pension_16_17 = 0.012,
                     p_sol_pension_17_18 = 0.014,
                     p_sol_pension_18_19 = 0.016,
                     p_sol_pension_19_20 = 0.018,
                     p_sol_pension_20 = 0.02
                     
                )
   Deducciones_emp <- data.frame(
      ####Porcentajes de deducciones del empleador
                     p_caja_comp = 0.04,
                     p_icbf = 0.03,
                     p_sena = 0.02,
                     p_prima = 0.0833, # Incluyendo auxilio de transporte
                     p_cesantias = 0.0833, # Incluyendo auxilio de transporte
                     p_int_cesantias = 0.12, # sobre la cesantiaas
                     p_vacaciones = 0.04167,
                     
                     p_ded_pension = 0.12,
                     p_ded_salud = 0.085,
                     p_arl = 0.02436
                     
   )
   
###Entrada de datos para calculo de nómina   
   
   observeEvent(input$Calcular_nom, {
      
      Nomina_data <- data.frame(
         Salario_Minimo = input$Salario_min,  
         Sueldo = input$Sueldo_nom,
         Dias = input$Dias_nom,
         
         H_diurnas_ext = input$H_diurnas_ext_nom,
         H_nocturnas_ext = input$H_nocturnas_ext_nom,
         H_diurnas_ext_dominical = input$H_diurnas_ext_dominical_nom,
         H_nocturnas_ext_dominical= input$H_diurnas_ext_dominical_nom,
         H_recargo_dom_fest= input$H_recargo_dom_fest_nom,
         H_recargo_noc = input$H_recargo_noc_nom,
         H_recargo_noc_dom_fest= input$H_recargo_noc_dom_fest_nom
         
        
         
         
      )
      
      Valor_dia=Nomina_data$Sueldo/30
      Valor_Hora= Valor_dia/8
      
      #Dataframe de calculo de nómina
      Calculo_nomina<- data.frame(
         Date = input$Date_nom,
         Codigo_Empleado = input$Cod_emp_nom,
         
         Sueldo_Basico = Nomina_data$Sueldo,
         
         Aux_Transporte = {
            if (Nomina_data$Sueldo <= 2*Nomina_data$Salario_Minimo) {
               input$Aux_tranporte
               
            }else {0}
            
         },
         
         Vr_dia = Valor_dia,
         Vr_Hora= Valor_Hora,
         
         
         Vr_diurnas_ext = {
            if (Nomina_data$Sueldo <= 2*Nomina_data$Salario_Minimo) {
               Valor_Hora*Compensacion_per$p_diurnas_ext*input$H_diurnas_ext_nom
               
            }else {0}
            
         },
         
         Vr_nocturnas_ext = {
            if (Nomina_data$Sueldo <= 2*Nomina_data$Salario_Minimo) {
               Valor_Hora*Compensacion_per$p_nocturnas_ext*input$H_nocturnas_ext_nom
               
            }else {0}
            
         },
         
         Vr_diurnas_ext_dominical= {
            if (Nomina_data$Sueldo <= 2*Nomina_data$Salario_Minimo) {
               Valor_Hora*Compensacion_per$p_diurnas_ext_dominical*input$H_diurnas_ext_dominical_nom
               
            }else {0}
            
         },
         
         Vr_nocturnas_ext_dominical={
            if (Nomina_data$Sueldo <= 2*Nomina_data$Salario_Minimo) {
               Valor_Hora*Compensacion_per$p_nocturnas_ext_dominical*input$H_diurnas_ext_dominical_nom
               
            }else {0}
            
         }, 
         
         Vr_recargo_dom_fest =Valor_Hora*Compensacion_per$p_recargo_dom_fest*input$H_recargo_dom_fest_nom,
         Vr_recargo_noc = Valor_Hora*Compensacion_per$p_recargo_noc*input$H_recargo_noc_nom,
         Vr_recargo_noc_dom_fest=Valor_Hora*Compensacion_per$p_recargo_noc_dom_fest*input$H_recargo_noc_dom_fest_nom,
         
         Otr_ingresos = input$Otr_ingresos_nom,
         
         Total_Recargo = Valor_Hora*Compensacion_per$p_recargo_dom_fest*input$H_recargo_dom_fest_nom + Valor_Hora*Compensacion_per$p_recargo_noc*input$H_recargo_noc_nom + Valor_Hora*Compensacion_per$p_recargo_noc_dom_fest*input$H_recargo_noc_dom_fest_nom,
         
         Total_Extras = {
            if (Nomina_data$Sueldo <= 2*Nomina_data$Salario_Minimo) {
               Valor_Hora*Compensacion_per$p_diurnas_ext*input$H_diurnas_ext_nom + Valor_Hora*Compensacion_per$p_nocturnas_ext*input$H_nocturnas_ext_nom + Valor_Hora*Compensacion_per$p_diurnas_ext_dominical*input$H_diurnas_ext_dominical_nom +  Valor_Hora*Compensacion_per$p_nocturnas_ext_dominical*input$H_diurnas_ext_dominical_nom
               
            }else {0}
            
         },
         
         Total_Ingresos =  Valor_Hora*Compensacion_per$p_recargo_dom_fest*input$H_recargo_dom_fest_nom + Valor_Hora*Compensacion_per$p_recargo_noc*input$H_recargo_noc_nom + Valor_Hora*Compensacion_per$p_recargo_noc_dom_fest*input$H_recargo_noc_dom_fest_nom + Valor_Hora*Compensacion_per$p_diurnas_ext*input$H_diurnas_ext_nom + Valor_Hora*Compensacion_per$p_nocturnas_ext*input$H_nocturnas_ext_nom + Valor_Hora*Compensacion_per$p_diurnas_ext_dominical*input$H_diurnas_ext_dominical_nom +  Valor_Hora*Compensacion_per$p_nocturnas_ext_dominical*input$H_diurnas_ext_dominical_nom + Valor_dia*Nomina_data$Dias + input$Otr_ingresos_nom
         
      )
      
      Calculo_deducciones<- data.frame(
         
         ded_pension = Calculo_nomina$Total_Ingresos*Deducciones_per$p_pension_staff,
         ded_salud = Calculo_nomina$Total_Ingresos*Deducciones_per$p_salud_staff,
         
         #Condicionales para solidaridad pensional
         ded_sol_pensional = {
            
            if (!is.null(Calculo_nomina$Total_Ingresos)) {
               
               if (Calculo_nomina$Total_Ingresos <= 4*Nomina_data$Salario_Minimo) { 0}
               
               else if (Calculo_nomina$Total_Ingresos > 4*Nomina_data$Salario_Minimo && Calculo_nomina$Total_Ingresos <= 16*Nomina_data$Salario_Minimo){
                  Calculo_nomina$Total_Ingresos*Deducciones_per$p_sol_pension_4_16
                  
               } else if (Calculo_nomina$Total_Ingresos > 16*Nomina_data$Salario_Minimo && Calculo_nomina$Total_Ingresos <= 17*Nomina_data$Salario_Minimo){
                  Calculo_nomina$Total_Ingresos*Deducciones_per$p_sol_pension_16_17
                  
               } else if (Calculo_nomina$Total_Ingresos > 17*Nomina_data$Salario_Minimo && Calculo_nomina$Total_Ingresos <= 18*Nomina_data$Salario_Minimo){
                  
                  Calculo_nomina$Total_Ingresos*Deducciones_per$p_sol_pension_17_18
                  
               } else if (Calculo_nomina$Total_Ingresos > 18*Nomina_data$Salario_Minimo && Calculo_nomina$Total_Ingresos <= 19*Nomina_data$Salario_Minimo){
                  Calculo_nomina$Total_Ingresos*Deducciones_per$p_sol_pension_18_19
                  
               } else if (Calculo_nomina$Total_Ingresos > 20*Nomina_data$Salario_Minimo){
                  Calculo_nomina$Total_Ingresos*Deducciones_per$p_sol_pension_20
                  
               } 
            }   
            
         },
         
         ded_prestamos = input$d_prestamos,
         ded_abonos = input$d_abonos,
         ded_otr_descuentos = input$d_otr_descuentos
      )
      
      Calculo_totales <- data.frame(
         Total_Ingreso_Aux_T= Calculo_nomina$Total_Ingresos + Calculo_nomina$Aux_Transporte,
         Total_ded =  Calculo_deducciones$ded_pension + Calculo_deducciones$ded_salud + Calculo_deducciones$ded_sol_pensional + Calculo_deducciones$ded_prestamos + Calculo_deducciones$ded_abonos + Calculo_deducciones$ded_otr_descuentos,
         
         Salario_Neto = Calculo_nomina$Total_Ingresos - Calculo_deducciones$ded_pension - Calculo_deducciones$ded_salud - Calculo_deducciones$ded_sol_pensional
         
      )
      
      ##Calculo de nómina
      Calculo_nomina <- cbind(Calculo_nomina, Calculo_deducciones, Calculo_totales, fill=TRUE)
      
   
      Nomina$Calculo_nom <- Calculo_nomina
      
   
    
   })
   
   
   
   ######  Display de Resultados
   
   output$res_tab_com <- renderTable({
      compensacion_tab<-select(Nomina$Calculo_nom, "Valor día:"= Vr_dia, "Valor hora:" = Vr_Hora, "Horas Extras Diurnas: " = Vr_diurnas_ext, "Horas Extras Nocturnas: "= Vr_nocturnas_ext, "Horas Extras Diurnas Dominicales: "= Vr_diurnas_ext_dominical,"Horas Extras Nocturnas Dominicales: " = Vr_nocturnas_ext_dominical, "Horas Recargo Dom/Fes: " = Vr_recargo_dom_fest, "Horas Recargo Nocturno: " = Vr_recargo_noc, "Horas Recargo Nocturno Dom/Fes: "= Vr_recargo_noc_dom_fest, "Total Recargo " = Total_Recargo,  "Total Horas Extras" = Total_Extras, "Total Ingreso" = Total_Ingresos, "Total Ingreso con Aux de Transporte"= Total_Ingreso_Aux_T)
           
       # se transponen los datos y se nombran las columnas          
      compensacion_tab<-data.frame(t(rbind(colnames(compensacion_tab),compensacion_tab)))
      compensacion_tab[[2]]<-dollar(as.numeric(compensacion_tab[[2]]))
      
      rename(compensacion_tab,Conceptos =X1, Valores= X2)
     

   })
   
   
   output$res_tab_ded <- renderTable({
      deducciones_tab<- select(Nomina$Calculo_nom, "Pensión:"= ded_pension, "Plan Obligatorio de Salud:" = ded_salud, "Fondo de Solidaridad Pensional: " = ded_sol_pensional, "Total Deducciones: "= Total_ded)
   
      
      deducciones_tab<-data.frame(t(rbind(colnames( deducciones_tab), deducciones_tab)))
      deducciones_tab[[2]]<-dollar(as.numeric(deducciones_tab[[2]]))
     
      
      rename( deducciones_tab,Conceptos =X1, Valores= X2)
      
      
   })
   
   
   
   output$res_Salario_Neto <- renderText({
      #arreglar
      paste(dollar(Nomina[["Calculo_nom"]][["Salario_Neto"]]) )
      
   })
   
   #############
   #####Resultado del Empleador
 
   observeEvent(input$Calcular_emp,{
      
      Empleador_nomina <-data_frame(
         Total_Basico_emp = colSums(select (Nomina$Data, contains ("Sueldo_Basico"))),
         AuxTransporte_emp= colSums(select (Nomina$Data, contains ("Aux_Transporte"))),
         Total_H_EXT_emp = colSums(select (Nomina$Data, contains ("Total_Extras"))),
         Total_Recargos_emp = colSums(select (Nomina$Data, contains ("Total_Recargo"))),
         Total_Otr_Ingreso_emp = colSums(select (Nomina$Data, contains ("Otr_ingresos"))),
         Total_Constituyente_Sal_emp = colSums(select (Nomina$Data, contains ("Total_Ingresos"))),
         Total_Ingreso_Aux_emp= colSums(select (Nomina$Data, contains ("Total_Ingreso_Aux_T")))
      )
      
      Empleador_Deducciones<-data_frame(
         Caja_comp_emp = Deducciones_emp$p_caja_comp*Empleador_nomina$Total_Constituyente_Sal_emp,
         Icbf_emp = Deducciones_emp$p_icbf*Empleador_nomina$Total_Constituyente_Sal_emp,
         Sena_emp = Deducciones_emp$p_sena*Empleador_nomina$Total_Constituyente_Sal_emp,
         Prima_emp = Deducciones_emp$p_prima*Empleador_nomina$Total_Ingreso_Aux_emp,
         Cesantias_emp = Deducciones_emp$p_cesantias*Empleador_nomina$Total_Ingreso_Aux_emp,
         Int_Cesantias_emp = Deducciones_emp$p_int_cesantias*Deducciones_emp$p_cesantias*Empleador_nomina$Total_Ingreso_Aux_emp,
         
         Vacaciones_emp = Deducciones_emp$p_vacaciones*Empleador_nomina$Total_Constituyente_Sal_emp,
         Ded_pension_emp = Deducciones_emp$p_ded_pension*Empleador_nomina$Total_Constituyente_Sal_emp,
         Ded_salud_emp = Deducciones_emp$p_ded_salud*Empleador_nomina$Total_Constituyente_Sal_emp,
         Ded_arl_emp = Deducciones_emp$p_arl*Empleador_nomina$Total_Constituyente_Sal_emp
      )
      
      Empleador_Pagado<-data_frame(
         
         Total_pagado = Empleador_nomina$Total_Ingreso_Aux_emp,
         
         Total_prest_Ded = Empleador_Deducciones$Caja_comp_emp + Empleador_Deducciones$Icbf_emp + Empleador_Deducciones$Sena_emp + Empleador_Deducciones$Prima_emp + Empleador_Deducciones$Cesantias_emp + Empleador_Deducciones$Int_Cesantias_emp + Empleador_Deducciones$Vacaciones_emp + Empleador_Deducciones$Ded_pension_emp + Empleador_Deducciones$Ded_salud_emp + Empleador_Deducciones$Ded_arl_emp,
         
         Total = Empleador_nomina$Total_Ingreso_Aux_emp + Empleador_Deducciones$Caja_comp_emp + Empleador_Deducciones$Icbf_emp + Empleador_Deducciones$Sena_emp + Empleador_Deducciones$Prima_emp + Empleador_Deducciones$Cesantias_emp + Empleador_Deducciones$Int_Cesantias_emp + Empleador_Deducciones$Vacaciones_emp + Empleador_Deducciones$Ded_pension_emp + Empleador_Deducciones$Ded_salud_emp + Empleador_Deducciones$Ded_arl_emp
         
         
      )
      
      Nomina$Empleador_nom <- cbind(Empleador_nomina, Empleador_Deducciones, Empleador_Pagado)
      
     
      
      ##Display empleador
      
      output$res_empleador_nomina_pagado <- renderTable({
         
         deducciones_tab<-select(Nomina$Empleador_nom, "Total Básico: "= Total_Basico_emp , "Auxilio De Transporte: " = AuxTransporte_emp, "Horas Extras: " = Total_H_EXT_emp , "Recargos: "= Total_Recargos_emp, "Otros Ingresos: "=Total_Otr_Ingreso_emp, "TOTAL SALARIO CONSTITUYENTE: "= Total_Constituyente_Sal_emp, "TOTAL SALARIO con Aux de Transp: " =Total_Ingreso_Aux_emp )
         
         
         deducciones_tab<-data.frame(t(rbind(colnames( deducciones_tab), deducciones_tab)))
         deducciones_tab[[2]]<-dollar(as.numeric(deducciones_tab[[2]]))
         
         rename( deducciones_tab,Conceptos =X1, Valores= X2)
         
         
      })
      
      output$res_empleador_nomina_deducciones <- renderTable({
         
         deducciones_tab<-select(Nomina$Empleador_nom, "Caja de Compensación: " = Caja_comp_emp, "Icbf: "= Icbf_emp, "Sena: "= Sena_emp, "Prima de Servicios: " =Prima_emp, "Cesantias: "= Cesantias_emp, "Int. Cesantías: "= Int_Cesantias_emp, "Vacaciones: "= Vacaciones_emp,"Aportes Pensión:"= Ded_pension_emp, "Aportes Salud" = Ded_salud_emp, "ARL: "= Ded_arl_emp )
         
         
         deducciones_tab<-data.frame(t(rbind(colnames( deducciones_tab), deducciones_tab)))
         deducciones_tab[[2]]<-dollar(as.numeric(deducciones_tab[[2]]))
         
         rename( deducciones_tab,Conceptos =X1, Valores= X2)
         
         
      })
      
      output$res_empleador_nomina_total <- renderTable({
         
         deducciones_tab<-select(Nomina$Empleador_nom, "TOTAL PAGADO: " = Total_pagado, "TOTAL PRESTACIONES Y DEDUCIDO: "= Total_prest_Ded, " TOTAL PAGADO MÁS DEDUCCIONES: " =Total)
         
         
         deducciones_tab<-data.frame(t(rbind(colnames( deducciones_tab), deducciones_tab)))
         deducciones_tab[[2]]<-dollar(as.numeric(deducciones_tab[[2]]))
         
         rename( deducciones_tab,Conceptos =X1, Valores= X2)
         
         
      })
      
      
   })
 
   
   
   ### save to RDS "part" 
   observeEvent(input$Save_Nomina,{
       Nomina$Data<- rbind(Nomina$Data, Nomina$Calculo_nom)
      
      saveRDS(Nomina$Data, "Nomina.rds")
      shinyalert(title = "Saved!", type = "success")
   })
   
   
   ####  
 
   
   
   #### MainBody_nomina is the id of DT table
   output$MainBody_nomina<-renderUI({
      fluidPage(
         hr(),
         column(6,offset = 6,
                HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                ### tags$head() This is to change the color of "Add a new row" button
                tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
                div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_nom",label = "Add", class="butt2") ),
                tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
                div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_nom",label = "Edit", class="butt4") ),
                tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
                div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_nom",label = "Delete", class="butt3") ),
                ### Optional: a html button 
                # HTML('<input type="submit" name="Add_row_nom" value="Add">'),
                HTML('</div>') ),
         
         column(12,dataTableOutput("Nomina_table_emp")),
         tags$script("$(document).on('click', '#Nomina_table_emp button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
         
      ) 
   })
   
   #### render DataTable part ####
   output$Nomina_table_emp<-renderDataTable({
      
      DT=Nomina$Data
      
      for (i in 3:26) {
         DT[[i]]<-dollar(as.numeric(DT[[i]]))
      }
      
      datatable(DT,selection = 'single',
                escape=F,  height = 1000, fillContainer= 
                TRUE, options = list(scrollY = 400)) })
  
   
   
   
   
   ### save to RDS part 
   observeEvent(input$Save_Nomina_cambios,{
      
      saveRDS(Nomina$Data, "Nomina.rds")
      shinyalert(title = "Saved!", type = "success")
   })
   
   
   
   ### delete selected rows part
   ### this is warning messge for deleting
   observeEvent(input$Del_row_nom, {
      showModal(
         if(length(input$Nomina_table_emp_rows_selected)>=1 ){
            modalDialog(
               title = "Warning",
               paste("Are you sure delete",length(input$Nomina_table_emp_rows_selected),"rows?" ),
               footer = tagList(
                  modalButton("Cancel"),
                  actionButton("ok", "Yes")
               ), easyClose = TRUE)
         }else{
            modalDialog(
               title = "Warning",
               paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
            )
         }
         
      )
   })
   
   ### If user say OK, then delete the selected rows
   observeEvent(input$ok, {
      Nomina$Data=Nomina$Data[-input$Nomina_table_emp_rows_selected,]
      removeModal()
   })
   
   ### edit button
   observeEvent(input$mod_row_nom,{
      showModal(
         if(length(input$Nomina_table_emp_rows_selected)>=1 ){
            modalDialog(
               fluidPage(
                  h3(strong("Modification"),align="center"),
                  hr(),
                  dataTableOutput('nom_modif'),
                  actionButton("save_changes_nom","Save changes"),
                  tags$script(HTML("$(document).on('click', '#save_changes_nom', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
         }else{
            modalDialog(
               title = "Warning",
               paste("Please select the row that you want to edit!" ),easyClose = TRUE
            )
         }
         
      )
   })
   
   
   
   
   #### modify part
   output$nom_modif<-renderDataTable({
      selected_row=input$Nomina_table_emp_rows_selected
      old_nom=Nomina$Data[selected_row,]
      row_change_nom=list()
      for (i in colnames(old_nom))
      {
         if (is.numeric(Nomina$Data[[i]]))
         {
            row_change_nom[[i]]<-paste0('<input class="new_input" value= ','"',old_nom[[i]],'"','  type="number" id=new_',i,' ><br>')
         } 
         else if( is.Date(Nomina$Data[[i]])){
            row_change_nom[[i]]<-paste0('<input class="new_input" value= ','"',old_nom[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
         }
         else 
            row_change_nom[[i]]<-paste0('<input class="new_input" value= ','"',old_nom[[i]],'"',' type="textarea"  id=new_',i,'><br>')
      }
      row_change_nom=as.data.table(row_change_nom)
      setnames(row_change_nom,colnames(old_nom))
      DT=row_change_nom
      DT 
   },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
   
   
   
   ### This is to replace the modified row to existing row
   observeEvent(input$newValue,
                {
                   newValue=lapply(input$newValue, function(col) {
                      if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                         as.numeric(as.character(col))
                      } else {
                         col
                      }
                   })
                   DF=data.frame(lapply(newValue, function(x) { t(data.frame(x))}))
                   colnames(DF)=colnames(Nomina$Data)
                   Nomina$Data[input$Nomina_table_emp_rows_selected]<-DF
                   
                }
   )
   ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
   ### can download the table in csv
   output$Download_xlsx<- downloadHandler(
      
      filename = function() {
         paste("Nomina", Sys.Date(), ".xlsx")
      },
      content = function(file) {
         write.xlsx(data.frame(Nomina$Data), file, row.names = F)
      }
      
         
     
   )
   output$downloadLiquidacion <- downloadHandler(
      filename = "Liquidacion_de_nomina.pdf",
      content = function(file) {
         file.copy("www/Liquidacion_de_nomina.pdf", file)
      }
   )
   
})



#####################################AQUI VA EL BOTON DE DESCARGA PARA LA TEORIA############################################################33

   

   







######################################################################################################################################
# Run the application
