
library(tidyverse)
library(labelled)
library(DT)
library(shiny)
library(shinythemes)
library(patchwork)
library(here)
library(DT)
library(DT)
library(shinythemes)
library(here)
library(plotly)
library(shinyWidgets)
library(stringr)
library(scales)
library(viridis)
library(ggrepel)
library(tidyverse)
library(wrapr)


source('utils.R')

##Preparo bases

##Ficha metodológica (ver compatibiidad de nombres)


# base_fichas <- readxl::read_excel("Base_Fichas_Tecnicas.xls")%>% 
#   janitor::clean_names() %>% 
#   mutate(nomindicador=replace(nomindicador, nomindicador=="Relación entre el ingreso medio per cápita del primer y décimo decil", 
#                               "Relación entre el Ingreso medio per cápita del primer y décimo decil"))%>%
#   mutate(nomindicador=replace(nomindicador, nomindicador=="Relación entre el ingreso medio per cápita del primer y quinto quintil", 
#                               "Relación entre el Ingreso medio per cápita del primer y quinto quintil")) 
# 

df_generica <- readxl::read_excel("Base_Motor_Demografica.xls") %>% 
  janitor::clean_names() %>% 
  select(- x1,- x2,- codind, - responsable) 

depto=geouy::load_geouy("Departamentos")

#df_generica$fecha=as.character(df_generica$fecha)                   


ui <- fluidPage(tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')), 
                navbarPage(
  theme = shinytheme("flatly"),
  "Demografia",
  collapsible=TRUE,
  
  
  tabPanel(
    title = "Tamaño",
    value = 'borelito',
    br(),
    div( id ="Sidebar",sidebarPanel(width = 3,
      #style = "position:fixed;width:22%;",
      selectInput(
        "indicador_tamano",
        "Seleccione el indicador:",
        choices = unique(df_generica %>%
                           filter(nomindicador == "Población total - proyecciones"  | 
                                    nomindicador == "Población por edades quinquenales - proyecciones"|
                                    nomindicador == "Población departamental (censos)"|
                                    nomindicador == "Población departamental - porcentaje (censos)"|
                                    nomindicador == "Población  departamental censada por edades quinquenales y sexo, Censo 2011"
                           ) %>%
                           pull(nomindicador))),
      
      uiOutput("selectcorte_tamano"),
      uiOutput("rango_tamano"),
      
      
      tags$a(href="https://umad.cienciassociales.edu.uy/", 
             "Unidad de Métodos y Acceso a Datos",
             style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
      br(),
      br(),
      img(src = "logo_umad.png", height="70%",
          width = "70%", align = "left"),
      br(),
      br()
      
      
      
    )),
    mainPanel(
      tags$style(type="text/css",
                 
                 ".shiny-output-error { visibility: hidden; }",
                 
                 ".shiny-output-error:before { visibility: hidden; }"
                 
      ),
      
      tags$h4(style="display:inline-block",
                      uiOutput("title_tamano")),
              plotly::plotlyOutput("plot_tamano",height = 'auto', width = 'auto'),
             tags$h6(style="display:inline-block",
              uiOutput("fuente_tamano")),
              br(),
              br(),
              DTOutput("tabla_tamano"),
              br(),
              downloadButton("tabla_resultado_tamano_descarga", "Descargá la tabla"),
              br(),
              br(),
              
    )
  ),
  
 tabPanel(
   title = "Estructura",
   value = 'borelito',
   br(),
   div( id ="Sidebar",sidebarPanel(width = 3,
     #style = "position:fixed;width:22%;",
     selectInput(
       "indicador_estructura",
       "Seleccione el indicador:",
       choices = unique(df_generica %>%
                          filter(nomindicador == "Relación de dependencia total (niños y ancianos por cada 100 personas de 15 a 64 años) por Departamento a 30 de junio de cada año"  | 
                                   nomindicador == "Porcentaje de población menor de 15 años por departamento a 30 de junio de cada año"|
                                   nomindicador == "Porcentaje de población de 15 a 64 años por departamento a 30 de junio de cada año"|
                                   nomindicador == "Porcentaje de población de 65 y más años por departamento a 30 de junio de cada año"|
                                   nomindicador == "Relación de masculinidad (hombres cada 100 mujeres)"|
                                   nomindicador == "Relación de masculinidad de la población de 65 y más años"|
                                   nomindicador == "Porcentaje de población menor de 15 años"|
                                   nomindicador == "Porcentaje de población de 15 a 64 años"|
                                   nomindicador == "Porcentaje de población de 65 y más años"|
                                   nomindicador == "Relación de dependencia total"|
                                   nomindicador == "Relación de dependencia de los niños"|
                                   nomindicador == "Relación de dependencia de la vejez"|
                                   nomindicador == "Índice de sobreenvejecimiento"|
                                   nomindicador == "Índice de envejecimiento"
                          )%>%
                          pull(nomindicador))),
     
     uiOutput("selectcorte_estructura"),
     uiOutput("rango_estructura"),
     
     
     tags$a(href="https://umad.cienciassociales.edu.uy/", 
            "Unidad de Métodos y Acceso a Datos",
            style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
     br(),
     br(),
     img(src = "logo_umad.png", height="70%",
         width = "70%", align = "left"),
     br(),
     br()
     
     
     
   )),
   mainPanel(
     tags$style(type="text/css",
                
                ".shiny-output-error { visibility: hidden; }",
                
                ".shiny-output-error:before { visibility: hidden; }"
                
     ),
     
     tags$h4(style="display:inline-block",
             uiOutput("title_estructura")),
     plotly::plotlyOutput("plot_estructura",height = 'auto', width = 'auto'),
     tags$h6(style="display:inline-block",
             uiOutput("fuente_estructura")),
     br(),
     br(),
     DTOutput("tabla_estructura"),
     br(),
     downloadButton("tabla_resultado_estructura_descarga", "Descargá la tabla"),
     br(),
     br(),
     
   )
 ),
 
 tabPanel(
   title = "Fecundidad",
   value = 'borelito',
   br(),
   div( id ="Sidebar",sidebarPanel(
     #style = "position:fixed;width:22%;",
     selectInput(
       "indicador_fecundidad",
       "Seleccione el indicador:",
       choices = unique(df_generica %>%
                          filter(nomindicador == "Nacimientos anuales"  | 
                                   nomindicador == "Tasa Global de Fecundidad"|
                                   nomindicador == "Tasa de fecundidad adolescente (por mil)"|
                                   nomindicador == "Edad media de la fecundidad"|
                                   nomindicador == "Nacimientos según departamento de residencia materna"|
                                   nomindicador == "Tasa global de fecundidad por departamento al 30 de junio de cada año (1996-2025)"|
                                   nomindicador == "Tasa global de fecundidad total país  (1996-2050)"|
                                   nomindicador == "Edad media a la maternidad"|
                                   nomindicador == "Tasa de fecundidad adolescente observada  (por mil) por departamento (1996-2020)"|
                                   nomindicador == "Porcentaje de embarazos no planificados por edad"
                          )%>%
                          pull(nomindicador))),
     
     uiOutput("selectcorte_fecundidad"),
     uiOutput("rango_fecundidad"),
     
     
     tags$a(href="https://umad.cienciassociales.edu.uy/", 
            "Unidad de Métodos y Acceso a Datos",
            style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
     br(),
     br(),
     img(src = "logo_umad.png", height="70%",
         width = "70%", align = "left"),
     br(),
     br()
     
     
     
   )),
   mainPanel(
     tags$style(type="text/css",
                
                ".shiny-output-error { visibility: hidden; }",
                
                ".shiny-output-error:before { visibility: hidden; }"
                
     ),
     
     tags$h4(style="display:inline-block",
             uiOutput("title_fecundidad")),
     plotly::plotlyOutput("plot_fecundidad",height = 'auto', width = 'auto'),
     tags$h6(style="display:inline-block",
             uiOutput("fuente_fecundidad")),
     br(),
     br(),
     DTOutput("tabla_fecundidad"),
     br(),
     downloadButton("tabla_resultado_fecundidad_descarga", "Descargá la tabla"),
     br(),
     br(),
     
   )
 ),
 
 tabPanel(
   title = "Mortalidad",
   value = 'borelito',
   br(),
   div( id ="Sidebar",sidebarPanel(
     #style = "position:fixed;width:22%;",
     selectInput(
       "indicador_mortalidad",
       "Seleccione el indicador:",
       choices = unique(df_generica %>%
                          filter(nomindicador == "Esperanza de vida al nacer (1996-2050)"  | 
                                   nomindicador == "Esperanza de vida al nacer por departamento (1996-2050)"|
                                   nomindicador == "Tasa de mortalidad neonatal (por 1.000 nacidos vivos) 1984-2020"|
                                   nomindicador == "Tasa de mortalidad posneonatal (por 1.000 nacidos vivos) 1984-2020"|
                                   nomindicador == "Tasa de mortalidad infantil (menores de 1 año por 1.000 nacidos vivos) 1984-2020"
                          )%>%
                          pull(nomindicador))),
     
     uiOutput("selectcorte_mortalidad"),
     uiOutput("selectcorte_mortalidad_depto"),
     uiOutput("rango_mortalidad"),
     
     
     tags$a(href="https://umad.cienciassociales.edu.uy/", 
            "Unidad de Métodos y Acceso a Datos",
            style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
     br(),
     br(),
     img(src = "logo_umad.png", height="70%",
         width = "70%", align = "left"),
     br(),
     br()
     
     
     
   )),
   mainPanel(
     tags$style(type="text/css",
                
                ".shiny-output-error { visibility: hidden; }",
                
                ".shiny-output-error:before { visibility: hidden; }"
                
     ),
     
     tags$h4(style="display:inline-block",
             uiOutput("title_mortalidad")),
     plotly::plotlyOutput("plot_mortalidad",height = 'auto', width = 'auto'),
     tags$h6(style="display:inline-block",
             uiOutput("fuente_mortalidad")),
     br(),
     br(),
     DTOutput("tabla_mortalidad"),
     br(),
     downloadButton("tabla_resultado_mortalidad_descarga", "Descargá la tabla"),
     br(),
     br(),
     
   )
 ),
 
 tabPanel(
   title = "Migración",
   value = 'borelito',
   br(),
   div( id ="Sidebar",sidebarPanel(
     #style = "position:fixed;width:22%;",
     selectInput(
       "indicador_migracion",
       "Seleccione el indicador:",
       choices = unique(df_generica %>%
                          filter(nomindicador == "Tasa inmigración departamental interna"  | 
                                   nomindicador == "Tasa emigración departamental interna"|
                                   nomindicador == "Tasa neta migración interna"|
                                   nomindicador == "Tasa neta de migración internacional total de Uruguay (por mil habitantes)"|
                                   nomindicador == "Tasa neta de migración internacional por departamento (por mil habitantes)"
                          )%>%
                          pull(nomindicador))),
     
     #uiOutput("selectcorte3"),
     
     
     tags$a(href="https://umad.cienciassociales.edu.uy/", 
            "Unidad de Métodos y Acceso a Datos",
            style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
     br(),
     br(),
     img(src = "logo_umad.png", height="70%",
         width = "70%", align = "left"),
     br(),
     br()
     
     
     
   )),
   mainPanel(tags$h4(style="display:inline-block",
                     uiOutput("title_indicador_migracion")),
             plotly::plotlyOutput("plot_migracion",height = 'auto', width = 'auto'),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             DTOutput("tabla_resultado_migracion"),
             br(),
             br(),
             br(),
             downloadButton("tabla_resultado_migracion_descarga", "Descargá la tabla"),
             br(),
             br(),
             br(),
             br(),
             br(),
             
   )
 )
 
 
 



 ))






server <- function(session, input, output) {
  


##TAMAÑO DE LA POBLACIÓN
  
  ##filto indicador
  base_tamano <- reactive({
    
    df_generica %>%
      filter(nomindicador == input$indicador_tamano)
    
  })
  
  ##corte
  output$selectcorte_tamano <- renderUI({
    selectInput("corte_tamano", "Resultados por:", choices = base_tamano()  %>% pull(corte) %>% unique())
  })
  
  ##años
  
  output$rango_tamano <- renderUI({
    
    if(input$indicador_tamano == "Población total - proyecciones"){
    
      sliderInput("rango_tamano", 
                label = "Rango de tiempo", 
                sep = "",
                dragRange = T,
                min = min(base_tamano()$fecha), 
                max = max(base_tamano()$fecha), 
                value = c(min(base_tamano()$fecha), 
                          max(base_tamano()$fecha)))

      
    } else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones"|
             input$indicador_tamano == "Población departamental (censos)"|
             input$indicador_tamano == "Población departamental - porcentaje (censos)"){
      
     selectInput("rango_tamano_ano",
                 label = "Años",
                 choices = unique(base_tamano()$fecha))
       
  
    } else if(input$indicador_tamano == "Población  departamental censada por edades quinquenales y sexo, Censo 2011"){
      
      selectInput("tamano_depto",
                  label = "Departamento",
                  choices = unique(base_tamano()$departamento_uy))
      
      
    }
    
    
      })
  
  
  
  
  base_tamano_rango <- reactive({
    
    req(input$rango_tamano)
    
  base_tamano() %>%
      filter(fecha >= input$rango_tamano[1] &
               fecha <= input$rango_tamano[2])
  })
  
  base_tamano_depto <- reactive({
    
    req(input$tamano_depto)
    
    base_tamano() %>%
      filter(departamento_uy == input$tamano_depto)
  })
  
  
  
  
  
output$title_tamano <- renderUI({ 
  helpText(HTML(unique(base_tamano()$nomindicador)))
})

output$fuente_tamano <- renderUI({ 
  helpText(HTML(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a",
                      unique(base_tamano()$fuente))))
})

output$plot_tamano <- plotly::renderPlotly({

  if(input$indicador_tamano == "Población total - proyecciones" & input$corte_tamano == "Total") {

    g1 <- base_tamano_rango() %>%
      filter(corte == input$corte_tamano) %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), y = valor, color=corte,group = corte, text = paste("</br>Año:",fecha,"</br>Valor:",valor)))+ 
      geom_line(size = 1, color="#3182BD") +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "none")+
      theme_minimal() +
      labs(x = "",
           y = "")
           
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2),
                     
                     annotations = list(x = 1, y = -0.4, 
      text = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
                         unique(base_tamano_rango()$fuente))))) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    



  } else if(input$indicador_tamano == "Población total - proyecciones" & input$corte_tamano == "Sexo"){

    g1 <- base_tamano_rango() %>%
      filter(corte == input$corte_tamano) %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), valor, color = sexo, group = sexo,text = paste("</br>Año:",fecha,"</br>Sexo:",sexo,"</br>Valor:",valor))) +
      geom_line(size = 1) +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text.y  = element_text(size = 8),legend.position = "bottom")+
      theme_minimal() +
      labs(x = "",
           y = "")
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)

  } else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones" & input$corte_tamano == "Sexo"){
    
    piramide = base_tamano_rango() %>%
      filter(fecha == input$rango_tamano_ano & corte == input$corte_tamano)%>%
      mutate(edad = gsub("años de edad","",edad))
    
    piramide$edad = factor(piramide$edad,levels = c("0-4 ","5-9 ","10-14 ","15-19 ",
                                                "20-24 ","25-29 ","30-34 ","35-39 ","40-44 ",
                                                "45-49 ","50-54 ","55-59 ","60-64 ","65-69 ","70-74 ",
                                                "75-79 ","80-84 ","85-89 ","90 y más "))
    
    g1 <- piramide %>%
      ggplot(aes(x = edad, fill = sexo, text=paste(paste("</br>Año:",fecha,"</br>Tramo de edad:",edad,"</br>Valor:",valor)))) +
      geom_col(data = filter(piramide, 
                             sexo == "varones"), 
               aes(y = round(-1*valor*100,2))) +
      geom_col(data = filter(piramide, 
                             sexo == "mujeres"), 
               aes(y = round(valor*100,2))) +  
      expand_limits(y = c(-50, 50)) +
      scale_y_continuous(breaks = seq(-50, 50, by = 10),
                         labels = abs) + 
      scale_fill_manual(name = "Sexo",
                        values = c("varones" = "#C6DBEF",
                                   "mujeres" = "#2171B5")) +
      coord_flip() +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            axis.text.y = element_text(size=7),
            axis.text.x =element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            strip.text.x = element_text(size = 8),
            plot.title = element_text(size=13),
            plot.caption = element_text(size=9,face = "italic",hjust = 0)
            
      )+
      theme_minimal()+
      labs(
        x = "",
        y = "",
        caption = "")
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  } else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones" & input$corte_tamano == "Total"){
    
    piramide = base_tamano_rango() %>%
      filter(fecha == input$rango_tamano_ano & corte == input$corte_tamano )%>%
      mutate(edad = gsub("años de edad","",edad))
    
    piramide$edad = factor(piramide$edad,levels = c("0-4 ","5-9 ","10-14 ","15-19 ",
                                                    "20-24 ","25-29 ","30-34 ","35-39 ","40-44 ",
                                                    "45-49 ","50-54 ","55-59 ","60-64 ","65-69 ","70-74 ",
                                                    "75-79 ","80-84 ","85-89 ","90 y más "))
    
    g1 <- piramide %>%
      ggplot(aes(x = edad, text=paste("</br>Año:",fecha,"</br>Tramo de edad:",edad,"</br>Valor:",valor))) +
      geom_col(data = piramide, 
               aes(y = round(valor,2)),fill="#3182BD") +
      expand_limits(y = c(-50, 50)) +
      scale_y_continuous(breaks = seq(-50, 50, by = 10),
                         labels = abs) + 
      coord_flip() +
      theme(legend.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size=7),
            axis.text.x =element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            strip.text.x = element_text(size = 8),
            plot.title = element_text(size=13),
            plot.caption = element_text(size=9,face = "italic",hjust = 0)
            
      )+theme_minimal()+labs(
        x = "",
        y = "",
        caption = "")
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  }else if(input$indicador_tamano == "Población departamental (censos)" & input$corte_tamano == "Total"){
    
    
    mapa = base_tamano() %>%
      filter(fecha == input$rango_tamano_ano & corte == input$corte_tamano )%>%
      mutate(nombre = toupper(stringi::stri_trans_general(str = departamento_uy, 
                                                          id = "Latin-ASCII")))
    
    mapa_geo = depto %>%
      left_join(mapa,by = "nombre") 
    
    g1 <- ggplot(mapa_geo,aes(fill = valor,text = paste("</br>Año:",fecha,"</br>Departamento:",departamento_uy,"</br>Valor:",valor))) + geom_sf() +
      geom_sf_text(aes(label = round(valor,1)), colour = "black",size=3,fontface = "bold")+
      scale_fill_gradient(low = "#9ECAE1", high = "#08306B")+
      labs(x = "",
           y = "",
           caption = "")+
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"))+
      theme_minimal()
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  }  else if(input$indicador_tamano == "Población departamental - porcentaje (censos)" & input$corte_tamano == "Total"){
    
    
    mapa = base_tamano() %>%
      filter(fecha == input$rango_tamano_ano & corte == input$corte_tamano )%>%
      mutate(nombre = toupper(stringi::stri_trans_general(str = departamento_uy, 
                                                          id = "Latin-ASCII")))
    
    mapa_geo = depto %>%
      left_join(mapa,by = "nombre") 
    
    g1 <- ggplot(mapa_geo,aes(fill = valor,text = paste("</br>Año:",fecha,"</br>Departamento:",departamento_uy,"</br>Valor:",round(valor,1)))) + geom_sf() +
      geom_sf_text(aes(label = paste0(round(valor,1),"%")), colour = "black",size=3,fontface = "bold")+
      scale_fill_gradient(low = "#9ECAE1", high = "#08306B")+
      labs(x = "",
           y = "",
           caption = "")+
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"))+
      theme_minimal()
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  } else if(input$indicador_tamano == "Población departamental (censos)" & input$corte_tamano == "Sexo"){
    
    
    mapa = base_tamano() %>%
      filter(fecha == input$rango_tamano_ano & corte == input$corte_tamano )%>%
      mutate(nombre = toupper(stringi::stri_trans_general(str = departamento_uy, 
                                                          id = "Latin-ASCII")))
    
    mapa_geo = depto %>%
      left_join(mapa,by = "nombre") 
    
    g1 <- ggplot(mapa_geo,aes(fill = valor,text = paste("</br>Año:",fecha,"</br>Departamento:",departamento_uy,"</br>Valor:",round(valor,1)))) + geom_sf() +
      geom_sf_text(aes(label = round(valor,1)), colour = "black",size=3,fontface = "bold")+
      scale_fill_gradient(low = "#9ECAE1", high = "#08306B")+
      facet_wrap(~sexo,ncol=2)+
      labs(x = "",
           y = "",
           caption = "")+
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"))+
      theme_minimal()
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  } else if(input$indicador_tamano == "Población  departamental censada por edades quinquenales y sexo, Censo 2011" & input$corte_tamano == "Total"){
    
    piramide = base_tamano_depto() %>%
      filter(departamento_uy == input$tamano_depto & corte == input$corte_tamano )%>%
      mutate(edad = gsub("años de edad","",edad))
    
    piramide$edad = factor(piramide$edad,levels = c("0-4 ","5-9 ","10-14 ","15-19 ",
                                                    "20-24 ","25-29 ","30-34 ","35-39 ","40-44 ",
                                                    "45-49 ","50-54 ","55-59 ","60-64 ","65-69 ","70-74 ",
                                                    "75-79 ","80-84 ","85-89 ","90-94 ","95-99 ","de 100 y más "))
    
    
    
    g1 <- piramide %>%
      ggplot(aes(x = edad, text=paste("</br>Departamento:",departamento_uy,"</br>Tramo de edad:",edad,"</br>Valor:",valor))) +
      geom_col(data = piramide, 
               aes(y = round(valor,2)),fill="#3182BD") +
      expand_limits(y = c(-50, 50)) +
      scale_y_continuous(breaks = seq(-50, 50, by = 10),
                         labels = abs) + 
      coord_flip() +
      theme(legend.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size=7),
            axis.text.x =element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            strip.text.x = element_text(size = 8),
            plot.title = element_text(size=13),
            plot.caption = element_text(size=9,face = "italic",hjust = 0)
              
            
      )+theme_minimal()+labs(
        x = "",
        y = "",
        caption = "")
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  }else if(input$indicador_tamano == "Población  departamental censada por edades quinquenales y sexo, Censo 2011" & input$corte_tamano == "Sexo"){
    
    piramide = base_tamano_depto() %>%
      filter(departamento_uy == input$tamano_depto & corte == input$corte_tamano )%>%
      mutate(edad = gsub("años de edad","",edad))
    
    piramide$edad = factor(piramide$edad,levels = c("0-4 ","5-9 ","10-14 ","15-19 ",
                                                    "20-24 ","25-29 ","30-34 ","35-39 ","40-44 ",
                                                    "45-49 ","50-54 ","55-59 ","60-64 ","65-69 ","70-74 ",
                                                    "75-79 ","80-84 ","85-89 ","90-94 ","95-99 ","de 100 y más "))
    
    
    
    g1 <- piramide %>%
      ggplot(aes(x = edad, fill = sexo, text=paste(paste("</br>Departamento:",departamento_uy,"</br>Tramo de edad:",edad,"</br>Valor:",valor)))) +
      geom_col(data = filter(piramide, 
                             sexo == "varones"), 
               aes(y = round(-1*valor*100,2))) +
      geom_col(data = filter(piramide, 
                             sexo == "mujeres"), 
               aes(y = round(valor*100,2))) +  
      expand_limits(y = c(-50, 50)) +
      scale_y_continuous(breaks = seq(-50, 50, by = 10),
                         labels = abs) + 
      scale_fill_manual(name = "Sexo",
                        values = c("varones" = "#C6DBEF",
                                   "mujeres" = "#2171B5")) +
      coord_flip() +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            axis.text.y = element_text(size=7),
            axis.text.x =element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            strip.text.x = element_text(size = 8),
            plot.title = element_text(size=13),
            plot.caption = element_text(size=9,face = "italic",hjust = 0)
            
      )+theme_minimal()+labs(
        x = "",
        y = "",
        caption = "")
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  }

})




##Tablas datatable

output$tabla_tamano <- renderDT({
  
  if(input$indicador_tamano == "Población total - proyecciones" & input$corte_tamano == "Total") {
    
    
    datatable(base_tamano_rango() %>%
              filter(corte == "Total") %>%
              arrange(fecha)%>%
              transmute(
              "Indicador" = nomindicador,
              "Año" = fecha,
              "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
          

    
  } else if(input$indicador_tamano == "Población total - proyecciones" & input$corte_tamano == "Sexo"){
    
    datatable(base_tamano_rango() %>%
                filter(corte == "Sexo") %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  } else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones" & input$corte_tamano == "Total"){
    
      
    datatable(base_tamano_rango() %>%
                filter(corte == "Total" & fecha == input$rango_tamano_ano) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Edad" = edad,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  }else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones" & input$corte_tamano == "Sexo"){
    
    datatable(base_tamano_rango() %>%
                filter(corte == "Sexo" & fecha == input$rango_tamano_ano) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Edad" = edad,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  }else if(input$indicador_tamano == "Población departamental (censos)" & input$corte_tamano == "Total"){
    
    datatable(base_tamano() %>%
                filter(corte == "Total" & fecha == input$rango_tamano_ano) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Departamento" = departamento_uy,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  } else if(input$indicador_tamano == "Población departamental (censos)" & input$corte_tamano == "Sexo"){
    
    datatable(base_tamano() %>%
                filter(corte == "Sexo" & fecha == input$rango_tamano_ano) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Departamento" = departamento_uy,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  }else if(input$indicador_tamano == "Población departamental - porcentaje (censos)" & input$corte_tamano == "Total"){
    
    datatable(base_tamano() %>%
                filter(corte == "Total" & fecha == input$rango_tamano_ano) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Departamento" = departamento_uy,
                  "Valor" = round(valor,1)),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  }else if(input$indicador_tamano == "Población  departamental censada por edades quinquenales y sexo, Censo 2011" & input$corte_tamano == "Total"){
    
    datatable(base_tamano_depto() %>%
                
                filter(corte == "Total" & departamento_uy == input$tamano_depto) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Departamento" = departamento_uy,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  }else if(input$indicador_tamano == "Población  departamental censada por edades quinquenales y sexo, Censo 2011" & input$corte_tamano == "Sexo"){
    
    datatable(base_tamano_depto() %>%
                
                filter(corte == "Sexo" & departamento_uy == input$tamano_depto) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Departamento" = departamento_uy,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
    
    
  }
  

    
})


##decarga

output$tabla_resultado_tamano_descarga <- downloadHandler(
  
  filename = function() {
    paste0(input$indicador_tamano,"-",input$corte_tamano, ".xlsx", sep = "")
  },
  content = function(file) {

  if(input$indicador_tamano == "Población total - proyecciones" & input$corte_tamano == "Total") {
    
    
    openxlsx::write.xlsx(base_tamano_rango() %>%
                           filter(corte == "Total") %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
    
    
  } else if(input$indicador_tamano == "Población total - proyecciones" & input$corte_tamano == "Sexo"){
    
    openxlsx::write.xlsx(base_tamano_rango() %>%
                           filter(corte == "Sexo") %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Sexo" = sexo,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
    
  } else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones" & input$corte_tamano == "Total"){
    
    
    
    openxlsx::write.xlsx(base_tamano_rango() %>%
                           filter(corte == "Total" & fecha == input$rango_tamano_ano) %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Edad" = edad,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
    
    
  } else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones" & input$corte_tamano == "Sexo"){
    
    openxlsx::write.xlsx(base_tamano_rango() %>%
                           filter(corte == "Sexo" & fecha == input$rango_tamano_ano) %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Edad" = edad,
                             "Sexo" = sexo,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
    
  }else if(input$indicador_tamano == "Población departamental (censos)" & input$corte_tamano == "Total"){
    
    openxlsx::write.xlsx(base_tamano() %>%
                           filter(corte == "Total" & fecha == input$rango_tamano_ano) %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Departamento" = departamento_uy,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
    
  } else if(input$indicador_tamano == "Población departamental (censos)" & input$corte_tamano == "Sexo"){
    
    openxlsx::write.xlsx(base_tamano() %>%
                           filter(corte == "Sexo" & fecha == input$rango_tamano_ano) %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Departamento" = departamento_uy,
                             "Sexo" = sexo,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
  }else if(input$indicador_tamano == "Población departamental - porcentaje (censos)" & input$corte_tamano == "Total"){
    
    openxlsx::write.xlsx(base_tamano() %>%
                           filter(corte == "Total" & fecha == input$rango_tamano_ano) %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Departamento" = departamento_uy,
                             "Valor" = round(valor,1),
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
    
  }else if(input$indicador_tamano == "Población  departamental censada por edades quinquenales y sexo, Censo 2011" & input$corte_tamano == "Total"){
    
    openxlsx::write.xlsx(base_tamano_depto() %>%
                           
                           filter(corte == "Total" & departamento_uy == input$tamano_depto) %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Departamento" = departamento_uy,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
    
  }else if(input$indicador_tamano == "Población  departamental censada por edades quinquenales y sexo, Censo 2011" & input$corte_tamano == "Sexo"){
    
    openxlsx::write.xlsx(base_tamano_depto() %>%
                           
                           filter(corte == "Sexo" & departamento_uy == input$tamano_depto) %>%
                           arrange(fecha)%>%
                           transmute(
                             "Indicador" = nomindicador,
                             "Año" = fecha,
                             "Departamento" = departamento_uy,
                             "Sexo" = sexo,
                             "Valor" = valor,
                             "Fuente" = fuente),file,
                         row.names = FALSE)
    
  }
  
  
  
})



##ESTRUCTURA DE LA POBLACIÓN


##filto indicador
base_estructura <- reactive({
  
  df_generica %>%
    filter(nomindicador == input$indicador_estructura)
  
})


##corte
output$selectcorte_estructura <- renderUI({
  selectInput("corte_estructura", "Seleccione:", choices = base_estructura()  %>% pull(departamento_uy) %>% unique())
})

##años

output$rango_estructura <- renderUI({
  
    sliderInput("rango_estructura", 
                label = "Rango de tiempo", 
                sep = "",
                dragRange = T,
                min = min(base_estructura()$fecha), 
                max = max(base_estructura()$fecha), 
                value = c(min(base_estructura()$fecha), 
                          max(base_estructura()$fecha)))
  
  
})




base_estructura_rango <- reactive({
  
  req(input$rango_estructura)
  
  base_estructura() %>%
    filter(fecha >= input$rango_estructura[1] &
             fecha <= input$rango_estructura[2])
})






output$title_estructura <- renderUI({ 
  helpText(HTML(unique(base_estructura()$nomindicador)))
})

output$fuente_estructura <- renderUI({ 
  helpText(HTML(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a",
                      unique(base_estructura()$fuente))))
})








output$plot_estructura <- plotly::renderPlotly({

    g1 <- base_estructura_rango() %>%
      filter(departamento_uy == input$corte_estructura) %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), y = valor, color=departamento_uy,group = departamento_uy, 
                 text = paste("</br>Año:",fecha,"</br>Área geográfica:",departamento_uy,"</br>Valor:",round(valor,1))))+
      geom_line(size = 1, color="#3182BD") +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "none")+
      theme_minimal()+
      labs(x = "",
           y = "")


    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2),

                     annotations = list(x = 1, y = -0.4,
                                        text = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
                                                            unique(base_estructura_rango()$fuente))))) %>%

      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)


 
  
})



##Tablas datatable

output$tabla_estructura <- renderDT({
  
    datatable(base_estructura_rango() %>%
                filter(departamento_uy == input$corte_estructura) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Área geográfica" = departamento_uy,
                  "Valor" = round(valor,1)),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  })


output$tabla_resultado_estructura_descarga <- downloadHandler(
  
  filename = function() {
    paste0(input$indicador_estructura,"-",input$corte_estructura, ".xlsx", sep = "")
  },
  content = function(file) {
    
      openxlsx::write.xlsx(base_estructura_rango() %>%
                             filter(departamento_uy == input$corte_estructura) %>%
                             arrange(fecha)%>%
                             transmute(
                               "Indicador" = nomindicador,
                               "Año" = fecha,
                               "Área geográfica" = departamento_uy,
                               "Valor" = round(valor,1),
                               "Fuente" = fuente),file,
                           row.names = FALSE)
      
      
    })







##FECUNDIDAD


##filto indicador
base_fecundidad <- reactive({
  
  df_generica %>%
    filter(nomindicador == input$indicador_fecundidad)
  
})


##corte
output$selectcorte_fecundidad <- renderUI({
  selectInput("corte_fecundidad", "Seleccione:", choices = base_fecundidad()  %>% pull(departamento_uy) %>% unique())
})

##años

output$rango_fecundidad <- renderUI({
  if(input$indicador_fecundidad == "Porcentaje de embarazos no planificados por edad") {
   
    selectInput("rango_fecundidad_ano",
                label = "Años",
                choices = unique(base_fecundidad()$fecha))
    
    
  } else{
  
  sliderInput("rango_fecundidad", 
              label = "Rango de tiempo", 
              sep = "",
              dragRange = T,
              min = min(base_fecundidad()$fecha), 
              max = max(base_fecundidad()$fecha), 
              value = c(min(base_fecundidad()$fecha), 
                        max(base_fecundidad()$fecha)))
  
  }
})




base_fecundidad_rango <- reactive({
  
  
  req(input$rango_fecundidad)
  
  base_fecundidad() %>%
    filter(fecha >= input$rango_fecundidad[1] &
             fecha <= input$rango_fecundidad[2])
})






output$title_fecundidad <- renderUI({ 
  helpText(HTML(unique(base_fecundidad()$nomindicador)))
})

output$fuente_fecundidad <- renderUI({ 
  helpText(HTML(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a",
                      unique(base_fecundidad()$fuente))))
})





output$plot_fecundidad <- plotly::renderPlotly({
  
  if(input$indicador_fecundidad == "Porcentaje de embarazos no planificados por edad") {
  
    
    g1 <- base_fecundidad() %>% 
         filter(departamento_uy == input$corte_fecundidad & fecha==input$rango_fecundidad_ano)%>% 
      ggplot(aes(x=edad, y=valor, fill=edad,text = paste("</br>Año:",fecha,"</br>Área geográfica:",departamento_uy,"</br>Edad:",edad,"</br>Valor:",round(valor,1)))) +
               geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
               scale_y_continuous(limits = c(0, 100))+
               scale_fill_manual(values=rev(RColorBrewer::brewer.pal(9,name="Blues")))+
               theme_minimal() +
               theme(axis.text.x = element_text(size = 10),legend.position = "none")+
               labs(x = "",
               y = "")
             
             
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2)
                     
                      # annotations = list(x = 1, y = -0.4,
                      #                    text = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
                      #                                        unique(base_fecundidad()$fuente))))
                     ) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  
  } else {
    
    
    g1 <- base_fecundidad_rango() %>%
      filter(departamento_uy == input$corte_fecundidad) %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), y = valor, color=departamento_uy,group = departamento_uy, 
                 text = paste("</br>Año:",fecha,"</br>Área geográfica:",departamento_uy,"</br>Valor:",round(valor,1))))+
      geom_line(size = 1, color="#3182BD") +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "none")+
      theme_minimal()+
      labs(x = "",
           y = "")
    
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2),
                     
                     annotations = list(x = 1, y = -0.4,
                                        text = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
                                                            unique(base_fecundidad_rango()$fuente))))) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
  
    
  }
    
})



##Tablas datatable

output$tabla_fecundidad <- renderDT({
  
  if(input$indicador_fecundidad == "Porcentaje de embarazos no planificados por edad") {
    
  
    datatable(base_fecundidad() %>% 
                filter(departamento_uy == input$corte_fecundidad & fecha==input$rango_fecundidad_ano)%>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Área geográfica" = departamento_uy,
                  "Edad" = edad,
                  "Valor" = round(valor,1)),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
 
  } else {
    
    datatable(base_fecundidad_rango() %>%
                filter(departamento_uy == input$corte_fecundidad) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Área geográfica" = departamento_uy,
                  "Valor" = round(valor,1)),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
})


output$tabla_resultado_fecundidad_descarga <- downloadHandler(
  
  filename = function() {
    paste0(input$indicador_fecundidad,"-",input$corte_fecundidad, ".xlsx", sep = "")
  },
  content = function(file) {
    
    if(input$indicador_fecundidad == "Porcentaje de embarazos no planificados por edad") {
      
    
      openxlsx::write.xlsx(base_fecundidad() %>% 
                             filter(departamento_uy == input$corte_fecundidad & fecha==input$rango_fecundidad_ano)%>%
                             arrange(fecha)%>%
                             transmute(
                               "Indicador" = nomindicador,
                               "Año" = fecha,
                               "Área geográfica" = departamento_uy,
                               "Valor" = round(valor,1),
                               "Fuente" = fuente),file,
                           row.names = FALSE)
    
      
    }else {
      
      
      openxlsx::write.xlsx(base_fecundidad_rango() %>%
                             filter(departamento_uy == input$corte_fecundidad) %>%
                             arrange(fecha)%>%
                             transmute(
                               "Indicador" = nomindicador,
                               "Año" = fecha,
                               "Área geográfica" = departamento_uy,
                               "Valor" = round(valor,1),
                               "Fuente" = fuente),file,
                           row.names = FALSE)
      
      
      
    }
    
    
    
  })






##MORTALIDAD


##filtro indicador

base_mortalidad <- reactive({
  
  df_generica %>%
    filter(nomindicador == input$indicador_mortalidad)
  
})


##corte
output$selectcorte_mortalidad <- renderUI({
  
  if(input$indicador_mortalidad == "Esperanza de vida al nacer (1996-2050)") {
    
    selectInput("corte_mortalidad", "Seleccione:", choices = c("Total","Sexo"))
    

  }else if(input$indicador_mortalidad == "Esperanza de vida al nacer por departamento (1996-2050)"){
    
    
    selectInput("corte_mortalidad_sexo", "Seleccione:", choices = c("Sexo"))
    
    
    
  }
  
  
  })

##años

output$rango_mortalidad <- renderUI({

    sliderInput("rango_mortalidad", 
                label = "Rango de tiempo", 
                sep = "",
                dragRange = T,
                min = min(base_mortalidad()$fecha), 
                max = max(base_mortalidad()$fecha), 
                value = c(min(base_mortalidad()$fecha), 
                          max(base_mortalidad()$fecha)))
    
  
})




output$selectcorte_mortalidad_depto <- renderUI({
  
    selectInput("corte_mortalidad_depto", "Área geográfica:", choices = base_mortalidad()  %>% pull(departamento_uy) %>% unique())
    
  
})






base_mortalidad_rango <- reactive({
  
  
  req(input$rango_mortalidad)
  
  base_mortalidad() %>%
    filter(fecha >= input$rango_mortalidad[1] &
             fecha <= input$rango_mortalidad[2])
})






output$title_mortalidad <- renderUI({ 
  helpText(HTML(unique(base_mortalidad()$nomindicador)))
})

output$fuente_mortalidad <- renderUI({ 
  helpText(HTML(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a",
                      unique(base_mortalidad()$fuente))))
})





output$plot_mortalidad <- plotly::renderPlotly({
  
  if(input$indicador_mortalidad == "Esperanza de vida al nacer (1996-2050)" &
     input$corte_mortalidad == "Total") {
    
    
    g1 <- base_mortalidad_rango() %>%
      filter(sexo == "ambos sexos") %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), y = valor, color=sexo,group = sexo, text = paste("</br>Año:",fecha,"</br>Valor:",valor)))+ 
      geom_line(size = 1, color="#3182BD") +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "none")+
      theme_minimal() +
      labs(x = "",
           y = "")
    
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2),
                     
                     annotations = list(x = 1, y = -0.4, 
                                        text = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
                                                            unique(base_mortalidad_rango()$fuente))))) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  } else if(input$indicador_mortalidad == "Esperanza de vida al nacer (1996-2050)" &
       input$corte_mortalidad == "Sexo") {
    
    
    g1 <- base_mortalidad_rango() %>%
      filter(sexo != "ambos sexos") %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), valor, color = sexo, group = sexo,text = paste("</br>Año:",fecha,"</br>Sexo:",sexo,"</br>Valor:",valor))) +
      geom_line(size = 1) +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text.y  = element_text(size = 8),legend.position = "bottom")+
      theme_minimal() +
      labs(x = "",
           y = "")
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  }  else if(input$indicador_mortalidad == "Esperanza de vida al nacer por departamento (1996-2050)" &
            input$corte_mortalidad_sexo == "Sexo") {
    
    
    g1 <- base_mortalidad_rango() %>%
      filter(sexo != "ambos sexos"& departamento_uy==input$corte_mortalidad_depto) %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), valor, color = sexo, group = sexo,text = paste("</br>Año:",fecha,"</br>Sexo:",sexo,"</br>Valor:",valor))) +
      geom_line(size = 1) +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text.y  = element_text(size = 8),legend.position = "bottom")+
      theme_minimal() +
      labs(x = "",
           y = "")
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  } else {
    
    
    
    g1 <- base_mortalidad_rango() %>%
      filter(departamento_uy == input$corte_mortalidad_depto) %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), y = valor, color=departamento_uy,group = departamento_uy, 
                 text = paste("</br>Año:",fecha,"</br>Área geográfica:",departamento_uy,"</br>Valor:",round(valor,1))))+
      geom_line(size = 1, color="#3182BD") +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme_minimal()+
      theme(axis.text = element_text(size = 8),legend.position = "none")+
      labs(x = "",
           y = "")
    
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(
        legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2),
                     
                     annotations = list(x = 1, y = -0.4,
                                        text = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
                                                            unique(base_mortalidad_rango()$fuente))))) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
    
  }
  
  
  
  
  
  
})



##Tablas datatable

output$tabla_fecundidad <- renderDT({
  
  if(input$indicador_fecundidad == "Porcentaje de embarazos no planificados por edad") {
    
    
    datatable(base_fecundidad() %>% 
                filter(departamento_uy == input$corte_fecundidad & fecha==input$rango_fecundidad_ano)%>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Área geográfica" = departamento_uy,
                  "Edad" = edad,
                  "Valor" = round(valor,1)),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  } else {
    
    datatable(base_fecundidad_rango() %>%
                filter(departamento_uy == input$corte_fecundidad) %>%
                arrange(fecha)%>%
                transmute(
                  "Indicador" = nomindicador,
                  "Año" = fecha,
                  "Área geográfica" = departamento_uy,
                  "Valor" = round(valor,1)),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
})


output$tabla_resultado_fecundidad_descarga <- downloadHandler(
  
  filename = function() {
    paste0(input$indicador_fecundidad,"-",input$corte_fecundidad, ".xlsx", sep = "")
  },
  content = function(file) {
    
    if(input$indicador_fecundidad == "Porcentaje de embarazos no planificados por edad") {
      
      
      openxlsx::write.xlsx(base_fecundidad() %>% 
                             filter(departamento_uy == input$corte_fecundidad & fecha==input$rango_fecundidad_ano)%>%
                             arrange(fecha)%>%
                             transmute(
                               "Indicador" = nomindicador,
                               "Año" = fecha,
                               "Área geográfica" = departamento_uy,
                               "Valor" = round(valor,1),
                               "Fuente" = fuente),file,
                           row.names = FALSE)
      
      
    }else {
      
      
      openxlsx::write.xlsx(base_fecundidad_rango() %>%
                             filter(departamento_uy == input$corte_fecundidad) %>%
                             arrange(fecha)%>%
                             transmute(
                               "Indicador" = nomindicador,
                               "Año" = fecha,
                               "Área geográfica" = departamento_uy,
                               "Valor" = round(valor,1),
                               "Fuente" = fuente),file,
                           row.names = FALSE)
      
      
      
    }
    
    
    
  })








  }

  
shinyApp(ui = ui, server = server)

