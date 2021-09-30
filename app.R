library(tidyverse)
library(labelled)
library(DT)
library(shiny)
library(shinythemes)
library(patchwork)
library(here)
library(DT)


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

df_generica$fecha=as.character(df_generica$fecha)                   


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
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              DTOutput("tabla_tamano"),
              br(),
              br(),
              br(),
              downloadButton("tabla_resultado_tamano_descarga", "Descargá la tabla"),
              br(),
              br(),
              br(),
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
                     uiOutput("title_estructura")),
             plotly::plotlyOutput("plot_estructura",height = 'auto', width = 'auto'),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             DTOutput("tabla_resultado_estructura"),
             br(),
             br(),
             br(),
             downloadButton("tabla_resultado_estructura_descarga", "Descargá la tabla"),
             br(),
             br(),
             br(),
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
     
    # uiOutput("selectcorte3"),
     
     
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
                     uiOutput("title_fecundidad")),
             plotly::plotlyOutput("plot_fecundidad",height = 'auto', width = 'auto'),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             DTOutput("tabla_resultado_fecundidad"),
             br(),
             br(),
             br(),
             downloadButton("tabla_resultado_fecundidad_descarga", "Descargá la tabla"),
             br(),
             br(),
             br(),
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
                     uiOutput("title_indicador_mortalidad")),
             plotly::plotlyOutput("plot_mortalidad",height = 'auto', width = 'auto'),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             DTOutput("tabla_resultado_mortalidad"),
             br(),
             br(),
             br(),
             downloadButton("tabla_resultado_mortalidad_descarga", "Descargá la tabla"),
             br(),
             br(),
             br(),
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

      
    }else if(input$indicador_tamano == "Población por edades quinquenales - proyecciones"|
             input$indicador_tamano == "Población departamental (censos)"){
      
     selectInput("rango_tamano_ano",
                 label = "Años",
                 choices = unique(base_tamano()$fecha))
       
  
    }
      })
  
  base_tamano_rango <- reactive({
    
    req(input$rango_tamano)
    
  base_tamano() %>%
      filter(fecha >= input$rango_tamano[1] &
               fecha <= input$rango_tamano[2])
  })
  
  
  
output$title_indicador_tamano <- renderUI({ 
  helpText(HTML(unique(base_tamano()$nomindicador)))
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
      labs(x = "",
           y = "",
           title = input$indicador_tamano,
           caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR)")
    
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
    



  } else if(input$indicador_tamano == "Población total - proyecciones" & input$corte_tamano == "Sexo"){

    g1 <- base_tamano_rango() %>%
      filter(corte == input$corte_tamano) %>%
      ggplot(aes(as.Date(as.character(fecha), format = "%Y"), valor, color = sexo, group = sexo,text = paste("</br>Año:",fecha,"</br>Sexo:",sexo,"</br>Valor:",valor))) +
      geom_line(size = 1) +
      geom_point(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "5 years",date_labels  = "%Y")+
      theme(axis.text.y  = element_text(size = 8),legend.position = "bottom")+
      labs(x = "",
           y = "",
           title = input$indicador_tamano,
           caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR)")
    
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
      ggplot(aes(x = edad, fill = sexo, text=paste(paste("</br>Año:",fecha,"</br>Valor:",valor)))) +
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
            
      )+labs(
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
      ggplot(aes(x = edad, text=paste(paste("</br>Año:",fecha,"</br>Valor:",valor)))) +
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
            
      )+labs(
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
    
  } else if(input$indicador_tamano == "Población departamental (censos)" & input$corte_tamano == "Total"){
    
    
    mapa = base_tamano() %>%
      filter(fecha == input$rango_tamano_ano & corte == input$corte_tamano )%>%
      mutate(nombre = toupper(stringi::stri_trans_general(str = departamento_uy, 
                                                          id = "Latin-ASCII")))
    
    mapa_geo = depto %>%
      left_join(mapa,by = "nombre") 
    
    g1 <- ggplot(mapa_geo,aes(fill = valor)) + geom_sf() +
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
        panel.grid.major = element_line(colour = "transparent"))
    
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
    
    g1 <- ggplot(mapa_geo,aes(fill = valor)) + geom_sf() +
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
        panel.grid.major = element_line(colour = "transparent"))
    
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




##Tablas shiny

output$tabla_resultado_id_tp <- renderDT({
  
  if(input$corte1 == "Región") {
    

    datatable(base_id_tp() %>%
              filter(corte_nueva == "Región") %>%
              arrange(anio)%>%
              transmute(
              "Año" = anio,
              "Región" = urbanoruraluy,
              "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
          

    
  } else if(input$corte1 == "Sexo del jefe(a)"){
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Sexo del jefe(a)") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo del jefe(a)" = sexojefatura,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte1 == "Pobreza") {
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Pobreza") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Pobreza" = pobreza,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }  else if (input$corte1 == "Quintil de ingreso") {
    

    datatable(base_id_tp() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Quintil de ingreso" = quintil,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte1 == "Decil de ingreso") {
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Decil de ingreso") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Decil de ingreso" = decil,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  } 
  else {
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Valor" = valor),
              rownames = FALSE,options = list(columnDefs = 
                                                list(list(className = 'dt-center', 
                                                          targets = "_all"))))
    
    
  }
  
})


output$tabla_resultado_id_tp_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_id_tp,"-",input$corte1, ".xlsx", sep = "")
  },
  content = function(file) {
  
  if(input$corte1 == "Región") {
    
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                filter(corte_nueva == "Región") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Región" = urbanoruraluy,
                  "Valor" = valor),
                  "Metadata" = base_id_tp() %>%
                  dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                  transmute(
                    "NOMBRE DEL INDICADOR" = nomindicador,
                    "FUENTE" = fuente,
                    "DEFINICIÓN" = definicion,
                    "FORMAS DE CÁLCULO"=calculo,
                    "COBERTURA"= cobertura,
                    "CITA"=cita)%>%
                  gather(key = "", value = " ")),file,
                row.names = FALSE)
                
    
  } else if(input$corte1 == "Sexo del jefe(a)"){
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Sexo del jefe(a)") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Sexo del jefe(a)" = sexojefatura,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  } else if (input$corte1 == "Pobreza") {
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Pobreza") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Pobreza" = pobreza,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  }  else if (input$corte1 == "Quintil de ingreso") {
    
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Quintil de ingreso") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Quintil de ingreso" = quintil,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)

    
  } else if (input$corte1 == "Decil de ingreso") {
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Decil de ingreso") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Decil de ingreso" = decil,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)

    
    
  } 
  else {
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Total") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
    
  }
  

})







output$plot_id_pu <- plotly::renderPlotly({
  
  if(input$corte2 == "Sexo del jefe(a)"){
    
    g1 <- base_id_pu() %>%
      filter(corte_nueva == "Sexo del jefe(a)") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexojefatura, group = sexojefatura)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(3,name="Blues")))+
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    
    
  } else if (input$corte2 == "Quintil de ingreso") {
    
    g1 <- base_id_pu() %>%
      filter(corte_nueva == "Quintil de ingreso") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = quintil, group = quintil)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(5,name="Blues")))+
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    
    
  }   else {
    
    g1 <- base_id_pu() %>%
      filter(corte_nueva=="Total") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor)) +
      geom_line(size = 1,color="#3182BD") +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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


output$tabla_resultado_id_pu <- renderDT({
  
  if(input$corte2 == "Sexo del jefe(a)"){
    
    
    datatable(base_id_pu() %>%
                filter(corte_nueva == "Sexo del jefe(a)") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo del jefe(a)" = sexojefatura,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte2 == "Quintil de ingreso") {
    

    datatable(base_id_pu() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Quintil de ingreso" = quintil,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }   else {
    
    datatable(base_id_pu() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
})


output$tabla_resultado_id_pu_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_id_pu,"-",input$corte2, ".xlsx", sep = "")
  },
  content = function(file) {
  
  if(input$corte2 == "Sexo del jefe(a)"){
    
    
    
    openxlsx::write.xlsx(list( "Data" = base_id_pu() %>%
                                 filter(corte_nueva == "Sexo del jefe(a)") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Sexo del jefe(a)" = sexojefatura,
                                           "Valor" = valor),
                               "Metadata" = base_id_pu() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  } else if (input$corte2 == "Quintil de ingreso") {
    
    
    
    openxlsx::write.xlsx(list( "Data" = base_id_pu() %>%
                                 filter(corte_nueva == "Quintil de ingreso") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Quintil de ingreso" = quintil,
                                           "Valor" = valor),
                               "Metadata" = base_id_pu() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  }   else {
    
    openxlsx::write.xlsx(list( "Data" = base_id_pu() %>%
                                 filter(corte_nueva == "Total") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Valor" = valor),
                               "Metadata" = base_id_pu() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  }
  
})








base_p <- reactive({
  
  validate(
    need(input$indicador_pobreza != "", "Seleccione un indicador")
  )
  
  df_generica %>%
    filter(categoria == "Pobreza",
           nomindicador == input$indicador_pobreza)%>% 
    mutate(valor = round(valor*100,1))
  
})


output$plot_p <- plotly::renderPlotly({
  
  if(input$corte3 == "Sexo"){
    
    g1 <- base_p() %>% 
      filter(corte_nueva == "Sexo") %>% 
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexo, group = sexo)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(2,name="Blues")))+
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    
    
  } else if (input$corte3 == "Ascendencia étnico-racial") {
    
    g1 <- base_p() %>% 
      filter(corte_nueva == "Ascendencia étnico-racial") %>% 
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = ascendencia, group = ascendencia)) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(2,name="Blues")))+
      geom_line(size = 1) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    
    
  } else if (input$corte3 == "Tramo de edad") {
    
    g1 <- base_p() %>% 
      filter(corte_nueva == "Tramo de edad") %>% 
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = tramo, group = tramo)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(7,name="Blues")))+
        scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    
    
  } else if (input$corte3 == "Sexo del jefe(a)") {
    
    g1 <- base_p() %>% 
      filter(corte_nueva == "Sexo del jefe(a)") %>% 
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexojefatura, group = sexojefatura)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(7,name="Blues")))+
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
  
  else {

    
    g1 <- base_p() %>% 
      filter(corte_nueva == "Total") %>% 
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = urbanoruraluy, group = urbanoruraluy)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(7,name="Blues")))+
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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



output$tabla_resultado_p <- renderDT({
  
  if(input$corte3 == "Sexo"){
    
    datatable(base_p() %>%
                filter(corte_nueva == "Sexo") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo" = sexo,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte3 == "Ascendencia étnico-racial") {
    
    datatable(base_p() %>%
                filter(corte_nueva == "Ascendencia étnico-racial") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Ascendencia étnico-racial" = ascendencia,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte3 == "Tramo de edad") {
    
    datatable(base_p() %>%
                filter(corte_nueva == "Tramo de edad") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Tramo de edad" = tramo,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte3 == "Sexo del jefe(a)") {
    
    datatable(base_p() %>%
                filter(corte_nueva == "Sexo del jefe(a)") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo del jefe(a)" = sexojefatura,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } 
  
  else {
    
    
  datatable(base_p() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  }
  
})


output$tabla_resultado_p_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_pobreza,"-",input$corte3, ".xlsx", sep = "")
  },
  content = function(file) {
  
  if(input$corte3 == "Sexo"){
    

    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Sexo") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Sexo" = sexo,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  } else if (input$corte3 == "Ascendencia étnico-racial") {
    
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Ascendencia étnico-racial") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Ascendencia étnico-racial" = ascendencia,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  } else if (input$corte3 == "Tramo de edad") {
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Tramo de edad") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Tramo de edad" = tramo,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  } else if (input$corte3 == "Sexo del jefe(a)") {
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Sexo del jefe(a)") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Sexo del jefe(a)" = sexojefatura,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
  } 
  
  else {
    
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Total") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%
                                 gather(key = "", value = " ")),file,
                         row.names = FALSE)
    
    
  }
  
})









base_nbi_p <- reactive({
  
  validate(
    need(input$indicador_nbi_p != "", "Seleccione un indicador")
  )
  
  df_generica %>%
    filter(categoria == "Necesidades Básicas",
           pestana=="Total País",
           nomindicador == input$indicador_nbi_p)%>% 
    mutate(valor = round(valor*100,1))
  
})


base_nbi_h <- reactive({
  
  validate(
    need(input$indicador_nbi_h != "", "Seleccione un indicador")
  )
  
  
  df_generica %>%
    filter(categoria == "Necesidades Básicas",
           pestana=="País Urbano",
           nomindicador == input$indicador_nbi_h)%>% 
  mutate(valor = round(valor*100,1))
  
})


##NBI TOTAL PAÍS


output$plot_nbi_p <- plotly::renderPlotly({

  if(input$corte4 == "Región") {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Región") %>%
      ggplot(aes(as.factor(anio), valor, color = urbanoruraluy, group = urbanoruraluy)) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(4,name="Blues")))+
      geom_line(size = 1) +
      theme(axis.text = element_text(size = 8))+
      
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    


  } else if(input$corte4 == "Sexo"){

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Sexo") %>%
      ggplot(aes(as.factor(anio), valor, color = sexo, group = sexo)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(2,name="Blues")))+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE) %>%
      plotly::config(showLink = FALSE)
    

  } else if (input$corte4 == "Ascendencia étnico-racial") {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Ascendencia étnico-racial") %>%
      ggplot(aes(as.factor(anio), valor, color = ascendencia, group = ascendencia)) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(2,name="Blues")))+
      geom_line(size = 1) +
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    

  } else if (input$corte4 == "Tramo de edad") {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Tramo de edad") %>%
      ggplot(aes(as.factor(anio), valor, color = tramo, group = tramo)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(7,name="Blues")))+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    

  } else if (input$corte4 == "Pobreza") {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Pobreza") %>%
      ggplot(aes(as.factor(anio), valor, color = pobreza, group = pobreza)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(2,name="Blues")))+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    
  } else if (input$corte4 == "Quintil de ingreso") {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Quintil de ingreso") %>%
      ggplot(aes(as.factor(anio), valor, color = quintil, group = quintil)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(5,name="Blues")))+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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

  else {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva=="Total") %>%
      ggplot(aes(as.factor(anio), valor, color = urbanoruraluy, group = urbanoruraluy)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(7,name="Blues")))+
      theme(axis.text = element_text(size = 8),legend.position = "bottom")+
      labs(x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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


output$tabla_resultado_nbi_p <- renderDT({
  
  if(input$corte4 == "Región") {
    
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Región") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Región" = urbanoruraluy,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  } else if(input$corte4 == "Sexo"){
    
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Sexo") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Ascendencia étnico-racial") {

    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Ascendencia étnico-racial") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Ascendencia étnico-racial" = ascendencia,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Tramo de edad") {
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Tramo de edad") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Tramo de edad" = tramo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Pobreza") {
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Pobreza") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Pobreza" = pobreza,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Quintil de ingreso") {
  
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Quintil de ingreso" = quintil,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
  else {
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
})




output$tabla_resultado_nbi_p_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_nbi_p,"-",input$corte4, ".xlsx", sep = "")
  },
  content = function(file) {
        
        if(input$corte4 == "Región") {
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Región") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Región"= urbanoruraluy,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%
                                       gather(key = "", value = " ")),file,
                               row.names = FALSE)
          
          
        } else if(input$corte4 == "Sexo"){
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Sexo") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Sexo"= sexo,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%
                                       gather(key = "", value = " ")),file,
                               row.names = FALSE)
          
        } else if (input$corte4 == "Ascendencia étnico-racial") {
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Ascendencia étnico-racial") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Ascendencia étnico-racial"= ascendencia,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%
                                       gather(key = "", value = " ")),file,
                               row.names = FALSE)
          
        } else if (input$corte4 == "Tramo de edad") {
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Tramo de edad") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Tramo de edad"= tramo,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%
                                       gather(key = "", value = " ")),file,
                               row.names = FALSE)
          
        } else if (input$corte4 == "Pobreza") {
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Pobreza") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Pobreza"= pobreza,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%
                                       gather(key = "", value = " ")),file,
                               row.names = FALSE)
          
        } else if (input$corte4 == "Quintil de ingreso") {
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Quintil de ingreso") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Quintil de ingreso"= quintil,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%
                                       gather(key = "", value = " ")),file,
                               row.names = FALSE)
          
        }
        
        else {
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Total") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%
                                       gather(key = "", value = " ")),file,
                               row.names = FALSE)
          
        }
        
      })



##País urbano

output$plot_nbi_h <- plotly::renderPlotly({

  if(input$corte5 == "Sexo") {

    g1 <- base_nbi_h() %>%
      filter(corte_nueva == "Sexo") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexo, group = sexo)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(4,name="Blues")))+
      scale_x_date(date_breaks = "4 years",date_labels  = "%Y")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    


  } else if(input$corte5 == "Tramo de edad"){

    g1 <- base_nbi_h() %>%
      filter(corte_nueva == "Tramo de edad") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = tramo, group = tramo)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(7,name="Blues")))+
      scale_x_date(date_breaks = "4 years",date_labels  = "%Y")+
      theme(legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    

  } else if(input$corte5 == "Quintil de ingreso"){

    g1 <- base_nbi_h() %>%
      filter(corte_nueva == "Quintil de ingreso") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = quintil, group = quintil)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(5,name="Blues")))+
      scale_x_date(date_breaks = "4 years",date_labels  = "%Y")+
      theme(legend.position = "bottom")+
      labs(color = "",
           x = "Año",
           y = "")

    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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
    

  }else {
    
    g1 <- base_nbi_h() %>%
      filter(corte_nueva=="Total") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = urbanoruraluy, group = urbanoruraluy)) +
      geom_line(size = 1) +
      scale_color_manual(values=rev(RColorBrewer::brewer.pal(7,name="Blues")))+
      scale_x_date(date_breaks = "4 years",date_labels  = "%Y")+
      theme(legend.position = "none")+
      labs(x = "Año",
           y = "")
    
    plotly::ggplotly(g1, width = (0.85*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("valor"))%>%
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

output$tabla_resultado_nbi_h <- renderDT({
  
  if(input$corte5 == "Sexo") {
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Sexo") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  } else if(input$corte5 == "Tramo de edad"){
  
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Tramo de edad") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Tramo de edad" = tramo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if(input$corte5 == "Quintil de ingreso"){
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Quintil de ingreso" = quintil,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }else {
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
  }
  
  
  
  
})


output$tabla_resultado_nbi_h_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_nbi_h,"-",input$corte5, ".xlsx", sep = "")
  },
  content = function(file) {

    if(input$corte5 == "Sexo") {
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Sexo") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Sexo" = sexo,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%
                                   gather(key = "", value = " ")),file,
                           row.names = FALSE)
      
      
    } else if(input$corte5 == "Tramo de edad"){
      
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Tramo de edad") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Tramo de edad" = tramo,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%
                                   gather(key = "", value = " ")),file,
                           row.names = FALSE)
      
    } else if(input$corte5 == "Quintil de ingreso"){
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Quintil de ingreso") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Quintil de ingreso" = quintil,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%
                                   gather(key = "", value = " ")),file,
                           row.names = FALSE)
      
    }else {
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Total") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%
                                   gather(key = "", value = " ")),file,
                           row.names = FALSE)
    }
    
    
    
    
  })



  }

  
shinyApp(ui = ui, server = server)

