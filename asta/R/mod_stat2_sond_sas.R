#' stat2_sond_sas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat2_sond_sas_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = "subitema",
    h2("Sondage Aleatoire Simple"),
    fluidRow(
      column(4,
             
             wellPanel(
               tags$p("Param\u00e8tres", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               sliderInput(ns("TailleEch"), 
                           "Choisissez la taille de l'\u00e9chantillon",
                           min=10,
                           max=5418,
                           value=100),
               selectInput(ns("NomVar1"),
                           "Choisissez un caract\u00e8re :", 
                           choices=c("Revenu Disponible"="REV_DISPONIBLE","Patrimoine"="PATRIMOINE")),
               actionButton(ns("go"),
                            label="Lancer le tirage et les calculs")
             ),
             wellPanel("Vous pouvez appuyer sur les ",span("fleches directionnelles", style="color:blue"),   "(→ ou ←) du clavier pour affiner de 1 en 1 la taille de l'\u00e9chantillon")
      ),
      column(4,
             
             
             infoBox(
               title = tags$p("Moyenne dans l'\u00e9chantillon", style = "font-size : 80%;"),
               value = textOutput(ns("meansas")),
               icon = icon("chart-line"),
               #fill = TRUE,
               color="blue",
               width = NULL
             ) ,
             infoBox(
               title = tags$p("Ecart-Type dans l'\u00e9chantillon", style = "font-size : 80%;"),
               value = textOutput(ns("sdsas")),
               icon = icon("chart-line"),
               #fill = TRUE,
               color="blue",
               width = NULL
             ) ,
             
             
             plotOutput(outputId = ns("plotsas"))
             
      ),
      column(4,
             
             infoBox(
               title = tags$p("Moyenne dans la Pop. m\u00e8re", style = "font-size : 80%;"),
               value = textOutput(ns("meanmere")),
               icon = icon("chart-line"),
               #fill = TRUE,
               color="light-blue",
               width = NULL
             ),
             infoBox(
               title = tags$p("Ecart-Type dans la Pop. m\u00e8re", style = "font-size : 80%;"),
               value = textOutput(ns("sdmere")),
               icon = icon("chart-line"),
               #fill = TRUE,
               color="light-blue",
               width = NULL
             ),
             plotOutput(outputId = ns("plotmere"))
      ))
  )
  
  
}
    
#' stat2_sond_sas Server Functions
#'
#' @noRd 
mod_stat2_sond_sas_server <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    local <- reactiveValues(dt = NULL, var = NULL)
    
    
    observeEvent(input$go, {
      local$dt <- global$data
      local$ech <- tirage_sas_m2(global$data, input$TailleEch)
      local$var <- input$NomVar1
      local$taille <- input$TailleEch
      
      
      
    })
    
    output$plotsas <- renderPlot({
      validate(
        need(expr = !is.null(local$ech),
             message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le graphique")
      )
      
      boxplot_tirage_m2(local$ech, local$var)
    })
    
    output$plotmere <- renderPlot({
      validate(
        need(expr = !is.null(local$dt),
             message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le graphique")
      )
      
      boxplot_tirage_m2(local$dt, local$var)
    })
    
    output$meansas <- renderText({
      req(local$dt)
      
      
      local$meansas <- mean(local$ech[, local$var], na.rm = TRUE)
      paste0(format_box(local$meansas), " \u20ac")
    })
    
    output$sdsas <- renderText({
      req(local$dt)
      
      
      local$sdsas <- sd(local$ech[, local$var], na.rm = TRUE)
      paste0(format_box(local$sdsas), " \u20ac")
    })
    
    output$meanmere <- renderText({
      req(local$dt)
      
      local$mean <- mean(local$dt[, local$var], na.rm = TRUE)
      paste0(format_box(local$mean), " \u20ac")
    })
    
    output$sdmere <- renderText({
      req(local$dt)
      
      local$sd <- sd(local$dt[, local$var], na.rm = TRUE)
      paste0(format_box(local$sd), " \u20ac")
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_stat2_sond_sas_ui("stat2_sond_sas")
    
## To be copied in the server
# mod_stat2_sond_sas_server("stat2_sond_sas")
