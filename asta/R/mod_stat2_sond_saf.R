#' stat2_sond_saf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat2_sond_saf_ui <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = "subitemb",
          h2("Sondage al\u00e9atoire Stratifi\u00e9"),
  fluidRow(
    column(4,
           
           wellPanel(
             tags$p("Param\u00e8tres", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
             sliderInput(ns("TailleEch"), 
                         "Choisissez la taille de l'\u00e9chantillon",
                         min=100,
                         max=2500,
                         value=100),
             selectInput(ns("NomVar1"),
                         "Choisissez une variable de stratification :", 
                         choices=c("Diplome"="DIPL","Pauvre"="PAUVRE", "PCS"="PCS")),
             selectInput(ns("NomVar2"),
                         "Choisissez un caract\u00e8re :", 
                         choices=c("Revenu Disponible"="REV_DISPONIBLE","Patrimoine"="PATRIMOINE")),
             actionButton(ns("go"),
                          label="Lancer le tirage et les calculs")
           ),
           wellPanel("Vous pouvez appuyer sur les ",span("fleches directionnelles", style="color:blue"),   "(→ ou ←) du clavier pour affiner de 1 en 1 la taille de l'echantillon")
    ),
    column(4,
           
           
           infoBox(
             title = tags$p("Moyenne dans l'\u00e9chantillon", style = "font-size : 80%;"),
             value = textOutput(ns("meansaf")),
             icon = icon("chart-line"),
             #fill = TRUE,
             color="blue",
             width = NULL
           ) ,
           infoBox(
             title = tags$p("Ecart-Type dans l'\u00e9chantillon", style = "font-size : 80%;"),
             value = textOutput(ns("sdsaf")),
             icon = icon("chart-line"),
             #fill = TRUE,
             color="blue",
             width = NULL
           ) ,
           
           
           plotOutput(outputId = ns("plotsaf1"))
           
    ),
    column(4,
           
           infoBox(
             title = tags$p("Moyenne dans la Pop. m\u00e8re", style = "font-size : 80%;"),
             value = textOutput(ns("meansafmere")),
             icon = icon("chart-line"),
             #fill = TRUE,
             color="light-blue",
             width = NULL
           ),
           infoBox(
             title = tags$p("Ecart-Type dans la Pop. m\u00e8re", style = "font-size : 80%;"),
             value = textOutput(ns("sdsafmere")),
             icon = icon("chart-line"),
             #fill = TRUE,
             color="light-blue",
             width = NULL
           ),
           plotOutput(outputId = ns("plotsaf2"))
    ))
  )
  
}
    
#' stat2_sond_saf Server Functions
#'
#' @noRd 
mod_stat2_sond_saf_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    local <- reactiveValues(dt = NULL, var = NULL, ech = NULL, taille = NULL)
    
   
    observeEvent(input$go, {
      local$dt <- global$data
      local$ech <- tirage_strat_m2 (input_data =  global$data,
                                    taille_strat =  input$TailleEch,
                                    var_strat = input$NomVar1)
      local$var <- input$NomVar2
      local$taille <- input$TailleEch
      
      
      
    })

    output$plotsaf1 <- renderPlot({
      validate(
        need(expr = !is.null(local$ech),
             message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le graphique")
      )

      boxplot_tirage_m2(local$ech, local$var)
    })

    output$plotsaf2 <- renderPlot({
      validate(
        need(expr = !is.null(local$dt),
             message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le graphique")
      )

      boxplot_tirage_m2(local$dt, local$var)
    })

    output$meansaf <- renderText({
      req(local$dt)


      local$meansaf <- mean(local$ech[, local$var], na.rm = TRUE)
      paste0(format_box(local$meansaf), " \u20ac")
    })

    output$sdsaf <- renderText({
      req(local$dt)


      local$sdsaf <- sd(local$ech[, local$var], na.rm = TRUE)
      paste0(format_box(local$sdsaf), " \u20ac")
    })

    output$meansafmere <- renderText({
      req(local$dt)

      local$mean <- mean(local$dt[, local$var], na.rm = TRUE)
      paste0(format_box(local$mean), " \u20ac")
    })

    output$sdsafmere <- renderText({
      req(local$dt)

      local$sd <- sd(local$dt[, local$var], na.rm = TRUE)
      paste0(format_box(local$sd), " \u20ac")
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_stat2_sond_saf_ui("stat2_sond_saf")
    
## To be copied in the server
# mod_stat2_sond_saf_server("stat2_sond_saf")
