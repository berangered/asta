#' stat2_sond_2deg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat2_sond_2deg_ui <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = "subitemc",
          h2("Sondage \u00e0 deux degr\u00e9s"),
  fluidRow(
    column(4,
           
           wellPanel(
             tags$p("Param\u00e8tres", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
             sliderInput(ns("TailleEch"), 
                         "Choisissez la taille de l'\u00e9chantillon",
                         min=100,
                         max=1000,
                         value=150),
             selectInput(ns("VarDegres"),"Choisissez une variable pour la selection des Unit\u00e9s primaires  :",  choices=c("Diplome"="DIPL","PCS"="PCS", "Activite"="ACT")),
             selectInput(ns("NomVarDegres"),"Choisissez un caract\u00e8re :", choices=c("Revenu Disponible"="REV_DISPONIBLE","Patrimoine"="PATRIMOINE")),
             actionButton(ns("go"),
                          label="Lancer le tirage et les calculs")
           ),
           wellPanel("Vous pouvez appuyer sur les ",span("fleches directionnelles", style="color:blue"),   "(→ ou ←) du clavier pour affiner de 1 en 1 la taille de l'echantillon")
    ),
    column(4,
           
           
           infoBox(
             title = tags$p("Moyenne dans l'\u00e9chantillon", style = "font-size : 80%;"),
             value = textOutput(ns("meandegres")),
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
           
           
           plotOutput(outputId = ns("plotsas1"))
           
    ),
    column(4,
           
           infoBox(
             title = tags$p("Moyenne dans la Pop. m\u00e8re", style = "font-size : 80%;"),
             value = textOutput(ns("meansasmere")),
             icon = icon("chart-line"),
             #fill = TRUE,
             color="light-blue",
             width = NULL
           ),
           infoBox(
             title = tags$p("Ecart-Type dans la Pop. m\u00e8re", style = "font-size : 80%;"),
             value = textOutput(ns("sdsasmere")),
             icon = icon("chart-line"),
             #fill = TRUE,
             color="light-blue",
             width = NULL
           ),
           plotOutput(outputId = ns("plotsas2"))
    ))
  )


}
    
#' stat2_sond_2deg Server Functions
#'
#' @noRd 
mod_stat2_sond_2deg_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    local <- reactiveValues(dt = NULL, var = NULL, taille = NULL, ech = NULL)
    
    
    observeEvent(input$go, {
      local$dt <- global$data
      local$varD <- input$VarDegres
      local$var <- input$NomVarDegres
      local$taille <- input$TailleEch
      local$ech <- tirage_degres_m2(input_data = global$data,
                                    taille_UP = 2,
                                    taille_tot =  local$taille,
                                    var_degres = local$varD)
     
      
      
      
    })
    
    output$plotsas1 <- renderPlot({
      validate(
        need(expr = !is.null(local$ech),
             message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le graphique")
      )
      
      boxplot_tirage_m2(local$ech, local$var)
    })
    
    output$plotsas2 <- renderPlot({
      validate(
        need(expr = !is.null(local$dt),
             message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le graphique")
      )
      
      boxplot_tirage_m2(local$dt, local$var)
    })
    
    output$meandegres <- renderText({
      req(local$ech)
      
      
      var <- as.numeric(local$var)
   
      local$meandegres <- mean(local$ech[, local$var],na.rm = TRUE)
       paste0(format_box(local$meandegres), " \u20ac")
    })
    
    output$sdsas <- renderText({
      req(local$ech)
      
      
      local$sdsas <- sd(local$ech[, local$var], na.rm = TRUE)
      paste0(format_box(local$sdsas), " \u20ac")
    })
    
    output$meansasmere <- renderText({
      req(local$dt)
      
      local$mean <- mean(local$dt[, local$var], na.rm = TRUE)
      paste0(format_box(local$mean), " \u20ac")
    })
    
    output$sdsasmere <- renderText({
      req(local$dt)
      
      local$sd <- sd(local$dt[, local$var], na.rm = TRUE)
      paste0(format_box(local$sd), " \u20ac")
    })
    
    
    
  })
} 
## To be copied in the UI
# mod_stat2_sond_2deg_ui("stat2_sond_2deg")
    
## To be copied in the server
# mod_stat2_sond_2deg_server("stat2_sond_2deg")
