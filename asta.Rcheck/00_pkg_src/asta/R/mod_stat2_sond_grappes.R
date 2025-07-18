#' stat2_sond_grappes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom stringr str_pad
mod_stat2_sond_grappes_ui <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = "subitemd",
          h2("Sondage en grappes"),
          fluidRow(
            column(4,
                   
                   wellPanel(
                     
                     tags$p("Param\u00e8tres", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                     selectInput(ns("NomVar1"),
                                 "Choisissez une variable pour la selection des UP :", 
                                 choices=c("Grappes de 20 logements"="grappe20","Grappes de 10 logements"="grappe10","Grappes de 5 logements"="grappe5")),
                     numericInput(ns("UP"), label = "Choisissez le nombre d'UP \u00e0 \u00e9chantillonner", min = 1, max = 20, value = 5),
                     selectInput(ns("NomVar2"),
                                 "Choisissez un caract\u00e8re :", 
                                 choices=c("Revenu Disponible"="REV_DISPONIBLE","Patrimoine"="PATRIMOINE")),
                     actionButton(ns("go"),
                                  label="Lancer le tirage et les calculs")
                   ),
                   infoBox(
                     title = tags$p("Taille de l'\u00e9chantillon", style = "font-size : 80%;"),
                     value = textOutput(ns("taillech")),
                     color="green",
                     width = NULL
                   ),
                   
                   
                   wellPanel("UP = unités primaires = grappes"
            )),
            
            column(4,
                   
                   
                   infoBox(
                     title = tags$p("Moyenne dans l'\u00e9chantillon", style = "font-size : 80%;"),
                     value = textOutput(ns("meangrappe")),
                     icon = icon("chart-line"),
                     #fill = TRUE,
                     color="blue",
                     width = NULL
                   ) ,
                   infoBox(
                     title = tags$p("Ecart-Type dans l'\u00e9chantillon", style = "font-size : 80%;"),
                     value = textOutput(ns("sdgrappe")),
                     icon = icon("chart-line"),
                     #fill = TRUE,
                     color="blue",
                     width = NULL
                   ) ,
                   
                   
                   plotOutput(outputId = ns("plotgrappe"))
                   
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
    
#' stat2_sond_grappes Server Functions
#'
#' @noRd 
mod_stat2_sond_grappes_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
   
    local <- reactiveValues(dt = NULL, var = NULL, ech = NULL)
    
    
    observeEvent(input$go, {
      local$dt <- global$data
      
     
      
      #on créée une variable géographique assez liée à notre variable d'intérêt, comme dans la vraie vie (ségragation géographique)
      #on génère un aléa qu'on ajoute ou enlève au revenu disponible : permet de faire passer qq moyens dans les riches, 
      #de faire descendre qq riches dans les pauvres, etc, bref ça brouille légèrement les cartes. 
      set.seed(12345)
      
      myDT <- local$dt %>% 
        mutate(ordre = REV_DISPONIBLE + rnorm(5418,0,25000)) %>% 
        arrange(ordre)
      
      set.seed(NULL)
      
      #puis je les trie sur cette nouvelle var et je découpe en "grappes" de 20 ménages (un peu comme dans l'EEC)
      #on donne un nom/numéro à la grappe (de G001 à G271)
      myDT$grappe20 <- floor(seq(from=1, by=0.05, length.out=5418))
      myDT$grappe20 <- paste0("G",str_pad(myDT$grappe20, 3, pad = "0"))
      
      #idem, mais en grappes de 10 ménages
      myDT$grappe10 <- floor(seq(from=1, by=0.1, length.out=5418))
      myDT$grappe10 <- paste0("G",str_pad(myDT$grappe10, 3, pad = "0"))
      
      #puis en grappes de 5 ménages
      myDT$grappe5 <- floor(seq(from=1, by=0.2, length.out=5418))
      myDT$grappe5 <- paste0("G",str_pad(myDT$grappe5, 3, pad = "0"))
      
      local$dt <- myDT
      
      
      
      
      local$ech <- as.data.frame(tirage_grappe_m2(local$dt, input$UP, input$NomVar1))
      local$var <- input$NomVar2
     
      
      
      
    })
    
    output$plotgrappe <- renderPlot({
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
    
    output$meangrappe <- renderText({
      req(local$dt)
      
      
      local$meangrappe <- mean(local$ech[, local$var], na.rm = TRUE)
      paste0(format_box(local$meangrappe), " \u20ac")
    })
    
    output$sdgrappe <- renderText({
      req(local$dt)
      
      
      local$sdgrappe <- sd(local$ech[, local$var], na.rm = TRUE)
      paste0(format_box(local$sdgrappe), " \u20ac")
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
    
    output$taillech <- renderText({
      req(local$ech)
      
      nrow(local$ech)
    })
    
    
  })
}
## To be copied in the UI
# mod_stat2_sond_grappes_ui("stat2_sond_grappes")
    
## To be copied in the server
# mod_stat2_sond_grappes_server("stat2_sond_grappes")
