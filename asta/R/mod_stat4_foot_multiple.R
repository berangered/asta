#' stat4_foot_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat4_foot_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabItem(tabName = "reg_multiple",
            h2("R\u00e9gression lin\u00e9aire multiple"),
            fluidRow(
              column(4,
                     
                     wellPanel(
                       tags$p("Param\u00e8tres", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       selectInput(ns("Varexpliquee"), 
                                   "Choisissez une variable \u00e0 expliquer",
                                   choices = c("Pourcentage de victoires"="pct_victoires")),
                       
                       selectizeInput(ns("Varexplicative"), 
                                      "Choisissez des variables explicatives",
                                      choices = c("Nb de matchs joués"="matchs","Nb moyen de buts marqués par match"="but_marques", "Nb moyen de buts encaissés par match"="but_encaiss", "Nb habitants (en milliers)"="pop", "Nb habitants (racine carrée)"="pop_racine", "Nb habitants (logarithme)"="pop_log","Continent d'appartenance"="continent"),
                                      multiple = TRUE  ),
                       
                       #checkboxInput(inputId=ns("constante"), "Retirer la constante", value = FALSE, width = NULL),
                       actionButton(inputId=ns("go"),"Mettre \u00e0 jour")),
                     
                     
                     wellPanel(
                       tags$p("Distribution des r\u00e9sidus", style = "font-size : 110%; font-weight : bold; text-decoration : underline;")
                       ,
                       #verbatimTextOutput(ns("coeffcorr")),br(),
                       plotOutput(ns("residus")),br(),
                       "Pour vérifier la normalité des résidus.",br(),
                       br(),
                       tags$p("Source : CEFIL 2021", style = "font-size : 90%; font-style : italic; text-align : right;")) 
              ),
              
              column(8,
                     wellPanel(
                       tags$p("Tableau statistique", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       verbatimTextOutput(ns("tab1")),br(),
                       tags$p("Source : CEFIL 2021", style = "font-size : 90%; font-style : italic; text-align : right;"))
                    
                      
              )
            )
    ))
  
}
    
#' stat4_foot_multiple Server Functions
#'
#' @noRd 
mod_stat4_foot_multiple_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    local <- reactiveValues(dt = NULL,var_expliquee = NULL )
    global <- reactiveValues(dt = foot)
    
    observeEvent(input$go, {
      local$dt <- global$dt
      local$constante <- FALSE #input$constante
      local$explicative <- input$Varexplicative
      
     # local$constante <- input$constante
 
      
    })
    
    output$tab1 <- renderPrint({
      
      validate(need(expr = !is.null(local$explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
    # browser()
      model <- model_lineaireM_tab(input_data = local$dt,
                                         var_expliquee = input$Varexpliquee,
                                         var_explicatives = local$explicative,
                                         constante = local$constante     )
      print(model)
    })
    
    output$coeffcorr <- renderPrint({
      
      validate(need(expr = !is.null(local$explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
      
      # browser()
      
      model <- model_lineaireM_tab(input_data = local$dt,
                                   var_expliquee = input$Varexpliquee,
                                   var_explicatives = local$explicative,
                                   constante = local$constante     )
      
      
      c <- round(as.numeric(model[9]),2)
      c
    })

    
    output$residus <- renderPlot({
      
      validate(need(expr = !is.null(local$explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
      
      # browser()
      
      resid <- model_lineaireM_resid(input_data = local$dt,
                                   var_expliquee = input$Varexpliquee,
                                   var_explicatives = local$explicative,
                                   constante = local$constante     )
      
      
      
      resid
    })
    
  })
}
    
## To be copied in the UI
# mod_stat4_foot_multiple_ui("stat4_foot_multiple_ui_1")
    
## To be copied in the server
# mod_stat4_foot_multiple_server("stat4_foot_multiple_ui_1")
