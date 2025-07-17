#' stat4_foot_simple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat4_foot_simple_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabItem(tabName = "reg_lineaire",
            h2("Calcul coefficient de corrélation linéaire"),
            fluidRow(tags$style("background-color : #E3F2FD;"),
              column(4,
                     
                     wellPanel(
                       tags$p("Param\u00e8tres", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       selectInput(ns("Varexpliquee"), 
                                   "Choisissez une première variable",
                                   choices = c("Pourcentage de victoires"="pct_victoires","Nb de matchs joués"="matchs","Nb moyen de buts marqués par match"="but_marques", "Nb moyen de buts encaissés par match"="but_encaiss", "Nb habitants (en milliers)"="pop", "Nb habitants (racine carrée)"="pop_racine", "Nb habitants (logarithme)"="pop_log")),
                       
                       selectInput(ns("Varexplicative"), 
                                   "Choisissez une seconde variable",
                                   choices = c("Pourcentage de victoires"="pct_victoires","Nb de matchs joués"="matchs","Nb moyen de buts marqués par match"="but_marques", "Nb moyen de buts encaissés par match"="but_encaiss", "Nb habitants (en milliers)"="pop", "Nb habitants (racine carrée)"="pop_racine", "Nb habitants (logarithme)"="pop_log")),
                       
                       
                       
                       #checkboxInput(inputId=ns("constante"), "Retirer la constante", value = FALSE, width = NULL),
                       
                       actionButton(inputId=ns("go"),"Calculer")),
                     
                     wellPanel(
                       tags$p("Coeff de corrélation linéaire", style = "font-size : 110%; font-weight : bold; text-decoration : underline;")
                       ,
                       verbatimTextOutput(ns("coeffcorr")),br(),
                       "Coefficient de corrélation linéaire entre les 2 variables quanti. Attention, ce n'est pas la même chose que le R2.",br(),
                       br(),
                       tags$p("Source : CEFIL 2021", style = "font-size : 90%; font-style : italic; text-align : right;"))
              ),
              
              column(8,
                     
                     wellPanel(
                       tags$p("Nuage de points", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       
                       plotOutput(ns("regline")),br(),
                       tags$p("Source : CEFIL 2021", style = "font-size : 90%; font-style : italic; text-align : right;")
                       
                     )#,wellPanel(
                      # tags$p("Résultat du modèle - sortie R", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      # verbatimTextOutput(ns("tab1")),br(),
                      # tags$p("Source : CEFIL 2021", style = "font-size : 90%; font-style : italic; text-align : right;"))
                     
                     
              )
            )
    ))
    
}
    
#' stat4_lineaire_simple Server Functions
#'
#' @noRd 
mod_stat4_foot_simple_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    local <- reactiveValues(dt = NULL, var_explicative = NULL,var_expliquee = NULL )
    global <- reactiveValues(dt = foot)
    
    observeEvent(input$go, {
      #local$dt <- global$data
      local$dt <- global$dt
      local$var_explicative <- input$Varexplicative
      local$var_expliquee <- input$Varexpliquee
      #local$constante <- input$constante
      local$constante <- FALSE
      
      local$model <- model_lineaireS_tab(input_data=local$dt,
                                         var_expliquee = local$var_expliquee ,
                                         var_explicative = local$var_explicative, constante = local$constante)
    })
    
    output$tab1 <- renderPrint({

      validate(need(expr = !is.null(local$dt),
                    message = "Choisissez deux variables dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))

      # browser()
      print(local$model)

    })

    output$regline <- renderPlot(
      {
        validate(
          need(expr = !is.null(local$dt),
               message = "Choisissez deux variables dans le menu d\u00e9roulant et cliquez pour afficher le graphique")
        )
     model_lineaireS_plot(input_data = local$dt, var_expliquee =  local$var_expliquee, var_explicative = local$var_explicative, constante = local$constante)
       
        
      }
      
    )
    
    output$coeffcorr <- renderPrint({
      
      validate(need(expr = !is.null(local$dt),
                    message = "Choisissez deux variables dans le menu d\u00e9roulant et cliquez pour afficher le résultat"))
      
       # browser()
      
      #c <- round(as.numeric(local$model[8]),2)
      
      req(local$dt)
      c <- cor(local$dt[,local$var_expliquee],local$dt[,local$var_explicative])
      #format_box(a)
      round(as.numeric(c),2)
      
    })
    
  })
}
    
## To be copied in the UI
# mod_stat4_foot_simple_ui("stat4_foot_simple_ui_1")
    
## To be copied in the server
# mod_stat4_foot_simple_server("stat4_foot_simple_ui_1")
