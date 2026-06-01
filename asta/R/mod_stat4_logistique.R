#' stat4_logistique UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom GGally  ggcoef
#' @importFrom questionr  odds.ratio
mod_stat4_logistique_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    
    tabItem(tabName = "reg_logistique",
            h2("R\u00e9gression logistique"),
            fluidRow(
              column(4,
                     
                     wellPanel(
                       tags$p("Param\u00e8tres", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       selectInput(ns("Varexpliquee"), 
                                   "Choisissez une variable \u00e0 expliquer",
                                   choices = c("Survie"="Survived")),
                       
                       selectizeInput(ns("Varexplicative"), 
                                      "Choisissez des variables explicatives",
                                      choices = c("Classe"="Class", "Sexe"="Sex", "Age"),
                                      multiple = TRUE ),
                       
                       actionButton(inputId=ns("go"),"Mettre \u00e0 jour")),
                     
                     wellPanel(span("Critère AIC  :", style="color:blue"), 
                               "Le meilleur modèle est celui possédant l’AIC le plus
faible. Le critère d'information d'Akaike est une mesure de la qualité d'un modèle statistique.
Ce critère propose un compromis entre qualité d'ajustement (fonction de maximum de vraissemblance)
                               et complexité du modèle (nombre de paramètres à estimer"),
                     
                     wellPanel(span("Coefficients du modèle  :", style="color:blue"), 
                               "Les coefficients du modèle (estimate) mesurent l'effet des variables explicatives sur le modèle.
                               On peut ainsi isoler l'effet de chaque modalité sur la variable expliquée. On peut ainsi produire des analyses de type 'toutes choses égales par ailleurs'..."),
                     wellPanel(span("Les Odds-Ratios :", style="color:blue"), 
                               "Assimilés au risque relatif, ils correspondent à l'exponentielle des coefficients. Ils indiquent par combien le risque d'avoir Y=1 est multiplié lorsqu'on a cette modalité de X"),
                               
                               
              ),
              
              column(8,
                     wellPanel(
                       tags$p("Tableau statistique", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       verbatimTextOutput(ns("tab1")),br(),
                       tags$p("Source : CEFIL 2021", style = "font-size : 90%; font-style : italic; text-align : right;")),
                     wellPanel(
                       tags$p("Odds-Ratios", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       verbatimTextOutput(ns("tab2")),br(),
                       tags$p("Source : CEFIL 2021", style = "font-size : 90%; font-style : italic; text-align : right;")),
                     
                     
                     
              )
            )
    ))
  
}
    
#' stat4_logistique Server Functions
#'
#' @noRd 
mod_stat4_logistique_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    local <- reactiveValues(dt = NULL, var_explicative = NULL ,var_expliquee = NULL )
    global <- reactiveValues(dt = titanic)
    
    observeEvent(input$go, {
       
    
      local$dt <- global$dt
      local$var_explicative <- input$Varexplicative
      local$var_expliquee <- input$Varexpliquee
 

      
      
      
      })
    
    output$tab1 <- renderPrint({
      
      validate(need(expr = !is.null(local$var_explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
      local$model <- model_logistique_tab(input_data=global$dt,
                                          var_expliquee = local$var_expliquee ,
                                          var_explicatives = local$var_explicative)
       # browser()
      print(summary(local$model))
      
    })

    output$tab2 <- renderPrint({
      
      validate(need(expr = !is.null(local$var_explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
      local$model <- model_logistique_tab(input_data=global$dt,
                                          var_expliquee = local$var_expliquee ,
                                          var_explicatives = local$var_explicative)
       # browser()
      print(questionr::odds.ratio(local$model))
      
    })
    
    output$plot1 <- renderPlot({
      
      validate(need(expr = !is.null(local$var_explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
   
      local$modelSS <- model_logistique_tab(input_data=global$dt,
                                              var_expliquee = local$var_expliquee ,
                                              var_explicatives = local$var_explicative)
     
   #   
   # browser()
       GGally::ggcoef_model(local$modelSS, exponentiate = TRUE)
    })
    
  })
}
    
## To be copied in the UI
# mod_stat4_logistique_ui("stat4_logistique_ui_1")
    
## To be copied in the server
# mod_stat4_logistique_server("stat4_logistique_ui_1")
