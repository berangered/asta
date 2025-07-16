#' stat4_lineaire_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat4_lineaire_multiple_ui <- function(id){
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
                                   choices = c("Vols de vehicules (pour 1000 hbts)"="Tx_vols_vehicules","Cambriolages (pour 1000 hbts)"="Tx_cambriolages")),
                       
                       selectizeInput(ns("Varexplicative"), 
                                      "Choisissez des variables explicatives",
                                      choices = c("Nb habitants (millions)"="nb_habitants","Part 65 ans ou +"="Part_6599","Taux de chomage"="Tx_chomage","Taux d'emploi"="Tx_emploi","Part de bac+5"="part_bacp5","Part de cadres"="Part_cadres","Part resid. secondaires"="part_resid_secondaires","Region"="GR_REG"),
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
    
#' stat4_lineaire_multiple Server Functions
#'
#' @noRd 
mod_stat4_lineaire_multiple_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    local <- reactiveValues(dt = NULL,var_expliquee = NULL )
    global <- reactiveValues(dt = departements_regr)
    
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
# mod_stat4_lineaire_multiple_ui("stat4_lineaire_multiple_ui_1")
    
## To be copied in the server
# mod_stat4_lineaire_multiple_server("stat4_lineaire_multiple_ui_1")
