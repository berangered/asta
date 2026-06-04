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
#' @importFrom tidyr  uncount

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
                     
                     wellPanel(span("Coefficients du modèle :", style="color:blue"), 
                               "Les coefficients du modèle (Estimate) mesurent l'effet des variables explicatives sur le modèle.
                               On peut ainsi isoler l'effet de chaque modalité sur la variable expliquée. On peut ainsi produire des analyses toutes choses égales par ailleurs."),
                     
                     wellPanel(span("Les Odds-Ratios :", style="color:blue"), 
                               "Assimilés au risque relatif, ils correspondent à l'exponentielle des coefficients. Ils indiquent par combien la probabilité de survie est multipliée selon la modalité prise en variable explicative."),
                     wellPanel(span("La matrice de confusion :", style="color:blue"), 
                               "Elle permet de mesurer la qualité du modèle. Elle croise, en ligne, la survie au naufrage du titanic (no/yes), avec, en colonne, la prédiction de survie à partir du modèle (0-non/1-oui)."),
                               
                               
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
                     wellPanel(
                       tags$p("Matrice de confusion", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       verbatimTextOutput(ns("tab3")),br(),
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
 
      local$model <- model_logistique_tab(input_data=global$dt,
                                          var_expliquee = local$var_expliquee ,
                                          var_explicatives = local$var_explicative)
      
      
     })
    
    output$tab1 <- renderPrint({
      
      validate(need(expr = !is.null(local$var_explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
      
      print(summary(local$model))
      })

    output$tab2 <- renderPrint({

      validate(need(expr = !is.null(local$var_explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))

      print(questionr::odds.ratio(local$model))
      })
    
    output$tab3 <- renderPrint({
      
      validate(need(expr = !is.null(local$var_explicative),
                    message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
      
      #on passe aussi la table titanic en données individuelles pour calculer les probas et les valeurs prédites, pour la matrice de confusion
      titanic_indiv <- tidyr::uncount(local$dt, weights = Freq)
      a <- paste0(local$var_expliquee, " ~", paste0(local$var_explicative, collapse = "+"))
      logit <- glm(data = titanic_indiv, as.formula(a), family = binomial(link = "logit"))
      titanic_indiv$prob <- logit$fitted.values
      titanic_indiv  <- titanic_indiv %>% mutate(pred = case_when(
        prob >=0.5 ~ "1-oui",
        T ~ "0-non"
      ))
      local$confusion <- table(titanic_indiv$Survived,titanic_indiv$pred)
      
      print(local$confusion)
    })
    
   #  output$plot1 <- renderPlot({
   #    
   #    validate(need(expr = !is.null(local$var_explicative),
   #                  message = "Choisissez une variable dans le menu d\u00e9roulant et cliquez pour afficher le tableau"))
   # 
   #    local$modelSS <- model_logistique_tab(input_data=global$dt,
   #                                            var_expliquee = local$var_expliquee ,
   #                                            var_explicatives = local$var_explicative)
   #   
   # #   
   # # browser()
   #     GGally::ggcoef_model(local$modelSS, exponentiate = TRUE)
   #  })
    
  })
}
    
## To be copied in the UI
# mod_stat4_logistique_ui("stat4_logistique_ui_1")
    
## To be copied in the server
# mod_stat4_logistique_server("stat4_logistique_ui_1")
