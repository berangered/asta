#' stat6_classif_generalisation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat6_classif_generalisation_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    "subitem__5",
    h2("Généralisation du modèle final"),
    
    fluidRow(column(3,
                    
                    
                    wellPanel(
                      tags$p("Paramètres",
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      actionButton(ns("go1"), "Ajuster le modèle retenu")
                      
                    ),
                    wellPanel(
                      tags$p("Description",
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      textOutput(ns("text1"))
                      
                    )
                    ),
             
             column(9, 
                    
                    fluidRow(
                      
                      
                      column(6,
                             
                             wellPanel(tags$p("Résultats du modèle", 
                                              style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                       verbatimTextOutput(ns("print1"))),
                             infoBox(
                               title = tags$p("Exactitude (accuracy)", style = "font-size : 80%;"),
                               value = textOutput(ns("accuracy")),
                               icon = icon("chart-line"),
                               color="blue",
                               width = NULL,
                               subtitle =h6("C'est la part d'individus bien classés par le modèle.")
                             ),
                             infoBox(
                               title = tags$p("Spécificité (ou specificity)", style = "font-size : 80%;"),
                               value = textOutput(ns("spec")),
                               icon = icon("chart-line"),
                               color="blue",
                               width = NULL,
                               subtitle =h6("C'est le taux de vrais négatifs parmi les négatifs réels.")
                             ),
                             infoBox(
                               title = tags$p("Sensibilité (ou sensitivity)", style = "font-size : 80%;"),
                               value = textOutput(ns("sens")),
                               icon = icon("chart-line"),
                               color="blue",
                               width = NULL,
                               subtitle =h6("C'est le taux de vrais positifs parmi les positifs réels.")
                             )
                             
                             
                      ),
                      
                      column(6,
                             
                             wellPanel(tags$p("Prévisions sur la base de test", 
                                              style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                       DTOutput(ns("dt1"))
                                       
                                       
                             ),
                             
                             infoBox(
                               title = tags$p("Aire sous la courbe ROC", style = "font-size : 80%;"),
                               value = textOutput(ns("auc")),
                               icon = icon("chart-line"),
                               color="blue",
                               width = NULL,
                               subtitle =h6("Cet indicateur permet de tenir compte du seuil choisi pour prédire.")
                             )
                             
                             
                      )
                      
                      
                      
                    )
                    
                    
                    ))
    
    
    
    
    
    
    
  )
  
  
}
    
#' stat6_classif_generalisation Server Functions
#'
#' @noRd 
mod_stat6_classif_generalisation_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    local <- reactiveValues(dt=NULL,
                            wflow=NULL,
                            fit_final=NULL,
                            pred=NULL)
    
    observeEvent(input$go1,{
      local$dt <- global$dt_train_valid
      local$wflow <- global$wflow
      local$fit_final <- local$wflow %>% fit(local$dt)
      # local$metrics <- metric_set(accuracy,roc_auc,sensitivity, specificity)
      # local$fit <- local$wflow %>% fit_resamples(local$folds,
      #                                      metrics = local$metrics,
      #                                      control = control_resamples(save_pred = TRUE))
      # local$pred <- collect_predictions(local$fit)
      local$pred <- augment(local$fit_final,global$dt_test) %>% select(target,starts_with(".pred"))
    })
    
    output$print1 <- renderPrint({
      
      req(local$dt)

    local$fit_final
      
      # shinipsum::random_print(type = "model")
      
    })
    
    
    output$dt1 <- renderDT({
      
      # shinipsum::random_DT(nrow = 5,ncol = 5)
      req(local$dt)
      local$pred
    })
    
    output$text1 <- renderText({
      # shinipsum::random_text(nwords = 100)
      "Le modèle final est généralement celui qui a les meilleures performances en fonction de l'objectif qu'on lui a fixé. 
      Ce modèle est ensuite ajusté sur la totalité de la base d'apprentissage, c'est à dire la base d'entraînement et la base de validation. 
      La base de test n'a donc pas du tout servi pour ajuster le modèle, mais elle est utilisée ici pour faire des prédictions, 
      à partir desquelles on peut observer la performance du modèle. 
      Mais on ne reviendra pas en arrière : le modèle a déjà été choisi à la phase précédente !"
    })
    
    
    output$accuracy <- renderText({
      
      req(local$dt)
      
      # shinipsum::random_text(nwords = 2)
      
      local$pred %>%
        accuracy(truth = target, .pred_class) %>%
        select(.estimate) %>%
        as.numeric() %>%
        format_box()
    })
    
    output$spec <- renderText({
      
      req(local$dt)
      
      # shinipsum::random_text(nwords = 2)
      
      local$pred %>%
        specificity(truth = target, .pred_class) %>%
        select(.estimate) %>%
        as.numeric() %>%
        format_box()
    })
    
    output$sens <- renderText({
      
      req(local$dt)
      
      # shinipsum::random_text(nwords = 2)
      local$pred %>%
        sensitivity(truth = target, .pred_class) %>%
        select(.estimate) %>% as.numeric() %>%
        format_box()
    })
    
    output$auc <- renderText({
      
      req(local$dt)
      
      # shinipsum::random_text(nwords = 2)
      
      local$pred %>%
        roc_auc(truth = target, .data[[names(local$pred)[3]]]) %>%
        select(.estimate) %>% as.numeric() %>%
        format_box()
    })
    
    
    
 
  })
}
    
## To be copied in the UI
# mod_stat6_classif_generalisation_ui("stat6_classif_generalisation")
    
## To be copied in the server
# mod_stat6_classif_generalisation_server("stat6_classif_generalisation")
