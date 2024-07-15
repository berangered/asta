#' stat6_classif_validation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom rsample vfold_cv
#' @importFrom tune control_resamples conf_mat_resampled autoplot fit_resamples collect_predictions
#' @importFrom yardstick metric_set accuracy roc_auc sensitivity specificity roc_curve conf_mat
mod_stat6_classif_validation_ui <- function(id){
  ns <- NS(id)
  
  tabItem("subitem__4",
          h2("Validation du modèle"),
          
          fluidRow(
            
            
            column(3,
                   
                   
                   wellPanel(
              
              
              
              tags$p("Paramètres", 
                     style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
              
              # sliderInput(ns("slide1"),"Validation croisée : nombre de folds",min=5,max=10,step=5,value = 10),
              actionButton(ns("go1"),"Evaluer les performances du modèle")
              
              
              
              ),
              
              wellPanel(
                
                tags$p("Descriptif", 
                       style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                textOutput(ns("txt1"))
                
                
                
              )
              
              
              ),
                   
                   
                   
                   
            column(9,
                   
                   
                   fluidRow(
                     
                     
                     column(6,
                                     
                                     wellPanel(tags$p("Table de confusion", 
                                                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                       plotOutput(ns("plot1"))),
                            
                                     infoBox( 
                                       title = tags$p("Exactitude (accuracy)", style = "font-size : 80%;"),
                                       value = textOutput(ns("accuracy")),
                                       icon = icon("chart-line"),
                                       color="blue",
                                       width = NULL,
                                       subtitle =h6("C'est la part d'individus bien classés par le modèle.")
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
                            
                            wellPanel(tags$p("Courbe ROC (Receiving Operating Chracteristic)", 
                                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       plotOutput(ns("plot2"))
                       
                       
                       ),
                       
                       infoBox(
                         title = tags$p("Aire sous la courbe ROC", style = "font-size : 80%;"),
                         value = textOutput(ns("AUC")),
                         icon = icon("chart-line"),
                         color="blue",
                         width = NULL,
                         subtitle =h6("Cet indicateur permet de tenir compte du seuil choisi pour prédire.")
                       ),
                      
                       infoBox(
                         title = tags$p("Spécificité (ou specificity)", style = "font-size : 80%;"),
                         value = textOutput(ns("spec")),
                         icon = icon("chart-line"),
                         color="blue",
                         width = NULL,
                         subtitle =h6("C'est le taux de vrais négatifs parmi les négatifs réels.")
                       )
                       
                       
                       )
                   
                   
                   
                   )
                   
                   )
            
            
            
            )
          
          
          )
  
  
}
    
#' stat6_classif_validation Server Functions
#'
#' @noRd 
mod_stat6_classif_validation_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    local <- reactiveValues(dt=NULL,
                            folds=NULL,
                            wflow=NULL,
                            metrics=NULL,
                            fit=NULL)
    
    observeEvent(input$go1,{
      # local$dt <- global$dt_train_valid
      local$dt <- global$dt_train
      # local$folds <- vfold_cv(local$dt,
      #                         v = input$slide1)
      # local$wflow <- global$wflow
      local$fit <- global$fit
      # local$metrics <- metric_set(accuracy,roc_auc,sensitivity, specificity)
      # local$fit <- local$wflow %>% fit_resamples(local$folds,
      #                                      metrics = local$metrics,
      #                                      control = control_resamples(save_pred = TRUE))
      # local$pred <- collect_predictions(local$fit)
      local$pred <- augment(local$fit,global$dt_valid) %>% select(target,starts_with(".pred"))
    })
    
    output$plot1 <- renderPlot({
      
      req(local$dt)
      # shinipsum::random_ggplot()
      # conf_mat_resampled(local$fit, tidy = FALSE) %>%
      #   autoplot(type = "heatmap")

      local$pred %>%
        conf_mat(target, .pred_class) %>%
        autoplot(type="heatmap")
      
    })
    
    
    output$plot2 <- renderPlot({
      
      req(local$dt)
      # shinipsum::random_ggplot()
      # local$pred %>% 
      #   roc_curve(truth = target, .data[[names(local$pred)[4]]]) %>% 
      #   autoplot()
      pred1 <- names(local$pred)[3]
      roc_plot_valid <- local$pred %>% 
        roc_curve(truth = target, .data[[pred1]]) %>% 
        autoplot()
      roc_plot_valid
      
    })
    
    output$txt1 <- renderText({
      # shinipsum::random_text(nwords = 100)
      "Les performances des modèles de classification supervisée sont évalu\u00E9es 
      avec des indicateurs calculés à partir de la table de confusion. Dans cette table, tous les individus de la base de validation 
      sont classés en 4 catégories, en comparant leur prédiction à leur vraie valeur. 
      Il y a les vrais positifs (VP) et les vrais négatifs (VN) qui constituent les biens classés. 
      Il y a aussi les faux positifs (FP) et les faux négatifs (FN) qui sont mal classés. 
      La courbe ROC permet de comparer les performances des modèles en fonction des seuils."
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
    
    output$AUC <- renderText({
      
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
# mod_stat6_classif_validation_ui("stat6_classif_validation")
    
## To be copied in the server
# mod_stat6_classif_validation_server("stat6_classif_validation")
