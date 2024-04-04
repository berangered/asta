#' stat6_reg_generalisation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat6_reg_generalisation_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    
    
    tabName = "subitem__10",
    h2("Généralisation du modèle final"), 
    
    fluidRow(
      
      
      column(3,
             
             
             wellPanel(
               tags$p("Paramètres",
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               actionButton(ns("go1"), "Ajuster le modèle retenu")
               
             ),
             wellPanel(
               tags$p("Description",
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               textOutput(ns("txt1"))
               
             )
      ),
      
      column(9, 
             
             fluidRow(
               
               
               column(6,
                      
                      wellPanel(tags$p("Résultats du modèle", 
                                       style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                verbatimTextOutput(ns("print1"))),
                      
                      
                      wellPanel(
                        
                        tags$p("Indicateurs d'évaluation", 
                               style = "font-size : 110%; font-weight : bold; text-decoration : underline;")),
                        
                        column(6,infoBox(
                          title = tags$p("RSS (erreur quadratique)", style = "font-size : 80%;"),
                          value = textOutput(ns("rss")),
                          icon = icon("chart-line"),
                          color="blue",
                          width = NULL,
                          subtitle =h6("Ecart entre la vraie valeur et la valeur prédite au carré")
                        ),
                        
                        infoBox(
                          title = tags$p("MSE (erreur quadratique moyenne)", style = "font-size : 80%;"),
                          value = textOutput(ns("mse")),
                          icon = icon("chart-line"),
                          color="blue",
                          width = NULL,
                          subtitle =h6("Ecart quadratique rapporté au nombre d'individus")
                        )),
                        column(6, infoBox(
                          title = tags$p("RMSE (racine de l'erreur quadratique moyenne)", style = "font-size : 80%;"),
                          value = textOutput(ns("rmse")),
                          icon = icon("chart-line"),
                          color="blue",
                          width = NULL,
                          subtitle =h6("Racine carré du MSE")
                        ), 
                        
                        infoBox(
                          title = tags$p("R² (COEFFICIENT DE DETERMINATION)", style = "font-size : 80%;"),
                          value = textOutput(ns("rsq")),
                          icon = icon("chart-line"),
                          color="blue",
                          width = NULL
                        ))
                      
                        
                        
                        
                      
                      
                      
                      
               ),
               
               column(6,
                      
                      wellPanel(tags$p("Prévisions sur la base de test", 
                                       style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                DTOutput(ns("dt1"))
                                
                                
                      )
                      
                     
                      
                      
               )
               
               
               
             )
             
             
      )
      
      
    )
    
    
    
    
    
    
    
  )
  
  
  
}
    
#' stat6_reg_generalisation Server Functions
#'
#' @noRd 
mod_stat6_reg_generalisation_server <- function(id,global){
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
      local$rss <- sum((local$pred[,".pred"] - local$pred[,"target"])^2)
      local$mse <- local$rss/nrow(local$pred)
      local$rmse <- sqrt(local$mse)
      local$rsq <- rsq(local$pred,target,.pred) %>% select(.estimate) %>% as.numeric()
    })
    
    
    
    output$txt1 <- renderText({
      shinipsum::random_text(nwords = 100)
      
      "Le modèle final est généralement celui qui été choisi, en tenant compte des performances observées pendant la phase de validation précédentes. 
      Ce modèle est ajusté sur la totalité de la base d'apprentissage, c'est à dire la base d'entraînement et la base de validation. 
      La base de test n'a donc pas du tout servi pour ajuster le modèle, mais elle est utilisée ici pour faire des prédictions, 
      à partir desquelles on peut observer la performance du modèle. 
      Mais on ne reviendra pas en arrière : le modèle a déjà été choisi à la phase précédente !  
      "
      
      
    })
    
    output$print1 <- renderPrint({
      
      # shinipsum::random_print(type="model")
      
      req(local$dt)
      # 
      # extract_preprocessor(local$fit)
      
      local$fit_final
      
      
    })
    
    
    
    output$rss <- renderText({
      # shinipsum::random_text(nwords = 2)
      req(local$dt)
      
      local$rss %>% round() %>% format(big.mark = " ", scientific = FALSE)
    })
    
    output$mse <- renderText({
      # shinipsum::random_text(nwords = 2)
      req(local$dt)
      
      local$mse %>% round()  %>% format(big.mark = " ", scientific = FALSE)
    })
    
    output$rmse <- renderText({
      # shinipsum::random_text(nwords = 2)
      
      req(local$dt)
      
      local$rmse %>% round(1) %>% format(big.mark = " ", scientific = FALSE, decimal.mark = ",")
      
    })
    
    output$rsq <- renderText({
      # shinipsum::random_text(nwords = 2)
      
      req(local$dt)
      
      local$rsq %>% round(2) %>% format(big.mark = " ", scientific = FALSE, decimal.mark = ",")
    })
    
    
    output$dt1 <- renderDT({
      # shinipsum::random_DT(nrow=10,ncol=4)
      req(local$dt)
      local$pred %>% select(target,starts_with(".pred")) %>% round(1)
    })   
    
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_stat6_reg_generalisation_ui("stat6_reg_generalisation")
    
## To be copied in the server
# mod_stat6_reg_generalisation_server("stat6_reg_generalisation")
