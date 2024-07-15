#' stat6_reg_validation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom yardstick rsq 
mod_stat6_reg_validation_ui <- function(id){
  ns <- NS(id)
  
  
  tabItem(
    
    
    tabName = "subitem__9",
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
               
               
             )),
      
      
      column(9,
                       
                       
                       column(6,
                              
                              
                              wellPanel(
                                
                                tags$p("Indicateurs d'évaluation", 
                                       style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                
                                infoBox(
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
                                ),
                                
                                infoBox(
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
                                )
                                
                              )),
             
             
                      column(6,
                             
                             
                             wellPanel(
                               
                               
                               tags$p("Prévisions sur la base de validation", 
                                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                               
                               DTOutput(ns("dt1"))
                               
                               
                             )
                             ))
      
      
      
    )
    
    
    
    
    
    
    
  )
  
  
  
}
    
#' stat6_reg_validation Server Functions
#'
#' @noRd 
mod_stat6_reg_validation_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    local <- reactiveValues(dt=NULL,
                            fit=NULL,
                            pred=NULL)
    
    observeEvent(input$go1,{
      local$dt <- global$dt_train
      local$fit <- global$fit
      local$pred <- augment(local$fit,global$dt_valid) %>% select(target,starts_with(".pred"))
      local$rss <- sum((local$pred[,".pred"] - local$pred[,"target"])^2)
      local$mse <- local$rss/nrow(local$pred)
      local$rmse <- sqrt(local$mse)
      local$rsq <- rsq(local$pred,target,.pred) %>% select(.estimate) %>% as.numeric()
    })
    
    
    
    output$txt1 <- renderText({
      # shinipsum::random_text(nwords = 100)
      "Les performances des modèles de régression supervisée sont évaluées avec des indicateurs qui mesurent globalement les écarts entre la valeur à prédire (target) et la prédiction (.pred) effectuée sur la base de validation."
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
# mod_stat6_reg_validation_ui("stat6_reg_validation")
    
## To be copied in the server
# mod_stat6_reg_validation_server("stat6_reg_validation")
