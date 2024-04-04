#' stat6_reg_preparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom recipes step_impute_mean step_rm
mod_stat6_reg_preparation_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    
    
    tabName = "subitem__7",
    h2("Préparation et transformation de la base (Preprocessing)"),
    
    fluidRow(
      
      
      column(3,
             
             wellPanel(
               
               tags$p("Paramètres", 
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               
               sliderInput(ns("slide1"),
                           label = "Quel part de la base voulez-vous conserver pour l'entraînement  ?",
                           min = 60,
                           max = 90,
                           value = 80,
                           step = 5),
               
               actionButton(ns("go1"),label = "Partitionner et explorer la base d'entraînement"),
               
               h5("Le reste de la base se répartit équitablement entre la base de validation et la base de test")
               
               
             ),
             
             
             wellPanel(
               
               tags$p("Paramètres", 
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               
               
               selectInput(ns("select1"),
                           label = "Quelle recette voulez-vous appliquer ?",
                           choices = c("Centrer-réduire","Imputation avec moyenne"),
                           multiple = FALSE
                           ),
               # selectInput(ns("select2"),
               #             label = "Sur quelles variables ?",
               #             choices = c("VAR1","VAR2","VAR3"),
               #             multiple = TRUE
               # ),
               actionButton(ns("go2"),label = "Cliquer pour appliquer une recette")
               
               
               
             )
             
             
             ),
      
      
      column(9,
             
             
             column(8,
                    
                    
                    wellPanel(
                      
                      tags$p("Exploration de la base d'entraînement", 
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      
                      verbatimTextOutput(ns("print1"))) 
                    
                    
                    ),
             column(4,
                    
                    
                    wellPanel(
                      
                      tags$p("Partitionnement de la base brute", 
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      
                      infoBox(
                        title = tags$p("Base d'entraînement", style = "font-size : 80%;"),
                        value = textOutput(ns("entrainement")),
                        icon = icon("chart-line"),
                        color="blue",
                        width = NULL
                      ),
                      
                      infoBox(
                        title = tags$p("Base de validation", style = "font-size : 80%;"),
                        value = textOutput(ns("validation")),
                        icon = icon("chart-line"),
                        color="blue",
                        width = NULL
                      ),
                      
                      infoBox(
                        title = tags$p("Base de test", style = "font-size : 80%;"),
                        value = textOutput(ns("test")),
                        icon = icon("chart-line"),
                        color="blue",
                        width = NULL
                      )
                      ) ,
                    
                    wellPanel(
                      tags$p("Recettes appliquées sur la base", 
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      
                      
                      textOutput(ns("text1"))
                    )
                    
                    
                    )
             
             
             
             )
      
      
      
      
    )
    
    
    
    
    
    
    
  )
  
  
}
    
#' stat6_reg_preparation Server Functions
#'
#' @noRd 
mod_stat6_reg_preparation_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
    local <- reactiveValues(dt = NULL,
                            dt_split =NULL,
                            dt_train = NULL,
                            dt_valid = NULL,
                            dt_test = NULL,
                            part_train = NULL,
                            part_valid = NULL,
                            part_test = NULL,
                            dt_train_valid = NULL,
                            rec = NULL,
                            dt_train_rec = NULL)
    
    
    observeEvent(input$go1,{
      
      local$dt <- global$dt
      set.seed(1234)
      local$dt_split <- initial_validation_split(local$dt,
                                                 prop = c(input$slide1/100,(1-input$slide1/100)/2))
      local$part_train <- input$slide1
      local$part_valid <- (100-input$slide1)/2
      local$part_test <- local$part_valid
      local$dt_train <- training(local$dt_split)
      global$dt_train <- local$dt_train
      local$dt_valid <- validation(local$dt_split)
      global$dt_valid <- local$dt_valid
      local$dt_train_valid <- local$dt_train %>% bind_rows(local$dt_valid)
      global$dt_train_valid <- local$dt_train_valid
      local$dt_test <- testing(local$dt_split)
      global$dt_test <- local$dt_test
      local$rec <- recipe(target~ .,data=local$dt_train)
      global$rec <- local$rec
      local$dt_train_rec <- bake(prep(local$rec),new_data = NULL)
      local$rec_chr <- "Aucune transformation"
    })
    
    
    
    
    observeEvent(input$go2,{
      
      
      if (input$select1 == "Centrer-réduire"){
        
        local$rec <- local$rec %>% step_normalize(all_numeric_predictors())
        
      }else if(input$select1 == "Imputation avec moyenne"){
        
        local$rec <- local$rec %>%  step_impute_mean(all_numeric_predictors())
        
      }
      
      # else if(input$select1 == "Retirer des variables"){
      #   
      #   local$rec <- local$rec %>%  step_rm(all_numeric_predictors())
      # }
      
      local$dt_train_rec <- bake(prep(local$rec),new_data = NULL)
      global$rec <- local$rec
      local$rec_chr <- local$rec$steps %>% purrr::map_chr(~ attributes(.x)$class[1])
      
      
    })
    
    
    
    
    output$print1 <- renderPrint({
      
      # shinipsum::random_print(type = "table")
      
      req(local$dt)

      skim(local$dt_train_rec)
      
    })
    
    output$text1 <- renderPrint({
      
      # shinipsum::random_print(type = "table")
      
      req(local$dt)
      
      # skim(local$dt_train_rec)
      # local$rec$steps %>% purrr::map_chr(~ attributes(.x)$class[1])
      local$rec_chr
    })
    
    output$entrainement <- renderText({
      # shinipsum::random_text(nwords = 2)
      req(local$dt)
      paste(format(local$part_train,digits=1),"%")
      #   format_box()
    })
    
    output$validation <- renderText({
      # shinipsum::random_text(nwords = 2)
      req(local$dt)
      paste(format(local$part_valid,digits=1),"%")
    })
    
    output$test <- renderText({
      # shinipsum::random_text(nwords = 2)
      req(local$dt)
      paste(format(local$part_test,digits=1),"%")
    })
    
 
  })
}
    
## To be copied in the UI
# mod_stat6_reg_preparation_ui("stat6_reg_preparation")
    
## To be copied in the server
# mod_stat6_reg_preparation_server("stat6_reg_preparation")
