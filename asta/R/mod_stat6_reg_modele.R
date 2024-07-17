#' stat6_reg_modele UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom parsnip linear_reg svm_linear
#' @importFrom workflows extract_preprocessor
#' @importFrom LiblineaR LiblineaR
mod_stat6_reg_modele_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    
    
    tabName = "subitem__8",
    h2("Choix du modèle"),
    
    fluidRow(column(3,
                    
                    wellPanel(
                      
                      tags$p("Paramètres", 
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      selectInput(ns("select1"),
                                  "Choissisez un algorithme :",
                                  choices = c("Régression Linéaire","Arbre","Forêt aléatoire","SVM")),
                      actionButton(ns("go1"),"Ajustement du modèle")
                      
                    ),
                    
                    wellPanel(
                      
                      tags$p("Hyper-paramètres",
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      selectInput(ns("select2"),
                                  "Choisissez un paramètre :",
                                  choices = NULL),
                      sliderInput(ns("slide1"),label = textOutput(ns("text2")),min = 1,max = 30,step = 1,value = 5),
                      actionButton(ns("go2"),"Mettre à jour le modèle")
                      
                      
                    ),
                    
                    wellPanel(
                      
                      tags$p("Descriptif du modèle", 
                             style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                      textOutput(ns("txt1"))
                      
                      
                    )
                    
                    
                    
                    
                    ),
             column(9,
                    column(6,
                           
                           wellPanel(
                             
                             
                             
                             tags$p("Modèle entraîné sur la base d'entraînement", 
                                    style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                             verbatimTextOutput(ns("print1"))
                             
                             
                             
                             
                           )
                           
                           
                           ),
                    column(6,
                           
                           wellPanel(
                             
                             
                             tags$p("Estimations sur la base d'entraînement", 
                                    style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                             DTOutput(ns("dt1"))
                             
                             
                           )
                           
                           
                           )))
    
    
    
    
    
    
    
  )
  
  
}
    
#' stat6_reg_modele Server Functions
#'
#' @noRd 
mod_stat6_reg_modele_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    local <- reactiveValues(dt = NULL,
                            rec=NULL,
                            mod = NULL,
                            wflow = NULL,
                            fit = NULL,
                            pred = NULL,
                            descriptif = NULL)
    
    
    observeEvent(input$go1,{
      
      local$dt <- global$dt_train
      local$rec <- global$rec
      
      if(input$select1 == "Régression Linéaire"){
        local$mod <- linear_reg() %>% 
          set_engine("lm") 
        local$descriptif <- "La regression linéaire a pour objectif d'expliquer 
        et de prédire les valeurs d'une variable quantitative 
        à partir de variables qualitatives et quantitatives."
      }else if(input$select1 == "Arbre"){
        local$mod <- decision_tree(
          # cost_complexity = 0.001,
          # tree_depth = 7,
          # min_n = NULL
        ) %>%
          set_engine("rpart") %>%
          set_mode("regression")
        
        local$descriptif <- "Les arbres de regression sont des outils 
        d'exploration des données et d'aide 
        à la décision qui permettent d'expliquer 
        et de prédire une variable quantitative à partir de variables explicatives 
        quantitatives et/ou qualitatives. Dans cet exemple, les arbres sont construits avec 
        l'algorithme CART qui permet d'obtenir des classes d'individus le 
        plus homogène possible, selon la variable d'intérêt (target)."
        
        local$hyper <- c("Complexité","Profondeur max","Nombre d'individus min")
        
        
      }else if(input$select1 == "Forêt aléatoire"){
        local$mod <- 
          rand_forest(
            # trees = 1000,
            #           mtry = 3,
            #           min_n = NULL
          ) %>% 
          set_engine("ranger") %>% 
          set_mode("regression")
        local$descriptif <- "Les forêts aléatoires en regression supervisée permettent de prédire une variable quantitative
        à partir de variables quantitatives et/ou qualitatives. 
        Cette famille d'algorithme permet de pallier le manque de stabilité des arbres. 
        Elle consiste à agréger un grand nombre d'arbres (500 par défaut), 
        construits sur des échantillons bootstrap. Pour la construction des arbres, 
        à chaque noeud, seul un sous-ensemble de variables (3 par défaut) est sélectionné aléatoirement 
        pour choisir la coupure. "
        
        local$hyper <- c("Nombre d'arbres","Nombre de variables","Nombre d'individus min")
        
      }else if(input$select1 == "SVM"){
        local$mod <- 
          svm_linear() %>%
          set_mode("regression") %>%
          set_engine("LiblineaR")
        local$descriptif <- "Un SVM (Support Vector Machine) en régression supervisée cherche à minimiser l'erreur de prédiction 
        en ajustant un hyperplan de manière à ce qu'il maximise la marge entre les données d'entraînement 
        et la fonction de régression. 
        La marge représente la distance entre les points les plus proches de l'hyperplan, appelés vecteurs de support."
      }
      
      
      
      local$wflow <- workflow() %>%
        add_model(local$mod) %>%
        add_recipe(local$rec)
      
      global$wflow <- local$wflow
      
      local$fit <- local$wflow %>% fit(local$dt)
      global$fit <- local$fit
      
      local$pred <- augment(local$fit,local$dt)
      global$pred <- local$pred
      
      
      updateSelectInput(session = session,inputId = "select2",choices = local$hyper)
      
    }
    )
    
    observeEvent(input$select2,{
      
      if(input$select2 == "Profondeur max"){
        
        local$min <- 1
        local$max <- 30
        local$step <- 1
        local$value <- 5
        
      }else if(input$select2 == "Nombre d'individus min"){
        
        local$min <- 2
        local$max <- nrow(local$dt)
        local$step <- 1
        local$value <- 100
        
      }else if(input$select2 == "Complexité"){
        
        local$min <- 0
        local$max <- 0.1
        local$step <- 0.001
        local$value <- 0.01
        
      # }else if(input$select2 == "Nombre de voisins"){
      # 
      #   local$min <- 1
      #   local$max <- 100
      #   local$step <- 1
      #   local$value <- 5
        
      }else if(input$select2 == "Nombre d'arbres"){
        
        local$min <- 0
        local$max <- 5000
        local$step <- 50
        local$value <- 500
        
      }else if(input$select2 == "Nombre de variables"){
        
        local$min <- 1
        local$max <- length(local$dt)
        local$step <- 1
        local$value <- 3
        
      }
      
      updateSliderInput(session = session,inputId = "slide1",min = local$min)
      updateSliderInput(session = session,inputId = "slide1",max = local$max)
      updateSliderInput(session = session,inputId = "slide1",step = local$step)
      updateSliderInput(session = session,inputId = "slide1",value = local$value)
      
      
    })
    
    
    observeEvent(input$go2,{
      
      if(input$select1 == "Régression Logistique"){
        local$mod <- logistic_reg() %>% 
          set_engine("glm")
        
      }else if(input$select2 == "Profondeur max"){
        local$mod <- decision_tree(
          # cost_complexity = 0.001,
          tree_depth = input$slide1,
          # min_n = input$slide1
        ) %>%
          set_engine("rpart") %>%
          set_mode("regression")
        
      }else if(input$select1 == "Arbre" & input$select2 == "Nombre d'individus min"){
        local$mod <- decision_tree(
          # cost_complexity = 0.001,
          # tree_depth = input$slide1,
          min_n = input$slide1
        ) %>%
          set_engine("rpart") %>%
          set_mode("regression")
        
      }else if(input$select2 == "Complexité"){
        local$mod <- decision_tree(
          cost_complexity = input$slide1,
          # tree_depth = input$slide1,
          # min_n = input$slide1
        ) %>%
          set_engine("rpart") %>%
          set_mode("regression")
        
      # } else if(input$select2 == "Nombre de voisins"){
      #   local$mod <- nearest_neighbor(
      #     neighbors = input$slide1
      #   ) %>% 
      #     set_engine("kknn") %>% 
      #     set_mode("classification")
        
      }else if(input$select2 == "Nombre d'arbres"){
        local$mod <- 
          rand_forest(
            trees = input$slide1,
            #           mtry = 3,
            #           min_n = NULL
          ) %>% 
          set_engine("ranger") %>% 
          set_mode("regression")
        
      }else if(input$select2 == "Nombre de variables"){
        local$mod <- 
          rand_forest(
            # trees = input$slide1,
            mtry = input$slide1,
            #           min_n = NULL
          ) %>% 
          set_engine("ranger") %>% 
          set_mode("regression")
        
      }else if(input$select1 == "Forêt aléatoire" & input$select2 == "Nombre d'individus min"){
        local$mod <- 
          rand_forest(
            # trees = input$slide1,
            # mtry = input$slide1,
            min_n = input$slide1
          ) %>% 
          set_engine("ranger") %>% 
          set_mode("regression")
        
      }
      local$wflow <- workflow() %>%
        add_model(local$mod) %>%
        add_recipe(local$rec)
      global$wflow <- local$wflow
      local$fit <- local$wflow %>% fit(local$dt)
      global$fit <- local$fit
      local$pred <- augment(local$fit,local$dt)
      global$pred <- local$pred
      
      
      
    })
    
    
    
    output$print1 <- renderPrint({
      
      # shinipsum::random_print(type = "model")
      
      req(local$dt)
      # 
      # extract_preprocessor(local$fit)
      
  local$fit
      
    })
    
    
    output$dt1 <- renderDT({
      # shinipsum::random_DT(nrow = 10,ncol = 4)
      req(local$dt)
      local$pred %>% select(target,starts_with(".pred")) %>% round(1)
      
    })
    
    output$txt1 <- renderText({
      
      req(local$dt)
      local$descriptif
      
      # shinipsum::random_text(nwords = 100)
    })
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_stat6_reg_modele_ui("stat6_reg_modele")
    
## To be copied in the server
# mod_stat6_reg_modele_server("stat6_reg_modele")
