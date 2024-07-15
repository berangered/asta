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
                                  choices = c("Régression Linéaire","Arbre","Forêt aléatoire","SVM","KNN")),
                      actionButton(ns("go1"),"Ajustement du modèle")
                      
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
      }else if(input$select1 == "SVM"){
        local$mod <- 
          svm_linear() %>%
          set_mode("regression") %>%
          set_engine("LiblineaR")
        local$descriptif <- "Un SVM (Support Vector Machine) en régression supervisée cherche à minimiser l'erreur de prédiction 
        en ajustant un hyperplan de manière à ce qu'il maximise la marge entre les données d'entraînement 
        et la fonction de régression. 
        La marge représente la distance entre les points les plus proches de l'hyperplan, appelés vecteurs de support."
      }else if(input$select1 == "KNN"){
        local$mod <- 
          nearest_neighbor() %>%
          set_mode("regression") %>%
          set_engine("kknn")
        local$descriptif <- "L'algorithme des k plus proches voisins consiste à prédire l'étiquette d'un individu 
        en faisant la moyenne parmi les étiquettes des k plus proches voisins. 
        La valeur de la prédiction va bien entendu dépendre du choix de K. 
        Si K est trop grand, il aura tendance à toujours prédire le groupe majoritaire. 
        Si K est trop petit, il y a un risque de sur-apprentissage."
      }
      
      
      
      local$wflow <- workflow() %>%
        add_model(local$mod) %>%
        add_recipe(local$rec)
      
      global$wflow <- local$wflow
      
      local$fit <- local$wflow %>% fit(local$dt)
      global$fit <- local$fit
      
      local$pred <- augment(local$fit,local$dt)
      global$pred <- local$pred
      
    }
    )
    
    
    
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
