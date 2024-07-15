#' stat6_classif_modele UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom parsnip set_engine augment logistic_reg fit decision_tree set_mode nearest_neighbor rand_forest
#' @importFrom workflows workflow add_model add_recipe extract_fit_parsnip
#' @import kknn
mod_stat6_classif_modele_ui <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = "subitem__3",
          
          h2("Les modèles/algorithmes"),
          
          fluidRow(column(3,
                          
                          wellPanel(
                            
                            tags$p("Paramètres", 
                                   style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                            selectInput(ns("select1"),
                                        "Choissisez un algorithme :",
                                        choices = c("Régression Logistique","Arbre","Forêt aléatoire","KNN")),
                            actionButton(ns("go1"),"Ajustement du modèle")
                          
                          ),
                          
                          wellPanel(
                            
                            tags$p("Descriptif du modèle", 
                                           style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                            textOutput(ns("text1"))
                            
                            
                            )
                          
                          
                          # wellPanel(
                          #   
                          #   tags$p("Hyper-paramètres", 
                          #          style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                          #   selectInput(ns("select2"),
                          #               "Quel hyper-paramètres voulez-vous modifier ? :",
                          #               choices = NULL),
                          #   sliderInput(ns("slide1"),label = textOutput(ns("text2")),min = 1,max = 10,step = 1,value = 5),
                          #   actionButton(ns("go2"),"Mettre à jour le modèle")
                          #   
                          #   
                          #   )
                          
                          ),column(9,
                                   
                                   fluidRow(column(6,wellPanel( tags$p("Modèle entraîné sur la base d'entraînement", 
                                                                       style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                                                verbatimTextOutput(ns("print1"))
                                                                
                                                                
                                   )),column(6,wellPanel(tags$p("Estimations sur la base d'entraînement", 
                                                                style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                                         DTOutput(ns("dt1")))))
                                   
                                   
                                   
                                  
                                   
                                   ))
    
    
    
  )
  
  
}
    
#' stat6_classif_modele Server Functions
#'
#' @noRd 
mod_stat6_classif_modele_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    local <- reactiveValues(dt = NULL,
                            rec=NULL,
                            mod = NULL,
                            wflow = NULL,
                            fit = NULL,
                            pred = NULL,
                            descriptif = NULL)
    
    # local$rec <- global$rec
    
    
    observeEvent(input$go1,{
      local$dt <- global$dt_train
      local$rec <- global$rec #importation de la recette de l'étape précédente
      if(input$select1 == "Régression Logistique"){
        local$mod <- logistic_reg() %>% 
          set_engine("glm")
        local$descriptif <- "La regression logistique a pour objectif d'expliquer 
        et de prédire les valeurs d'une variable qualitative, le plus souvent binaire, 
        à partir de variables qualitatives et quantitatives."
      }else if(input$select1 == "Arbre"){
        local$mod <- decision_tree(
            # cost_complexity = 0.001,
            # tree_depth = 7,
            # min_n = NULL
          ) %>%
          set_engine("rpart") %>%
          set_mode("classification")
        local$descriptif <- "Les arbres de classification sont des outils 
        d'exploration des données et d'aide 
        à la décision qui permettent d'expliquer 
        et de prédire une variable qualitative à partir de variables explicatives 
        quantitatives et/ou qualitatives. Dans cet exemple, les arbres sont construits avec 
        l'algorithme CART qui permet d'obtenir des classes d'individus le 
        plus homogène possible, selon la variable d'intérêt (target)."
      }else if(input$select1 == "KNN"){
        local$mod <- nearest_neighbor(
            # neighbors = 3
          ) %>% 
          set_engine("kknn") %>% 
          set_mode("classification")
        local$descriptif <- "L'algorithme des k plus proches voisins consiste à prédire l'étiquette d'un individu 
        en faisant un vote à la majorité parmi les étiquettes des k plus proches voisins. 
        L'étiquette va bien entendu dépendre du choix de K. 
        S'il est trop grand, il aura tendance à toujours prédire le groupe majoritaire. 
        Si il est trop petit, il y a un risque de sur-apprentissage."
      }else if(input$select1 == "Forêt aléatoire"){
        local$mod <- 
          rand_forest(
            # trees = 1000,
            #           mtry = 3,
            #           min_n = NULL
            ) %>% 
          set_engine("ranger") %>% 
          set_mode("classification")
        local$descriptif <- "Les forêts aléatoires permettent de prédire une variable qualitative (ou quantitative)
        à partir de variables quantitatives et/ou qualitatives. 
        Cette famille d'algorithme permet de pallier le manque de stabilité des arbres. 
        Elle consiste à agréger un grand nombre d'arbres (500 par défaut), 
        construits sur des échantillons bootstrap. Pour la construction des arbres, 
        à chaque noeud, seul un sous-ensemble de variables (3 par défaut) est sélectionné aléatoirement 
        pour choisir la coupure. "
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
    
    output$text1 <- renderText({
      
      # shinipsum::random_text(nwords = 100)
      local$descriptif
      
    })
    
    
    output$print1 <- renderPrint({
      
      req(local$dt)
      
    local$fit
      
      # shinipsum::random_print(type = "model")
      
    })
    
    
    output$dt1 <- renderDT({
      # shinipsum::random_DT(nrow = 10,ncol = 4)
      req(local$dt)
      # ncol <- length(local$pred)
      # datatable(local$pred[,(ncol-3):ncol])#4 dernières colonnes
      local$pred %>% select(target,starts_with(".pred"))
      
    })
    
 
  })
}
    
## To be copied in the UI
# mod_stat6_classif_modele_ui("stat6_classif_modele")
    
## To be copied in the server
# mod_stat6_classif_modele_server("stat6_classif_modele")
