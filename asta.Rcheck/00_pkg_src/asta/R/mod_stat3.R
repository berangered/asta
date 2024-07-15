#' stat3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT formatRound
mod_stat3_ui <- function(id){
  ns <- NS(id)

  tabPanel(title = "Stat 3",
           
           dashboardPage(
             
             dashboardHeader(title = "Analyse exploratoire de donn\u00e9es"),
             dashboardSidebar(
               fluidRow(collapsed = FALSE,
                        
                        
                        sidebarMenu(id = "tabs_regression",
                                    
                                    
                                    menuItem(
                                      "Donnees",
                                      menuSubItem("Visualisation", tabName = "viz"),
                                      menuSubItem("Description", tabName = "description"),
                                      icon = icon("th"),
                                      selected = FALSE
                                    ),
                                    
                                    
                                    menuItem(
                                      
                                      "M\u00e9thodes factorielles",
                                      icon = icon("th"),
                                      selected = FALSE,
                                      menuSubItem("ACP", 
                                                  tabName = "acp"),
                                      menuSubItem("AFC (en construction)", 
                                                  tabName = "afc"),
                                      menuSubItem("ACM (en construction)", 
                                                  tabName = "acm")
                                      
                                      
                                    ),
                                    menuItem(
                                      
                                      "Classification",
                                      icon = icon("th"),
                                      selected = FALSE,
                                      menuSubItem("CAH", 
                                                  tabName = "cah"),
                                      
                                      menuSubItem("K-means", 
                                                  tabName = "kmeans")
                                      
                                      
                                    )
                                    
                                    
                        )
                        
                        
                        
                        
                        
               ),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(
                 href = 'https://www.cefil.fr/',
                 tags$img(
                   src = 'www/logo_cefil.jpg',
                   title = "CEFIL",
                   height = '95'
                 ) ,
                 style = "text-align: center; float:bottom;"
               )),
             dashboardBody(
               
               tabItems(
                 
                 tabItem(
                   
                   tabName = "viz",
                   h2("Visualisation du fichier"),
                   tags$br(), 
                   DT::DTOutput(ns('dt1'))
                   
                   ),
                 
                 tabItem(
                   
                   tabName = "description",
                   h2("Description des donn\u00e9es"),
                   tags$br(),
                   tags$p("Il s’agit d’un jeu de donn\u00e9es donnant des informations socio-\u00e9conomiques sur les d\u00e9partements francais. 
                          Les donn\u00e9es ont \u00e9t\u00e9 constitu\u00e9es à partir du site internet statistiques-locales.fr", 
                          style = "font-size : 110%; "),
                   fluidRow(
                     
                     column(4,
                     
                     wellPanel(
                       tags$p("Dictionnaire des variables", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                       
                       tags$br(),  
                       tags$p("nb_habitants : nombre d'habitants", style = "font-size : 110% "),
                       tags$p("Densite_pop : densit\u00e9 de population ", style = "font-size : 110% "),
                       tags$p("Part_6599 : part des personnes entre 65 et 99 ans (pourcentage) ", style = "font-size : 110% "),
                       tags$p("part_bacp5 :  part de diplôm\u00e9s de niveau bac +5 ou sup\u00e9rieur", style = "font-size : 110% "),
                       tags$p("Tx_activite : taux d'activit\u00e9 (pourcentage) ", style = "font-size : 110% "),
                       tags$p("Tx_chomage : taux de chomage (pourcentage) ", style = "font-size : 110% "),
                       tags$p("Tx_emploi : taux d'emploi", style = "font-size : 110% "),
                       tags$p("Part_cadres : part de cadres  ", style = "font-size : 110% "),
                       tags$p("Salaire_horaire : salaire horaire  ", style = "font-size : 110% "),
                       tags$p("Part_prest : poids des prestations sociales dans les revenus  ", style = "font-size : 110% "),
                       tags$p("part_resid_secondaires : part des résidences secondaires  ", style = "font-size : 110% "),
                       tags$p("part_maisons : part des maisons  ", style = "font-size : 110% "),
                       tags$p("part_propriétaires : part des propriétaires  ", style = "font-size : 110% "),
                       tags$p("Tx_vols_vehicules : nombre de v\u00e9hicules vol\u00e9s pour 1000 habitants  ", style = "font-size : 110% "),
                       tags$p("GR_REG :   groupes de r\u00e9gions en 6 modalit\u00e9s (IDF, NO, NE, SO, SE, DOM)", style = "font-size : 110% ")
                       
                     )
                     ),
                     column(8,
                            
                            tags$img(
                              src = 'www/france_dpt.jpg',
                              title = "D\u00e9partements",
                              height = '350'
                            )    
                            
                            )
                     
                   )),
                   
                 
                 tabItem(
                   tabName = "acp",
                   mod_stat3_acp_ui(ns("stat3_acp_1")), 
                 ),
                 tabItem(
                   tabName = "ad",
                   h2("Analyse Discriminante"),
                   wellPanel("L’analyse factorielle discriminante (AFD) ou simplement analyse discriminante
                             est une technique statistique qui vise à décrire, expliquer et prédire l’appartenance
                             à des groupes prédéfinis (classes, modalités de la variable à prédire…) d’un ensemble d’observations 
                             (individus, exemples…) à partir d’une série de variables prédictives (descripteurs, variables exogènes…).", br(),
                             "Source : Wikipedia")
                 ),
                 tabItem(
                   tabName = "cah",
                   mod_stat3_cah_ui(ns("stat3_cah_1")), 
                 ),
                 tabItem(
                   tabName = "kmeans",
                   mod_stat3_kmeans_ui(ns("stat3_kmeans_1")), 
                 )
                 
               )
               
               
             )
             
             
             
           )
           
           )
  
  
}
    
#' stat3 Server Functions
#'
#' @noRd 
mod_stat3_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    global <- reactiveValues(data = departements)
    local <- reactiveValues(dt = state)
    
    output$dt1 <- renderDT({
      
      global$data %>% DT::datatable(class = "display compact", options = list(
        scrollX = TRUE)) %>% 
         formatRound("Tx_emploi", 1)
      
    })
    
    mod_stat3_acp_server("stat3_acp_1", global=global)
    mod_stat3_cah_server("stat3_cah_1", global=global) 
    mod_stat3_kmeans_server("stat3_kmeans_1", global=global)
  })
}
    
## To be copied in the UI
# mod_stat3_ui("stat3_ui")
    
## To be copied in the server
# mod_stat3_server("stat3_ui")
