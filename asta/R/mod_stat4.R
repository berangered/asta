#' stat4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat4_ui <- function(id){
  ns <- NS(id)
  
  
  tabPanel(title = "Stat 4",
           
           
           dashboardPage(
             
             dashboardHeader(title = "R\u00e9gression"),
             dashboardSidebar(
               
               fluidRow(collapsed = FALSE,
                        
                        
                        sidebarMenu(id = "tabs_regression",
                                    
                                    
                                    menuItem(
                                      "Donn\u00e9es",
                                      menuSubItem("Visualisation", tabName = "viz_reg"),
                                      menuSubItem("Description", tabName = "description_reg"),
                                      icon = icon("th"),
                                      selected = FALSE
                                    ),
                                    
                                    menuItem(
                                      
                                      "R\u00e9gressions lineaires",
                                      icon = icon("th"),
                                      selected = FALSE,
                                      menuSubItem("départements - corrélations", 
                                                  tabName = "reg_lineaire"),
                                      menuSubItem("départements - régression", 
                                                  tabName = "reg_multiple"),
                                      menuSubItem("foot - corrélations", 
                                                  tabName = "reg_foot"),
                                      menuSubItem("foot - régression", 
                                                  tabName = "foot_multiple")
                                      
                                      
                                    ), 
                                    
                                    menuItem(tabName = "reg_nl",
                                        
                                      "R\u00e9gressions non lineaires",
                                      icon = icon("th"),
                                      selected = FALSE
                                      
                                      
                                    ),
                                    
                                    menuItem(tabName = "reg_logistique",
                                      
                                      "R\u00e9gressions logistiques",
                                      icon = icon("th"),
                                      selected = FALSE
                                      
                                      
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
               )
               
             ),
             dashboardBody(
               
               tabItems(
                 
                 tabItem(
                   
                   tabName = "viz_reg",
                   h2("Visualisation des fichiers"),
                   tags$br(), 
                   
                   tabsetPanel(type="tabs",
                               tabPanel("départements",
                                 
                                 DT::DTOutput(ns('dt1'))
                                 
                               ),
                               tabPanel("foot",
                                        
                                        DT::DTOutput(ns('dt3'))
                                        
                               ),
                               tabPanel("titanic",
                                 
                                        DT::DTOutput(ns('dt2'))
                                 
                               ))
                   
                   
                   
                 ),
                 
                 tabItem(
                   
                   
                   
                   tabName = "description_reg",
                   h2("Description des donn\u00e9es"),
                   tags$br(),
                   
                   
                   tabsetPanel(type="tabs",
                               tabPanel("Départements",
                                        
                                        tags$br(),
                                        
                                        tags$p("La base départements est un extrait issu de statistiques locales. Elle contient 100 départements (hors Mayotte) et 10 variables portant sur la population et ses caractéristiques socio-économiques.", 
                                               style = "font-size : 110% "),
                                        
                                        tags$br(),
                                        
                                        fluidRow(
                                          
                                          column(4,
                                                        
                                                  wellPanel(
                                                    
                                                    tags$p("Dictionnaire des variables", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                                    
                                                    tags$br(),  
                                                    tags$p("nb_habitants : nombre d'habitants du département (en millions)", style = "font-size : 110% "),
                                                    tags$p("Part_6599 : part d'habitants âgés de 65 ans ou plus", style = "font-size : 110% "),
                                                    tags$p("part_bacp5 : part d'habitants diplômés de niveau bac+5", style = "font-size : 110% "),
                                                    tags$p("Tx_chomage : taux de chômage", style = "font-size : 110% "),
                                                    tags$p("Tx_emploi : taux d'emploi", style = "font-size : 110% "),
                                                    tags$p("Part_cadres : part de PCS cadres dans la population", style = "font-size : 110% "),
                                                    tags$p("part_resid_secondaires : part de résidences secondaires dans les logements", style = "font-size : 110% "),
                                                    tags$p("Tx_cambriolages : nombre de cambriolages pour 1000 habitants", style = "font-size : 110% "),
                                                    tags$p("Tx_vols_vehicules : nombre de vols de véhicules pour 1000 habitants", style = "font-size : 110% "),
                                                    tags$p("GR_REG : région d'appartenance, en 6 modalités (NO, NE, SO, SE, IDF, DOM)", style = "font-size : 110% ")
                                                    
                                                    
                                                    
                                                  )      
                                                        
                                                        
                                                        ))
                                        
                               ),
                               tabPanel("Foot",
                                        
                                        tags$br(),
                                        
                                        tags$p("La base Foot est constituée des 213 pays ayant participé à au moins une coupe du monde de football masculine, en qualifications ou en phases finales, entre 1990 et 2022.", 
                                               style = "font-size : 110% "),
                                        
                                        tags$br(),
                                        
                                        fluidRow(
                                          
                                          column(4,
                                                 
                                                 wellPanel(
                                                   
                                                   tags$p("Dictionnaire des variables", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                                   
                                                   tags$br(),  
                                                   tags$p("pays : nom du pays", style = "font-size : 110% "),
                                                   tags$p("matchs : nombre de matchs joués (qualifications ou phases finales)", style = "font-size : 110% "),
                                                   tags$p("pct_victoires : pourcentage de matchs gagnés", style = "font-size : 110% "),
                                                   tags$p("but_marques : nombre moyen de buts marqués par match", style = "font-size : 110% "),
                                                   tags$p("but_encaiss : nombre moyen de buts encaissés par match", style = "font-size : 110% "),
                                                   tags$p("continent : continent auquel appartient le pays (en 4 modalités)", style = "font-size : 110% "),
                                                   tags$p("pop : nombre d'habitants du pays en 2022 (en milliers)", style = "font-size : 110% "),
                                                   tags$p("pop_racine : racine carré de la variable pop", style = "font-size : 110% "),
                                                   tags$p("pop_log : logarithme de la variable pop", style = "font-size : 110% ")
                                                   
                                                   
                                                   
                                                 )      
                                                 
                                                 
                                          ))
                                        
                               ),
                               tabPanel("titanic",
                                        
                                        tags$br(),
                                        
                                        tags$p("Cette base de données, disponible dans le package dataset de R, 
                                        fournit des informations sur le sort des passagers du Titanic, selon leur classe de voyage, 
                                        leur sexe et leur âge. Les données présentes dans cette base proviennent à l’origine 
                                        de données collectées par le  «  British Board of Trade » et reprises dans  
                                        «  Report on the Loss of the ‘Titanic’ (S.S.). British Board of Trade Inquiry Report (reprint). 
                                               Gloucester, UK: Allan Sutton Publishing ». ", 
                                               style = "font-size : 110% "),
                                        
                                        tags$br(),
                                        
                                        tags$p("Le RMS Titanic est un paquebot transatlantique britannique qui fait naufrage dans l'océan Atlantique Nord en 1912 à la suite d'une collision avec un iceberg, lors de son voyage inaugural de Southampton à New York. Entre 1 490 et 1 520 personnes trouvent la mort, ce qui fait de cet événement l'une des plus grandes catastrophes maritimes survenues en temps de paix et la plus grande pour l'époque. Source : wikipedia", style = "font-size : 110% "),
                                        
                                        tags$br(),
                                        
                                        fluidRow(
                                          
                                          column(4,
                                                 
                                                 wellPanel(
                                                   tags$p("Dictionnaire des variables", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                                   
                                                   tags$br(),  
                                                   tags$p("Survived : Les passagers ont-ils survécu ? (oui ou non)", style = "font-size : 110% "),
                                                   tags$p("Class : classe du billet (première classe, deuxième classe, troisième classe ou équipage)", style = "font-size : 110% "),
                                                   tags$p("Sex : homme ou femme", style = "font-size : 110% "),
                                                   tags$p("Age : enfant ou adulte", style = "font-size : 110% "),
                                                   tags$p("Freq :  fréquence associée à chaque case du tri croisé.", style = "font-size : 110% ")
                                                  
                                                   
                                                 )
                                          ),
                                          column(8,
                                                 
                                                 wellPanel(
                                                 
                                                 
                                                 
                                                 tags$img(
                                                   src = 'www/titanic.jpg',
                                                   title = "CEFIL",
                                                   height = '450'
                                                 )
                                                 
                                                 
                                                 )
                                          )
                                          
                                        )    
                                        
                               ))
                   
                   
                  
                   
                   
                 ),
                 
                 tabItem(
                   tabName = "reg_lineaire",
                   mod_stat4_lineaire_simple_ui(ns("stat4_lineaire_simple_ui_1")), 
                    ),
                 tabItem(
                   tabName = "reg_multiple",
                   mod_stat4_lineaire_multiple_ui(ns("stat4_lineaire_multiple_ui_1")), 
                 ),
                 tabItem(
                   tabName = "reg_foot",
                   mod_stat4_foot_simple_ui(ns("stat4_foot_simple_ui_1")), 
                 ),
                 tabItem(
                   tabName = "foot_multiple",
                   mod_stat4_foot_multiple_ui(ns("stat4_foot_multiple_ui_1")), 
                 ),
                 tabItem(
                   tabName = "reg_nl",
                   mod_stat4_non_lineaire_ui(ns("stat4_non_lineaire_ui_1")), 
                 ),
                 tabItem(
                   tabName = "reg_logistique",
                   mod_stat4_logistique_ui(ns("stat4_logistique_ui_1") )
                 )
                 
             )
             )
             )
  )
  }

#' stat4 Server Functions
#'
#' @noRd 
mod_stat4_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    global <- reactiveValues(data = grandile)
    local <- reactiveValues(dt1 = departements_regr, dt2=titanic, dt3=foot)
    
    output$dt1 <- renderDT({
      
      local$dt1 %>% DT::datatable(class = "display")
      
    })
    
    output$dt2 <- renderDT({
      
      local$dt2 %>% DT::datatable(class = "display")
      
    })
    
    output$dt3 <- renderDT({
      
      local$dt3 %>% DT::datatable(class = "display")
      
    })
 
    mod_stat4_lineaire_simple_server("stat4_lineaire_simple_ui_1", global=global)
    mod_stat4_lineaire_multiple_server("stat4_lineaire_multiple_ui_1",global=global)
    mod_stat4_foot_simple_server("stat4_foot_simple_ui_1", global=global)
    mod_stat4_foot_multiple_server("stat4_foot_multiple_ui_1",global=global)
    mod_stat4_non_lineaire_server("stat4_non_lineaire_ui_1", global=global)
    mod_stat4_logistique_server("stat4_logistique_ui_1", global=global)
  })
}
    
## To be copied in the UI
# mod_stat4_ui("stat4_ui")
    
## To be copied in the server
# mod_stat4_server("stat4_ui")
