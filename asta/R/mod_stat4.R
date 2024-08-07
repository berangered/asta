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
                                      menuSubItem("R\u00e9gressions lineaires Simple", 
                                                  tabName = "reg_lineaire"),
                                      menuSubItem("R\u00e9gressions lineaires multiples", 
                                                  tabName = "reg_multiple")
                                      
                                      
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
                               tabPanel("gapminder",
                                 
                                 DT::DTOutput(ns('dt1'))
                                 
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
                               tabPanel("gapminder",
                                        
                                        tags$br(),
                                        
                                        tags$p("La base Gapminder utilisée est un extrait du dataset Gapminder, inclus dans le package du même nom. Ici la base Gapminder comprend 168 lignes correspondant à 168 pays et 10 variables décrivant la situation démographique, sociale et économique des pays.", 
                                               style = "font-size : 110% "),
                                        
                                        tags$br(),
                                        
                                        fluidRow(
                                          
                                          column(4,
                                                        
                                                  wellPanel(
                                                    
                                                    tags$p("Dictionnaire des variables", style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
                                                    
                                                    tags$br(),  
                                                    tags$p("Country : nom du pays", style = "font-size : 110% "),
                                                    tags$p("Year : années des données", style = "font-size : 110% "),
                                                    tags$p("Infant_Mortality : taux de mortalité infantile en pour mille", style = "font-size : 110% "),
                                                    tags$p("Life_expectancy : espérance de vie en années", style = "font-size : 110% "),
                                                    tags$p("Fertility :  indicateur conjoncturel de fécondité en nombre d’enfants par femme", style = "font-size : 110% "),
                                                    tags$p("Population : nombre d’habitants", style = "font-size : 110% "),
                                                    tags$p("GDP : produit intérieur brut PIB en dollars", style = "font-size : 110% "),
                                                    tags$p("Continent : nom du continent", style = "font-size : 110% "),
                                                    tags$p("Region : nom de la région du monde", style = "font-size : 110% "),
                                                    tags$p("Gdp_per_capita : pib par habitant en dollars ppa (parité de pouvoir d’achat).", style = "font-size : 110% ")
                                                    
                                                    
                                                    
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
    local <- reactiveValues(dt1 = gapminder, dt2=titanic)
    
    output$dt1 <- renderDT({
      
      local$dt1 %>% DT::datatable(class = "display")
      
    })
    
    output$dt2 <- renderDT({
      
      local$dt2 %>% DT::datatable(class = "display")
      
    })
 
    mod_stat4_lineaire_simple_server("stat4_lineaire_simple_ui_1", global=global)
    mod_stat4_lineaire_multiple_server("stat4_lineaire_multiple_ui_1",global=global)
    mod_stat4_non_lineaire_server("stat4_non_lineaire_ui_1", global=global)
    mod_stat4_logistique_server("stat4_logistique_ui_1", global=global)
  })
}
    
## To be copied in the UI
# mod_stat4_ui("stat4_ui")
    
## To be copied in the server
# mod_stat4_server("stat4_ui")
