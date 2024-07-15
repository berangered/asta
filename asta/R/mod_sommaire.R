#' sommaire UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sommaire_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(    title = "Sommaire",
    fluidPage(
      # tags$style('.container-fluid {
      #                        background-color: #fafad2 	;
      #         }'),
     wellPanel( 
       fluidRow(
       column(2,
       HTML('<div style=\"float:left\"><img src=\"www/hex_AstA2.png\" alt=\"alt\" style=\"float:left;width:80px;height:95px;vertical-align: middle;\"> </a></div>'),
        ),
       column(8,"  ",
      tags$p("Bienvenue dans AstA !", style = "text-align: center; font-size: 60px;")), 
      column(2,
      HTML('<div style=\"float:right\"><img src=\"www/logo_cefil.jpg\" alt=\"alt\" style=\"float:right;width:70px;height:95px;vertical-align: top;padding-top:5px;\"> </a></div>')
      )
      )),
      
    br(),
    tags$h2("~  Programme du parcours statistique des attach\u00e9s  ~", style = "background-color: #e7e7e7 ;"),
    tags$br(),
    h3("Stat 1 : Statistiques descriptives et inf\u00e9rentielles"),
    h3("Stat 2 : Enqu\u00eates et sondages"),
    h3(span("Stat 3 : Analyse exploratoire de donn\u00e9es")),
    h3("Stat 4 : Mod\u00e9lisation"),
    h3("Stat 5 : S\u00e9ries temporelles"),
    h3(span("Stat 6 : Machine learning")),
    tags$br(),tags$br(),tags$br()
    
   
      
    
  )
  )
}
    
#' sommaire Server Functions
#'
#' @noRd 
mod_sommaire_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sommaire_ui("sommaire_ui")
    
## To be copied in the server
# mod_sommaire_server("sommaire_ui")
