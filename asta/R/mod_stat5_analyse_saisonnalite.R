#' stat5_analyse_saisonnalite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom stats decompose monthplot
mod_stat5_analyse_saisonnalite_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = "subitem_2",
    h2("Etude de la saisonnalit\u00e9"),
    br(),
    br(),
    
    fluidRow(
      
      column(4,
             
             wellPanel(
               
               
               tags$p("Param\u00e8tres", 
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               
               
               selectInput(
                 inputId = ns("select1"),
                 label = "Choisissez une s\u00e9rie :",
                 choices = c("Trafic a\u00e9rien"="airpass",
                             "Fr\u00e9quentation hoteliere"="hotel",
                             "Taux de chomage au sens du BIT (en %)" = "chomage_bit",
                             "S\u00e9curit\u00e9 routiere"="accidents")
               ),
               
               checkboxInput(inputId = ns("check1"),
                             label = "Appliquez le logarithme",
                             value = FALSE),
               
               actionButton(
                 ns("go1"),
                 label = "Cliquez pour afficher"
               )
               
               
               
             )
             
             
      ),
      
      column(8,
             
             wellPanel(
               tags$p("Graphique", 
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               plotOutput(ns("plot1"))
             )
             
      )
      
      
      
    )
    
    
    
    
  )
  
  
}
    
#' stat5_analyse_saisonnalite Server Functions
#'
#' @noRd 
mod_stat5_analyse_saisonnalite_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    
    local <- reactiveValues(ts=NULL,log_ts=NULL,check=NULL)
    
    observeEvent(input$go1,{
      local$ts <- eval(parse(text=input$select1))
      local$log_ts <- log(local$ts)
      local$check <- input$check1
      
    })
    
    
    output$plot1 <- renderPlot({
      
      validate(need(expr = !is.null(local$ts),
                    message = "Choisissez une s\u00e9rie temporelle dans le menu d\u00e9roulant et cliquez pour afficher le tableau")) 
      
      
      
      if(local$check){  
        
        graph_month(local$log_ts)
        
      }else{
        
        graph_month(local$ts)
        
      }
    })
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_stat5_analyse_saisonnalite_ui("stat5_analyse_saisonnalite")
    
## To be copied in the server
# mod_stat5_analyse_saisonnalite_server("stat5_analyse_saisonnalite")
