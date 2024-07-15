#' stat6_reg_donnees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stat6_reg_donnees_ui <- function(id){
  ns <- NS(id)
  
  
  tabItem(
    
    
    tabName = "subitem__6",
          h2("Les données"),
    
    fluidRow(
      
      
      
      
      
      column(3,
             
             wellPanel(
               
               tags$p("Paramètres", 
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               
               selectInput(inputId = ns("select1"),
                           label = "Choississez une base de données :",
                           choices = c("ozone","grandile")),
               
               actionButton(ns("go1"),label = "Explorer la base")
               
             ),
             
             
             
             
             wellPanel(
               
               tags$p("Description de la base de données", 
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               
               textOutput(ns("txt1"))
               
             )
             
             
             
      ),
      
      column(9, 
             
             wellPanel(
               
               tags$p("Exploration de la base brute", 
                      style = "font-size : 110%; font-weight : bold; text-decoration : underline;"),
               
               verbatimTextOutput(ns("print1"))
               
             ) 
             
      )
      
      
      
      
    )
    
          
          
          
          
          
          
          )
  
  
  
}
    
#' stat6_reg_donnees Server Functions
#'
#' @noRd 
mod_stat6_reg_donnees_server <- function(id,global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    local <- reactiveValues(dt=NULL,descriptif=NULL)
    
    observeEvent(input$go1,{
      local$dt <- eval(parse(text = input$select1))
      
      if (input$select1 == "ozone"){
        local$dt <- local$dt %>% mutate(target=maxO3) %>% 
          select(-maxO3)
        local$descriptif <- "Le jeu de données ozone permet de mesurer la pollution de l'air : concentration de polluants, conditions météréologiques, la nébulosité, la température, le vent...
        \n Peut-on prédire la concentration maximum en ozone dans une journée (target) à partir des autres indicateurs de la base ?"
        
      }else if(input$select1 == "grandile"){
        local$dt <- local$dt %>% 
          select(-starts_with("LIB")) %>% 
          mutate(target=REV_DISPONIBLE) %>%
          select(-IDENT,-REV_DISPONIBLE)
        
        local$descriptif <- "Le jeu de données Grandile est constitué de ménages habitant à Grandile. Peut-on prédire le revenu disponible d'un ménage (target) à partir de ses autres caractéristiques socio-économiques ?"
        
      }
      global$dt <- local$dt
    })
    
    
    output$print1 <- renderPrint({

      # shinipsum::random_print(type = "table")
            
      req(local$dt)

      skim(local$dt)
      
    })
    
    
    output$txt1 <- renderText({
      
      req(local$dt)
      local$descriptif

      # shinipsum::random_text(nwords = 100)
    })
    
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_stat6_reg_donnees_ui("stat6_reg_donnees")
    
## To be copied in the server
# mod_stat6_reg_donnees_server("stat6_reg_donnees")
