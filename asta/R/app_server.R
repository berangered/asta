#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  
  
  mod_stat1_server("stat1_ui_1")
  
  mod_stat2_server("stat2_ui_1")
  
  mod_stat3_server("stat3_ui")
  
  mod_stat4_server("stat4_ui")
  
  mod_stat5_server("stat5_ui")
  
  mod_stat6_server("stat6_ui")
  
}
