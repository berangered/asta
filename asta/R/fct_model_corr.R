#' model_corr 
#'
#' Fonction qui calcule un coeff de corrélation entre 2 variables 
#' 
#'
#' @param input_data un dataframe
#' @param var_expliquee variable quantitative
#' @param var_explicative variable quantitative
#' 
#' @importFrom stats cor as.formula
#' 
#' @return un coeff de correlation
#' @export
#' 
#' @examples model_corr(grandile, "REV_DISPONIBLE", "PATRIMOINE")
model_lineaireS_tab <- function(input_data, var_expliquee, var_explicative){
  
  
  a <- paste0(input_data,"$",var_expliquee)
  b <- paste0(input_data,"$",var_explicative)
  result <- cor(as.formula(a),as.formula(b) )
  result
}