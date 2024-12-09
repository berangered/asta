#' boxplot_tirage_m2 
#'
#' @description A fct function that builds box plot 
#' @param data un dataframe
#' @param var_plot une variable quantitative
#'
#' @return a graph
#' @import ggplot2
#' @import ggthemes
#' @export
#' @examples boxplot_tirage_m2(grandile, "PATRIMOINE")
boxplot_tirage_m2 <- function(data, var_plot) {
  ggplot(data) +
    geom_boxplot(aes(y = .data[[var_plot]]),  color = "blue") + scale_x_discrete() + 
    ylim (min = 9800, max = 116800) +
    labs(caption = "Source : Cefil 2021") + theme_economist()
  
}

