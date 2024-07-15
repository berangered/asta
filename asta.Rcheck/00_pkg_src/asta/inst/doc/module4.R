## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(asta)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
model_lineaireS_tab <- function(input_data, var_expliquee, var_explicative, constante = TRUE){
  
  if (constante == TRUE) {
  a <- paste0(var_expliquee, "~ 0 + ", var_explicative)
   model <- lm(as.formula(a), data = input_data)
  result <- summary(model)
  result}
  else {  a <- paste0(var_expliquee, "~", var_explicative)
   model <- lm(as.formula(a), data = input_data)
  result <- summary(model)
  result}
}

model_lineaireS_tab(grandile, "REV_DISPONIBLE", "PATRIMOINE")

## -----------------------------------------------------------------------------
model_lineaireS_plot <- function(input_data, var_expliquee, var_explicative, constante=TRUE){
  
    if (constante == TRUE) { a <- paste0(var_expliquee, "~ 0 + ", var_explicative)
  model <- lm(as.formula(a), data = input_data)
  result <- summary(model)
  
  a <- input_data %>% ggplot(aes(x = .data[[var_explicative]],y = .data[[var_expliquee]])) + 
    geom_point(alpha=0.2,size=1,color="darkblue") + 
    theme_minimal() +
    geom_smooth(method="lm",color="red")
  
  
  coeff=coefficients(model)
  a + geom_abline(intercept = 0, slope = coeff[1])}
  
  else {
  
  a <- paste0(var_expliquee, "~", var_explicative)
  model <- lm(as.formula(a), data = input_data)
  result <- summary(model)
  
  a <- input_data %>% ggplot(aes(x = .data[[var_explicative]],y = .data[[var_expliquee]])) + 
    geom_point(alpha=0.2,size=1,color="darkblue") + 
    theme_minimal() +
    geom_smooth(method="lm",color="red")
  
  
  coeff=coefficients(model)
  a + geom_abline(intercept = coeff[1], slope = coeff[2])}
  }

model_lineaireS_plot(grandile, "REV_DISPONIBLE", "PATRIMOINE")


## -----------------------------------------------------------------------------
model_lineaireM_tab <- function(input_data, var_expliquee, var_explicatives){
   a <- paste0(var_expliquee, "~", paste0(var_explicatives, collapse = "+"))
  model <- lm(as.formula(a), data = input_data)
  result <- summary(model)
  result
}

model_lineaireM_tab(iris, "Petal.Length", c("Sepal.Length", "Petal.Width", "Sepal.Width"))

## -----------------------------------------------------------------------------
model_logistique_tab <- function(input_data, var_expliquee, var_explicatives, constante = TRUE){
    # browser()
  data_log <- input_data %>%
    mutate(var_expliquee_num = ifelse(.data[[var_expliquee]] == "0",0, 1)) 
  
  
  if (constante == FALSE) {
    a <- paste0("var_expliquee_num ~ 0 + ", paste0(var_explicatives, collapse = "+"))
    model <- glm(as.formula(a),family = binomial(link = "logit"), data = data_log)
    result <- summary(model)
    result
  }
  else{ 
    a <- paste0("var_expliquee_num ~", paste0(var_explicatives, collapse = "+"))
    model <- glm(as.formula(a),family = binomial(link = "logit"), data = data_log)
    result <- summary(model)
    result}
  
  
}


model_logistique_tab(input_data = grandile, var_expliquee = "PAUVRE", var_explicatives = c("REV_DISPONIBLE","ACT"), constante = TRUE)


