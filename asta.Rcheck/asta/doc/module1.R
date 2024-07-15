## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,warning=FALSE,echo=FALSE,include=FALSE-----------------------------
library(asta)
library(questionr)
library(dplyr)
library(kableExtra)
library(DT)
library(ggplot2)
library(plotly)
library(rAmCharts)
library(purrr)
library(tidyr)

## -----------------------------------------------------------------------------
DT::datatable(head(grandile,10))

## -----------------------------------------------------------------------------
#Préparation de la fonction pour un amPie
a <- freq(grandile$LIB_PCS,valid = FALSE) 
b <-rownames(a)
c <- cbind(a,b) %>% as.data.frame() %>% 
  rename(label = b,value = n) %>% 
  select(label,value)

rAmCharts::amPie(c,show_values = TRUE)

rambarplot_quali(grandile,"LIB_PCS")



## -----------------------------------------------------------------------------
#Exemple
graphggplotly_diagbarre(grandile,var = "PCS")

## -----------------------------------------------------------------------------
#Exemple
a <- tri_plat(grandile,"LIB_PCS") 

#Fonction pour mettre en forme le tableau
a %>% DT::datatable(class = "cell-border hover order-colum")



## -----------------------------------------------------------------------------
graphggplotly_histo(grandile,"REV_DISPONIBLE",10)



## -----------------------------------------------------------------------------
moy <- mean(grandile$AGE)
median <- median(grandile$AGE)
q1 <- quantile(grandile$AGE,0.25)
q3 <- quantile(grandile$AGE,0.75)

indic_centrale_age <- data.frame(moy,median,q1,q3)
indic_centrale_age %>% map_chr(format_box)

format_box(mean(grandile$PATRIMOINE))

moy <- mean(grandile$AGE)
format_box(moy)


## -----------------------------------------------------------------------------
graphggplotly_nuage(grandile,"REV_DISPONIBLE","PATRIMOINE")



## -----------------------------------------------------------------------------

a <- cor(grandile[,"REV_DISPONIBLE"],grandile[,"PATRIMOINE"])

format_box(a)

## -----------------------------------------------------------------------------
#Exemple
t1 <- tableau_croise(grandile,"PCS","DIPL",ligne=TRUE)
# a$Var1 <- c(unique(data[,varquali1]),"Ensemble")
DT::datatable(t1,class = "compact",rownames = FALSE)

## -----------------------------------------------------------------------------
t1 <- table(grandile[,"PCS"],grandile[,"DIPL"])
chi2 <- as.numeric(chisq.test(t1)[1])
vcramer <- cramer.v(t1)
format_box(chi2)
format_box(vcramer)

## -----------------------------------------------------------------------------
graphggplotly_qualiquanti(grandile,"PCS","REV_DISPONIBLE")

## -----------------------------------------------------------------------------
tab <- tab_moyenne(grandile,"PCS","AGE")

#Fonction pour calculer la variation inter-groupe sur la quanti
inter <- sum(tab$Contrib_inter)
format_box(inter)

#Fonction pour calculer la variation intra-groupe sur la quanti
intra <- sum(tab$Contrib_intra)
format_box(intra)

#Variance sur la quanti
var <- var(grandile[,"AGE"])
format_box(var)

#Rapport de corrélation
eta2 <- (inter/var)*100
paste(format_box(eta2),"%")


## -----------------------------------------------------------------------------
#Visualisation de l'échantillon
taille_echant <- 1000

a <- grandile %>% 
  sample_n(taille_echant) %>% 
  select(1,5,9,10,11) 

DT::datatable(a) 

## -----------------------------------------------------------------------------
#Estimation avec la moyenne empirique
b <- a %>% mutate(PAUVRE = ifelse(PAUVRE == "1",TRUE,FALSE))
format_box(mean(b$AGE))
format_box(mean(b$REV_DISPONIBLE))
format_box(mean(b$PATRIMOINE))
format_box(mean(b$PAUVRE)*100)

## -----------------------------------------------------------------------------


# #' simul_moyenne
# #'
# #' Fonction qui simule une série d'estimations d'indicateurs 
# #' à l'aide de la moyenne empirique
# #'
# #' @param data un dataframe
# #' @param var une variable quantitative
# #' @param taille_echant taille de l'échantillon tiré
# #' @param nbr_simul nombre de tirage d'échantillons
# #'
# #' @return un vecteur numérique
# #' @export
# #'
# #' @examples  simul_moyenne(grandile,"AGE",1000,1000) 
# simul_moyenne <- function(data,var,taille_echant,nbr_simul){
#   replicate(nbr_simul, {
#   a <- data %>% sample_n(taille_echant)
#   b <- mean(a[,var])
#   b 
# })
# }


# 
# #' histoplotly_simul
# #'
# #' Histogramme de la simulation d'estimation avec la moyenne empirique
# #'
# #' @param serie un vecteur numérique
# #' @param var une variable quantitative
# #'
# #' @return un graphique GGPLOT2
# #' @export
# #'
# #' @examples 
# #' a <- simul_moyenne(grandile,"AGE",1000,1000)
# #' histoplotly_simul(a,"AGE")
# histoplotly_simul <- function(serie,var){
#   a <- serie %>% as.data.frame() %>% rename({{var}} := ".")
# ggplot(a) + aes(.data[[var]])+geom_histogram()
# 
# }
dt <- grandile %>% mutate(PAUVRE = ifelse(PAUVRE == "1",TRUE,FALSE))
a <- simul_moyenne(dt,"PAUVRE",1000,1000)*100
histoplotly_simul(a,"PAUVRE")
b <- simul_moyenne(dt,"AGE",1000,1000)
histoplotly_simul(b,"AGE")
c <- simul_moyenne(dt,"REV_DISPONIBLE",1000,1000)
histoplotly_simul(c,"REV_DISPONIBLE")
d <- simul_moyenne(dt,"PATRIMOINE",1000,1000)
histoplotly_simul(d,"PATRIMOINE")



