## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(asta)
library(datasets)
library(lubridate)
library(ggplot2)
library(dygraphs)

## -----------------------------------------------------------------------------

#Fonction qui transforme une ts en dataframe
ts_to_dt <- function(serie_ts,date_debut,periode,taille){
  
  data.frame(
  date=seq(date_debut, by = periode, length.out = taille),
  as.data.frame(serie_ts)
  )
}


#Exemple
airpass_dt <- ts_to_dt(airpass,
         date_debut = my("01/1949"),
         periode = "month",
         taille = 144)


## -----------------------------------------------------------------------------

#Représentation graphique avec PLOT
plot_ts <- function(serie_ts){
  plot.ts(eval(parse(text = serie_ts)))
}
plot_ts("airpass")


#Représentation graphique avec DYGRAPHS
# dygraph_ts <- function(serie_ts){
#   dygraph(serie_ts)
# }

dygraph_ts(airpass)
dygraph_ts(hotel)
# hotel_cut <- window(hotel,start = c(2011,1),end = c(2019,12))
# dygraph_ts(hotel_cut)

#dygraph_ts(visiteurs) Celle-là ne fonctionne pas

#Représentation graphique avec GGPLOT
plot_dt <- function(dt){
  ggplot(dt)+
    aes(x = date,y=x) +
    geom_line()
}
#Exemple
plot_dt(airpass_dt)


## -----------------------------------------------------------------------------
# graph_month <-function(serie_ts){
#   monthplot(serie_ts,cex.main = 1,ylab = "",col='dodgerblue',col.base = 'indianred',lwd.base = 3)
# }
graph_month(airpass)
graph_month(hotel)
graph_month(visiteurs)
graph_month(accidents)

## -----------------------------------------------------------------------------

#fonction qui prend une série temporelle en entrée et qui renvoie 
#le modèle de regression linéaire en sortie
# cvs_reg_model <- function(x){
#   
#   n <- length(x) #Nombre d'observations de la série temporelle
#   t <- 1:n #création d'une série
#   f <- frequency(x) #nombre de périodes par saison
#   ns <- n/f #Nombre de saisons (Valeur entière)
#   for (i in 1:f) {
#     su <- rep(0,times=f)
#     su[i] <- 1
#     s <- rep(su,times=ns)
#     assign(paste("s",i,sep=""),s)
#     rm(s,su)
#   } #création des variables explicatives s1 jusqu'à sf
#   a <- paste0("x~t-1+",paste0("s",1:f,collapse = "+"))
#   reg <- lm(as.formula(a)) #modèle de regression
#   reg
#   
# }

#Récupération de la série grace au menu déroulant
ts1 <- eval(parse(text="airpass"))
ts2 <- eval(parse(text="hotel"))
ts3 <- eval(parse(text="visiteurs"))

#Exemples pour la fonction
reg_airpass <- cvs_reg_model(log(airpass))
summary(reg_airpass)

reg_visiteurs <- cvs_reg_model(visiteurs)
summary(reg_visiteurs)

#Version modifiée de la fréquentation hôtelière

reg_hotel <- cvs_reg_model(hotel)
summary(reg_hotel)

#x est une série temporelle avec saisonnalité (avec frequency)
# cvs_reg_desais <- function(x){
#   n <- length(x) #Nombre d'observations de la série temporelle
#   t <- 1:n
#   f <- frequency(x) #saisonnalité
#   ns <- n/f #Nombre de saisons (Valeur entière)
#   for (i in 1:f) {
#     su <- rep(0,times=f)
#     su[i] <- 1
#     s <- rep(su,times=ns)
#     assign(paste("s",i,sep=""),s)
#     rm(s,su)
#   } #création des variables explicatives s1 jusqu'à sf
#   a <- paste0("x~t-1+",paste0("s",1:f,collapse = "+"))
#   reg <- lm(as.formula(a)) #modèle de regression
#   #récupération des coefficients issus de la regression et centrage
#   a <- mean(reg$coefficients[2:(f+1)])
#   b <- reg$coefficients[1]
#   c <- reg$coefficients[2:(f+1)]-a
#   
#   #on centre les coefficients pour respecter les ordres de grandeur
#   d <- paste0("c[",1:f,"]")
#   e <- paste0("s",1:f)
#   f <- paste0(d,"*",e,collapse = "+")
#   
#   #création d'un vecteur avec les coeffs
#   #de desaisonnalisation
#   g <- eval(parse(text = f))
#   x - g
# }

#Exemple avec airpass 
x <- airpass
y <- log(airpass)
# on désaisonnalise la série passée en logarithme puis on repasse à l'exponentiel
ycvs <- cvs_reg_desais(y)
xcvs <- exp(ycvs)


#1 - test sur la série Airpass

#Fonction qui prend la serie desaisonnalisée et la série brute, et qui renvoie 
# les deux séries sur un même graphique
# dygraph_reg_cvs <- function(x,x_cvs){
#   x_brut_cvs <- cbind(x,x_cvs)
#   dygraph(x_brut_cvs)
# } 



dygraph_reg_cvs(x,xcvs)


#Test sur la fréquentation hôtelière (on ne garde que la partie qui fonctionne bien)
x <- hotel
xcvs <- cvs_reg_desais(x)
dygraph_reg_cvs(x,xcvs)


## -----------------------------------------------------------------------------

#Dans l'application, il faut choisir une série 
#puis choisir un type de modèle
# cvs_decompo_graph <- function(x,...){
#   decomp.x <- decompose(x,...)
#   plot(decomp.x)
# }

#Choisir dans le menu un modèle de type multiplicatif au additif


#Décomposition de airpass
cvs_decompo_graph(airpass,type="multiplicative")
cvs_decompo_graph(airpass,type="additive")

#décomposition de visiteurs
cvs_decompo_graph(visiteurs,type="additive")

#Décomposition de fréquentation hôtelière
cvs_decompo_graph(hotel,type="additive")
cvs_decompo_graph(hotel,type="multiplicative")
# decompose(hotel)

#Décomposition de fréquentation hôtelière
cvs_decompo_graph(chomage_bit,type="additive")
cvs_decompo_graph(chomage_bit,type="multiplicative")

cvs_decompo_desais <- function(x,...){
  decomp.x <- decompose(x,...)
  if (decomp.x$type == "multiplicative"){
    x/decomp.x$seasonal
  } else {
      x - decomp.x$seasonal
  }
}


#Cette fonction ne désaisonnalise pour l'instant que les 
#série temporelle de période 12
# cvs_mm_desais <- function(x,...){
# 
#   m2_12 <- function(a){
# 
# y <- (1/12)*stats::filter(a,c(0.5,rep(1,times=11),0.5))
# return(y)
#   }
# 
#   m2_4 <- function(a){
# 
#   y <- (1/4)*stats::filter(a,c(0.5,rep(1,times=3),0.5))
#   return(y)
# 
# }
# 
# #   m3=function(b){
# # y <- (1/3)*stats::filter(b,rep(1,times=3))
# # return(y)
# #   }
# 
#   m3_3 <- function(x){
#   if (frequency(x)==4){
#     a <- rep(0,times=3)
#   } else{
#     a <- rep(0,times=11)
#   }
#   y <- (1/9)*stats::filter(x,c(1,a,2,a,3,a,2,a,1))
#   return(y)
# }
# 
#   if(frequency(x)==12){
# 
#     a <- m2_12(x) #Ce filtre permet d'estimer la tendance pure de période 12
#     b <- x - a  #b correspond à la composante Saison + Bruit
# 
#     c <- m3_3(b) #Estimation des coefficients des saisonniers
#     d <- c - m2_12(c) #Moyennisation pour que la somme de 12 périodes
#       #consécutives des coefficients saisonniers soit égale à 0
# 
#   }else{
#      a <- m2_4(x) #Ce filtre permet d'estimer la tendance pure de période 4
#     b <- x - a  #b correspond à la composante Saison + Bruit
# 
#     c <- m3_3(b) #Estimation des coefficients saisonniers
#     d <- c - m2_4(c) #Moyennisation pour que la somme de 12 périodes
#       #consécutives des coefficients saisonniers soit égale à 0
#   }
# 
#   x - d
# 
# }



# #Modèle multiplicatif
# airpass_mm_cvs <- exp(cvs_mm_desais(log(airpass)))
# 
# #Modèle additif
hotel_mm_cvs <- cvs_mm_desais(hotel)
chomage_bit_mm_cvs <- cvs_mm_desais(chomage_bit)
airpass_mm_cvs <- cvs_mm_desais(airpass)
# 
# 
# 
# #Représentation graphique
# dygraph_reg_cvs(airpass,airpass_mm_cvs)
dygraph_reg_cvs(hotel,hotel_mm_cvs)
dygraph_reg_cvs(chomage_bit,chomage_bit_mm_cvs)
dygraph_reg_cvs(airpass,airpass_mm_cvs)


