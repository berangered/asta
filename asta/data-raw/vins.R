## code to prepare `vins` dataset goes here


vins <- read.csv2("data-raw/dataset/winequality-white.csv")

x <- vins %>% 
  mutate_if(is.character,as.numeric) %>% 
  #Si le vin a une note inférieure à 6, on considère qu'il est mauvais
  mutate(quality = as.factor(ifelse(quality >= 6,"bon","mauvais"))) %>% 
  mutate_if(is.numeric,round,digits=3)

vins <- x

usethis::use_data(vins, overwrite = TRUE)
