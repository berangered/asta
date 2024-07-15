## code to prepare `ozone` dataset goes here

ozone <- read.csv("data-raw/dataset/ozone.txt",sep=";")
ozone <- ozone %>% mutate(Ne18 = as.integer(Ne18)) %>% 
  select(-date)

usethis::use_data(ozone, overwrite = TRUE)
