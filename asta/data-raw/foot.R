## code to prepare `foot` dataset goes here


library(dplyr)

foot <- readRDS("data-raw/dataset/foot.rds")
foot$pop_racine <- round(foot$pop_racine,3)
foot$pop_log <- round(foot$pop_log,3)

usethis::use_data(foot, overwrite = TRUE)
