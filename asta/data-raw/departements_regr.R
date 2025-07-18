library(dplyr)

departements_regr <- read.csv2("data-raw/dataset/statloc.csv", sep=";")
regions <- read.csv2("data-raw/dataset/region_2022.csv", sep=",") %>% select(REG, GR_REG)
tabpass <- read.csv2("data-raw/dataset/tabpass_dep_reg.csv", sep=",") %>% select(DEP, REG)

#on enrichit avec une variable de région
departements_regr <- departements_regr %>% left_join(tabpass,by=c("Code" = "DEP"))
departements_regr <- departements_regr %>% left_join(regions,by="REG")
table(departements_regr$GR_REG)

#on passe les noms des départements en rownames
rownames(departements_regr) <- departements_regr$Libelle
departements_regr <- departements_regr %>% select(-Libelle, -Code, -REG)

#On enlève quelques variables pour pas qu'il n'y en ait trop
departements_regr <- departements_regr %>% select(-Part_0015, -Part_menages_imposes, -Niv_vie_median, 
                                        -Part_revenus_du_patrimoine, -Tx_procurations, -Tx_coups_blessures, -Part_salaries)

departements_regr$GR_REG <- factor(departements_regr$GR_REG, levels = c("NE", "NO", "SE", "SO", "IDF", "DOM"))
departements_regr$nb_habitants <- round(departements_regr$nb_habitants/1000000,3)
departements_regr$Tx_emploi <- round(departements_regr$Tx_emploi,1)

departements_regr <- departements_regr %>% 
  select(-Densite_pop, -Tx_activite, -Salaire_horaire, -Part_prest, -part_maisons, -part_proprietaires, -Part_cadres, -part_bacp5 ) %>% 
  select(Tx_cambriolages, Tx_vols_vehicules, everything())

usethis::use_data(departements_regr, overwrite = TRUE)