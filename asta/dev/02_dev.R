# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
# usethis::use_package( "thinkr" )
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/

#Module pour le sommaire (la page d'accueil)
golem::add_module( name = "sommaire" )

#Stat 1 et les sous-modules
golem::add_module( name = "stat1" ) # Name of the module
golem::add_module( name = "stat1_uni_quali" ) # Name of the module
golem::add_module( name = "stat1_uni_quanti" ) # Name of the module
golem::add_module( name = "stat1_bi_quantiquanti" ) # Name of the module
golem::add_module( name = "stat1_bi_qualiquali" ) # Name of the module
golem::add_module( name = "stat1_bi_quantiquali" ) # Name of the module
golem::add_module( name = "stat1_inf_estimations" ) # Name of the module
golem::add_module( name = "stat1_inf_simulations" ) # Name of the module
golem::add_module( name = "stat1_inf_intervalles" ) # Name of the module
golem::add_module( name = "stat1_inf_tests" ) # Name of the module

#Stat2 et les sous-modules
golem::add_module( name = "stat2" ) # Name of the module
golem::add_module( name = "stat2_sond_sas" ) # Name of the module
golem::add_module( name = "stat2_sond_saf" ) # Name of the module
golem::add_module( name = "stat2_sond_2deg" ) # Name of the module
golem::add_module( name = "stat2_sond_grappes" ) # Name of the module
golem::add_module( name = "stat2_pond_base" ) # Name of the module
golem::add_module( name = "stat2_pond_univ" ) # Name of the module
golem::add_module( name = "stat2_pond_coher" ) # Name of the module
golem::add_module( name = "stat2_pond_vraiss" ) # Name of the module
golem::add_module( name = "stat2_pond_agreg" ) # Name of the module
golem::add_module( name = "stat2_redress_na" ) # Name of the module
golem::add_module( name = "stat2_redress_corrna" ) # Name of the module
golem::add_module( name = "stat2_redress_impact" ) # Name of the module

#Stat3
golem::add_module( name = "stat3" )
golem::add_module( name = "stat3_acp" )
golem::add_module( name = "stat3_cah" )
golem::add_module( name = "stat3_kmeans" )

#Stat4
golem::add_module( name = "stat4" )
golem::add_module(name = "stat4_lineaire_simple")
golem::add_module(name = "stat4_lineaire_multiple")
golem::add_module(name = "stat4_non_lineaire")
golem::add_module(name = "stat4_logistique")

#Stat5
golem::add_module( name = "stat5" )
golem::add_module(name = "stat5_analyse_visualisation")
golem::add_module(name = "stat5_analyse_saisonnalite")
# golem::add_module(name = "stat5_analyse_autocorrelation")
golem::add_module(name = "stat5_desaisonnalisation_regression")
golem::add_module(name = "stat5_desaisonnalisation_mm")

#Stat6
golem::add_module( name = "stat6" )
#Classifs supervisées
golem::add_module(name = "stat6_classif_donnees")
golem::add_module(name = "stat6_classif_preparation")
golem::add_module(name = "stat6_classif_modele")
golem::add_module(name = "stat6_classif_validation")
golem::add_module(name = "stat6_classif_generalisation")
#Regressions
golem::add_module(name = "stat6_reg_donnees")
golem::add_module(name = "stat6_reg_preparation")
golem::add_module(name = "stat6_reg_modele")
golem::add_module(name = "stat6_reg_validation")
golem::add_module(name = "stat6_reg_generalisation")

## Add helper functions ----
## Creates fct_* and utils_*

#Module2 : tirage d'échantillons
golem::add_fct( "importation_m2" ) 
golem::add_fct( "tirage_sas_m2" ) 
golem::add_fct( "tirage_strat_m2" )
golem::add_fct( "tirage_degres_m2" )
golem::add_fct( "boxplot_tirage_m2" )
golem::add_fct( "data_apur_m2" )
golem::add_fct( "plot_apur_m2" )

#Module1 : stat desc et stat inférentielle
golem::add_fct("tri_plat")
golem::add_fct("graphggplotly_diagbarre")
golem::add_fct("rambarplot_quali")
golem::add_fct("graphggplotly_histo")
golem::add_fct("format_box")
golem::add_fct("graphggplotly_nuage")
golem::add_fct("graphggplotly_qualiquanti")
golem::add_fct("tab_moyenne")
golem::add_fct("tableau_croise")
golem::add_fct("simul_moyenne")
golem::add_fct("histoplotly_simul")

#☺Module4 : modélisation
golem::add_fct("model_lineaireS_tab")
golem::add_fct("model_lineaireS_plot")
golem::add_fct("model_lineaireM_tab")
golem::add_fct("model_logistique_tab")
golem::add_fct("model_logistiqueSS_tab")

#Module5 : série temporelles
golem::add_fct("dygraph_ts")
golem::add_fct("graph_month")
golem::add_fct("cvs_reg_desais")
golem::add_fct("cvs_reg_model")
golem::add_fct("dygraph_reg_cvs")
golem::add_fct("cvs_decompo_graph")
golem::add_fct("cvs_mm_desais")

golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )
golem::use_favicon(path = "favicon.ico")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "grandile", open = FALSE )
usethis::use_data_raw( name = "airpass", open = FALSE )
usethis::use_data_raw( name = "hotel", open = FALSE )
usethis::use_data_raw(name="visiteurs",open = FALSE)
usethis::use_data_raw(name="chomage_bit",open = FALSE)
usethis::use_data_raw(name="accidents",open = FALSE)
usethis::use_data_raw(name="departements",open = FALSE)
usethis::use_data_raw(name="vins",open = FALSE)
usethis::use_data_raw(name="ozone",open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("module5")
usethis::use_vignette("module2")
usethis::use_vignette("module1")
usethis::use_vignette("module4")
usethis::use_vignette("module3")
usethis::use_vignette("module6")
devtools::build_vignettes("module1")

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

