pkgname <- "asta"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "asta-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('asta')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("boxplot_tirage_m2")
### * boxplot_tirage_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: boxplot_tirage_m2
### Title: boxplot_tirage_m2
### Aliases: boxplot_tirage_m2

### ** Examples

boxplot_tirage_m2(grandile, "PATRIMOINE")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("boxplot_tirage_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cvs_decompo_graph")
### * cvs_decompo_graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cvs_decompo_graph
### Title: cvs_decompo_graph
### Aliases: cvs_decompo_graph

### ** Examples

cvs_decompo_graph(airpass,type="multiplicative")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cvs_decompo_graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cvs_mm_desais")
### * cvs_mm_desais

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cvs_mm_desais
### Title: cvs_mm_desais
### Aliases: cvs_mm_desais

### ** Examples

cvs_mm_desais(hotel)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cvs_mm_desais", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cvs_reg_desais")
### * cvs_reg_desais

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cvs_reg_desais
### Title: cvs_reg_desais
### Aliases: cvs_reg_desais

### ** Examples

x <- airpass
y <- log(airpass)  
ycvs <- cvs_reg_desais(y)
exp(ycvs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cvs_reg_desais", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cvs_reg_model")
### * cvs_reg_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cvs_reg_model
### Title: cvs_reg_model
### Aliases: cvs_reg_model

### ** Examples

reg_airpass <- cvs_reg_model(log(airpass))
summary(reg_airpass)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cvs_reg_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("data_apur_m2")
### * data_apur_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: data_apur_m2
### Title: data_apur_m2
### Aliases: data_apur_m2

### ** Examples

data_apur_m2(grandile_apur, var_apur = "SUPERF_LOG")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("data_apur_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dygraph_reg_cvs")
### * dygraph_reg_cvs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dygraph_reg_cvs
### Title: dygraph_reg_cvs
### Aliases: dygraph_reg_cvs

### ** Examples

x <- airpass
y <- log(airpass)  
ycvs <- cvs_reg_desais(y)
xcvs <- exp(ycvs)
dygraph_reg_cvs(x,xcvs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dygraph_reg_cvs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dygraph_ts")
### * dygraph_ts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dygraph_ts
### Title: dygraph_ts
### Aliases: dygraph_ts

### ** Examples

dygraph_ts(airpass)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dygraph_ts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format_box")
### * format_box

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format_box
### Title: format_box
### Aliases: format_box

### ** Examples

moy <- mean(grandile$AGE)
format_box(moy)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format_box", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("graph_month")
### * graph_month

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: graph_month
### Title: graph_month
### Aliases: graph_month

### ** Examples

graph_month(airpass)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("graph_month", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("graphggplotly_diagbarre")
### * graphggplotly_diagbarre

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: graphggplotly_diagbarre
### Title: graphggplotly_diagbarre
### Aliases: graphggplotly_diagbarre

### ** Examples

graphggplotly_diagbarre(grandile,var = "PCS")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("graphggplotly_diagbarre", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("graphggplotly_histo")
### * graphggplotly_histo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: graphggplotly_histo
### Title: graphggplotly_histo
### Aliases: graphggplotly_histo

### ** Examples

graphggplotly_histo(grandile,"REV_DISPONIBLE",10)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("graphggplotly_histo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("graphggplotly_nuage")
### * graphggplotly_nuage

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: graphggplotly_nuage
### Title: graphggplotly_nuage
### Aliases: graphggplotly_nuage

### ** Examples

graphggplotly_nuage(grandile,"REV_DISPONIBLE","PATRIMOINE")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("graphggplotly_nuage", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("graphggplotly_qualiquanti")
### * graphggplotly_qualiquanti

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: graphggplotly_qualiquanti
### Title: graphggplotly_qualiquanti
### Aliases: graphggplotly_qualiquanti

### ** Examples

graphggplotly_qualiquanti(grandile,"PCS","REV_DISPONIBLE")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("graphggplotly_qualiquanti", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("histoplotly_simul")
### * histoplotly_simul

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: histoplotly_simul
### Title: histoplotly_simul
### Aliases: histoplotly_simul

### ** Examples

a <- simul_moyenne(grandile,"AGE",1000,1000)
histoplotly_simul(a,"AGE")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("histoplotly_simul", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("model_lineaireM_resid")
### * model_lineaireM_resid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: model_lineaireM_resid
### Title: model_lineaireM_resid
### Aliases: model_lineaireM_resid

### ** Examples

model_lineaireM_resid(iris, "Petal.Length", c("Sepal.Length", "Petal.Width", "Sepal.Width"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("model_lineaireM_resid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("model_lineaireM_tab")
### * model_lineaireM_tab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: model_lineaireM_tab
### Title: model_lineaireM_tab
### Aliases: model_lineaireM_tab

### ** Examples

model_lineaireM_tab(iris, "Petal.Length", c("Sepal.Length", "Petal.Width", "Sepal.Width"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("model_lineaireM_tab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("model_lineaireS_plot")
### * model_lineaireS_plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: model_lineaireS_plot
### Title: model_lineaireS_plot
### Aliases: model_lineaireS_plot

### ** Examples

model_lineaireS_plot(grandile, "REV_DISPONIBLE", "PATRIMOINE", TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("model_lineaireS_plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("model_lineaireS_tab")
### * model_lineaireS_tab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: model_lineaireS_tab
### Title: model_lineaireS_tab
### Aliases: model_lineaireS_tab

### ** Examples

model_lineaireS_tab(grandile, "REV_DISPONIBLE", "PATRIMOINE", TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("model_lineaireS_tab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("model_logistiqueSS_tab")
### * model_logistiqueSS_tab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: model_logistiqueSS_tab
### Title: model_logistiqueSS_tab
### Aliases: model_logistiqueSS_tab

### ** Examples

model_logistiqueSS_tab(input_data = titanic, 
var_expliquee = "Survived", 
var_explicatives = c("Sex","Age"), constante = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("model_logistiqueSS_tab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("model_logistique_tab")
### * model_logistique_tab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: model_logistique_tab
### Title: model_logistique_tab
### Aliases: model_logistique_tab

### ** Examples

model_logistiqueSS_tab(input_data = titanic, 
var_expliquee = "Survived", 
var_explicatives = c("Sex","Age"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("model_logistique_tab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_apur_m2")
### * plot_apur_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_apur_m2
### Title: plot_apur_m2
### Aliases: plot_apur_m2

### ** Examples

plot_apur_m2(grandile_apur,var_apur =  "SUPERF_LOG")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_apur_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pond_m2")
### * pond_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pond_m2
### Title: table_ponderee
### Aliases: pond_m2

### ** Examples

pond_m2(grandile_redress, "ACT")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pond_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rambarplot_quali")
### * rambarplot_quali

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rambarplot_quali
### Title: rambarplot_quali
### Aliases: rambarplot_quali

### ** Examples

rambarplot_quali(grandile,"LIB_PCS")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rambarplot_quali", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simul_moyenne")
### * simul_moyenne

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simul_moyenne
### Title: simul_moyenne
### Aliases: simul_moyenne

### ** Examples

 simul_moyenne(grandile,"AGE",1000,1000) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simul_moyenne", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tab_moyenne")
### * tab_moyenne

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tab_moyenne
### Title: tab_moyenne
### Aliases: tab_moyenne

### ** Examples

tab_moyenne(grandile,"PCS","AGE")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tab_moyenne", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tableau_croise")
### * tableau_croise

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tableau_croise
### Title: tableau_croise
### Aliases: tableau_croise

### ** Examples

tableau_croise(grandile,"DIPL","PCS",ligne=TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tableau_croise", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tirage_degres_m2")
### * tirage_degres_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tirage_degres_m2
### Title: tirage_degres_m2
### Aliases: tirage_degres_m2

### ** Examples

tirage_degres_m2(grandile, 2,500, "DIPL")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tirage_degres_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tirage_grappe_m2")
### * tirage_grappe_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tirage_grappe_m2
### Title: Tirage_grappe_m2
### Aliases: tirage_grappe_m2

### ** Examples

tirage_grappe_m2(grandile, 4, "DIPL")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tirage_grappe_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tirage_sas_m2")
### * tirage_sas_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tirage_sas_m2
### Title: tirage_sas_m2
### Aliases: tirage_sas_m2

### ** Examples

tirage_sas_m2(grandile, 12)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tirage_sas_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tirage_strat_m2")
### * tirage_strat_m2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tirage_strat_m2
### Title: tirage_strat_m2
### Aliases: tirage_strat_m2

### ** Examples

tirage_strat_m2(grandile, 400, "DIPL")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tirage_strat_m2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tri_plat")
### * tri_plat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tri_plat
### Title: tri_plat
### Aliases: tri_plat

### ** Examples

tri_plat(grandile,"LIB_PCS")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tri_plat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
