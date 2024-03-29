#' tirage_degres_m2 
#'
#' @description A fct function that does a cluster sampling with 2 sample random sampling
#' @param input_data un dataframe
#' @param taille_UP un nombre d'unités primaires
#' @param taille_tot un nombre d'unités échantillonnées total
#' @param var_degres une variable de stratification
#'
#' @return a dataframe, result of a cluster sampling with 2 samplings
#'
#' @import dplyr
#' @export
#' @examples tirage_degres_m2(grandile, 2,500, "DIPL")
tirage_degres_m2 <-
  function(input_data,
           taille_UP,
           taille_tot,
           var_degres) {
    UP <-
      input_data %>% group_by(.data[[var_degres]]) %>%
      summarise(Effectifs = n()) %>% select(.data[[var_degres]])
    
    sond1 <-
      UP[sample(1:nrow(UP), taille_UP), ] %>% mutate(ECH = "1")  %>%
      right_join(input_data, by = var_degres) %>%
      dplyr::filter(ECH == "1")
    
    sond2 <-  as.data.frame(tirage_sas_m2(sond1, taille_tot))
    sond2
  }




