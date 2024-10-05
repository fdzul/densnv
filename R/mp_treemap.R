#' Treemap of dengue cases
#'
#' @param snv_dataset is the sinave dataset
#' @param country It is a logical value to indicate whether the data of the SINAVE is from the country (TRUE) or from a specific state (FALSE)
#' @param cve_edo is the state id of INEGI.
#'
#' @return
#' @export
#'
#' @examples
mp_treemap <- function(snv_dataset, country, cve_edo = NULL){
    if(country == TRUE){
        snv_dataset  |>
            dplyr::filter(ANO == 2024)  |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA"))  |>
            dplyr::filter(DES_DIAG_FINAL %in%
                              c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                                "DENGUE GRAVE"))  |>
            dplyr::group_by(DES_EDO_RES,DES_DIAG_FINAL)  |>
            dplyr::summarise(value = dplyr::n(),
                             .groups = "drop")  |>
            dplyr::mutate(DES_EDO_RES = stringr::str_to_title(DES_EDO_RES),
                          DES_DIAG_FINAL = stringr::str_to_title(DES_DIAG_FINAL))  |>
            dplyr::mutate(DES_DIAG_FINAL = factor(DES_DIAG_FINAL,
                                                  levels = c("Dengue Con Signos De Alarma",
                                                             "Dengue Grave",
                                                             "Dengue No Grave"),
                                                  labels = c("DSA", "DG", "DNG")))  |>
            ggplot2::ggplot(ggplot2::aes(area = value,
                                         fill = DES_EDO_RES,
                                         subgroup = DES_EDO_RES,
                                         subgroup2 = value,
                                         label = paste(DES_DIAG_FINAL, value, sep = "\n"))) +
            treemapify::geom_treemap() +
            treemapify::geom_treemap_subgroup_border(color = "white",
                                                     size = 2) +
            treemapify::geom_treemap_subgroup2_border(color = "white",
                                                      size = 0.3) +
            treemapify::geom_treemap_text(fontface = "italic",
                                          colour = "black",
                                          place = "bottom",
                                          alpha = 0.5,
                                          size = 10,
                                          grow = F) +
            treemapify::geom_treemap_subgroup_text(place = "middle",
                                                   colour = "White",
                                                   #alpha = 0.8,
                                                   grow = T)+
            ggplot2::theme(legend.position = "none") +
            ggplot2::scale_fill_viridis_d()
    } else {
        snv_dataset  |>
            dplyr::filter(ANO == 2024)  |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA"))  |>
            dplyr::filter(DES_DIAG_FINAL %in%
                              c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                                "DENGUE GRAVE"))  |>
            dplyr::filter(CVE_EDO_REP %in% c(cve_edo)) |>
            dplyr::group_by(DES_MPO_RES,DES_DIAG_FINAL)  |>
            dplyr::summarise(value = dplyr::n(),
                             .groups = "drop")  |>
            dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES),
                          DES_DIAG_FINAL = stringr::str_to_title(DES_DIAG_FINAL))  |>
            dplyr::mutate(DES_DIAG_FINAL = factor(DES_DIAG_FINAL,
                                                  levels = c("Dengue Con Signos De Alarma",
                                                             "Dengue Grave",
                                                             "Dengue No Grave"),
                                                  labels = c("DSA", "DG", "DNG")))  |>
            ggplot2::ggplot(ggplot2::aes(area = value,
                                         fill = DES_MPO_RES,
                                         subgroup = DES_MPO_RES,
                                         subgroup2 = value,
                                         #label = paste(DES_DIAG_FINAL, value, sep = "\n"),
                                         label = paste(DES_DIAG_FINAL, value, sep = "\n"))) + #DES_DIAG_FINAL
            treemapify::geom_treemap() +
            treemapify::geom_treemap_subgroup_border(color = "white",
                                                     size = 2) +
            treemapify::geom_treemap_subgroup2_border(color = "white",
                                                      size = 0.3) +
            treemapify::geom_treemap_text(fontface = "italic",
                                          colour = "black",
                                          place = "bottom",
                                          size = 10,
                                          alpha = 0.5,
                                          grow = F) +
            treemapify::geom_treemap_subgroup_text(place = "middle",
                                                   colour = "White",
                                                   alpha = 1,
                                                   grow = T) +
            ggplot2::theme(legend.position = "none") +
            ggplot2::scale_fill_viridis_d()
    }
}
