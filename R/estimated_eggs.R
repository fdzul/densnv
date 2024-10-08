#' Estimated cases of dengue and eggs
#'
#' @param path_dengue It is the path of the dataset of the sinave
#' @param localidad Its the locality target
#' @param year is the year
#' @param fac_axes is the factor for the dual axis
#' @param municipio is the municipality
#' @param path_ovitraps It is the path of the dataset of ovitrap
#'
#' @return
#' @export
#'
#' @examples 1+1
estimated_eggs <- function(path_dengue,
                           localidad,
                           year,
                           fac_axes,
                           municipio,
                           path_ovitraps){
    # Step 1. load the dengue cases data ######
    x <- densnv::read(path = path_dengue,
                      vbd = "DEN",
                      complete = TRUE) |>
        dplyr::filter(ANO == year) |>
        dplyr::filter(DES_MPO_REP %in% c(municipio)) |>
        dplyr::mutate(week = lubridate::epiweek(FEC_INI_SIGNOS_SINT)) |>
        dplyr::group_by(DES_MPO_REP, week, ESTATUS_CASO) |>
        dplyr::summarise(n = dplyr::n(),
                         .groups = "drop") |>
        tidyr::pivot_wider(id_cols = c(DES_MPO_REP, week),
                           names_from = ESTATUS_CASO,
                           values_fill = 0,
                           values_from = n) |>
        dplyr::mutate(Porcentaje_Positividad = round((`2`/(`2` + `3`))*100,
                                                     digits = 0),
                      estimados = ((Porcentaje_Positividad/100) * `1`) + `2`) |>
        dplyr::rename(probable = `1`,
                      confirmado =`2`,
                      descartado = `3`) |>
        dplyr::mutate(estimados = as.numeric(estimados))


    # Step 2. load the ovitraps data ######
    y <- deneggs::ovitraps_read(path = path_ovitraps,
                                current_year = TRUE) |>
        dplyr::mutate(sec_manz = paste(sector, manzana, sep = "")) |>
        dplyr::group_by(week, Localidad) |>
        dplyr::summarise(n_blocks = data.table::uniqueN(sec_manz),
                         n_block_positive = data.table::uniqueN(sec_manz[eggs > 0]),
                         n_ovitraps_install = length(unique(ovitrap)),
                         n_ovitraps_positive = sum(eggs > 0, na.rm = TRUE),
                         n_ovitraps_negative = sum(eggs <= 0, na.rm = TRUE),
                         n_ovitraps_lectura = n_ovitraps_positive + n_ovitraps_negative,
                         sum_ovitrap_positive = sum(eggs, na.rm = TRUE),
                         perc_lectura =  round(n_ovitraps_lectura/n_ovitraps_install, 2)*100,
                         perc_OP = round(n_ovitraps_positive/n_ovitraps_lectura, 2)*100,
                         perc_MP = round(n_block_positive/n_blocks, 2),
                         avg_HOP = round(sum_ovitrap_positive/n_ovitraps_positive,1),
                         avg_HMP = round(sum_ovitrap_positive/n_block_positive,1),
                         .groups = "drop") |>
        dplyr::rename(locality = Localidad) |>
        dplyr::select(week, locality, avg_HOP, avg_HMP, perc_OP, perc_MP, perc_lectura) |>
        dplyr::filter(locality %in% c(localidad))


    ggplot2::ggplot(data = x) +
        ggplot2::geom_line(ggplot2::aes(x = week,
                                        y = estimados),
                           linewidth = 2,
                           color = "#E01A59") +
        ggplot2::geom_point(ggplot2::aes(x = week,
                                         y = estimados),
                            size = 5,
                            fill = "#E01A59",
                            stroke = 0.7,
                            color = "white",
                            shape = 21) +
        ggplot2::geom_line(data = y,
                           ggplot2::aes(x = week,
                                        y = avg_HMP/fac_axes),
                           linewidth = 2,
                           col = "#0E5F76") +
        ggplot2::geom_point(data = y,
                            ggplot2::aes(x = week,
                                         y = avg_HMP/fac_axes),
                            size = 5,
                            shape = 21,
                            col = "white",
                            stroke = 0.7,
                            fill = "#0E5F76") +
        #ggplot2::facet_wrap("DES_MPO_RES",
        #                    scales = "free_y") +
        ggplot2::theme(legend.position = c(.1, .9),
                       legend.background = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = seq(from = 0,
                                                 to = lubridate::epiweek(Sys.Date()),
                                                 by = 2)) +
        ggplot2::ylab("Número de Casos Estimados") +
        ggplot2::xlab("Semanas Epidemiológicas") +
        ggplot2::theme(strip.text = ggplot2::element_text(size = 12)) +
        ggplot2::scale_y_continuous(name = "Número de Casos Estimados",
                                    # Add a second axis and specify its features
                                    sec.axis = ggplot2::sec_axis(~.*fac_axes,
                                                                 name="Promedio de Huevos por Manzana")) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(color = "#E01A59",
                                                            size=13),
                       axis.title.y.right = ggplot2::element_text(color = "#0E5F76",
                                                                  size=13),
                       axis.text.y.right = ggplot2::element_text(color = "#0E5F76",
                                                                 size=10),
                       axis.text.y = ggplot2::element_text(color = "#E01A59",
                                                           size=10))
}
