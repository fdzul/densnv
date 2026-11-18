#' mp_animated_mpo
#'
#' Animated map of confirmed dengue cases by municipality and epidemiological
#' week, for the whole country or for one state.
#'
#' @param data SINAVE dengue dataset (e.g. read with
#'   `data.table::fread("DENGUE2_.txt", quote = "", fill = TRUE)`).
#' @param year year to map (ANO column).
#' @param name name of the gif file (without extension);
#'   `"_dot_animated_map.gif"` or `"_areal_animated_map.gif"` is added.
#' @param dir directory where the animation is saved (with "/" at the end).
#' @param country logical. TRUE = all the municipalities of Mexico; FALSE =
#'   the municipalities of the state given in `cve_edo`.
#' @param breaks numeric, step between the breaks of the legend (e.g. 5 gives
#'   1, 6, 11, ...). If NULL, about 5 breaks are computed.
#' @param save logical. If TRUE the gif is saved in `dir`; if FALSE it is only
#'   shown.
#' @param cve_edo INEGI state code ("20" or 20). Required if
#'   `country = FALSE`. See \href{https://www.inegi.org.mx/}{INEGI}.
#' @param text_size size of the number of cases in each municipality.
#' @param dot logical. TRUE = proportional bubbles; FALSE = areal (choropleth)
#'   map.
#' @param pal palette function for the areal map (e.g. `viridis::viridis`).
#'
#' @details
#' Confirmed cases (`ESTATUS_CASO` 2) with final diagnosis DNG, DCSA or DG are
#' counted by municipality of residence and epidemiological week. The
#' animation is built with [tmap::tm_animate()] and saved with
#' [tmap::tmap_animation()] (needs the \pkg{gifski} package).
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a gif file of animation.
#' @export
#'
#' @seealso [tmap::tmap_animation()], [mp_animated_loc()]
#'
#' @examples
#' \dontrun{
#' dataset <- data.table::fread("DENGUE2_.txt", quote = "", fill = TRUE)
#'
#' # Oaxaca, bubbles
#' mp_animated_mpo(data = dataset,
#'                 year = 2026,
#'                 name = "Oaxaca",
#'                 dir = "6.Maps/",
#'                 country = FALSE,
#'                 breaks = 1,
#'                 save = FALSE,
#'                 cve_edo = "20",
#'                 text_size = 0.3,
#'                 dot = TRUE,
#'                 pal = viridis::viridis)
#' }
mp_animated_mpo <- function(data,
                            year,
                            name,
                            dir,
                            country,
                            breaks = NULL,
                            save,
                            cve_edo = NULL,
                            text_size,
                            dot = NULL,
                            pal = NULL){

    # Step 1. aggregate the cases by municipality and week ###
    x <- data |>
        dplyr::filter(ESTATUS_CASO %in% c(2)) |>
        dplyr::filter(ANO %in% c(year))|>
        dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                          "OTROS PAISES DE LATINOAMERICA",
                                          "ESTADOS UNIDOS DE NORTEAMERICA"))|>
        dplyr::filter(DES_DIAG_FINAL %in%
                          c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                            "DENGUE GRAVE"))|>
        dplyr::mutate(CVE_EDO_RES = stringr::str_pad(CVE_EDO_RES,
                                                     width = 2,
                                                     side = "left",
                                                     pad = "0"),
                      CVE_MPO_RES = stringr::str_pad(CVE_MPO_RES,
                                                     width = 3,
                                                     side = "left",
                                                     pad = "0")) |>
        dplyr::group_by(CVE_EDO_RES, CVE_MPO_RES, SEM)|>
        dplyr::summarise(n = dplyr::n(), .groups = 'drop')|>
        tidyr::pivot_wider(id_cols = c(CVE_EDO_RES,CVE_MPO_RES),
                           names_from = SEM,
                           values_from = n,
                           values_fill = 0,
                           names_sort = TRUE)

    # Step 2. load the municipalities and states ####
    mex_sf <- rgeomex::AGEM_inegi19_mx

    edo_sf <- rgeomex::AGEE_inegi19_mx

    # Step 3. joint ####
    # numeric keys: robust to "14", 14, "014" or " 14" in the SINAVE file
    x <- x |>
        dplyr::mutate(ent_i = as.integer(as.numeric(trimws(CVE_EDO_RES))),
                      mun_i = as.integer(as.numeric(trimws(CVE_MPO_RES)))) |>
        dplyr::select(-CVE_EDO_RES, -CVE_MPO_RES)
    y <- rgeomex::AGEM_inegi19_mx |>
        dplyr::mutate(ent_i = as.integer(as.numeric(CVE_ENT)),
                      mun_i = as.integer(as.numeric(CVE_MUN))) |>
        dplyr::left_join(x, by = c("ent_i", "mun_i")) |>
        dplyr::select(-ent_i, -mun_i)

    # Step 4. sustitute the values na by 0 ####
    y[is.na(y)] <- 0

    # Step 5. extract the estate or not
    if(!is.null(cve_edo)){
        cve_edo <- stringr::str_pad(as.character(cve_edo), width = 2,
                                    side = "left", pad = "0")
    }
    if(country == FALSE){
        y <- y |> dplyr::filter(CVE_ENT %in% c(cve_edo))
        edo_sf <- rgeomex::AGEM_inegi19_mx |> dplyr::filter(CVE_ENT %in% c(cve_edo))
        mex_sf <- rgeomex::AGEE_inegi19_mx |> dplyr::filter(CVE_ENT %in% c(as.numeric(cve_edo)))

    } else{
        y
    }

    # Step 5.1. check that there are cases to map ####
    if(nrow(x) == 0){
        stop("There are no confirmed cases (DNG, DCSA, DG) for year ", year, ".")
    }
    if(max(sf::st_drop_geometry(y)[, setdiff(names(x), c("ent_i", "mun_i")), drop = FALSE],
           na.rm = TRUE) < 1){
        stop("There are no confirmed cases in the selected area (cve_edo = ",
             cve_edo, ", year = ", year, "). Check year, cve_edo and the file.")
    }

    # Step 6. Make the dataset tidydaset (pivot_longer) ####
    y <- y|>
        tidyr::pivot_longer(cols = !c(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO, geometry),
                            names_to = "week",
                            values_to = "n")|>
        as.data.frame() |>
        sf::st_set_geometry(value = "geometry")|>
        dplyr::mutate(week = as.numeric(week))|>
        dplyr::mutate(week = forcats::fct_reorder(paste("Semana Epidemiol\u00f3gica",
                                                        week, sep = " "),
                                                  week))

    # breaks of the legend: never from > to, never repeated
    max_n <- max(y$n, na.rm = TRUE)
    by_n  <- if(is.null(breaks) || breaks <= 0) max(1, round(max_n / 5)) else breaks
    ticks <- unique(c(1, seq(from = 1, to = max(max_n, 2), by = by_n)))

    # Step 7. make the map ####
    if(dot == TRUE){
        animated_map <- tmap::tm_shape(shp = mex_sf) +
            tmap::tm_polygons(fill = "gray85",
                              col = "white",
                              lwd = 0.01) +
            tmap::tm_shape(shp = edo_sf) +
            tmap::tm_borders(col = "white",
                             lwd = 0.2) +
            tmap::tm_shape(shp = y |> dplyr::filter(n >= 1)) +
            tmap::tm_bubbles(size = "n",
                             size.scale = tmap::tm_scale_continuous(
                                 values.scale = 2,
                                 ticks = ticks),
                             size.legend = tmap::tm_legend_hide(),
                             # para ver la leyenda usa en su lugar:
                             # size.legend = tmap::tm_legend(title = "Número de Casos"),
                             fill = "n",
                             fill.scale = tmap::tm_scale_continuous(
                                 values = "-viridis",
                                 ticks = ticks),
                             fill.legend = tmap::tm_legend_hide(),
                             fill_alpha = 0.5,
                             lwd = 0.5,
                             col = "black",
                             shape = 21) +
            tmap::tm_text(text = "n",
                          size = text_size,
                          col = "black") +
            tmap::tm_animate(frames = "week", fps = 2) +
            tmap::tm_layout(legend.title.size = 0.5,
                            legend.text.size = 0.7)
        if(save == TRUE){
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 #delay = vel,
                                 filename = paste(dir, paste(name, "dot_animated_map.gif",
                                                             sep = "_"), sep = ""),
                                 width = 1400,
                                 height = 1400)
        } else{
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 #delay = vel,
                                 width = 1400,
                                 height = 1400)
        }
    } else{
        animated_map <- tmap::tm_shape(shp = mex_sf) +
            tmap::tm_polygons(fill = "gray85",
                              col = "white",
                              lwd = 0.01) +
            tmap::tm_shape(shp = edo_sf) +
            tmap::tm_borders(col = "white",
                             lwd = 0.2) +
            tmap::tm_shape(shp = y |> dplyr::filter(n > 0)) +
            tmap::tm_polygons(fill = "n",
                              fill.scale = tmap::tm_scale_continuous(
                                  ticks = ticks,
                                  values = pal(n = max(y$n), direction = -1)),
                              fill.legend = tmap::tm_legend(title = "",
                                                            text.size = 5,
                                                            show = FALSE,
                                                            position =  "bottom",
                                                            orientation = "landscape"),
                              col = "black",
                              lwd = 0.1) +
            tmap::tm_text(text = "n",
                          size = text_size) +
            tmap::tm_animate(frames = "week", fps = 2) +
            tmap::tm_layout(legend.title.size = 0.5,
                            legend.text.size = 0.7)
        if(save == TRUE){
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 #delay = vel,
                                 filename = paste(dir, paste(name, "areal_animated_map.gif",
                                                             sep = "_"), sep = ""),
                                 width = 1400,
                                 height = 1400)
        } else{
            tmap::tmap_animation(animated_map,
                                 dpi = 300,
                                 #delay = vel,
                                 width = 1400,
                                 height = 1400)
        }
    }




}
