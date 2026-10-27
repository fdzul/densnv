#' mp_animated_mpo
#'
#' Animated map areal of dengue cases by municipality
#'
#' @param data is dengue dataset.
#' @param year is the year of dataset
#' @param name is the name of the gif file.
#' @param dir is the directory where the animation will be saved.
#' @param country is logical value for indicating if the map is for all country o by state.
#' @param breaks is the numeric for defining the breaks of legend.
#' @param save is the logical value for indicating if the gif is saved or not.
#' @param cve_edo is the id of state according the \href{**INEGI**}{https://www.inegi.org.mx/default.html}.
#' @param dot is the logical value, if dot TRUE the tmap is dot, else is areal.
#' @param pal is the palette.
#' @param text_size is the size of text in the circle.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a gif file of animation.
#' @export
#'
#' @seealso \link[tmap]{tmap_animation}
#'
#' @examples 1+1
#' @details \link[tmap]{tmap_animation}.
#'
#' @examples
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
    y <- dplyr::left_join(x = rgeomex::AGEM_inegi19_mx,
                          y = x,
                          by = c("CVE_ENT" = "CVE_EDO_RES",
                                 "CVE_MUN" = "CVE_MPO_RES"))

    # Step 4. sustitute the values na by 0 ####
    y[is.na(y)] <- 0

    # Step 5. extract the estate or not
    if(country == FALSE){
        y <- y |> dplyr::filter(CVE_ENT %in% c(cve_edo))
        edo_sf <- rgeomex::AGEM_inegi19_mx |> dplyr::filter(CVE_ENT %in% c(cve_edo))
        mex_sf <- rgeomex::AGEE_inegi19_mx |> dplyr::filter(CVE_ENT %in% c(as.numeric(cve_edo)))

    } else{
        y
    }

    # Step 6. Make the dataset tidydaset (pivot_longer) ####
    y <- y|>
        tidyr::pivot_longer(cols = !c(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO, geometry),
                            names_to = "week",
                            values_to = "n")|>
        as.data.frame() |>
        sf::st_set_geometry(value = "geometry")|>
        dplyr::mutate(week = as.numeric(week))|>
        dplyr::mutate(week = forcats::fct_reorder(paste("Semana Epidemiológica",
                                                        week, sep = " "),
                                                  week))

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
                                 ticks = seq(from = 1, to = max(y$n), by = breaks)),
                             size.legend = tmap::tm_legend_hide(),
                             # para ver la leyenda usa en su lugar:
                             # size.legend = tmap::tm_legend(title = "Número de Casos"),
                             fill = "n",
                             fill.scale = tmap::tm_scale_continuous(
                                 values = "-viridis",
                                 ticks = seq(from = 1, to = max(y$n), by = breaks)),
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
                                  ticks = seq(from = 1, to = max(y$n), by = breaks),
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
