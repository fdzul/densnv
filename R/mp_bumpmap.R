#' Static Bump Map
#'
#' Creates a map with sigmoid curves linking each state (or municipality) to a
#' side ranking of the number of dengue cases (DNG, DWS and SD).
#'
#' @param dataset data.frame with the SINAVE dengue dataset.
#' @param year year to plot (ANO column).
#' @param state logical. TRUE = national map by state; FALSE = municipalities
#'   of the state given in `cve_edo`.
#' @param pal_vir viridis palette option ("viridis", "magma", "inferno",
#'   "plasma", "cividis", "turbo", ...).
#' @param cve_edo two-digit INEGI state code (e.g. "14").
#'   Only used when `state = FALSE`.
#' @param axis_start distance (in degrees) between the map edge and the end of
#'   the curves. Defaults to 5 with `state = TRUE` and 0.5 with `state = FALSE`.
#' @param country_text_x leftward offset of the name. With `state = FALSE` it
#'   is measured from the end of the straight segment (default 0.02).
#' @param country_text_y vertical offset of the name. With `state = FALSE` it
#'   is the height of the text above its line (default 0.005).
#' @param label_line `state = FALSE` only: length (in degrees) of the straight
#'   segment that follows each curve; the municipality name is written on it.
#'   Increase it if long names extend past the segment (default 1.6).
#' @param size_text_value text size of the number of cases.
#' @param size_text_country text size of the name.
#' @param line_size width of the sigmoid curves.
#' @param value_text with `state = FALSE`, gap (in degrees) between the end of
#'   the straight segment and the number of cases (default 0.05).
#' @param label_spacing `state = FALSE` only: vertical spacing (in degrees)
#'   between ranking rows. If NULL, the rows are spread over the height of the
#'   state. Increase it (e.g. 0.04 - 0.08) if names overlap.
#' @param top_n `state = FALSE` only: number of municipalities with the most
#'   cases to show in the ranking. NULL = all.
#' @param label_width `state = FALSE` only: space (in degrees) reserved to the
#'   right of the number of cases so it is not clipped.
#' @param smooth smoothness of the sigmoid curves.
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- data.table::fread("DENGUE2_.txt", quote = "", fill = TRUE)
#' static_bump_map(dataset = dataset, year = 2026, state = TRUE,
#'                 size_text_value = 2, size_text_country = 2,
#'                 country_text_x = 0.5, country_text_y = 0.8,
#'                 line_size = 1.5, pal_vir = "viridis")
#' static_bump_map(dataset = dataset, year = 2026, state = FALSE,
#'                 cve_edo = "14", label_spacing = 0.05,
#'                 size_text_value = 2, size_text_country = 2,
#'                 line_size = 0.6, pal_vir = "turbo")
#' }
mp_bumpmap <- function(dataset,
                       year,
                       state = TRUE,
                       pal_vir = "viridis",
                       cve_edo = NULL,
                       axis_start = NULL,
                       country_text_x = NULL,
                       country_text_y = NULL,
                       size_text_value = 2,
                       size_text_country = 2,
                       line_size = 1,
                       value_text = NULL,
                       label_spacing = NULL,
                       top_n = NULL,
                       label_line = 1.6,
                       label_width = 0.4,
                       smooth = 10){

    # Step 0. validaciones y valores por defecto ####
    if(!state && is.null(cve_edo)){
        stop("Con state = FALSE debes indicar cve_edo (p. ej. cve_edo = \"14\").")
    }
    if(!is.null(cve_edo)){
        cve_edo <- stringr::str_pad(cve_edo, width = 2, side = "left", pad = "0")
    }
    if(is.null(axis_start))     axis_start     <- if(state) 5 else 0.5
    if(is.null(country_text_x)) country_text_x <- if(state) 0.5 else 0.02
    if(is.null(country_text_y)) country_text_y <- if(state) 0 else 0.005
    if(is.null(value_text))     value_text     <- if(state) 0 else 0.05

    # Step 1. casos de dengue del año ####
    df <- dataset |>
        dplyr::filter(ANO == year) |>
        dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                          "OTROS PAISES DE LATINOAMERICA",
                                          "ESTADOS UNIDOS DE NORTEAMERICA")) |>
        dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                            "DENGUE NO GRAVE",
                                            "DENGUE GRAVE")) |>
        dplyr::mutate(CVE_EDO_REP = stringr::str_pad(CVE_EDO_REP, width = 2,
                                                     side = "left", pad = "0"),
                      CVE_MPO_REP = stringr::str_pad(CVE_MPO_REP, width = 3,
                                                     side = "left", pad = "0"))

    # Step 2. unir con los polígonos ####
    if(state){
        df <- df |>
            dplyr::summarise(value = dplyr::n(), .by = CVE_EDO_REP)

        sf_df <- dplyr::left_join(x = rgeomex::AGEE_inegi19_mx,
                                  y = df,
                                  by = c("CVE_ENT" = "CVE_EDO_REP"))
        base_map <- rgeomex::AGEE_inegi19_mx
        outline  <- NULL
    } else {
        df <- df |>
            dplyr::filter(CVE_EDO_REP %in% cve_edo) |>
            dplyr::summarise(value = dplyr::n(),
                             .by = c(CVE_EDO_REP, CVE_MPO_REP))

        sf_df <- dplyr::left_join(x = rgeomex::AGEM_inegi19_mx,
                                  y = df,
                                  by = c("CVE_ENT" = "CVE_EDO_REP",
                                         "CVE_MUN" = "CVE_MPO_REP"))
        base_map <- rgeomex::AGEM_inegi19_mx |>
            dplyr::filter(CVE_ENT %in% cve_edo)
        outline  <- rgeomex::AGEE_inegi19_mx |>
            dplyr::filter(CVE_ENT %in% cve_edo)
    }

    sf_df <- sf_df |>
        dplyr::filter(!is.na(value)) |>
        dplyr::mutate(NOMGEO = stringr::str_trim(NOMGEO, side = "both")) |>
        sf::st_make_valid()

    if(!state && !is.null(top_n)){
        sf_df <- sf_df |>
            dplyr::slice_max(value, n = top_n, with_ties = FALSE)
    }

    if(nrow(sf_df) == 0){
        stop("No hay casos de dengue para los filtros indicados (year / cve_edo).")
    }

    # Step 3. punto interior de cada polígono ####
    # se proyecta a EPSG:6372 (metros) para evitar el warning de lon/lat y se
    # usa solo la geometría para evitar el warning de atributos constantes.
    pts <- sf_df |>
        sf::st_geometry() |>
        sf::st_transform(6372) |>
        sf::st_point_on_surface() |>
        sf::st_transform(sf::st_crs(sf_df)) |>
        sf::st_coordinates() |>
        dplyr::as_tibble()

    # Step 4. posiciones del ranking ####
    n <- nrow(sf_df)
    if(state){
        y_range <- c(10, 35)
        xend    <- -85
    } else {
        bb    <- sf::st_bbox(outline)
        y_mid <- mean(c(bb[["ymin"]], bb[["ymax"]]))
        if(is.null(label_spacing)){
            y_range <- c(bb[["ymin"]], bb[["ymax"]])
        } else {
            half    <- (n - 1) * label_spacing / 2
            y_range <- c(y_mid - half, y_mid + half)
        }
        xend <- bb[["xmax"]]
    }

    # cada polígono recibe un renglón único (sin empates), el mayor arriba
    ranking <- pts |>
        dplyr::mutate(value        = sf_df$value,
                      country      = sf_df$NOMGEO,
                      fine_cap     = if(n > 1){
                          scales::rescale(rank(value, ties.method = "first"),
                                          to = y_range)
                      } else {
                          mean(y_range)
                      },
                      x_axis_start = xend + axis_start)

    sdf <- sf_df |>
        dplyr::mutate(fine_cap = ranking$fine_cap)

    # Step 5. curvas sigmoides (reemplaza a ggbump::geom_sigmoid) ####
    # misma fórmula logística de ggbump, definida aquí para no depender de él
    sigmoid_curves <- function(ranking, smooth = 10, n = 100){
        t <- seq(-smooth, smooth, length.out = n)
        s <- exp(t) / (exp(t) + 1)
        purrr::pmap_dfr(list(ranking$X, ranking$x_axis_start,
                             ranking$Y, ranking$fine_cap,
                             ranking$country),
                        function(x0, x1, y0, y1, id){
                            dplyr::tibble(x = (t + smooth) / (2 * smooth) * (x1 - x0) + x0,
                                          y = s * (y1 - y0) + y0,
                                          country = id,
                                          fine_cap = y1)
                        })
    }
    curves <- sigmoid_curves(ranking, smooth = smooth)

    # Step 6. mapa base ####
    p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = base_map,
                         linewidth = .01,
                         fill = "gray85",
                         color = "white") +
        ggplot2::geom_sf(data = sdf,
                         ggplot2::aes(fill = fine_cap),
                         linewidth = .01,
                         color = "white")

    if(!is.null(outline)){
        p <- p +
            ggplot2::geom_sf(data = outline,
                             linewidth = .3,
                             fill = "transparent",
                             color = "black")
    }

    p <- p +
        # curva del polígono al ranking
        ggplot2::geom_path(data = curves,
                           ggplot2::aes(x = x, y = y,
                                        group = country,
                                        color = fine_cap),
                           alpha = if(state) .3 else .25,
                           linewidth = line_size) +
        # punto dentro del polígono
        ggplot2::geom_point(data = ranking,
                            ggplot2::aes(x = X, y = Y, fill = fine_cap),
                            color = "white",
                            shape = 21,
                            stroke = .5,
                            size = 2)

    # Step 7. etiquetas ####
    if(state){
        # nacional: igual que antes (pocos estados, ggrepel funciona bien)
        p <- p +
            ggrepel::geom_text_repel(data = ranking,
                                     ggplot2::aes(x = x_axis_start - country_text_x,
                                                  y = fine_cap,
                                                  label = country,
                                                  color = fine_cap),
                                     hjust = 1,
                                     direction = "y",
                                     max.overlaps = Inf,
                                     segment.color = NA,
                                     size = size_text_country,
                                     nudge_y = country_text_y) +
            ggplot2::geom_text(data = ranking,
                               ggplot2::aes(x = x_axis_start - value_text,
                                            y = fine_cap,
                                            label = value,
                                            color = fine_cap),
                               hjust = 0,
                               size = size_text_value,
                               nudge_x = 1) +
            ggplot2::coord_sf(clip = "off")
    } else {
        # municipal: SIN repel, para que cada nombre quede sobre SU línea.
        # [curva] ── tramo recto con el nombre encima ── número
        #                    Guadalajara
        #   ~~~~~~~~~~~~~────────────────────  49
        ranking <- ranking |>
            dplyr::mutate(x_line_end = x_axis_start + label_line)

        p <- p +
            # tramo recto: continúa la curva y sirve de "renglón" del nombre
            ggplot2::geom_segment(data = ranking,
                                  ggplot2::aes(x = x_axis_start,
                                               xend = x_line_end,
                                               y = fine_cap,
                                               yend = fine_cap,
                                               color = fine_cap),
                                  alpha = .25,
                                  linewidth = line_size) +
            # nombre del municipio, encima de su línea y alineado a la derecha
            ggplot2::geom_text(data = ranking,
                               ggplot2::aes(x = x_line_end - country_text_x,
                                            y = fine_cap + country_text_y,
                                            label = country,
                                            color = fine_cap),
                               hjust = 1,
                               vjust = 0,
                               size = size_text_country) +
            # número de casos, al final de la línea
            ggplot2::geom_text(data = ranking,
                               ggplot2::aes(x = x_line_end + value_text,
                                            y = fine_cap,
                                            label = value,
                                            color = fine_cap),
                               hjust = 0,
                               vjust = 0.5,
                               size = size_text_value) +
            # se amplía el lienzo para que quepan las líneas, nombres y números
            ggplot2::coord_sf(xlim = c(bb[["xmin"]],
                                       xend + axis_start + label_line +
                                           value_text + label_width),
                              ylim = range(c(bb[["ymin"]], bb[["ymax"]],
                                             y_range + country_text_y)),
                              expand = TRUE,
                              clip = "off")
    }

    p +
        ggplot2::scale_fill_viridis_c(option = pal_vir) +
        ggplot2::scale_color_viridis_c(option = pal_vir) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                       legend.position = "none",
                       plot.caption = ggplot2::element_text(color = "gray40"),
                       plot.title = ggplot2::element_text(color = "gray40",
                                                          size = 16,
                                                          face = "bold"),
                       plot.subtitle = ggplot2::element_text(color = "gray40",
                                                             size = 8))
}
