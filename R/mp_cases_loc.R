#' Interactive map of cases by locality
#'
#' Leaflet map with proportional circles of arbovirus cases by locality, state
#' and municipal boundaries, and a panel with the localities with the most
#' cases. Clicking a row of the panel zooms the map to that locality.
#'
#' @param data sf object returned by [cases_by_loc()]. To map specific
#'   epidemiological weeks, filter them there with its `week` argument
#'   (e.g. `cases_by_loc(..., week = 1:10)`).
#' @param var name of the column to map (default `"probable"`; also
#'   `"confirmado"`, `"DNG"`, `"DCSA"`, `"DG"`, `"DENV1"`, ...).
#' @param top_n number of localities shown in the panel (default 10). With
#'   more than 20, the panel shows 20 rows and a scroll bar.
#' @param title title of the legend and the panel (default `"Casos Probables"`).
#' @param palette viridis option for the circles (default `"D"`).
#' @param r_min,r_max minimum and maximum radius of the circles in pixels.
#' @param cve_edo INEGI state code ("14" or 14). If NULL (default), the whole
#'   country is mapped.
#' @param mun_boundaries logical. Draw the municipal boundaries
#'   (`rgeomex::AGEM_inegi19_mx`). Default TRUE.
#' @param edo_boundaries logical. Draw the state boundaries
#'   (`rgeomex::AGEE_inegi19_mx`). Default TRUE.
#' @param table_position position of the panel: `"topright"` (default),
#'   `"topleft"`, `"bottomleft"` or `"bottomright"`.
#'
#' @details
#' Boundaries are drawn below the circles and can be turned on and off from the
#' layers control. Hovering over a municipality shows its name. The home
#' button returns to the state (if `cve_edo` is given) or to Mexico.
#'
#' @return a leaflet htmlwidget.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' den <- cases_by_loc(path = "DENGUE2_.txt", snv_dataset = "DENV")
#' mp_cases_loc(den)
#'
#' # Oaxaca, weeks 1 to 10, confirmed cases
#' den_w <- cases_by_loc(path = "DENGUE2_.txt", snv_dataset = "DENV",
#'                       week = 1:10)
#' mp_cases_loc(den_w, var = "confirmado", title = "Casos Confirmados",
#'              cve_edo = "20", top_n = 15)
#' }
mp_cases_loc <- function(data,
                         var = "probable",
                         top_n = 10,
                         title = "Casos Probables",
                         palette = "D",
                         r_min = 4,
                         r_max = 20,
                         cve_edo = NULL,
                         mun_boundaries = TRUE,
                         edo_boundaries = TRUE,
                         table_position = "topright"){

    # Step 0. boundaries and state filter ####
    edo <- rgeomex::AGEE_inegi19_mx |> sf::st_transform(4326)
    mun <- rgeomex::AGEM_inegi19_mx |> sf::st_transform(4326)

    if(!is.null(cve_edo)){
        cve_edo <- stringr::str_pad(as.character(cve_edo), width = 2,
                                    side = "left", pad = "0")
        edo  <- edo |> dplyr::filter(CVE_ENT %in% cve_edo)
        mun  <- mun |> dplyr::filter(CVE_ENT %in% cve_edo)
        data <- data |> sf::st_transform(4326)
        data <- data[edo, ]
    }

    # home: the state if cve_edo is given, otherwise Mexico
    bb <- if(!is.null(cve_edo)) sf::st_bbox(edo) else
        c(xmin = -118.4, ymin = 14.5, xmax = -86.7, ymax = 32.7)

    # Step 1. prepare the data ####
    data <- data |>
        dplyr::mutate(n = .data[[var]]) |>
        dplyr::filter(n >= 1) |>
        dplyr::arrange(dplyr::desc(n))       # big circles below, small on top

    if(nrow(data) == 0){
        stop("There are no localities with ", var, " >= 1.")
    }

    # if the names are missing, recover them from ITER 2020 by cvegeo
    if(!all(c("nom_loc", "nom_mun") %in% names(data))){
        nombres <- rgeomex::iter_2020_loc |>
            as.data.frame() |>
            dplyr::mutate(cvegeo = paste0(entidad, mun, loc)) |>
            dplyr::select(cvegeo, dplyr::any_of(c("nom_mun", "nom_loc"))) |>
            dplyr::distinct(cvegeo, .keep_all = TRUE)
        data <- data |>
            dplyr::select(-dplyr::any_of(c("nom_mun", "nom_loc"))) |>
            dplyr::left_join(nombres, by = "cvegeo")
    }

    has <- function(col) col %in% names(data)
    nom_loc <- if(has("nom_loc")) dplyr::coalesce(data$nom_loc, data$cvegeo) else data$cvegeo
    nom_mun <- if(has("nom_mun")) data$nom_mun else ""
    xy      <- sf::st_coordinates(data)

    # Step 2. colour palette and radius ####
    pal <- leaflet::colorNumeric(palette = viridis::viridis(10, option = palette),
                                 domain = data$n)

    radio <- function(x){
        if(max(x) == min(x)) return(rep((r_min + r_max) / 2, length(x)))
        r_min + (x - min(x)) / (max(x) - min(x)) * (r_max - r_min)
    }

    # Step 3. popup (only the columns that exist) ####
    fila <- function(lab, col){
        if(has(col)) paste0("<b>", lab, ":</b> ", data[[col]]) else NULL
    }
    popup <- paste0(
        "<b>Municipio:</b> ", nom_mun, "<br>",
        "<b>Localidad:</b> ", nom_loc, "<br>",
        "<b>Probables:</b> ", data$probable, "<br>",
        "<b>Confirmados:</b> ", data$confirmado,
        if(has("DNG")) paste0("<br>", fila("DNG", "DNG"), " | ",
                              fila("DCSA", "DCSA"), " | ",
                              fila("DG", "DG")) else "",
        if(has("DENV1")) paste0("<br>", fila("DENV1", "DENV1"), " | ",
                                fila("DENV2", "DENV2"), " | ",
                                fila("DENV3", "DENV3"), " | ",
                                fila("DENV4", "DENV4")) else "")

    # Step 4. panel with the top localities ####
    top <- utils::head(data.frame(rank = seq_len(nrow(data)),
                                  loc = nom_loc,
                                  n = data$n,
                                  lng = xy[, 1],
                                  lat = xy[, 2]),
                       top_n)

    # same colour as the circle on the map; dark text on light colours
    top_col <- pal(top$n)
    lum     <- colSums(grDevices::col2rgb(top_col) * c(0.299, 0.587, 0.114))
    txt_col <- ifelse(lum > 150, "#222222", "white")

    # more than 20 localities: 20 rows visible and a scroll bar
    scroll <- if(nrow(top) > 20) "max-height:400px;overflow-y:auto;" else ""

    filas <- paste0(
        "<tr onclick='flyLoc(", top$lat, ",", top$lng, ")' ",
        "style='cursor:pointer;'>",
        "<td style='text-align:right;color:#888;padding:2px 4px 2px 0;'>", top$rank, "</td>",
        "<td style='padding:2px 6px 2px 0;white-space:nowrap;'><b>", top$loc, "</b></td>",
        "<td style='text-align:right;padding:2px 0;'>",
        "<span style='display:inline-block;min-width:24px;padding:0 5px;",
        "border-radius:4px;text-align:center;color:", txt_col,
        ";background:", grDevices::adjustcolor(top_col, alpha.f = 0.8),
        ";'>", top$n, "</span></td>",
        "</tr>",
        collapse = "")

    tabla <- paste0(
        "<details open style='font-family:Helvetica,Arial,sans-serif;font-size:11px;",
        "width:max-content;'>",
        "<summary style='cursor:pointer;font-weight:bold;font-size:11px;",
        "color:white;background:#0E5F76;padding:3px 6px;border-radius:4px;'>",
        "Top ", top_n, " · ", title, "</summary>",
        "<div style='", scroll, "margin-top:4px;'>",
        "<table style='border-collapse:collapse;width:auto;'>",
        "<thead><tr style='border-bottom:1px solid #ccc;color:#555;'>",
        "<th style='padding:0 4px 0 0;'>#</th>",
        "<th style='text-align:left;padding:0 6px 0 0;'>Localidad</th>",
        "<th style='padding:0;'>Casos</th>",
        "</tr></thead><tbody>", filas, "</tbody></table></div>",
        "<div style='color:#999;font-size:9px;margin-top:3px;'>",
        "Clic en una fila para ir a la localidad</div></details>")

    # Step 5. map ####
    overlays <- c(if(edo_boundaries) "Límite estatal",
                  if(mun_boundaries) "Límite municipal",
                  title)

    m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Positron") |>
        leaflet::addProviderTiles("Esri.WorldImagery",    group = "Satelite") |>
        leaflet::addProviderTiles("OpenStreetMap",        group = "OpenStreetMap") |>
        leaflet::addProviderTiles("CartoDB.DarkMatter",   group = "Dark") |>
        leaflet::addProviderTiles("Esri.WorldTopoMap",    group = "Topografico") |>
        leaflet::addMapPane("limites", zIndex = 410) |>   # below the circles
        leaflet::addMapPane("casos",   zIndex = 420)

    # municipal boundaries (below the state ones)
    if(mun_boundaries){
        m <- m |>
            leaflet::addPolygons(data = mun,
                                 color = "gray50",
                                 weight = 0.5,
                                 fill = TRUE,
                                 fillOpacity = 0,
                                 label = stringr::str_trim(mun$NOMGEO),
                                 highlightOptions = leaflet::highlightOptions(
                                     color = "#0E5F76", weight = 2,
                                     bringToFront = FALSE),
                                 options = leaflet::pathOptions(pane = "limites"),
                                 group = "Límite municipal")
    }

    # state boundaries
    if(edo_boundaries){
        m <- m |>
            leaflet::addPolylines(data = edo,
                                  color = "black",
                                  weight = 1.5,
                                  opacity = 0.8,
                                  options = leaflet::pathOptions(pane = "limites"),
                                  group = "Límite estatal")
    }

    m |>
        # dengue cases by locality (on top of the boundaries)
        leaflet::addCircleMarkers(data = data,
                                  radius = radio(data$n),
                                  color = "white",
                                  weight = 0.5,
                                  fillColor = pal(data$n),
                                  fillOpacity = 0.8,
                                  popup = popup,
                                  label = paste0(nom_loc, ": ", data$n, " ",
                                                 tolower(title)),
                                  labelOptions = leaflet::labelOptions(
                                      style = list("font-size" = "12px"),
                                      direction = "auto"),
                                  options = leaflet::pathOptions(pane = "casos"),
                                  group = title) |>
        leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
        # home button: back to the state or to Mexico
        leaflet::addEasyButton(leaflet::easyButton(
            icon = "fa-home",
            title = if(!is.null(cve_edo)) "Regresar al estado" else "Regresar a México",
            position = "topleft",
            onClick = htmlwidgets::JS(paste0(
                "function(btn, map){ map.flyToBounds([[",
                bb[["ymin"]], ",", bb[["xmin"]], "],[",
                bb[["ymax"]], ",", bb[["xmax"]], "]]); }")))) |>
        leaflet::addLegend(position = "topleft",
                           pal = pal,
                           values = data$n,
                           title = gsub(" ", "<br>", title),
                           opacity = 0.8) |>
        leaflet::addLayersControl(baseGroups = c("Positron", "Satelite",
                                                 "OpenStreetMap", "Dark",
                                                 "Topografico"),
                                  overlayGroups = overlays,
                                  position = "topright",
                                  options = leaflet::layersControlOptions(
                                      collapsed = TRUE)) |>
        leaflet::addControl(html = tabla,
                            position = table_position,
                            className = "info legend") |>
        leaflet::addScaleBar(position = "bottomleft") |>
        # click on the panel -> zoom to the locality
        htmlwidgets::onRender("
            function(el, x) {
                var map = this;
                window.flyLoc = function(lat, lng) {
                    map.flyTo([lat, lng], 14);
                };
            }")
}
