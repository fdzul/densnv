#' mp_heatmap
#'
#'
#'
#' @param locality is the locality locality target.
#' @param cve_edo is a string of the state id.
#' @param geocoded_datasets is geocoded dataset.
#' @param status_caso It is a numerical value to select the Status of the case. There are three options, 1 for the probable cases database, 2 for the confirmed cases database, and 3 for the discarded cases database.
#' @param zoom map zoom; an integer from 3 (continent) to 21 (building), default value 10 (city).
#' @param week It is epidemiological week.
#' @param alpha is a numerical parameter that controls the transparency of the heatmap. Values range from 0 to 1, where 0 is completely transparent and 1 does not make the heat map transparent.
#' @param map_type character string providing google map theme. options available are "terrain", "satellite", "roadmap", and "hybrid"
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return ggplot object
#' @export
#' @import dplyr
#' @import rgeomex
#' @import sf
#' @import ggmap
#' @import ggplot2
#' @import cowplot
#'
#' @examples
mp_heatmap <- function(locality,
                       cve_edo,
                       geocoded_datasets,
                       status_caso,
                       zoom,
                       week,
                       alpha,
                       map_type){

    # Step 1. transform dataset #####
    z <- geocoded_datasets |>
        dplyr::filter(ESTATUS_CASO %in% c(status_caso)) |>
        dplyr::mutate(x = long,
                      y = lat) |>
        sf::st_as_sf(coords = c("long", "lat"),
                     crs = 4326)

    # Step 2. extract the locality ####
    locality <- rgeomex::extract_locality(cve_edo = cve_edo,
                                          locality = locality)

    # Step 3. extract the geocoded cases of merida ####
    z <- z[locality, ]  |>
        sf::st_drop_geometry() |>
        dplyr::filter(SEM %in% c(week))


    a <- sf::st_centroid(sf::st_union(locality)) |>
        sf::st_coordinates()


    ggmap_sf <- ggmap::get_googlemap(c(as.vector(a)[1],
                                       as.vector(a)[2]),
                                     zoom = zoom,
                                     maptype = map_type)

    ggmap::ggmap(ggmap_sf) +
        ggplot2::stat_density_2d(data = z,
                                 ggplot2::aes(x = x,
                                              y = y,
                                              fill = ..level..),
                                 geom = "polygon",
                                 alpha = alpha) +
        ggplot2::scale_fill_gradient(low = "green",
                                     high = "red",
                                     guide = "none") +
        cowplot::theme_map()

}
