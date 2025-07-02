#' The mp_heatmap function generates a heat map.
#'
#' @param locality is the locality locality target.
#' @param cve_edo is a string of the state id.
#' @param geocoded_datasets is geocoded dataset.
#' @param status_caso It is a numerical value to select the Status of the case. There are three options, 1 for the probable cases database, 2 for the confirmed cases database, and 3 for the discarded cases database.
#' @param zoom map zoom; an integer from 3 (continent) to 21 (building), default value 10 (city).
#' @param week It is epidemiological week.
#' @param kernel It is kernel density for select the blocks.
#' @param alpha is a numerical parameter that controls the transparency of the heatmap. Values range from 0 to 1, where 0 is completely transparent and 1 does not make the heat map transparent.
#' @param map_type character string providing google map theme. options available are "terrain", "satellite", "roadmap", and "hybrid"
#' @param static is a logical valur, if static == TRUE the map is static, else (statis = FALSE) the maps is interactive.
#' @param palette is the palette for kde. example palette = viridis::turbo.
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
#' @examples 1+1
mp_heatmap <- function(locality,
                       cve_edo,
                       geocoded_datasets,
                       status_caso,
                       week,
                       kernel,
                       alpha,
                       zoom,
                       map_type,
                       static,
                       palette = NULL){

    # Step 1. transform dataset #####
    z <- geocoded_datasets |>
        dplyr::filter(accuracy != "locality") |>
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
        #sf::st_drop_geometry() |>
        dplyr::filter(SEM %in% c(week))

    if(static == TRUE) {
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
                                                  fill = ggplot2::after_stat(level)),
                                     geom = "polygon",
                                     alpha = alpha) +
            ggplot2::scale_fill_gradient(low = "#A2FC3CFF",
                                         high = "#E01E5A",
                                         guide = "none") +
            cowplot::theme_map()
    } else{

        # Step 3. extract the geocoded cases of locality
        confirmados <- z |>
            dplyr::filter(ESTATUS_CASO == 2)
        probables <- z |>
            dplyr::filter(ESTATUS_CASO == 1)

        # Step 4. Create kernel density output
        kde <- KernSmooth::bkde2D(x = cbind(z$x, z$y),
                                  bandwidth = c(0.0045, 0.0068),
                                  gridsize = c(500,500)) # 500

        # Step 5. Create Raster from Kernel Density output
        KernelDensityRaster <- raster::raster(list(x = kde$x1 ,
                                                   y = kde$x2 ,
                                                   z = kde$fhat))

        # Step 6. convert the raster to rast
        kde_rast <- KernelDensityRaster |>
            terra::rast() |>
            terra::mask(mask = locality)

        # Step 1. load the blocks ####

        if(cve_edo %in% c("01", "02", "03", "04", "05", "06", "07",
                          "08", "09", "10")){
            blocks <- rgeomex::blocks_ine20_mx_a |>
                dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
                sf::st_make_valid()
        }

        if(cve_edo %in% c("11", "12", "13", "14")){
            blocks <- rgeomex::blocks_ine20_mx_b |>
                dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
                sf::st_make_valid()
        }

        if(cve_edo %in% c("15", "16", "17", "18", "19")){
            blocks <- rgeomex::blocks_ine20_mx_c |>
                dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
                sf::st_make_valid()
        }

        if(cve_edo %in% c("20", "21", "22",
                          "23", "24", "25")){
            blocks <- rgeomex::blocks_ine20_mx_d |>
                dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
                sf::st_make_valid()
        }

        if(cve_edo %in% c("26", "27", "28", "29", "30", "31", "32")){
            blocks <- rgeomex::blocks_ine20_mx_e |>
                dplyr::filter(entidad %in% c(as.numeric(cve_edo))) |>
                sf::st_make_valid()
        }

        # Step 2. load the kernel density ###
        library(tidyterra)
        kde_rast_b <- kde_rast |>
            dplyr::filter(layer >= kernel) |>
            ppmData::rast_to_sf()

        # Step 3 extract the blocks in the heatmaps ###
        blocks_heatmap <- blocks[kde_rast_b,]

        # Step 7. make the map
        if(nrow(probables) == 0){
            mapview::mapview(locality,
                             col.regions = "#4662D7FF",
                             alpha.regions = alpha,
                             legend = FALSE,
                             layer.name = "Límite de la Ciudad") +
            mapview::mapview(kde_rast,
                             layer.name = "kde",
                             na.color = "transparent",
                             alpha.regions = alpha,
                             legend = FALSE,
                             col.regions = palette,
                             trim = TRUE) +
                mapview::mapview(confirmados,
                                 col.regions = "#E01E5A",
                                 alpha.regions = alpha,
                                 color = "white",
                                 layer.name = "Confirmado") +
                mapview::mapview(blocks_heatmap,
                                 alpha.regions = 0.1,
                                 color = "white",
                                 legend = FALSE,
                                 layer.name = "Manzana")
        } else if(nrow(confirmados) == 0) {
            mapview::mapview(locality,
                             col.regions = "#4662D7FF",
                             alpha.regions = alpha,
                             legend = FALSE,
                             layer.name = "Límite de la Ciudad") +
            mapview::mapview(kde_rast,
                             layer.name = "kde",
                             na.color = "transparent",
                             alpha.regions = alpha,
                             legend = FALSE,
                             col.regions = palette,
                             trim = TRUE) +
                mapview::mapview(probables,
                                 col.regions = "#2EB67D",
                                 alpha.regions = alpha,
                                 color = "white",
                                 layer.name = "Probable") +
                mapview::mapview(blocks_heatmap,
                                 alpha.regions = 0.1,
                                 color = "white",
                                 legend = FALSE,
                                 layer.name = "Manzana")
        } else{
            mapview::mapview(locality,
                             col.regions = "#4662D7FF",
                             alpha.regions = alpha,
                             legend = FALSE,
                             layer.name = "Límite de la Ciudad") +
            mapview::mapview(kde_rast,
                             layer.name = "kde",
                             na.color = "transparent",
                             alpha.regions = alpha,
                             legend = FALSE,
                             col.regions = palette,
                             trim = TRUE) +
                mapview::mapview(probables,
                                 col.regions = "#2EB67D",
                                 alpha.regions = alpha,
                                 color = "white",
                                 layer.name = "Probable") +
                mapview::mapview(confirmados,
                                 col.regions = "#E01E5A",
                                 alpha.regions = alpha,
                                 color = "white",
                                 layer.name = "Confirmado") +
                mapview::mapview(blocks_heatmap,
                                 alpha.regions = 0.1,
                                 color = "white",
                                 legend = FALSE,
                                 layer.name = "Manzana")
        }
    }


}
