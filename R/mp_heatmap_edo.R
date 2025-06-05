#' The mp_heatmap_edo function generates a heatmap by state.
#'
#' @param cve_edo is a string of the state id.
#' @param geocoded_dataset is geocoded dataset.
#' @param status_caso It is a numerical value to select the Status of the case. There are three options, 1 for the probable cases database, 2 for the confirmed cases database, and 3 for the discarded cases database.
#' @param week It is epidemiological week.
#' @param kernel It is kernel density for select the blocks.
#' @param alpha is a numerical parameter that controls the transparency of the heatmap. Values range from 0 to 1, where 0 is completely transparent and 1 does not make the heat map transparent.
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
mp_heatmap_edo <- function(cve_edo,
                           geocoded_dataset,
                           week,
                           kernel,
                           alpha,
                           palette){

    # Step 1. transform dataset #####
    z <- geocoded_dataset |>
        dplyr::filter(accuracy != "locality") |>
        dplyr::mutate(x = long,
                      y = lat) |>
        sf::st_as_sf(coords = c("long", "lat"),
                     crs = 4326)

    # Step 2. extract the locality ####
    entidad <- rgeomex::AGEE_inegi19_mx |>
        dplyr::filter(CVE_ENT %in% c(cve_edo))
    mun <- rgeomex::AGEM_inegi19_mx |>
        dplyr::filter(CVE_ENT %in% c(cve_edo))

    # Step 3. extract the geocoded cases ####
    z <- z[entidad, ]  |>
        #sf::st_drop_geometry() |>
        dplyr::filter(SEM %in% c(week))

    # Step 3. extract the geocoded cases of locality
    confirmados <- z |>
        dplyr::filter(ESTATUS_CASO == 2)
    probables <- z |>
        dplyr::filter(ESTATUS_CASO == 1)

    # Step 4. Create kernel density output
    kde <- KernSmooth::bkde2D(x = cbind(z$x, z$y),
                              bandwidth = c(0.0045, 0.0068),
                              gridsize = c(1500,1500))

    # Step 5. Create Raster from Kernel Density output
    KernelDensityRaster <- raster::raster(list(x = kde$x1,
                                               y = kde$x2,
                                               z = kde$fhat))

    # Step 6. convert the raster to rast
    kde_rast <- KernelDensityRaster |>
        terra::rast() |>
        terra::mask(mask = entidad)



    # Step 1. load the blocks ####

    if(cve_edo %in% c("01", "02", "03", "04", "05", "06", "07",
                      "08", "09", "10")){
        blocks <- rgeomex::blocks_ine20_mx_a |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo)))
    }

    if(cve_edo %in% c("11", "12", "13", "14")){
        blocks <- rgeomex::blocks_ine20_mx_b |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo)))
    }

    if(cve_edo %in% c("15", "16", "17", "18", "19")){
        blocks <- rgeomex::blocks_ine20_mx_c |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo)))
    }

    if(cve_edo %in% c("20", "21", "22",
                      "23", "24", "25")){
        blocks <- rgeomex::blocks_ine20_mx_d |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo)))
    }

    if(cve_edo %in% c("26", "27", "28", "29", "30", "31", "32")){
        blocks <- rgeomex::blocks_ine20_mx_e |>
            dplyr::filter(entidad %in% c(as.numeric(cve_edo)))
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
        mapview::mapview(entidad,
                         alpha.regions = 0.1,
                         legend = FALSE,
                         layer.name = "Limite Estatal") +
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
            mapview::mapview(mun,
                             alpha.regions = 0.1,
                             color = "white",
                             legend = FALSE,
                             layer.name = "Municipio") +
            mapview::mapview(blocks_heatmap,
                             alpha.regions = 0.1,
                             color = "white",
                             legend = FALSE,
                             layer.name = "Manzana")
    } else if(nrow(confirmados) == 0) {
        mapview::mapview(entidad,
                         alpha.regions = 0.1,
                         legend = FALSE,
                         layer.name = "Limite Estatal") +
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
            mapview::mapview(mun,
                             alpha.regions = 0.1,
                             color = "white",
                             legend = FALSE,
                             layer.name = "Municipio") +
            mapview::mapview(blocks_heatmap,
                             alpha.regions = 0.1,
                             color = "white",
                             legend = FALSE,
                             layer.name = "Manzanas")
    } else{
        mapview::mapview(entidad,
                         alpha.regions = 0.1,
                         legend = FALSE,
                         layer.name = "Limite Estatal") +
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
            mapview::mapview(mun,
                             alpha.regions = 0.1,
                             color = "white",
                             legend = FALSE,
                             layer.name = "Municipios") +
            mapview::mapview(blocks_heatmap,
                             alpha.regions = 0.1,
                             color = "white",
                             legend = FALSE,
                             layer.name = "Manzanas")
    }



}
