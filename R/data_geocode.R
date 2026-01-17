#' data_geocode
#'
#' this function creates an address vector.
#'
#' @param infile is the name of file create with \link[denhotspots]{subset_den}.
#' @param data is a string for define the addreses or data. if data TRUE, is return the dataset.
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a vector.
#' @export
#'
#' @examples 1+1
data_geocode <- function(infile, data, sinave_new){
    x <- data.table::fread(paste0("./", infile, ".csv" )) |>
        dplyr::mutate(CVE_LOC_REP = stringr::str_pad(CVE_LOC_REP,
                                                     width = 4,
                                                     side = "left",
                                                     pad = "0"),
                      CVE_MPO_REP = stringr::str_pad(CVE_MPO_REP,
                                                     width = 3,
                                                     side = "left",
                                                     pad = "0"),
                      CVE_EDO_REP = stringr::str_pad(CVE_EDO_REP,
                                                     width = 2,
                                                     side = "left",
                                                     pad = "0")) |>
        dplyr::filter(CVE_EDO_REP != "00") |>
        dplyr::mutate(CVE_LOC_RES = stringr::str_pad(CVE_LOC_RES,
                                                     width = 4,
                                                     side = "left",
                                                     pad = "0"),
                      CVE_MPO_RES = stringr::str_pad(CVE_MPO_RES,
                                                     width = 3,
                                                     side = "left",
                                                     pad = "0"),
                      CVE_EDO_RES = stringr::str_pad(CVE_EDO_RES,
                                                     width = 2,
                                                     side = "left",
                                                     pad = "0")) |>
        dplyr::filter(CVE_EDO_RES != "00") |>
        dplyr::mutate(CVEGEO_REP = paste0(CVE_EDO_REP,
                                          CVE_MPO_REP,
                                          CVE_LOC_REP),
                      CVEGEO_RES = paste0(CVE_EDO_RES,
                                          CVE_MPO_RES,
                                          CVE_LOC_RES)) |>
        dplyr::mutate(autoctono = CVEGEO_REP == CVEGEO_RES)


    # Step 2. extract the autoctonous and imported cases ####
    importado <- w |>
        dplyr::filter(autoctono == FALSE) |>
        dplyr::filter(DES_LOC_REP != "OTROS PAISES DE LATINOAMERICA") |>
        dplyr::mutate(dir = paste(stringr::str_to_title(DES_LOC_REP),
                                  stringr::str_to_title(DES_MPO_REP),
                                  stringr::str_to_title(DES_EDO_REP),
                                  sep = ", ")) |>
        dplyr::select(VEC_ID, dir)

    x <- w |>
        dplyr::filter(autoctono == TRUE)


    # Step 3. data manimupation ####

    # house number ####
    x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = "[[:punct:]]", replacement = "")
    x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = "SN|\\t|SIN|SN|#", replacement = "")
    x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = " M | MZ | MZN", replacement = "MANZANA ")
    x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = " Z", replacement = " ")
    x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = "L", replacement = "LOTE ")
    x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = " T", replacement = " ")

    # street name ####
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "\\s{2}", replacement = "")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "[[:punct:]]", replacement = "")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "\\.", replacement = "")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "\\t{1,30}", replacement = "")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " SIN NUMERO|SIN NOMBRE|SIN NUMERO| SIN NUM| CALLE DOMICILIO CONOCIDO ,",
                                      replacement = "")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "SIN DOCUMENTAR|SIN DATO EXACTO|SIN DATO|CONOCIDO SIN NUMERO",
                                      replacement = "")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " SEC ",
                                      replacement = "")
    x$IDE_CAL <- paste(x$DES_CAL, x$IDE_CAL, sep = " ")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "CALLE CALLE ", replacement = "CALLE ")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL,
                                      pattern = "CALLE AV |CALLE AVE |CALLE AVENIDA |CALLE AV",
                                      replacement = "AVENIDA ")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " NUM ",  replacement = " NUMERO ")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " MZA | MZ | MZNA | MZ\\d{1}| M | MA ",  replacement = " MANZANA ")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " LT | LTE | LT\\d{1}| L\\d{1}| L ",  replacement = " LOTE ")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "C\\s{1,2}\\d",  replacement = "")
    x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "CALLE SINNOMBRE|CALLE CONOCIDO",  replacement = "")
    x$IDE_CAL[x$IDE_CAL  %in% c(stringr::str_subset(x$IDE_CAL, "CARRE"))] <- NA
    x$IDE_CAL[x$IDE_CAL  %in% c(stringr::str_subset(x$IDE_CAL, " FRENTE "))] <- NA
    x$IDE_CAL[x$IDE_CAL  %in% c(stringr::str_subset(x$IDE_CAL, "CAMINO "))] <- NA

    ##  neighborhoods
    x$IDE_COL <- stringr::str_replace_all(x$IDE_COL, pattern = "\\.", replacement = "")
    x$IDE_COL <- paste(stringr::str_extract(x$IDE_COL, "[A-Z]{1}[a-z]{1,40}"),
                       stringr::str_extract(x$IDE_COL, "([[:digit:]., -]|[[:upper:]., ]){1,40}"),
                       sep = " ")

    # Step 4. crate the vector  vector addresses

    addresses <- paste(paste(x$IDE_CAL,
                             x$NUM_EXT, sep = " "),
                       x$IDE_COL,
                       paste(x$DES_MPO_RES, x$IDE_CP, sep = " "),
                       paste(stringr::str_to_title(x$DES_EDO_RES), "Mexico", sep = ","),
                       sep = ", ")
    addresses <- stringr::str_replace(addresses, pattern = " C,| B,| F,| V,| P,| R,| E, | A,", replacement = " ,")
    addresses <- stringr::str_replace(addresses,
                                      pattern = "\\w{1,10} 0, ",
                                      replacement = " ,")
    addresses <- stringr::str_replace(addresses, pattern = "\\w{1,10} SN ,", replacement = " ,")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "(?<=Colonia ).+(?= CENTRO)",
                                          replacement = " Colonia CENTRO ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " Colonia  Colonia CENTRO  CENTRO |Colonia  Colonia CENTRO  CENTRO ",
                                          replacement = " Colonia CENTRO ")


    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " Ejido | Rancher |Rancher | Poblado | Pueblo | Ampliaci ",
                                          replacement = " ")


    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " 1 RA ",
                                          replacement = " PRIMERA ")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " ANDRE ",
                                          replacement = " ANDRES")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " 1ER PRIV ",
                                          replacement = " PRIMERA PRIVADA ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " SANTA FE TEXACAL \\(CENTRO URBANO\\)",
                                          replacement = " SANTA FE TEXACAL")



    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " Colonia LOMAS DEL TAPATÍO , 45628 COL. LOMAS DEL TAPATIO,",
                                          replacement = "  Colonia LOMAS DEL TAPATÍO,")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " SECC ",
                                          replacement = " SECCION ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " 3RA | 3RA\\.",
                                          replacement = " TERCERA ")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "LA CRUCITA \\(BARRIO DE GUADALUPE\\)",
                                          replacement = " LA CRUCITA ")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " III ",
                                          replacement = " TRES ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "CALLE DOMICILIO CONOCIDO ,|CALLE DOMICILI CONOCIDO ,",
                                          replacement = "")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " #",
                                          replacement = " ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "NA ,",
                                          replacement = " ")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " MICH,",
                                          replacement = " ,")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " VER,",
                                          replacement = " ,")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " , ",
                                          replacement = " ,")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "CALLE CALLEJON ",
                                          replacement = "CALLEJON ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "CALLE DOM CON ,",
                                          replacement = "")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "CALLEJON CALLEJON ",
                                          replacement = "CALLEJON ")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "CENTRO  CENTRO,",
                                          replacement = " CENTRO,")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "QUINT ROO",
                                          replacement = " ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " YUC",
                                          replacement = " ")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = ",CENTRO IXHUATLÁN DEL SURESTE ,",
                                          replacement = ",CENTRO,")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = "62744 CUAUTLA  MOR",
                                          replacement = "CUAUTLA")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " JAL,",
                                          replacement = ",")
    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " NVO LEON,",
                                          replacement = ",")

    addresses <- stringr::str_replace_all(addresses,
                                          pattern = " AV ",
                                          replacement = " ")


    # Step 5. bind the importados y autoctonos ####

    if(is.null(importado$dir) == FALSE){
        addresses <- tibble::tibble(addresses = c(addresses, importado$dir),
                                    VEC_ID = c(x$VEC_ID, importado$VEC_ID))
    } else{
        addresses
    }

    if(data == TRUE){
        w
    } else {
        addresses
    }


}

