#' This function counts VBD cases per agem.
#'
#' @param path_vbd Is the directory where the file is located.
#' @param vbd Is the parameter for define the vector-borne diseases
#' @param status_case It is a numerical value to select the Status of the case. There are three options, 1 for the probable cases database, 2 for the confirmed cases database, and 3 for the discarded cases dataset.
#' @param country It is a logical value to define if the result is by state (set TRUE) or by country (set FALSE)
#' @param cve_edo Is a string of the state id-
#'
#' @return a sf object
#' @export
#'
#' @examples cases_by_agem(".",vbd = "DENGUE",status_case = 2, country = TRUE)
cases_by_agem <- function(path_vbd, vbd, status_case, country, cve_edo = NULL){


    # Step 1. read the dataset ####
    y <- densnv::read(path = path_vbd,
                      vbd = vbd,
                      complete = TRUE) |>
        dplyr::filter(ESTATUS_CASO %in% c(status_case)) |>
        dplyr::mutate(CVE_EDO_REP = stringr::str_pad(CVE_EDO_REP,
                                                     width = 2,
                                                     side = "left",
                                                     pad = "0")) |>
        dplyr::mutate(CVE_MPO_REP = stringr::str_pad(CVE_MPO_REP,
                                                     width = 3,
                                                     side = "left",
                                                     pad = "0")) |>
        dplyr::summarise(n = dplyr::n(),
                         .by = c(CVE_EDO_REP, CVE_MPO_REP))


    # step 2. left joint between aggregated data and AGEM ######
    xy <- dplyr::left_join(x = rgeomex::AGEM_inegi19_mx,
                           y = y,
                           by = c("CVE_ENT"  = "CVE_EDO_REP",
                                  "CVE_MUN"  = "CVE_MPO_REP"))

    # step 3. sustitute the NA values by 0 ####
    xy[is.na(xy)] <- 0

    # Step 3. return the cases by agem ####
    if(country == FALSE){
        xy |>
            dplyr:::filter(CVE_ENT %in% c(cve_edo))
    } else{
        xy
    }

}
