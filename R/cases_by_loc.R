#' Arbovirus cases by locality
#'
#' Reads a SINAVE vector-borne disease dataset (dengue, Zika or chikungunya),
#' counts probable and confirmed cases by locality of report, and joins them to
#' the INEGI 2020 locality catalogue (ITER 2020) to return one point per
#' locality with cases.
#'
#' @param path path to the SINAVE txt file (e.g. `"DENGUE2_.txt"`,
#'   `"ZIKA2_.txt"` or `"CHIKUNGUNYA2_.txt"`).
#' @param snv_dataset arbovirus of the dataset: `"DENV"` (dengue), `"ZIKV"`
#'   (Zika) or `"CHIKV"` (chikungunya).
#' @param year year to keep (ANO column). If NULL (default), the most recent
#'   year in the dataset is used.
#' @param week epidemiological weeks to keep (SEM column), e.g. `1:10` or
#'   `c(5, 6, 7)`. If NULL (default), all weeks are used.
#'
#' @details
#' Only probable and confirmed cases are kept (`ESTATUS_CASO` 1 and 2);
#' discarded cases are excluded. The locality key `cvegeo` (9 digits) is built
#' from the state, municipality and locality of report
#' (`CVE_EDO_REP` + `CVE_MPO_REP` + `CVE_LOC_REP`). Records with missing codes
#' (`"00"`, `"000"`, `"0000"`) and localities not found in ITER 2020 are
#' dropped.
#'
#' Columns returned for every arbovirus:
#' \describe{
#'   \item{cvegeo}{INEGI locality key (state + municipality + locality).}
#'   \item{nom_ent, nom_mun, nom_loc}{names of the state, municipality and
#'     locality (ITER 2020).}
#'   \item{probable}{number of notified cases (probable + confirmed).}
#'   \item{confirmado}{number of confirmed cases.}
#'   \item{altitud}{altitude of the locality (m).}
#'   \item{pobtot}{total population of the locality (Census 2020).}
#'   \item{geometry}{point of the locality (EPSG:4326).}
#' }
#'
#' Additional columns for DENV:
#' \describe{
#'   \item{DNG, DCSA, DG}{cases by final diagnosis: non-severe dengue,
#'     dengue with warning signs and severe dengue.}
#'   \item{DENV1, DENV2, DENV3, DENV4}{cases by serotype (RT-PCR triplex).}
#' }
#'
#' @return an sf object of points (EPSG:4326), one row per locality with
#'   cases.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @seealso [rgeomex::iter_2020_loc]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' den  <- cases_by_loc(path = "DENGUE2_.txt",      snv_dataset = "DENV")
#' zik  <- cases_by_loc(path = "ZIKA2_.txt",        snv_dataset = "ZIKV")
#' chik <- cases_by_loc(path = "CHIKUNGUNYA2_.txt", snv_dataset = "CHIKV")
#'
#' # a specific year
#' den25 <- cases_by_loc(path = "DENGUE2_.txt", snv_dataset = "DENV",
#'                       year = 2025)
#'
#' # epidemiological weeks 1 to 10
#' den_w <- cases_by_loc(path = "DENGUE2_.txt", snv_dataset = "DENV",
#'                       week = 1:10)
#'
#' mapview::mapview(den, zcol = "confirmado", cex = "probable")
#' }
cases_by_loc <- function(path, snv_dataset = c("DENV", "ZIKV", "CHIKV"),
                         year = NULL,
                         week = NULL){

    # Step 0. checks ####
    snv_dataset <- match.arg(snv_dataset)
    if(!file.exists(path)){
        stop("The file does not exist: ", path)
    }

    # Step 1. load the ITER 2020 ####
    loc <- rgeomex::iter_2020_loc |>
        dplyr::mutate(cvegeo = paste0(entidad, mun, loc)) |>
        dplyr::select(cvegeo,
                      dplyr::any_of(c("nom_ent", "nom_mun", "nom_loc")),
                      longitud, latitud, altitud, pobtot)

    # Step 2. load the sinave dataset (common steps) ####
    x <- data.table::fread(path,
                           encoding = "Latin-1",
                           quote = "",
                           fill = TRUE) |>
        dplyr::filter(ESTATUS_CASO %in% c(1, 2)) |>
        dplyr::mutate(CVE_EDO_REP = stringr::str_pad(CVE_EDO_REP, width = 2,
                                                     side = "left", pad = "0"),
                      CVE_MPO_REP = stringr::str_pad(CVE_MPO_REP, width = 3,
                                                     side = "left", pad = "0"),
                      CVE_LOC_REP = stringr::str_pad(CVE_LOC_REP, width = 4,
                                                     side = "left", pad = "0")) |>
        dplyr::filter(CVE_EDO_REP != "00",
                      CVE_MPO_REP != "000",
                      CVE_LOC_REP != "0000") |>
        dplyr::mutate(cvegeo = paste0(CVE_EDO_REP, CVE_MPO_REP, CVE_LOC_REP))

    if(is.null(year)){
        year <- max(x$ANO, na.rm = TRUE)
    }
    x <- x |> dplyr::filter(ANO %in% year)
    if(!is.null(week)){
        x <- x |> dplyr::filter(SEM %in% week)
    }

    if(nrow(x) == 0){
        stop("There are no probable or confirmed cases for year ", year,
             if(!is.null(week)) paste0(" and weeks ", min(week), "-", max(week)),
             ".")
    }

    # Step 3. aggregate by locality ####
    if(snv_dataset == "DENV"){
        data <- x |>
            dplyr::mutate(DENGUE_SER_TRIPLEX = dplyr::case_when(
                DENGUE_SER_TRIPLEX == 1 ~ "DENV1",
                DENGUE_SER_TRIPLEX == 2 ~ "DENV2",
                DENGUE_SER_TRIPLEX == 3 ~ "DENV3",
                DENGUE_SER_TRIPLEX == 4 ~ "DENV4")) |>
            dplyr::summarise(probable   = dplyr::n(),
                             confirmado = sum(ESTATUS_CASO == 2, na.rm = TRUE),
                             DNG   = sum(DES_DIAG_FINAL == "DENGUE NO GRAVE", na.rm = TRUE),
                             DCSA  = sum(DES_DIAG_FINAL == "DENGUE CON SIGNOS DE ALARMA", na.rm = TRUE),
                             DG    = sum(DES_DIAG_FINAL == "DENGUE GRAVE", na.rm = TRUE),
                             DENV1 = sum(DENGUE_SER_TRIPLEX == "DENV1", na.rm = TRUE),
                             DENV2 = sum(DENGUE_SER_TRIPLEX == "DENV2", na.rm = TRUE),
                             DENV3 = sum(DENGUE_SER_TRIPLEX == "DENV3", na.rm = TRUE),
                             DENV4 = sum(DENGUE_SER_TRIPLEX == "DENV4", na.rm = TRUE),
                             .by = cvegeo)
    } else {
        # ZIKV and CHIKV
        data <- x |>
            dplyr::summarise(probable   = dplyr::n(),
                             confirmado = sum(ESTATUS_CASO == 2, na.rm = TRUE),
                             .by = cvegeo)
    }

    # Step 4. join with the localities ####
    dplyr::left_join(x = data,
                     y = loc,
                     by = "cvegeo") |>
        dplyr::filter(!is.na(longitud), !is.na(latitud)) |>
        sf::st_as_sf(coords = c("longitud", "latitud"),
                     crs = 4326)
}
