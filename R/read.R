#' The read function reads the dataset of sinave vector-borne diseases.
#'
#' @param path is the directory where the files are located.
#' @param vbd is the parameter for define the vector-borne diseases.
#' @param complete is a logical value for define the group of vector-borne diseases, if is TRUE, the define DENV, CHIKV & ZIKV, else for other etvs.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a dataframe
#' @export
#' @import utils
#' @importFrom stringr str_subset
#' @importFrom data.table fread
#' @importFrom purrr map_dfr
#' @importFrom purrr map
#'
#' @examples 1+1
read <- function(path, vbd, complete){

    # Step 1 create the list directories ####
    l_files <- list.files(path = path,
                          pattern = "txt",
                          full.names = TRUE)

    # Step 2 extract only the txt of dengue ####
    l <- unlist(purrr::map(l_files, stringr::str_subset, c(vbd)))


    # Step 3. read the dataset ####

    if(complete == FALSE){
        read_arbo <- function(x){
            vect_cols <- c("VEC_ID","FOL_ID","IDE_EDA_ANO", "IDE_SEX",
                           "DES_CAL","IDE_CAL", "NUM_EXT", "NUM_INT",
                           "IDE_COL", "IDE_CP",
                           "CVE_LOC_RES", "DES_LOC_RES", "CVE_MPO_RES", "DES_MPO_RES",
                           "DES_JUR_RES",
                           "CVE_EDO_RES", "DES_EDO_RES",
                           "CVE_EDO_REP", "DES_EDO_REP",
                           "CVE_LOC_REP", "DES_LOC_REP",
                           "CVE_MPO_REP", "DES_MPO_REP",
                           "ESTATUS_CASO", "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE", "DES_DIAG_FINAL",
                           "FEC_INI_SIGNOS_SINT", "ANO", "SEM",
                           "MANEJO",
                           "DES_INS_UNIDAD",
                           "DENGUE_SER_TRIPLEX",
                           "FEC_INGRESO")
            data.table::fread(x,
                              header = TRUE,
                              quote = "",
                              select = vect_cols,
                              encoding = "Latin-1",
                              fill = TRUE)
        }
        purrr::map_dfr(l, read_arbo)
    } else {
        data.table::fread(l,
                          header = TRUE,
                          quote = "",
                          encoding = "Latin-1",
                          fill = TRUE)
    }
}
