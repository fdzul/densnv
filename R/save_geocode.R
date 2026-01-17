#' save_geocode
#'
#' @param geocode It is the geocoded dataset.
#' @param data is the dataset
#' @param directory This is the folder where the dataset will be saved.
#' @param infile is the name of the dataset file
#'
#' @returns a RData object
#' @export
#'
#' @examples 1+1
save_geocode <- function(geocode, data, directory, infile){
    geocode <- geocode[!duplicated(geocode$index),]
    data <- dplyr::left_join(x = data,
                             y = geocode,
                             by = "VEC_ID")
    save(data,
         file = paste(paste(paste(directory, "/geo", sep = ""),
                            infile, sep = "_"), ".RData",
                      sep = ""))
}
