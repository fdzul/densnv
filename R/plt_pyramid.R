#' Age-sex pyramid of confirmed dengue cases
#'
#' Builds a population pyramid of confirmed dengue cases from the SINAVE
#' dataset, with males on the left, females on the right and the age groups
#' in the centre. Each side is headed by a Font Awesome icon and the total
#' number of cases by sex, which act as the legend.
#'
#' @param data data.frame with the SINAVE dengue dataset. It must contain the
#'   columns `ESTATUS_CASO`, `IDE_SEX`, `IDE_EDA_ANO`, `CVE_EDO_REP` and
#'   `CVE_MPO_REP` (names are standardised with [janitor::clean_names()]).
#'   Only confirmed cases (`ESTATUS_CASO == 2`) are used.
#' @param col_male colour for males (default `"#0072B2"`, Okabe-Ito blue).
#' @param col_female colour for females (default `"#CC79A7"`, Okabe-Ito
#'   reddish purple).
#' @param gap width of the central space for the age labels, as a proportion
#'   of the largest count (default 0.18). Increase it if the labels look
#'   crowded.
#' @param size_age text size of the age-group labels (default 3.5).
#' @param size_count text size of the number of cases (default 3).
#' @param icon_male,icon_female names of free Font Awesome icons drawn above
#'   each side (default `"person"` and `"person-dress"`; other options are
#'   `"mars"`/`"venus"` or `"child"`/`"child-dress"`). See
#'   <https://fontawesome.com/search?ic=free>.
#' @param show_zero logical. If TRUE, age groups without cases are labelled
#'   with 0 (default FALSE).
#' @param title,subtitle plot title and subtitle (default NULL, none).
#' @param caption plot caption (default cites SINAVE/DGE).
#'
#' @details
#' Ages are grouped into 15 classes: `<1`, `1-4`, `5-9`, ..., `60-64` and
#' `65+` (intervals are closed on the left). Age groups without cases are
#' kept with a count of 0, so the pyramid never has gaps. The number of cases
#' is printed inside each bar when it fits and outside otherwise. The x axis
#' shows absolute counts on both sides.
#'
#' Icons are rendered from SVG with \pkg{fontawesome} and \pkg{rsvg}, so no
#' system font is required and they are kept when saving with
#' [ggplot2::ggsave()].
#'
#' @return a ggplot object.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @seealso [fontawesome::fa()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- data.table::fread("DENGUE2_.txt", quote = "", fill = TRUE)
#'
#' plt_pyramid(data = x,
#'             title = "Casos confirmados de dengue por grupo de edad y sexo",
#'             subtitle = "Jalisco, 2026")
#'
#' # other colours and icons
#' plt_pyramid(data = x,
#'             col_male = "#1B9E77", col_female = "#D95F02",
#'             icon_male = "mars", icon_female = "venus",
#'             show_zero = TRUE)
#'
#' ggplot2::ggsave("pyramid.png", width = 8, height = 7, dpi = 300)
#' }
