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
plt_pyramid <- function(data,
                        col_male   = "#0072B2",
                        col_female = "#CC79A7",
                        gap = 0.18,
                        size_age = 3.5,
                        size_count = 3,
                        icon_male = "person",
                        icon_female = "person-dress",
                        show_zero = FALSE,
                        title = NULL,
                        subtitle = NULL,
                        caption = "Fuente: SINAVE/DGE, Secretaría de Salud."){
    dat <- data |>
        janitor::clean_names() |>
        dplyr::filter(estatus_caso == 2) |>
        dplyr::mutate(cve_edo_rep = stringr::str_pad(cve_edo_rep, pad = "0",
                                                     width = 2, side = "left"),
                      cve_mpo_rep = stringr::str_pad(cve_mpo_rep, pad = "0",
                                                     width = 3, side = "left"),
                      sex = dplyr::case_when(ide_sex == 1 ~ "Female",
                                             ide_sex == 2 ~ "Male"),
                      # Inf en lugar de max(): no falla si nadie tiene 65+ años
                      age_class = cut(x = ide_eda_ano,
                                      breaks = c(0, 1, 5, 10, 15, 20, 25, 30,
                                                 35, 40, 45, 50, 55, 60, 65, Inf),
                                      labels = c("<1", "1-4", "5-9", "10-14",
                                                 "15-19", "20-24", "25-29",
                                                 "30-34", "35-39", "40-44",
                                                 "45-49", "50-54", "55-59",
                                                 "60-64", "65+"),
                                      right = FALSE,
                                      include.lowest = TRUE)) |>
        dplyr::filter(!is.na(sex), !is.na(age_class)) |>
        dplyr::summarise(count = dplyr::n(), .by = c(age_class, sex)) |>
        # grupos sin casos aparecen con 0 (la pirámide no se "rompe")
        tidyr::complete(age_class, sex, fill = list(count = 0L))

    # ícono de Font Awesome como imagen para ggplot
    # (https://fontawesome.com/search?ic=free)
    icon_grob <- function(name, color){
        svg <- fontawesome::fa(name, fill = color, height = "200px")
        png <- rsvg::rsvg_png(charToRaw(as.character(svg)), height = 400)
        grid::rasterGrob(png::readPNG(png), interpolate = TRUE)
    }

    max_n <- max(dat$count, 1)
    g     <- max_n * gap                    # mitad del espacio central
    n_age <- nlevels(dat$age_class)

    # coordenadas de cada barra: hombres a la izquierda, mujeres a la derecha
    d <- dat |>
        dplyr::mutate(y    = as.integer(age_class),
                      xmin = ifelse(sex == "Male", -g - count, g),
                      xmax = ifelse(sex == "Male", -g, g + count),
                      # el número va dentro de la barra si cabe; si no, afuera
                      inside = count >= max_n * 0.10,
                      x_lab  = dplyr::case_when(
                          inside & sex == "Male"   ~ -g - count / 2,
                          inside & sex == "Female" ~  g + count / 2,
                          sex == "Male"            ~ -g - count - max_n * 0.03,
                          TRUE                     ~  g + count + max_n * 0.03),
                      hjust_lab = dplyr::case_when(inside ~ 0.5,
                                                   sex == "Male" ~ 1,
                                                   TRUE ~ 0),
                      col_lab = ifelse(inside, "white",
                                       ifelse(sex == "Male", col_male, col_female)))

    d_lab <- if(show_zero) d else dplyr::filter(d, count > 0)

    # totales por sexo para los encabezados
    tot_m <- sum(d$count[d$sex == "Male"])
    tot_f <- sum(d$count[d$sex == "Female"])

    # eje x: marcas simétricas con etiquetas en valor absoluto
    brks <- pretty(c(0, max_n), n = 4)
    brks <- brks[brks <= max_n]

    ggplot2::ggplot(d) +
        # líneas guía solo en la zona de barras (no cruzan las edades)
        ggplot2::geom_segment(data = data.frame(x = c(-g - brks, g + brks)),
                              ggplot2::aes(x = x, xend = x,
                                           y = 0.4, yend = n_age + 0.6),
                              color = "gray90", linewidth = 0.3) +
        # barras
        ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax,
                                        ymin = y - 0.42, ymax = y + 0.42,
                                        fill = sex),
                           color = "white", linewidth = 0.3) +
        # número de casos
        ggplot2::geom_text(data = d_lab,
                           ggplot2::aes(x = x_lab, y = y, label = count,
                                        hjust = hjust_lab, color = col_lab),
                           size = size_count, fontface = "bold") +
        ggplot2::scale_color_identity() +
        # grupos de edad al centro
        ggplot2::geom_text(data = dplyr::distinct(d, age_class, y),
                           ggplot2::aes(x = 0, y = y, label = age_class),
                           size = size_age, color = "gray25", fontface = "bold") +
        # íconos y totales por sexo (hacen de leyenda)
        ggplot2::annotation_custom(icon_grob(icon_male, col_male),
                                   xmin = -g - max_n / 2 - max_n * 0.15,
                                   xmax = -g - max_n / 2 + max_n * 0.15,
                                   ymin = n_age + 1.25, ymax = n_age + 2.15) +
        ggplot2::annotate("text", x = -g - max_n / 2, y = n_age + 0.9,
                          label = paste0("Hombres (n = ", tot_m, ")"),
                          color = col_male, size = 4, fontface = "bold") +
        ggplot2::annotation_custom(icon_grob(icon_female, col_female),
                                   xmin = g + max_n / 2 - max_n * 0.15,
                                   xmax = g + max_n / 2 + max_n * 0.15,
                                   ymin = n_age + 1.25, ymax = n_age + 2.15) +
        ggplot2::annotate("text", x = g + max_n / 2, y = n_age + 0.9,
                          label = paste0("Mujeres (n = ", tot_f, ")"),
                          color = col_female, size = 4, fontface = "bold") +
        ggplot2::annotate("text", x = 0, y = n_age + 0.9,
                          label = "Edad", color = "gray25",
                          size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = c(Male = col_male,
                                              Female = col_female),
                                   guide = "none") +
        ggplot2::scale_x_continuous("Número de casos",
                                    breaks = c(-g - brks, g + brks),
                                    labels = c(brks, brks),
                                    expand = ggplot2::expansion(mult = 0.02)) +
        ggplot2::scale_y_continuous(NULL,
                                    limits = c(0.4, n_age + 2.2),
                                    expand = c(0, 0)) +
        ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(size = 10,
                                                           color = "gray40"),
                       axis.title.x = ggplot2::element_text(size = 12,
                                                            color = "gray30",
                                                            margin = ggplot2::margin(t = 6)),
                       plot.title = ggplot2::element_text(face = "bold",
                                                          color = "gray20"),
                       plot.subtitle = ggplot2::element_text(color = "gray40"),
                       plot.caption = ggplot2::element_text(color = "gray50",
                                                            size = 8),
                       plot.background = ggplot2::element_rect(fill = "white",
                                                               color = NA),
                       plot.margin = ggplot2::margin(10, 15, 10, 15))
}
