#' mp_animated_loc
#'
#' Animated map of arbovirus cases by locality and epidemiological week.
#'
#' Reads the SINAVE dataset (same processing as [cases_by_loc()]), counts the
#' cases by locality and epidemiological week and builds an animated map (one
#' frame per week) following the workflow of [mp_animated()]. For the whole
#' country the state boundaries are drawn; for a state, its municipal
#' boundaries.
#'
#' @param path path to the SINAVE txt file (e.g. `"DENGUE2_.txt"`,
#'   `"ZIKA2_.txt"` or `"CHIKUNGUNYA2_.txt"`).
#' @param snv_dataset arbovirus of the dataset: `"DENV"` (dengue), `"ZIKV"`
#'   (Zika) or `"CHIKV"` (chikungunya).
#' @param year year of the dataset (ANO column). If NULL (default), the most
#'   recent year in the dataset is used.
#' @param week epidemiological weeks to animate (SEM column), e.g. `1:52`. If
#'   NULL (default), all weeks are used.
#' @param var one or more types of cases to map: `"probable"` (probable +
#'   confirmed, default), `"confirmado"` (confirmed). Only for
#'   `snv_dataset = "DENV"`: `"DNG"` (non-severe dengue), `"DCSA"` (dengue
#'   with warning signs), `"DG"` (severe dengue), `"DENV1"`, `"DENV2"`,
#'   `"DENV3"` or `"DENV4"` (serotype, RT-PCR triplex). With more than one
#'   value, `multi` defines how they are combined.
#' @param multi how to combine several values of `var`:
#'   `"sum"` (default) counts together the cases of any of them in one map
#'   (e.g. `var = c("DCSA", "DG")`); `"facet"` draws one panel per value in
#'   every frame (e.g. `var = c("DENV1", "DENV2", "DENV3", "DENV4")`).
#' @param ncol number of columns of panels when `multi = "facet"`
#'   (default NA, automatic).
#' @param country logical. TRUE (default) = whole country with state
#'   boundaries; FALSE = the state given in `cve_edo` with municipal
#'   boundaries.
#' @param cve_edo INEGI state code ("20" or 20). Required if `country = FALSE`.
#' @param breaks numeric, step between the breaks of the legend and of the
#'   colour and size scales (e.g. `breaks = 5` gives 1, 6, 11, ...). If NULL
#'   (default), 5 integer breaks from 1 to the maximum number of cases are
#'   used (e.g. 1, 6, 12, 18, 23). With `"quantile"`, the quartiles of the
#'   cases by locality and week are used (minimum, Q1, median, Q3 and
#'   maximum); repeated values are dropped, so there may be fewer than 5
#'   breaks when most localities have 1 or 2 cases.
#' @param pal colour palette of the bubbles (default `"-viridis"`; the minus
#'   sign reverses it so that more cases are darker).
#' @param text_size size of the number of cases inside the bubbles (default
#'   0.4; use 0 to hide it).
#' @param fps frames (weeks) per second (default 2).
#' @param save logical. If TRUE the gif is saved in `dir`; if FALSE (default)
#'   the animation is only shown.
#' @param name name of the gif file (without extension); `"_loc_animated_map.gif"`
#'   is added at the end.
#' @param dir directory where the gif is saved (with "/" at the end).
#' @param size_range minimum and maximum size of the bubbles
#'   (default `c(2.5, 14)`).
#' @param strip_size text size of the panel titles (default 18). Each title
#'   shows the category and its cases of the week.
#' @param map_legend logical. Show the legend of the bubbles (size and colour
#'   by number of cases) below the maps (default TRUE).
#' @param legend_title title of the bubble legend (default
#'   `"Casos por localidad"`).
#' @param curve logical. Add below the maps a line chart of the weekly cases
#'   of each category, with the current week marked (default TRUE).
#' @param width,height size of the gif in pixels. `height` defaults to 2000
#'   (1600 without the curve); `width` is computed from the number of panels,
#'   `ncol` and the shape of the map, so the curve has the same width as the
#'   maps.
#'
#' @details
#' Only probable and confirmed cases (`ESTATUS_CASO` 1 and 2) are used,
#' counted by locality of report (`CVE_EDO_REP` + `CVE_MPO_REP` +
#' `CVE_LOC_REP`) and joined to the INEGI 2020 localities
#' (`rgeomex::iter_2020_loc`). Colour and size scales are the same in every
#' frame and in every panel, so they can be compared. With `multi = "facet"`,
#' a case can appear in more than one panel (e.g. a confirmed DENV2 case in
#' `"confirmado"` and in `"DENV2"`). Each frame is a \pkg{ggplot2} figure (one
#' panel per value of `var` with `multi = "facet"`, or a single panel) with the
#' week in the title, and the frames are joined with \pkg{gifski}; the gif is shown in the
#' RStudio Viewer (and saved in `dir` if `save = TRUE`). Saving the gif
#' needs the \pkg{gifski} package.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a gif file of animation (with `multi = "facet"`, the path of the
#'   gif, invisibly).
#' @export
#'
#' @seealso [mp_animated()], [cases_by_loc()]
#'
#' @examples
#' \dontrun{
#' # whole country, with state boundaries
#' mp_animated_loc(path = "DENGUE2_.txt", snv_dataset = "DENV",
#'                 year = 2026, week = 1:52,
#'                 save = TRUE, name = "dengue_mexico_2026", dir = "./")
#'
#' # Oaxaca, confirmed cases, with municipal boundaries
#' mp_animated_loc(path = "DENGUE2_.txt", snv_dataset = "DENV",
#'                 year = 2026, week = 1:52, var = "confirmado",
#'                 country = FALSE, cve_edo = "20",
#'                 save = TRUE, name = "dengue_oaxaca_2026", dir = "./")
#'
#' # severe forms together (DCSA + DG) in one map
#' mp_animated_loc(path = "DENGUE2_.txt", snv_dataset = "DENV",
#'                 year = 2026, week = 1:52,
#'                 var = c("DCSA", "DG"), multi = "sum",
#'                 save = TRUE, name = "dcsa_dg_mexico_2026", dir = "./")
#'
#' # one panel per serotype
#' mp_animated_loc(path = "DENGUE2_.txt", snv_dataset = "DENV",
#'                 year = 2026, week = 1:52,
#'                 var = c("DENV1", "DENV2", "DENV3", "DENV4"),
#'                 multi = "facet", ncol = 2,
#'                 save = TRUE, name = "serotipos_mexico_2026", dir = "./")
#' }
mp_animated_loc <- function(path,
                            snv_dataset = c("DENV", "ZIKV", "CHIKV"),
                            year = NULL,
                            week = NULL,
                            var = "probable",
                            multi = c("sum", "facet"),
                            ncol = NA,
                            country = TRUE,
                            cve_edo = NULL,
                            breaks = NULL,
                            pal = "-viridis",
                            text_size = 0.4,
                            fps = 2,
                            save = FALSE,
                            name = NULL,
                            dir = "./",
                            width = NULL,
                            height = NULL,
                            size_range = c(2.5, 14),
                            strip_size = 18,
                            curve = TRUE,
                            map_legend = TRUE,
                            legend_title = "Casos por localidad"){

    # Step 0. checks ####
    snv_dataset <- match.arg(snv_dataset)
    multi       <- match.arg(multi)
    var_opts <- c("probable", "confirmado", "DNG", "DCSA", "DG",
                  "DENV1", "DENV2", "DENV3", "DENV4")
    var <- unique(match.arg(var, choices = var_opts, several.ok = TRUE))

    if(!file.exists(path)){
        stop("The file does not exist: ", path)
    }
    if(snv_dataset != "DENV" && any(!var %in% c("probable", "confirmado"))){
        stop("DNG, DCSA, DG and DENV1-DENV4 are only available for ",
             "snv_dataset = \"DENV\". For ", snv_dataset,
             " use \"probable\" or \"confirmado\".")
    }
    if(!country && is.null(cve_edo)){
        stop("With country = FALSE you must provide cve_edo (e.g. cve_edo = \"20\").")
    }
    if(save && is.null(name)){
        stop("With save = TRUE you must provide name.")
    }
    if(!is.null(cve_edo)){
        cve_edo <- stringr::str_pad(as.character(cve_edo), width = 2,
                                    side = "left", pad = "0")
    }
    facet <- multi == "facet"
    if(is.null(height)) height <- if(curve) 2000 else 1600

    # Step 1. read the sinave dataset (as in cases_by_loc) ####
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
    if(!country){
        x <- x |> dplyr::filter(CVE_EDO_REP %in% cve_edo)
    }

    # the dengue file must have the diagnosis / serotype columns
    need <- c(DNG = "DES_DIAG_FINAL", DCSA = "DES_DIAG_FINAL",
              DG = "DES_DIAG_FINAL", DENV1 = "DENGUE_SER_TRIPLEX",
              DENV2 = "DENGUE_SER_TRIPLEX", DENV3 = "DENGUE_SER_TRIPLEX",
              DENV4 = "DENGUE_SER_TRIPLEX")
    falta <- setdiff(unique(need[intersect(var, names(need))]), names(x))
    if(length(falta) > 0){
        stop("The file has no column ", paste(falta, collapse = ", "),
             ". Is it the dengue file (DENGUE2_.txt)? Check path and snv_dataset.")
    }

    # Step 1.1. select the type of case (one or several) ####
    # TRUE for the records that belong to each value of var
    cond <- function(d, v){
        switch(v,
               probable   = rep(TRUE, nrow(d)),
               confirmado = d$ESTATUS_CASO %in% 2,
               DNG   = d$DES_DIAG_FINAL %in% "DENGUE NO GRAVE",
               DCSA  = d$DES_DIAG_FINAL %in% "DENGUE CON SIGNOS DE ALARMA",
               DG    = d$DES_DIAG_FINAL %in% "DENGUE GRAVE",
               DENV1 = d$DENGUE_SER_TRIPLEX %in% 1,
               DENV2 = d$DENGUE_SER_TRIPLEX %in% 2,
               DENV3 = d$DENGUE_SER_TRIPLEX %in% 3,
               DENV4 = d$DENGUE_SER_TRIPLEX %in% 4)
    }

    if(facet){
        # one copy of the records per value of var (one panel each)
        x <- purrr::map_dfr(var, function(v){
            x[cond(x, v), ] |> dplyr::mutate(categoria = v)
        })
    } else {
        # cases of any of the values of var, counted together
        keep <- Reduce(`|`, lapply(var, function(v) cond(x, v)))
        x <- x[keep, ] |>
            dplyr::mutate(categoria = paste(var, collapse = " + "))
    }

    if(nrow(x) == 0){
        stop("There are no ", paste(var, collapse = "/"),
             " cases for the selected year/weeks/state.")
    }

    # Step 2. aggregate the cases by locality and week ####
    x <- x |>
        dplyr::group_by(cvegeo, categoria, SEM) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        tidyr::pivot_wider(id_cols = c(cvegeo, categoria),
                           names_from = SEM,
                           values_from = n,
                           values_fill = 0,
                           names_sort = TRUE)

    # Step 3. load the localities, municipalities and states ####
    loc <- rgeomex::iter_2020_loc |>
        as.data.frame() |>
        dplyr::mutate(cvegeo = paste0(entidad, mun, loc)) |>
        dplyr::select(cvegeo, longitud, latitud) |>
        dplyr::distinct(cvegeo, .keep_all = TRUE)

    if(country){
        base_sf  <- rgeomex::AGEE_inegi19_mx               # states (fill)
        limit_sf <- rgeomex::AGEE_inegi19_mx               # state boundaries
    } else {
        base_sf  <- rgeomex::AGEM_inegi19_mx |>            # municipalities (fill)
            dplyr::filter(CVE_ENT %in% cve_edo)
        limit_sf <- rgeomex::AGEE_inegi19_mx |>            # state outline
            dplyr::filter(CVE_ENT %in% cve_edo)
    }

    # Step 4. joint ####
    y <- dplyr::left_join(x = x,
                          y = loc,
                          by = "cvegeo") |>
        dplyr::filter(!is.na(longitud), !is.na(latitud)) |>
        sf::st_as_sf(coords = c("longitud", "latitud"), crs = 4326) |>
        sf::st_transform(sf::st_crs(base_sf))

    # Step 5. make the dataset tidy (pivot_longer) ####
    y <- y |>
        tidyr::pivot_longer(cols = !c(cvegeo, categoria, geometry),
                            names_to = "week",
                            values_to = "n") |>
        as.data.frame() |>
        sf::st_set_geometry(value = "geometry") |>
        dplyr::mutate(week = as.numeric(week)) |>
        dplyr::mutate(week = forcats::fct_reorder(paste("Semana Epidemiológica",
                                                        week, sep = " "),
                                                  week),
                      categoria = factor(categoria,
                                         levels = if(facet) var else
                                             paste(var, collapse = " + "))) |>
        dplyr::filter(n >= 1) |>
        dplyr::arrange(dplyr::desc(n))          # small bubbles on top

    # breaks of the colour and size scales (shared by all frames and panels)
    # (the upper limit is at least 2: with max_n = 1 the limits c(1, 1)
    #  give the error "'breaks' are not unique")
    max_n <- max(y$n)
    lim_n <- max(max_n, 2)
    # default: 5 integer breaks from 1 to the maximum (fewer if max < 5)
    ticks <- if(is.null(breaks)){
        round(seq(from = 1, to = lim_n, length.out = 5))
    } else if(identical(breaks, "quantile")){
        # quartiles of the cases: min, Q1, median, Q3, max (repeated values
        # are dropped, which is common because most counts are 1 or 2)
        round(stats::quantile(y$n, probs = c(0, 0.25, 0.5, 0.75, 1),
                              names = FALSE))
    } else {
        seq(from = 1, to = lim_n, by = breaks)
    }
    ticks <- unique(ticks[ticks >= 1 & ticks <= lim_n])
    if(length(ticks) < 2) ticks <- c(1, lim_n)

    # Step 6. one ggplot per week (one panel per value of var) -> gif ####
    # (tmap mixes up the panel labels when facets and animation are combined)
    {
        opt  <- sub("^-", "", pal)
        dirc <- if(startsWith(pal, "-")) -1 else 1
        semanas <- levels(droplevels(y$week))
        sem_num <- as.numeric(sub("Semana Epidemiológica ", "", semanas))

        # weekly totals by panel (for the panel titles and the curve)
        tot <- y |>
            sf::st_drop_geometry() |>
            dplyr::mutate(week_num = as.numeric(sub("Semana Epidemiológica ", "",
                                                    as.character(week)))) |>
            dplyr::summarise(total = sum(n), .by = c(categoria, week_num)) |>
            tidyr::complete(categoria, week_num = sem_num,
                            fill = list(total = 0))

        # colours of the curves (Okabe-Ito)
        col_cat <- stats::setNames(
            rep(c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                  "#E69F00", "#56B4E9", "#F0E442", "#000000"),
                length.out = nlevels(y$categoria)),
            levels(y$categoria))

        tmp_dir <- tempfile("frames_")
        dir.create(tmp_dir)
        dpi_f <- 150

        # automatic width: the panels (fixed map aspect) fill the figure, so
        # the curve below has the same width as the grid of maps
        if(is.null(width)){
            nv   <- nlevels(y$categoria)
            ncl  <- if(is.na(ncol)) ceiling(sqrt(nv)) else min(ncol, nv)
            nrw  <- ceiling(nv / ncl)
            bbx  <- sf::st_bbox(base_sf)
            asp  <- (bbx[["xmax"]] - bbx[["xmin"]]) / (bbx[["ymax"]] - bbx[["ymin"]])
            if(isTRUE(sf::st_is_longlat(base_sf))){
                asp <- asp * cos(mean(c(bbx[["ymin"]], bbx[["ymax"]])) * pi / 180)
            }
            px      <- dpi_f / 72                          # pixels per point
            title_h <- (strip_size + 4 + 14) * px
            strip_h <- (strip_size + 12) * px
            gap     <- 0.4 / 2.54 * dpi_f                  # panel.spacing (0.4 cm)
            leg_h   <- if(map_legend) max(size_range) * 2.85 * px + 40 else 0
            map_h   <- height * (if(curve) 4 / 5.3 else 1) - title_h - leg_h - 20
            panel_h <- (map_h - nrw * strip_h - (nrw - 1) * gap) / nrw
            width   <- round(ncl * panel_h * asp + (ncl - 1) * gap + 30)
            width   <- max(width, 1200)                    # room for the curve legend
        }

        guia <- if(map_legend){
            ggplot2::guide_legend(nrow = 1,
                                  title.position = "top",
                                  title.hjust = 0.5,
                                  label.position = "bottom",
                                  label.hjust = 0.5)
        } else "none"

        pngs <- vapply(seq_along(semanas), function(i){
            d <- y[y$week == semanas[i], ]

            # panel titles: category + cases of the week
            tot_i <- tot[tot$week_num == sem_num[i], ]
            etiq  <- stats::setNames(
                paste0(tot_i$categoria, " · ",
                       format(tot_i$total, big.mark = ","), " casos"),
                as.character(tot_i$categoria))

            p <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = base_sf, fill = "gray85", color = "white",
                                 linewidth = if(country) 0.2 else 0.15) +
                ggplot2::geom_sf(data = limit_sf, fill = NA,
                                 color = if(country) "white" else "gray20",
                                 linewidth = if(country) 0.2 else 0.5) +
                ggplot2::geom_sf(data = d,
                                 ggplot2::aes(size = n, fill = n),
                                 shape = 21, color = "black",
                                 stroke = 0.3, alpha = 0.75)
            if(text_size > 0){
                p <- p +
                    ggplot2::geom_sf_text(data = d, ggplot2::aes(label = n),
                                          size = text_size * 7, color = "black")
            }
            p <- p +
                ggplot2::facet_wrap(~ categoria,
                                    ncol = if(is.na(ncol)) NULL else ncol,
                                    drop = FALSE,
                                    labeller = ggplot2::as_labeller(etiq)) +
                # same name, limits, breaks and guide -> one legend with coloured
                # bubbles, in one row, with the title above the bubbles
                ggplot2::scale_fill_viridis_c(legend_title,
                                              option = opt, direction = dirc,
                                              limits = c(1, lim_n), breaks = ticks,
                                              guide = guia) +
                ggplot2::scale_size_continuous(legend_title,
                                               range = size_range,
                                               limits = c(1, lim_n),
                                               breaks = ticks,
                                               guide = guia) +
                ggplot2::labs(title = semanas[i]) +
                ggplot2::theme_void(base_size = 16) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5,
                                                       size = strip_size + 4,
                                                       color = "gray20",
                                                       margin = ggplot2::margin(b = 8)),
                    strip.text = ggplot2::element_text(face = "bold", color = "white",
                                                       size = strip_size,
                                                       margin = ggplot2::margin(5, 0, 5, 0)),
                    strip.background = ggplot2::element_rect(fill = "#0E5F76",
                                                             color = NA),
                    panel.border = ggplot2::element_rect(fill = NA, color = "gray70"),
                    panel.spacing = ggplot2::unit(0.4, "cm"),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.title = ggplot2::element_text(face = "bold",
                                                         size = strip_size - 2,
                                                         color = "gray20"),
                    legend.text = ggplot2::element_text(size = strip_size - 4,
                                                        color = "gray20"),
                    legend.margin = ggplot2::margin(4, 0, 0, 0),
                    plot.margin = ggplot2::margin(10, 10, 10, 10))

            # weekly curve below the maps, with the current week marked
            if(curve){
                g <- ggplot2::ggplot(tot, ggplot2::aes(x = week_num, y = total,
                                                       color = categoria)) +
                    ggplot2::geom_vline(xintercept = sem_num[i],
                                        linetype = "dashed", color = "gray50") +
                    ggplot2::geom_line(linewidth = 1) +
                    ggplot2::geom_point(data = tot_i, size = 3.5) +
                    ggplot2::scale_color_manual(NULL, values = col_cat) +
                    ggplot2::scale_x_continuous("Semana epidemiológica",
                                                breaks = pretty(sem_num, n = 10)) +
                    ggplot2::scale_y_continuous("Casos") +
                    ggplot2::theme_minimal(base_size = 15) +
                    # legend inside the plot, top left (first weeks)
                    ggplot2::theme(legend.position = "inside",
                                   legend.position.inside = c(0.01, 0.97),
                                   legend.justification = c(0, 1),
                                   legend.direction = "vertical",
                                   legend.background = ggplot2::element_rect(
                                       fill = ggplot2::alpha("white", 0.8),
                                       color = NA),
                                   legend.key = ggplot2::element_blank(),
                                   legend.text = ggplot2::element_text(size = 14),
                                   panel.grid.minor = ggplot2::element_blank(),
                                   plot.background = ggplot2::element_rect(fill = "white",
                                                                           color = NA),
                                   plot.margin = ggplot2::margin(5, 20, 10, 20))
                p <- cowplot::plot_grid(p, g, ncol = 1,
                                        rel_heights = c(4, 1.3))
            }

            f <- file.path(tmp_dir, sprintf("frame_%03d.png", i))
            ggplot2::ggsave(f, p, width = width / dpi_f, height = height / dpi_f,
                            dpi = dpi_f)
            f
        }, character(1))

        gif <- if(save){
            paste(dir, paste(name, "loc_animated_map.gif", sep = "_"), sep = "")
        } else {
            tempfile(fileext = ".gif")
        }
        gifski::gifski(pngs, gif_file = gif, width = width, height = height,
                       delay = 1 / fps, progress = FALSE)
        unlink(tmp_dir, recursive = TRUE)

        # show the gif in the RStudio Viewer (the Viewer only opens files
        # inside tempdir(), so the gif is copied there with a small html page)
        if(interactive()){
            view_dir <- tempfile("viewer_")
            dir.create(view_dir)
            file.copy(gif, file.path(view_dir, "animation.gif"))
            html <- file.path(view_dir, "index.html")
            writeLines(paste0("<html><body style='margin:0;background:white;'>",
                              "<img src='animation.gif' ",
                              "style='width:100%;height:auto;'></body></html>"),
                       html)
            viewer <- getOption("viewer")
            if(!is.null(viewer)) viewer(html) else utils::browseURL(gif)
        }
        return(invisible(gif))
    }
}
