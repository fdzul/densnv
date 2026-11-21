#' rt_dengue
#'
#' Effective reproductive number (Rt) of dengue for the country, a state, a
#' municipality or a locality, with the daily incidence of the SINAVE cases
#' and a parametric serial interval ([EpiEstim::estimate_R()]).
#'
#' @param dataset SINAVE dengue dataset (data.table or data.frame, e.g. read
#'   with `data.table::fread("DENGUE2_.txt", quote = "", fill = TRUE)`). It
#'   needs `FEC_INI_SIGNOS_SINT`, `ESTATUS_CASO` and, depending on `level`
#'   and `place`, `CVE_EDO_*`, `CVE_MPO_*` and `CVE_LOC_*` (`DES_*` give the
#'   name of the area).
#' @param level level of analysis: `"country"`, `"state"`, `"municipality"`
#'   or `"locality"`.
#' @param place place used to select the cases: `"REP"` place of report
#'   (default; the cases are geocoded with it) or `"RES"` place of residence.
#' @param cve_edo INEGI state code (e.g. `"31"` or 31). Required for
#'   `"state"`, `"municipality"` and `"locality"`.
#' @param cve_mpo INEGI municipality code (e.g. `"050"` or 50). Required for
#'   `"municipality"` and `"locality"`.
#' @param cve_loc INEGI locality code (e.g. `"0001"` or 1). Required for
#'   `"locality"`. `cve_mpo` and `cve_loc` can be vectors of the same length
#'   (pairs municipality-locality).
#' @param status_caso case status: 1 probable, 2 confirmed (default
#'   `c(1, 2)`).
#' @param ref_date reference date (`Date` or `"YYYY-MM-DD"`); NULL (default)
#'   uses the latest date of onset of the selected cases.
#' @param period_days days, up to `ref_date`, of the daily incidence used to
#'   estimate Rt (default 365).
#' @param window_days days, up to `ref_date`, of the window whose Rt gives
#'   the phase of the outbreak (default 21).
#' @param mean_si,std_si mean and standard deviation (days) of the serial
#'   interval, gamma distribution (default 21 and 6.1).
#' @param plot logical. If TRUE (default) the Rt curve is printed.
#'
#' @details
#' By default the area is selected with the place of report of the case
#' (`place = "REP"`), not the place of residence (`"RES"`): in non-endemic
#' areas (e.g. Mexico City) the positive cases are reported in the place where
#' they were exposed, so the place of report is the place of transmission. In
#' endemic areas both give almost the same cases.
#'
#' Rt is estimated with [EpiEstim::estimate_R()] (Cori et al. 2013), method
#' `"parametric_si"`, on weekly sliding windows with at least 10 cases
#' (curve; with fewer cases the estimate reflects the prior) that start at
#' least `mean_si` + 2 `std_si` days after the first date of onset of the
#' dataset or of the period, whichever is later (before it the
#' infectiousness of the previous cases is unknown and
#' Rt is overestimated), and on the window of
#' `window_days` days up to `ref_date`. The phase of the outbreak is given by
#' the 95\% credible interval of the Rt of that window: "En ascenso" (lower
#' limit > 1), "En descenso" (upper limit < 1) or "Estable". The last days
#' are usually incomplete (delay of report), which lowers Rt at the end.
#'
#' @return a list (invisibly) with:
#' \describe{
#'   \item{rt}{object of [EpiEstim::estimate_R()], weekly sliding windows
#'     with at least 10 cases; NULL if it cannot be estimated.}
#'   \item{rt_ventana}{data.frame with the window (`inicio`, `fin`), Rt
#'     (`rt`), its 95\% credible interval (`rt_inf`, `rt_sup`) and the phase
#'     (`fase`).}
#'   \item{incidencia}{data.frame with the daily incidence (`dates`, `I`).}
#'   \item{area}{name of the area.}
#'   \item{plot}{ggplot of the Rt curve (NULL if Rt was not estimated).}
#' }
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @references Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new
#'   framework and software to estimate time-varying reproduction numbers
#'   during epidemics. *American Journal of Epidemiology*, 178(9), 1505-1512.
#'
#' @seealso [EpiEstim::estimate_R()], [mp_active_foci()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- data.table::fread("DENGUE2_.txt", quote = "", fill = TRUE)
#'
#' # Mexico
#' rt_mx <- rt_dengue(dataset = dataset,
#'                    level = "country",
#'                    place = "REP",
#'                    cve_edo = NULL,
#'                    cve_mpo = NULL,
#'                    cve_loc = NULL,
#'                    status_caso = c(1, 2),
#'                    ref_date = NULL,
#'                    period_days = 365,
#'                    window_days = 21,
#'                    mean_si = 21,
#'                    std_si = 6.1,
#'                    plot = TRUE)
#' rt_mx$rt_ventana
#'
#' # Acapulco de Juarez (Guerrero), locality 0001 of the municipality 001
#' rt_aca <- rt_dengue(dataset = dataset,
#'                     level = "locality",
#'                     place = "REP",
#'                     cve_edo = "12",
#'                     cve_mpo = "001",
#'                     cve_loc = "0001",
#'                     status_caso = c(1, 2),
#'                     ref_date = NULL,
#'                     period_days = 365,
#'                     window_days = 21,
#'                     mean_si = 21,
#'                     std_si = 6.1,
#'                     plot = TRUE)
#' }
rt_dengue <- function(dataset,
                      level = c("country", "state", "municipality", "locality"),
                      place = c("REP", "RES"),
                      cve_edo = NULL,
                      cve_mpo = NULL,
                      cve_loc = NULL,
                      status_caso = c(1, 2),
                      ref_date = NULL,
                      period_days = 365,
                      window_days = 21,
                      mean_si = 21,
                      std_si = 6.1,
                      plot = TRUE){

    level <- match.arg(level)
    place <- match.arg(place)
    col_edo <- paste0("CVE_EDO_", place)
    col_mpo <- paste0("CVE_MPO_", place)
    col_loc <- paste0("CVE_LOC_", place)
    rlang::check_installed("EpiEstim", reason = "to estimate Rt.")

    # Step 0. checks ####
    a_num <- function(x) suppressWarnings(as.integer(as.numeric(trimws(x))))
    a_utf8 <- function(x){
        x <- as.character(x)
        bad <- !is.na(x) & !validUTF8(x)
        x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
        x
    }
    need <- c("FEC_INI_SIGNOS_SINT", "ESTATUS_CASO",
              switch(level,
                     country      = character(0),
                     state        = col_edo,
                     municipality = c(col_edo, col_mpo),
                     locality     = c(col_edo, col_mpo, col_loc)))
    falta <- setdiff(need, names(dataset))
    if(length(falta) > 0){
        stop("The dataset does not have the columns: ", paste(falta, collapse = ", "), ".")
    }
    if(level != "country" && is.null(cve_edo)) stop("cve_edo is required.")
    if(level %in% c("municipality", "locality") && is.null(cve_mpo)){
        stop("cve_mpo is required.")
    }
    if(level == "locality"){
        if(is.null(cve_loc)) stop("cve_loc is required.")
        if(length(cve_loc) != length(cve_mpo)){
            stop("cve_mpo and cve_loc must have the same length.")
        }
    }

    # Step 1. cases of the area (place of report or residence) ####
    x <- as.data.frame(dataset)
    # first date of the dataset: before it there is no history of cases
    datos_ini <- suppressWarnings(min(as.Date(x$FEC_INI_SIGNOS_SINT), na.rm = TRUE))
    x <- x[x$ESTATUS_CASO %in% status_caso, , drop = FALSE]
    if(level != "country"){
        x <- x[a_num(x[[col_edo]]) %in% a_num(cve_edo), , drop = FALSE]
    }
    if(level == "municipality"){
        x <- x[a_num(x[[col_mpo]]) %in% a_num(cve_mpo), , drop = FALSE]
    }
    if(level == "locality"){
        par_x <- paste(a_num(x[[col_mpo]]), a_num(x[[col_loc]]))
        x <- x[par_x %in% paste(a_num(cve_mpo), a_num(cve_loc)), , drop = FALSE]
    }
    x$onset <- as.Date(x$FEC_INI_SIGNOS_SINT)
    x <- x[!is.na(x$onset), , drop = FALSE]

    # name of the area
    nombre <- function(col){
        if(col %in% names(x) && nrow(x) > 0){
            paste(unique(a_utf8(x[[col]])), collapse = ", ")
        } else NA_character_
    }
    area <- switch(level,
                   country      = "M\u00e9xico",
                   state        = nombre(paste0("DES_EDO_", place)),
                   municipality = nombre(paste0("DES_MPO_", place)),
                   locality     = nombre(paste0("DES_LOC_", place)))
    if(is.na(area) || area == "") {
        area <- paste(c(cve_edo, cve_mpo, cve_loc), collapse = "-")
    }

    # Step 2. daily incidence of the period ####
    ref_date <- if(is.null(ref_date)){
        if(nrow(x) == 0) stop("There are no cases in the selected area.")
        max(x$onset)
    } else as.Date(ref_date)
    per_ini <- ref_date - period_days + 1
    win_ini <- ref_date - window_days + 1
    x <- x[x$onset >= per_ini & x$onset <= ref_date, , drop = FALSE]
    incidencia <- data.frame(dates = per_ini + seq_len(period_days) - 1,
                             I = tabulate(as.numeric(x$onset - per_ini) + 1,
                                          nbins = period_days))

    # Step 3. Rt (EpiEstim, parametric serial interval) ####
    min_casos_semana <- 10
    historia <- ceiling(mean_si + 2 * std_si)    # days of history of cases
    # the history counts from the first date of the dataset or of the period
    # (whichever is later): the cases before the period are not in the
    # incidence, so their infectiousness is not counted
    inicio_hist <- max(datos_ini, per_ini)
    rt <- rt_win <- NULL
    if(sum(incidencia$I) > 0 && period_days > window_days){
        # weekly sliding windows with at least min_casos_semana cases (with
        # fewer cases the estimate is dominated by the prior of EpiEstim) and
        # that start at least `historia` days after the first date of the
        # dataset (before, the infectiousness of the previous cases is unknown
        # and Rt is overestimated)
        t_end   <- 8:period_days
        t_start <- t_end - 6
        casos_w <- vapply(t_end, function(k) sum(incidencia$I[(k - 6):k]), numeric(1))
        ok <- casos_w >= min_casos_semana &
            incidencia$dates[t_start] >= inicio_hist + historia
        if(any(ok)){
            rt <- tryCatch(suppressWarnings(suppressMessages(
                EpiEstim::estimate_R(incidencia,
                                     method = "parametric_si",
                                     config = EpiEstim::make_config(
                                         list(mean_si = mean_si, std_si = std_si,
                                              t_start = t_start[ok],
                                              t_end = t_end[ok]))))),
                error = function(e) NULL)
        }
        rt_win <- tryCatch(suppressWarnings(suppressMessages(
            EpiEstim::estimate_R(incidencia,
                                 method = "parametric_si",
                                 config = EpiEstim::make_config(
                                     list(mean_si = mean_si, std_si = std_si,
                                          t_start = period_days - window_days + 1,
                                          t_end = period_days)))))$R,
            error = function(e) NULL)
    }
    if(win_ini < inicio_hist + historia) rt_win <- NULL    # no history: unreliable
    rt_med <- if(is.null(rt_win)) NA_real_ else rt_win[["Mean(R)"]][1]
    rt_inf <- if(is.null(rt_win)) NA_real_ else rt_win[["Quantile.0.025(R)"]][1]
    rt_sup <- if(is.null(rt_win)) NA_real_ else rt_win[["Quantile.0.975(R)"]][1]
    fase <- if(is.na(rt_med)) "Sin estimar" else
            if(rt_inf > 1) "En ascenso" else
            if(rt_sup < 1) "En descenso" else "Estable"
    rt_ventana <- data.frame(inicio = win_ini,
                             fin = ref_date,
                             casos = sum(incidencia$I[incidencia$dates >= win_ini]),
                             rt = rt_med,
                             rt_inf = rt_inf,
                             rt_sup = rt_sup,
                             fase = fase)
    if(sum(incidencia$I) < 50){
        warning("Fewer than 50 cases in the period (", sum(incidencia$I),
                "): Rt is unreliable.")
    }

    # Step 4. plot ####
    g <- NULL
    if(!is.null(rt)){
        g <- tryCatch(
            plot(rt,
                 legend = FALSE,
                 what = "R",
                 options_R = list(col = "red", transp = 0.5)) +
                ggplot2::geom_hline(yintercept = 1, linetype = 2, col = "gray40") +
                ggplot2::geom_vline(xintercept = win_ini, col = "blue", linetype = 3) +
                ggplot2::ylab("N\u00famero reproductivo efectivo (Rt)") +
                ggplot2::xlab("") +
                ggplot2::ggtitle(paste0(area, ". Rt de la ventana (", format(win_ini, "%d/%m"),
                                        " a ", format(ref_date, "%d/%m/%Y"), "): ",
                                        round(rt_med, 2), " (IC95% ", round(rt_inf, 2),
                                        " a ", round(rt_sup, 2), "), ", tolower(fase))) +
                ggplot2::theme(title = ggplot2::element_text(size = 8),
                               axis.text.x = ggplot2::element_text(size = 6)),
            error = function(e) NULL)
        if(plot && !is.null(g)) print(g)
    }

    message(area, ": ", sum(incidencia$I), " cases (", per_ini, " to ", ref_date,
            "); Rt ", win_ini, " to ", ref_date, " = ", round(rt_med, 2),
            " (95% CrI ", round(rt_inf, 2), " to ", round(rt_sup, 2), "), ",
            tolower(fase), ".")

    invisible(list(rt = rt,
                   rt_ventana = rt_ventana,
                   incidencia = incidencia,
                   area = area,
                   plot = g))
}
