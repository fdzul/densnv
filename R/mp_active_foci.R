#' mp_active_foci
#'
#' Identifies the active dengue transmission foci of a locality and the blocks
#' (manzanas) to control, using a space-time window: cases within the flight
#' range of *Aedes aegypti* (`spatial_scale`) and within the intrinsic +
#' extrinsic incubation periods (`temporal_scale`).
#'
#' @param geocoded_dataset data.table or data.frame with the geocoded cases.
#'   It must have the columns `long`, `lat` (EPSG:4326), `ESTATUS_CASO` and
#'   `FEC_INI_SIGNOS_SINT` (date of onset of symptoms).
#' @param cve_edo INEGI state code of the locality (e.g. `"31"`).
#' @param locality name of the locality, as used by
#'   [rgeomex::extract_locality()] (e.g. `"Merida"`).
#' @param status_caso case status to use: 1 probable, 2 confirmed (default
#'   `c(1, 2)`).
#' @param spatial_scale spatial window in metres: flight range of
#'   *Aedes aegypti* (default 400).
#' @param temporal_scale temporal window in days: intrinsic + extrinsic
#'   incubation periods (default 21).
#' @param min_cases minimum number of cases within the space-time window to
#'   define a focus (default 3).
#' @param buffer_m buffer for control around the transmission zone, in metres
#'   (default 200; about 100 m is one block).
#' @param ref_date reference date (`Date` or `"YYYY-MM-DD"`). The cases with
#'   onset in the `temporal_scale` days up to this date are used. If NULL
#'   (default), the latest date of onset in the locality is used.
#' @param method how the foci are identified:
#'   \describe{
#'     \item{`"window"`}{(default) fixed window: only the cases of the last
#'       `temporal_scale` days are used, and DBSCAN is applied with neighbours
#'       within `spatial_scale` metres. Answers "where are the foci today?".}
#'     \item{`"st-dbscan"`}{space-time DBSCAN: all the cases up to `ref_date`
#'       are used, and two cases are neighbours if they are within
#'       `spatial_scale` metres AND within `temporal_scale` days. It rebuilds
#'       the transmission chains, so a focus can last several weeks. Active
#'       foci are those with at least one case in the last `temporal_scale`
#'       days; they are classified as new (started in the window) or
#'       persistent (started before).}
#'   }
#' @param n_sim number of permutations of the Knox test (default 999;
#'   0 skips the test).
#' @param perm_days period, in days up to `ref_date`, of the cases used by the
#'   Knox test (their dates are permuted; default 365). It is also the period
#'   used by `method = "st-dbscan"`. Must be >= `temporal_scale`.
#' @param alpha significance level of the Knox test by focus, compared with
#'   the Holm-adjusted p-value (default 0.05).
#' @param seed seed of the permutations, to reproduce the p-values (default
#'   NULL).
#' @param place place used to select the cases of the locality for Rt with
#'   [rt_dengue()]: `"REP"` place of report (default; the cases are geocoded
#'   with it) or `"RES"` place of residence.
#' @param mean_si,std_si mean and standard deviation (days) of the serial
#'   interval of dengue, gamma distribution, used to estimate Rt with
#'   [rt_dengue()] (default 21 and 6.1).
#' @param coverage_min minimum proportion (0-1) of the locality covered by
#'   the recent cases (circles of `spatial_scale` metres) to consider the
#'   cases dispersed over the locality. NULL (default) depends on
#'   `temporal_scale`: 0.40 with 21 days or less, 0.50 with 28 days or more,
#'   and linear in between (calibrated with 71 localities, 2026; the coverage
#'   grows with the window).
#' @param foci_min proportion (0-1) of the recent cases in foci from which the
#'   cases are also considered dispersed over the locality, whatever the
#'   coverage (default 0.9; safeguard for large cities, where the coverage is
#'   low even when the transmission covers the whole urban area).
#' @param capacity_cases operational capacity: number of cases per week that
#'   the brigades can work (the house of the case and the houses around it).
#'   It is local information given by the expert (default NULL, not
#'   evaluated). Used in the diagnosis to tell whether the outbreak exceeds
#'   the operational capacity (barrido vs operativo integral).
#' @param print_map logical. If TRUE (default) the map is printed; FALSE is
#'   useful in loops over several localities or dates (the map is still
#'   returned in `map`).
#' @param save logical. If TRUE the selected blocks are saved as a GeoPackage
#'   in `dir` (default FALSE).
#' @param dir directory where the blocks are saved (with "/" at the end).
#'
#' @details
#' The foci are identified with the DBSCAN criterion:
#' \enumerate{
#'   \item Temporal window: the window is the `temporal_scale` days up to
#'     `ref_date` (both included). With `method = "window"` only these cases
#'     are used; with `method = "st-dbscan"` all the cases up to `ref_date`
#'     are used and the time difference is part of the neighbourhood.
#'   \item Spatial window: a case is a *core* case if there are at least
#'     `min_cases` neighbours (itself included) within `spatial_scale` metres
#'     (and within `temporal_scale` days with ST-DBSCAN). Neighbouring core
#'     cases belong to the same focus, and non-core neighbours of a core case
#'     join that focus.
#'   \item Transmission zone: union of circles of `spatial_scale` metres
#'     around the cases of each focus in the window.
#'   \item Control zone: transmission zone plus `buffer_m` metres. The blocks
#'     that intersect the control zone are selected.
#' }
#' Knox test (space-time interaction; Knox 1964): the Knox statistic is the
#' number of pairs of cases of the last `perm_days` days that are within
#' `spatial_scale` metres and within `temporal_scale` days of each other, the
#' same space-time window that defines the foci (and the same statistic as
#' `knox::knox_mc()` and `denhotspots::knox()`). Its null distribution is
#' obtained by permutation: the locations stay fixed (so the spatial
#' distribution of the population is respected) and the dates of onset are
#' permuted at random (so the epidemic curve is respected). p-value =
#' (1 + permutations with as many pairs or more) / (`n_sim` + 1); RR =
#' observed pairs / mean of the permuted pairs.
#' \enumerate{
#'   \item Global test: all the pairs of the period. It answers whether the
#'     cases of the locality are closer in space and time than expected by
#'     chance. It is a formal test.
#'   \item Test by focus: only the pairs with both cases inside the
#'     transmission zone of the focus, with the dates permuted among all the
#'     cases of the period; p-values adjusted with Holm (`p_holm`). The foci
#'     were selected because their cases are close in space and time, so
#'     these p-values are optimistic: use them to rank and prioritise the
#'     foci, not as a formal test.
#' }
#' Unlike the size of the largest focus, the number of pairs does not
#' percolate: in a dense city the permuted dates do not join more cases into
#' the same pairs.
#'
#' Diagnosis of the epidemiological scenario (`diagnostico`). Following dengue
#' outbreak experts, transmission becomes generalised with:
#' \enumerate{
#'   \item A sustained and significant increase of the outbreak: the cases of
#'     the current window are significantly above the mean per window of
#'     `temporal_scale` days of the period (Poisson test, p < `alpha`), the
#'     previous window is also above that mean and the effective reproductive
#'     number of the current window is above 1 (Rt with [rt_dengue()]: all
#'     the cases reported in the locality, `place`, geocoded or not;
#'     parametric serial interval with `mean_si` and `std_si`; Cori et al.
#'     2013). Rt is ignored if it cannot be estimated.
#'   \item Cases that exceed the operational capacity: the cases per week of
#'     the current window (all the cases, probable and confirmed) are more
#'     than `capacity_cases`.
#'   \item Cases dispersed over the locality: the area within
#'     `spatial_scale` metres of the recent cases covers `coverage_min` or
#'     more of the locality, or `foci_min` or more of the recent cases are in
#'     foci.
#' }
#' The phase of the outbreak is given by the 95\% credible interval of Rt:
#' "En ascenso" (lower limit > 1), "En descenso" (upper limit < 1) or
#' "Estable".
#'
#' Scenarios (rules of thumb calibrated with 71 localities, 2026):
#' \enumerate{
#'   \item Insufficient cases: fewer than 50 cases in the period.
#'   \item Generalised transmission: criterion 1 and (2 or 3). Strategy:
#'     barrido; operativo integral (megaoperativo) if the blocks exceed the
#'     operational capacity.
#'   \item No active foci. Strategy: entomological surveillance and control
#'     around the cases.
#'   \item Focalised transmission: 50\% or more of the recent cases in foci.
#'     Strategy: focalised control, prioritising the significant foci.
#'   \item Focalised transmission with dispersed cases: fewer than 50\% of
#'     the recent cases in foci. Strategy: focalised control and control
#'     around the isolated cases.
#' }
#' With criterion 1 and Rt in ascent ("En ascenso") but without generalised
#' transmission, the action starts with an early warning (alerta temprana). The space-time interaction
#' (Knox) is reported as "Sí" (p < `alpha`), "No concluyente" (RR > 1.05) or
#' "No". The strategies summarise the "Resumen de Estrategias Operativas"
#' (focalizado, barrido, operativo integral). Areas are clipped to the
#' locality.
#'
#' DBSCAN is run with the \pkg{dbscan} package (Hahsler, Piekenbrock and Doran
#' 2019): [dbscan::frNN()] finds the neighbours within `spatial_scale` metres
#' and, with ST-DBSCAN, the neighbours further than `temporal_scale` days are
#' removed before [dbscan::dbscan()] is applied with `minPts = min_cases`.
#' Distances are computed in metres (EPSG:6372, Mexico ITRF2008 / LCC).
#' Blocks come from `rgeomex::blocks_ine20_mx_*` (INE 2020). The map opens
#' with the Positron style (OpenFreeMap); a collapsible layers control turns
#' on and off the basemaps (Esri Dark Gray, Esri Light Gray, OpenStreetMap,
#' World Imagery, OpenTopoMap) and the zones, blocks and cases. The
#' transmission zone of each focus is coloured by its Knox test: red,
#' significant (Holm p-value < `alpha`); yellow, excess of pairs not
#' significant (RR > 1); grey, no excess (RR <= 1). With `n_sim = 0` all the
#' zones are red.
#'
#' @return a list (invisibly) with:
#' \describe{
#'   \item{map}{interactive map (mapgl), also printed.}
#'   \item{foci}{sf with one row per active focus: type (new/persistent),
#'     cases, recent cases (in the window), confirmed cases, Knox pairs,
#'     expected pairs and RR, p-value, Holm p-value and significance, first and last
#'     onset, duration (days), transmission and control areas (km2) and number
#'     of blocks; geometry = control zone.}
#'   \item{transmission}{sf with the transmission zone of each focus.}
#'   \item{blocks}{sf with the blocks to control and their focus.}
#'   \item{cases}{sf with the cases of the window (and, with ST-DBSCAN, the
#'     previous cases of the active foci), their focus (NA if isolated) and
#'     group.}
#'   \item{knox}{global Knox test: list with the number of permutations
#'     (`n_sim`), the period (`periodo`), the number of cases (`n_casos`), the
#'     pairs within `spatial_scale` (`pares_espacio`), the Knox statistic
#'     (`knox`), the expected pairs (`esperados`), `rr`, `p_value` and the
#'     Knox statistic of each permutation (`sim`); NULL if `n_sim = 0`.}
#'   \item{diagnostico}{diagnosis of the epidemiological scenario: one-row
#'     summary (`tabla`, to bind several localities), scenario
#'     (`escenario`), operational strategy (`accion`), detail of the criteria
#'     (`detalle`, also printed), Rt by weekly sliding windows (`rt`, object
#'     of [EpiEstim::estimate_R()]) and its plot (`rt_plot`), cases by window of `temporal_scale` days
#'     (`casos_ventana`, the first is the current window), cases by month
#'     (`casos_mes`), proportion of the pairs of cases within
#'     `temporal_scale` days (`pct_pares_tiempo`), coverage of the locality
#'     by the recent cases (`cobertura_casos`), recent cases in foci
#'     (`pct_casos_en_focos`) and area of the transmission zones
#'     (`pct_area_transmision`).}
#' }
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @references Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new
#'   framework and software to estimate time-varying reproduction numbers
#'   during epidemics. *American Journal of Epidemiology*, 178(9), 1505-1512.
#'
#'   Knox EG (1964). The detection of space-time interactions.
#'   *Journal of the Royal Statistical Society, Series C*, 13(1), 25-30.
#'
#'   Kulldorff M, Hjalmars U (1999). The Knox method and other tests
#'   for space-time interaction. *Biometrics*, 55(2), 544-552.
#'
#'   Hahsler M, Piekenbrock M, Doran D (2019). dbscan: Fast
#'   Density-Based Clustering with R. *Journal of Statistical Software*,
#'   91(1), 1-30. \doi{10.18637/jss.v091.i01}
#'
#' @seealso [rt_dengue()], [mp_heatmap_gl()], [dbscan::dbscan()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- mp_active_foci(geocoded_dataset = z,
#'                       cve_edo = "31",
#'                       locality = "Merida",
#'                       status_caso = c(1, 2),
#'                       spatial_scale = 400,
#'                       temporal_scale = 21,
#'                       min_cases = 3,
#'                       buffer_m = 200,
#'                       ref_date = NULL,
#'                       method = "window",
#'                       n_sim = 999,
#'                       perm_days = 365,
#'                       alpha = 0.05,
#'                       seed = 123,
#'                       place = "REP",
#'                       mean_si = 21,
#'                       std_si = 6.1,
#'                       capacity_cases = NULL,
#'                       coverage_min = NULL,
#'                       foci_min = 0.9,
#'                       print_map = TRUE,
#'                       save = FALSE,
#'                       dir = "./")
#' res$foci      # summary of the foci
#' res$blocks    # blocks to control
#' res$knox      # global Knox test
#' hist(res$knox$sim)                      # null distribution of the pairs
#' abline(v = res$knox$knox, col = "red")  # observed pairs
#' res$diagnostico$tabla       # scenario, criteria and action
#' res$diagnostico$casos_mes   # cases by month of the period
#'
#' # space-time DBSCAN: transmission chains, new and persistent foci
#' res_st <- mp_active_foci(geocoded_dataset = z,
#'                          cve_edo = "31",
#'                          locality = "Merida",
#'                          status_caso = c(1, 2),
#'                          spatial_scale = 400,
#'                          temporal_scale = 21,
#'                          min_cases = 3,
#'                          buffer_m = 200,
#'                          ref_date = NULL,
#'                          method = "st-dbscan",
#'                          n_sim = 999,
#'                          perm_days = 365,
#'                          alpha = 0.05,
#'                          seed = 123,
#'                          place = "REP",
#'                          mean_si = 21,
#'                          std_si = 6.1,
#'                          capacity_cases = NULL,
#'                          coverage_min = NULL,
#'                          foci_min = 0.9,
#'                          print_map = TRUE,
#'                          save = FALSE,
#'                          dir = "./")
#' }
mp_active_foci <- function(geocoded_dataset,
                           cve_edo,
                           locality,
                           status_caso = c(1, 2),
                           spatial_scale = 400,
                           temporal_scale = 21,
                           min_cases = 3,
                           buffer_m = 200,
                           ref_date = NULL,
                           method = c("window", "st-dbscan"),
                           n_sim = 999,
                           perm_days = 365,
                           alpha = 0.05,
                           seed = NULL,
                           place = c("REP", "RES"),
                           mean_si = 21,
                           std_si = 6.1,
                           capacity_cases = NULL,
                           coverage_min = NULL,
                           foci_min = 0.9,
                           print_map = TRUE,
                           save = FALSE,
                           dir = "./"){

    # Step 0. checks ####
    method <- match.arg(method)
    place  <- match.arg(place)
    need <- c("long", "lat", "ESTATUS_CASO", "FEC_INI_SIGNOS_SINT")
    falta <- setdiff(need, names(geocoded_dataset))
    if(length(falta) > 0){
        stop("geocoded_dataset has no column(s): ", paste(falta, collapse = ", "))
    }
    cve_edo <- stringr::str_pad(as.character(cve_edo), 
                                width = 2,
                                side = "left", 
                                pad = "0")
    crs_m <- 6372                                   # metres

    # text to valid UTF-8 (SINAVE and some catalogues are Latin-1): 
    # MapLibre needs UTF-8 to build the GeoJSON
    a_utf8 <- function(x){
        fix <- function(v){
            v <- as.character(v)
            bad <- !is.na(v) & !validUTF8(v)
            v[bad] <- iconv(v[bad], from = "latin1", to = "UTF-8", sub = "")
            enc2utf8(v)
        }
        dplyr::mutate(x, dplyr::across(dplyr::where(is.character), fix))
    }

    # Step 1. locality ####
    loc <- rgeomex::extract_locality(cve_edo = cve_edo,
                                     locality = locality) |>
        sf::st_transform(4326) |>
        a_utf8() |>          
        sf::st_make_valid()

    # Step 2. cases of the locality  ####
    z <- geocoded_dataset |>
        as.data.frame() |>
        a_utf8() |> 
        dplyr::filter(!is.na(long), !is.na(lat),
                      ESTATUS_CASO %in% status_caso) |>
        dplyr::mutate(onset = as.Date(FEC_INI_SIGNOS_SINT)) |>
        dplyr::filter(!is.na(onset)) |>
        sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
    z <- z[loc, ]

    if(nrow(z) == 0){
        stop("There are no geocoded cases with date of onset in ", 
             locality, ".")
    }

    # Step 3. spatial and temporal window ####
    # temporal window: the temporal_scale days up to ref_date (both included);
    # spatial window: spatial_scale metres (eps of DBSCAN)
    ref_date <- if(is.null(ref_date)) max(z$onset) else as.Date(ref_date)
    win_ini  <- ref_date - temporal_scale + 1          # first day of the window
    per_ini  <- ref_date - perm_days + 1               # first day of the period
    if(perm_days < temporal_scale){
        stop("perm_days must be greater than or equal to temporal_scale.")
    }

    # cases of the period (perm_days up to ref_date): used by ST-DBSCAN and by
    # the Knox test (their locations stay fixed, their dates are permuted)
    z_per <- z |>
        dplyr::filter(onset >= per_ini, onset <= ref_date) |>
        sf::st_transform(crs_m)

    # Step 4. foci with DBSCAN (package dbscan, Hahsler et al. 2019) ####
    if(method == "window"){
        # 4a. fixed window: only the cases of the last temporal_scale days,
        #     neighbours = cases within spatial_scale metres
        z <- z |>
            dplyr::filter(onset >= win_ini, onset <= ref_date) |>
            sf::st_transform(crs_m)
        if(nrow(z) == 0){
            stop("There are no cases between ", win_ini, " and ", ref_date, ".")
        }
        xy <- sf::st_coordinates(z)                     # metres (EPSG:6372)
        nn <- dbscan::frNN(xy, eps = spatial_scale)     # neighbours in space

    } else {
        # 4b. ST-DBSCAN: all the cases of the period (perm_days up to ref_date),
        #     neighbours = cases within spatial_scale metres AND temporal_scale days
        z <- z_per
        if(nrow(z) == 0){
            stop("There are no cases between ", per_ini, " and ", ref_date, ".")
        }
        xy <- sf::st_coordinates(z)
        nn <- dbscan::frNN(xy, eps = spatial_scale)
        t  <- as.numeric(z$onset)
        for(i in seq_along(nn$id)){
            keep <- abs(t[nn$id[[i]]] - t[i]) <= temporal_scale
            nn$id[[i]]   <- nn$id[[i]][keep]            # neighbours in space AND time
            nn$dist[[i]] <- nn$dist[[i]][keep]
        }
    }

    # DBSCAN: core = at least min_cases cases (itself included) in the
    # neighbourhood; border cases join the focus of their core case;
    # cluster 0 = isolated cases
    db <- dbscan::dbscan(nn, minPts = min_cases, borderPoints = TRUE)
    z$core <- lengths(nn$id) >= (min_cases - 1)
    z$foco <- ifelse(db$cluster == 0, NA_integer_, as.integer(db$cluster))

    if(method == "window"){
        z$reciente <- TRUE
    } else {
        z$reciente <- z$onset >= win_ini

        # active foci: those with at least one case in the last temporal_scale days
        activos <- unique(z$foco[!is.na(z$foco) & z$reciente])
        z$foco[!z$foco %in% activos] <- NA_integer_
        z$foco <- match(z$foco, sort(activos))           # renumber 1, 2, ...

        # keep the cases of the window and the previous cases of the active foci
        z <- z[z$reciente | !is.na(z$foco), ]
        if(!any(z$reciente)){
            stop("There are no cases between ", win_ini, " and ", ref_date, ".")
        }
    }

    # Step 4.1. Knox test (space-time interaction) ####
    # Knox statistic = number of pairs of cases of the period that are within
    # spatial_scale metres AND within temporal_scale days of each other (the
    # same statistic as knox::knox_mc() and denhotspots::knox()).
    # H0: the date of each case is independent of its location. The locations
    # stay fixed (population) and the dates of onset are permuted (epidemic
    # curve). The pairs close in space are found once with dbscan::frNN(), so
    # only those pairs are checked in each permutation.
    # By focus: only the pairs with both cases inside the transmission zone of
    # the focus; p-values adjusted with Holm for the number of foci.
    prueba_knox <- function(zonas = NULL){
        if(!is.null(seed)) set.seed(seed)
        tt  <- as.numeric(z_per$onset)
        n_p <- length(tt)
        nn_p <- dbscan::frNN(sf::st_coordinates(z_per), eps = spatial_scale)
        i <- rep(seq_len(n_p), lengths(nn_p$id))
        j <- unlist(nn_p$id, use.names = FALSE)
        keep <- i < j                                   # each pair once
        i <- i[keep]
        j <- j[keep]

        # pairs of each focus: both cases inside its transmission zone
        en_zona <- list()
        if(!is.null(zonas)){
            inc <- as.matrix(sf::st_intersects(z_per, zonas))   # cases x foci
            en_zona <- lapply(seq_len(nrow(zonas)),
                              function(f) inc[i, f] & inc[j, f])
        }

        # Knox statistic: global and by focus
        cuenta <- function(t){
            cerca <- abs(t[i] - t[j]) <= temporal_scale
            c(sum(cerca), vapply(en_zona, function(e) sum(cerca & e), numeric(1)))
        }
        obs <- cuenta(tt)
        message("Knox test: ", n_sim, " permutations of ", n_p, " cases...")
        sim <- vapply(seq_len(n_sim),
                      function(s) cuenta(tt[sample.int(n_p)]),
                      numeric(length(obs)))
        sim <- matrix(sim, nrow = length(obs))          # rows: global, foci

        p   <- (1 + rowSums(sim >= obs)) / (n_sim + 1)
        esp <- rowMeans(sim)
        rr  <- ifelse(esp > 0, obs / esp, NA_real_)

        global <- list(n_sim = n_sim,
                       periodo = c(per_ini, ref_date),
                       n_casos = n_p,
                       pares_espacio = length(i),         # pairs within spatial_scale
                       knox = obs[1],                     # pairs within space AND time
                       esperados = esp[1],
                       rr = rr[1],
                       p_value = p[1],
                       sim = sim[1, ])
        focos <- NULL
        if(!is.null(zonas)){
            focos <- data.frame(foco = zonas$foco,
                                knox_pares = obs[-1],
                                knox_esperados = round(esp[-1], 2),
                                knox_rr = round(rr[-1], 2),
                                p_value = p[-1],
                                p_holm = stats::p.adjust(p[-1], method = "holm"))
            focos$significativo <- focos$p_holm < alpha
        }
        list(global = global, focos = focos)
    }

    # Step 5. transmission and control zones ####
    # the zones are built with the recent cases (last temporal_scale days) of
    # each active focus: the area where transmission is happening now
    zf <- z[!is.na(z$foco) & z$reciente, ]

    if(nrow(zf) == 0){
        warning("No active foci between ", win_ini, " and ", ref_date, " (", min_cases,
                " or more cases within ", spatial_scale, " m and ", temporal_scale,
                " days).")
        transmission <- foci <- blocks_sel <- NULL
        # the global Knox test does not need foci
        knox <- if(n_sim > 0) prueba_knox(zonas = NULL)$global else NULL
    } else {
        transmission <- zf |>
            sf::st_buffer(dist = spatial_scale) |>
            dplyr::group_by(foco) |>
            dplyr::summarise(.groups = "drop")      # union by focus

        control <- transmission |>
            sf::st_buffer(dist = buffer_m)

        # blocks of the state
        bl <- if(cve_edo %in% sprintf("%02d", 1:10))  rgeomex::blocks_ine20_mx_a else
              if(cve_edo %in% sprintf("%02d", 11:14)) rgeomex::blocks_ine20_mx_b else
              if(cve_edo %in% sprintf("%02d", 15:19)) rgeomex::blocks_ine20_mx_c else
              if(cve_edo %in% sprintf("%02d", 20:25)) rgeomex::blocks_ine20_mx_d else
                                                      rgeomex::blocks_ine20_mx_e
        bl <- bl |>
            dplyr::filter(entidad %in% as.numeric(cve_edo))
        # first keep only the blocks near the control zones (faster)
        ctrl_bl <- sf::st_transform(control, sf::st_crs(bl))
        bl <- bl[sf::st_as_sfc(sf::st_bbox(ctrl_bl)), ] |>
            sf::st_make_valid() |>
            sf::st_transform(crs_m) |>
            dplyr::mutate(id_manzana = dplyr::row_number())

        # blocks that intersect the control zone, with their focus
        # (a block touching two foci is kept once, with the first focus)
        blocks_sel <- sf::st_join(bl, control |> dplyr::select(foco),
                                  join = sf::st_intersects, left = FALSE) |>
            dplyr::distinct(id_manzana, .keep_all = TRUE) |>
            dplyr::select(-id_manzana)

        # summary by focus
        # (with ST-DBSCAN a focus can include cases older than the window)
        resumen <- z[!is.na(z$foco), ] |>
            sf::st_drop_geometry() |>
            dplyr::summarise(casos = dplyr::n(),
                             casos_recientes = sum(reciente),
                             confirmados = sum(ESTATUS_CASO == 2),
                             primer_inicio = min(onset),
                             ultimo_inicio = max(onset),
                             .by = foco) |>
            dplyr::mutate(duracion_dias = as.numeric(ultimo_inicio - primer_inicio) + 1,
                          # new/persistent only makes sense with ST-DBSCAN
                          tipo = if(method == "st-dbscan"){
                              ifelse(primer_inicio >= win_ini, "Nuevo", "Persistente")
                          } else NA_character_)
        n_manz <- blocks_sel |>
            sf::st_drop_geometry() |>
            dplyr::count(foco, name = "manzanas")

        # Step 5.1. Knox test, global and by focus ####
        if(n_sim > 0){
            kx <- prueba_knox(zonas = transmission)
            knox <- kx$global
            resumen <- resumen |>
                dplyr::left_join(kx$focos, by = "foco")
        } else {
            resumen <- resumen |>
                dplyr::mutate(knox_pares = NA_real_, knox_esperados = NA_real_,
                              knox_rr = NA_real_, p_value = NA_real_,
                              p_holm = NA_real_, significativo = NA)
            knox <- NULL
        }

        foci <- control |>
            dplyr::mutate(area_control_km2 = round(as.numeric(sf::st_area(control)) / 1e6, 2),
                          area_transmision_km2 = round(as.numeric(sf::st_area(transmission)) / 1e6, 2)) |>
            dplyr::left_join(resumen, by = "foco") |>
            dplyr::left_join(n_manz, by = "foco") |>
            dplyr::mutate(manzanas = tidyr::replace_na(manzanas, 0L)) |>
            dplyr::relocate(foco, tipo, casos, casos_recientes, confirmados,
                            knox_pares, knox_esperados, knox_rr,
                            p_value, p_holm, significativo,
                            primer_inicio, ultimo_inicio, duracion_dias,
                            area_transmision_km2, area_control_km2, manzanas)

        # back to EPSG:4326
        transmission <- sf::st_transform(transmission, 4326)
        foci         <- sf::st_transform(foci, 4326)
        blocks_sel   <- sf::st_transform(blocks_sel, 4326)

        if(save){
            f <- paste0(dir, gsub(" ", "_", tolower(locality)),
                        "_active_foci_blocks_", ref_date, ".gpkg")
            sf::st_write(blocks_sel, f, delete_dsn = TRUE, quiet = TRUE)
            message("Blocks saved in ", f)
        }
    }

    # Step 5.2. diagnosis of the epidemiological scenario ####
    # Transmission becomes generalised (dengue outbreak experts) with:
    # 1. a sustained and significant increase of the outbreak: the cases of
    #    the window are significantly above the mean per window of the period
    #    (Poisson test), the previous window is also above that mean and Rt of
    #    the current window > 1 (EpiEstim);
    # 2. cases that exceed the operational capacity: cases per week of the
    #    window > capacity_cases (only if capacity_cases is given);
    # 3. cases dispersed over the locality: the area within spatial_scale
    #    metres of the recent cases covers coverage_min or more of the
    #    locality, or foci_min or more of the recent cases are in foci.
    # Generalised = 1 and (2 or 3). Rules of thumb, calibrated with 71
    # localities (2026); change them here.
    min_casos_diag    <- 50     # minimum cases of the period for a diagnosis
    # coverage of the locality by the recent cases (grows with the window):
    # 0.40 with 21 days, 0.50 with 28 days, linear in between
    umbral_dispersion <- if(!is.null(coverage_min)) coverage_min else
        min(max(0.40 + (temporal_scale - 21) * 0.10 / 7, 0.40), 0.50)
    umbral_focos      <- 0.5    # recent cases in foci: focalised vs dispersed

    loc_m    <- sf::st_union(sf::st_make_valid(sf::st_transform(loc, crs_m)))
    area_loc <- as.numeric(sf::st_area(loc_m))

    # temporal concentration and cases by month (descriptive)
    ts  <- sort(as.numeric(z_per$onset))
    n_t <- length(ts)
    pct_pares_tiempo <- if(n_t > 1){
        sum(findInterval(ts + temporal_scale, ts) - seq_len(n_t)) / choose(n_t, 2)
    } else NA_real_
    casos_mes <- as.data.frame(table(mes = format(z_per$onset, "%Y-%m")),
                               responseName = "casos")
    casos_mes$pct <- round(100 * casos_mes$casos / sum(casos_mes$casos), 1)

    # criterion 1. sustained and significant increase
    # complete windows of temporal_scale days, backwards from ref_date
    # (cv[1] = current window, cv[2] = previous window)
    n_vent <- floor(perm_days / temporal_scale)
    cv <- tabulate(floor(as.numeric(ref_date - z_per$onset) / temporal_scale) + 1,
                   nbins = n_vent)
    base <- if(n_vent >= 4) mean(cv[-(1:2)]) else NA_real_
    p_inc <- if(is.na(base)) NA_real_ else
             if(base == 0) as.numeric(cv[1] == 0) else
             stats::poisson.test(cv[1], r = base, alternative = "greater")$p.value
    razon <- if(is.na(base) || base == 0) NA_real_ else cv[1] / base

    # Rt of the locality with rt_dengue(): all the cases reported in the
    # locality (place), geocoded or not; INEGI keys of the locality polygon
    rt_loc <- tryCatch(
        suppressWarnings(suppressMessages(
            rt_dengue(dataset = geocoded_dataset,
                      level = "locality",
                      place = place,
                      cve_edo = cve_edo,
                      cve_mpo = loc$CVE_MUN,
                      cve_loc = loc$CVE_LOC,
                      status_caso = status_caso,
                      ref_date = ref_date,
                      period_days = perm_days,
                      window_days = temporal_scale,
                      mean_si = mean_si,
                      std_si = std_si,
                      plot = FALSE))),
        error = function(e){
            message("Rt not estimated: ", conditionMessage(e))
            NULL
        })
    rt <- if(is.null(rt_loc)) NULL else rt_loc$rt
    rv <- if(is.null(rt_loc)) NULL else rt_loc$rt_ventana
    rt_med <- if(is.null(rv)) NA_real_ else rv$rt
    rt_inf <- if(is.null(rv)) NA_real_ else rv$rt_inf
    rt_sup <- if(is.null(rv)) NA_real_ else rv$rt_sup
    fase   <- if(is.null(rv)) "Sin estimar" else rv$fase

    incremento <- !is.na(p_inc) && p_inc < alpha && cv[2] > base &&
        (is.na(rt_med) || rt_med > 1)

    # criterion 2. operational capacity
    # cases per week to work (house of the case and houses around it)
    n_manzanas <- if(is.null(blocks_sel)) 0L else nrow(blocks_sel)
    casos_semana <- cv[1] / (temporal_scale / 7)
    rebasa <- if(is.null(capacity_cases)) NA else casos_semana > capacity_cases

    # criterion 3. dispersion of the recent cases over the locality
    zr <- z[z$reciente, ]
    cobertura <- if(nrow(zr) == 0) 0 else
        sum(as.numeric(sf::st_area(sf::st_intersection(
            sf::st_union(sf::st_buffer(zr, dist = spatial_scale)), loc_m)))) / area_loc
    pct_casos_focos <- if(sum(z$reciente) == 0) 0 else nrow(zf) / sum(z$reciente)
    disperso <- cobertura >= umbral_dispersion || pct_casos_focos >= foci_min

    # foci: recent cases in foci and area of the transmission zones (clipped
    # to the locality)
    n_focos <- if(is.null(foci)) 0L else nrow(foci)
    pct_area <- if(is.null(transmission)) 0 else
        sum(as.numeric(sf::st_area(sf::st_intersection(
            sf::st_union(sf::st_transform(transmission, crs_m)), loc_m)))) / area_loc

    # space-time interaction (Knox, period of perm_days)
    interaccion <- if(is.null(knox)) "Sin prueba" else
                   if(is.na(knox$rr)) "Sin datos" else
                   if(knox$p_value < alpha) "S\u00ed" else
                   if(knox$rr > 1.05) "No concluyente" else "No"

    # scenario and operational strategy (Resumen de Estrategias Operativas)
    generalizada <- incremento && (disperso || isTRUE(rebasa))
    escenario <- if(nrow(z_per) < min_casos_diag) "Casos insuficientes" else
                 if(generalizada) "Transmisi\u00f3n generalizada" else
                 if(n_focos == 0) "Sin focos activos" else
                 if(pct_casos_focos >= umbral_focos) "Transmisi\u00f3n focalizada" else
                 "Transmisi\u00f3n focalizada con casos dispersos"
    txt_barrido <- paste0("Barrido: nebulizaci\u00f3n con m\u00e1quina pesada al 100% de la ",
                          "localidad (4 aplicaciones, una por semana, cada una en ",
                          "menos de 7 d\u00edas), control larvario al 100% en m\u00e1ximo 6 ",
                          "semanas (cobertura > 85%, ICP < 3%) y termonebulizaci\u00f3n ",
                          "en manzanas calientes (casos de las \u00faltimas 2 semanas).")
    txt_mega <- paste0("Operativo integral (megaoperativo): nebulizaci\u00f3n con m\u00e1quina ",
                       "pesada al 100% de la localidad (4 aplicaciones semanales), ",
                       "control larvario en el 70% de la localidad (\u00e1reas de mayor ",
                       "transmisi\u00f3n) en m\u00e1ximo 10 d\u00edas, termonebulizaci\u00f3n en manzanas ",
                       "calientes y comando estatal con evaluaci\u00f3n semanal.")
    txt_focal <- paste0("Estrategia focalizada en las zonas de control: nebulizaci\u00f3n ",
                        "con m\u00e1quina pesada (3 a 4 aplicaciones, una por semana), ",
                        "control larvario (cobertura > 85%, ICP < 3%) y ",
                        "termonebulizaci\u00f3n (casos de las \u00faltimas 2 semanas); ",
                        "priorizar los focos significativos (rojos).")
    accion <- switch(escenario,
        "Casos insuficientes" = paste0("Vigilancia: menos de ", min_casos_diag,
                                       " casos en el periodo para el diagn\u00f3stico."),
        "Transmisi\u00f3n generalizada" = if(is.na(rebasa)){
            paste0(txt_barrido, " Si los casos por semana rebasan la capacidad ",
                   "operativa: operativo integral (indicar capacity_cases).")
        } else if(rebasa) txt_mega else txt_barrido,
        "Sin focos activos" = paste0("Vigilancia entomol\u00f3gica (ovitrampas, ",
                                     "entomovirol\u00f3gica) y control alrededor de los casos."),
        "Transmisi\u00f3n focalizada" = txt_focal,
        "Transmisi\u00f3n focalizada con casos dispersos" =
            paste0(txt_focal, " Adem\u00e1s, control alrededor de los casos aislados."))
    alerta <- incremento && fase == "En ascenso" && !generalizada &&
        escenario != "Casos insuficientes"
    if(alerta){
        accion <- paste0("Alerta temprana (incremento significativo, Rt en ascenso). ",
                         accion)
    }
    if(!generalizada && isTRUE(rebasa)){
        accion <- paste0(accion, " Los casos por semana (", round(casos_semana, 1),
                         ") rebasan la capacidad operativa (", capacity_cases, ").")
    }

    # detail of the criteria (printed)
    detalle <- c(
        paste0("Incremento: ", cv[1], " casos en la ventana actual y ", cv[2],
               " en la previa; media por ventana de ", temporal_scale,
               " d\u00edas en el periodo: ", round(base, 1),
               if(!is.na(razon)) paste0(" (raz\u00f3n ", round(razon, 2), ", p = ",
                                        signif(p_inc, 2), ")"),
               "; Rt de la ventana actual: ",
               if(is.na(rt_med)) "sin estimar" else
                   paste0(round(rt_med, 2), " (IC95% ", round(rt_inf, 2), " a ",
                          round(rt_sup, 2), "), ", tolower(fase)),
               ". ", if(incremento) "Sostenido y significativo." else "No."),
        paste0("Capacidad operativa: ", round(casos_semana, 1),
               " casos por semana a trabajar",
               if(is.na(rebasa)) " (capacidad no indicada)." else
               paste0(" contra ", capacity_cases, " de capacidad. ",
                      if(rebasa) "La rebasa." else "No la rebasa.")),
        paste0("Dispersi\u00f3n: los casos recientes (a ", spatial_scale, " m) cubren ",
               round(100 * cobertura), "% de la localidad (umbral ",
               round(100 * umbral_dispersion), "%); casos recientes en focos: ",
               round(100 * pct_casos_focos), "% (umbral ", round(100 * foci_min), "%). ",
               if(disperso) "Dispersos en la localidad." else "No dispersos en toda la localidad."),
        paste0("Interacci\u00f3n espacio-temporal (Knox): ", interaccion,
               if(!is.null(knox) && !is.na(knox$rr))
                   paste0(" (RR = ", round(knox$rr, 2), ", p = ",
                          signif(knox$p_value, 2), ")"), "."))

    tabla_diag <- data.frame(
        localidad          = locality,
        ref_date           = ref_date,
        casos_periodo      = nrow(z_per),
        casos_ventana      = cv[1],
        razon_incremento   = round(razon, 2),
        p_incremento       = signif(p_inc, 2),
        rt                 = round(rt_med, 2),
        rt_inf             = round(rt_inf, 2),
        rt_sup             = round(rt_sup, 2),
        fase               = fase,
        incremento         = incremento,
        alerta_temprana    = alerta,
        cobertura_casos    = round(100 * cobertura, 1),
        umbral_cobertura   = round(100 * umbral_dispersion),
        dispersos          = disperso,
        pct_casos_en_focos = round(100 * pct_casos_focos),
        pct_area_focos     = round(100 * pct_area, 1),
        focos              = n_focos,
        focos_signif       = if(n_focos == 0) 0L else sum(foci$significativo, na.rm = TRUE),
        manzanas           = n_manzanas,
        casos_semana       = round(casos_semana, 1),
        capacidad          = if(is.null(capacity_cases)) NA_real_ else capacity_cases,
        rebasa_capacidad   = rebasa,
        interaccion        = interaccion,
        knox_rr            = if(is.null(knox)) NA_real_ else round(knox$rr, 2),
        knox_p             = if(is.null(knox)) NA_real_ else signif(knox$p_value, 2),
        escenario          = escenario,
        accion             = accion)

    diagnostico <- list(tabla = tabla_diag,
                        escenario = escenario,
                        accion = accion,
                        detalle = detalle,
                        rt = rt,
                        rt_plot = if(is.null(rt_loc)) NULL else rt_loc$plot,
                        casos_ventana = cv,
                        casos_mes = casos_mes,
                        pct_pares_tiempo = pct_pares_tiempo,
                        cobertura_casos = cobertura,
                        pct_casos_en_focos = pct_casos_focos,
                        pct_area_transmision = pct_area)

    cases <- sf::st_transform(z, 4326) |>
        dplyr::mutate(grupo = dplyr::case_when(!is.na(foco) & reciente  ~ "Caso en foco",
                                               !is.na(foco) & !reciente ~ "Caso previo del foco",
                                               TRUE                     ~ "Caso aislado"),
                      popup = paste0("<b>", grupo, "</b>",
                                     ifelse(is.na(foco), "", paste0(" (foco ", foco, ")")),
                                     "<br>Inicio de s\u00edntomas: ", onset,
                                     "<br>", ifelse(ESTATUS_CASO == 2,
                                                    "Confirmado", "Probable")))

    # Step 6. map ####
    loc   <- a_utf8(loc)
    cases <- a_utf8(cases)
    # basemaps: raster tiles, all hidden at the start (the map opens with the
    # Positron vector style); they are turned on in the layers control
    basemaps <- list(
        "Esri Dark Gray"  = list(tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}",
                                 attribution = "Tiles &copy; Esri"),
        "Esri Light Gray" = list(tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
                                 attribution = "Tiles &copy; Esri"),
        "OpenStreetMap"   = list(tiles = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
                                 attribution = "&copy; OpenStreetMap contributors"),
        "World Imagery"   = list(tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                                 attribution = "Tiles &copy; Esri"),
        "OpenTopoMap"     = list(tiles = "https://a.tile.opentopomap.org/{z}/{x}/{y}.png",
                                 attribution = "&copy; OpenStreetMap contributors, SRTM | &copy; OpenTopoMap (CC-BY-SA)"))

    m <- mapgl::maplibre(style = "https://tiles.openfreemap.org/styles/positron",
                         bounds = loc,
                         projection = "mercator")

    for(nm in names(basemaps)){
        m <- m |>
            mapgl::add_raster_source(id = paste0("src_", nm),
                                     tiles = basemaps[[nm]]$tiles,
                                     tileSize = 256,
                                     attribution = basemaps[[nm]]$attribution) |>
            mapgl::add_raster_layer(id = nm,
                                    source = paste0("src_", nm),
                                    visibility = "none")
    }

    m <- m |>
        mapgl::add_source(id = "area", data = loc) |>
        mapgl::add_line_layer(id = "ciudad-borde", source = "area",
                              line_color = "#444444", line_width = 1.2)

    # colours of the transmission zone by the Knox test (names = legend labels)
    col_knox <- c("#D7191C", "#FFC300", "#969696")
    names(col_knox) <- c(paste0("Transmisi\u00f3n significativa (Knox, p Holm < ", alpha, ")"),
                         "Exceso no significativo (Knox RR > 1)",
                         "Sin exceso (Knox RR \u2264 1)")

    if(!is.null(foci)){
        foci_map <- foci |>
            dplyr::mutate(popup = paste0("<b>Foco ", foco, "</b>",
                                         ifelse(is.na(tipo), "",
                                                paste0(" (", tolower(tipo), ")")),
                                         "<br>Casos: ", casos,
                                         " (recientes: ", casos_recientes,
                                         ", confirmados: ", confirmados, ")",
                                         ifelse(is.na(p_value), "",
                                                paste0("<br>Knox: ", knox_pares,
                                                       " pares (esperados ", knox_esperados,
                                                       ", RR ", knox_rr, ")",
                                                       "<br>p = ", signif(p_value, 2),
                                                       ", p Holm = ", signif(p_holm, 2),
                                                       ifelse(significativo,
                                                              " (significativo)",
                                                              " (no significativo)"))),
                                         "<br>Duraci\u00f3n: ", duracion_dias, " d\u00edas",
                                         "<br>Inicio: ", primer_inicio, " a ", ultimo_inicio,
                                         "<br>Manzanas a controlar: ", manzanas,
                                         "<br>\u00c1rea de control: ", area_control_km2, " km\u00b2"))
        # colour of the transmission zone by the Knox test of the focus
        trans_map <- transmission |>
            dplyr::left_join(sf::st_drop_geometry(foci) |>
                                 dplyr::select(foco, significativo, knox_rr, knox_pares),
                             by = "foco") |>
            dplyr::mutate(knox_clase = dplyr::case_when(
                is.na(significativo)                           ~ "Sin prueba",
                significativo                                  ~ "Significativo",
                knox_rr > 1 | (is.na(knox_rr) & knox_pares > 0) ~ "RR > 1",
                TRUE                                           ~ "RR <= 1")) |>
            dplyr::select(foco, knox_clase)
        m <- m |>
            mapgl::add_source(id = "control", data = a_utf8(foci_map)) |>
            mapgl::add_source(id = "transmision", data = trans_map) |>
            mapgl::add_source(id = "manzanas", data = blocks_sel |>
                                  dplyr::select(foco)) |>
            mapgl::add_fill_layer(id = "control-fill", source = "control",
                                  fill_color = "#FDAE61", fill_opacity = 0.25,
                                  popup = "popup") |>
            mapgl::add_line_layer(id = "control-borde", source = "control",
                                  line_color = "#F46D43", line_width = 1.5,
                                  line_dasharray = c(2, 2)) |>
            mapgl::add_fill_layer(id = "transmision-fill", source = "transmision",
                                  fill_color = mapgl::match_expr(column = "knox_clase",
                                                                 values = c("Significativo",
                                                                            "RR > 1",
                                                                            "RR <= 1"),
                                                                 stops = unname(col_knox),
                                                                 default = "#D7191C"),
                                  fill_opacity = 0.4) |>
            mapgl::add_line_layer(id = "manzanas-borde", source = "manzanas",
                                  line_color = "#7F2704", line_width = 0.6)
    }

    m <- m |>
        mapgl::add_source(id = "casos", data = cases |> dplyr::select(grupo, popup)) |>
        mapgl::add_circle_layer(id = "casos-puntos", source = "casos",
                                circle_radius = mapgl::interpolate(property = "zoom",
                                                                   values = c(10, 14, 18),
                                                                   stops = c(2.5, 4, 8)),
                                circle_color = mapgl::match_expr(column = "grupo",
                                                                 values = c("Caso en foco",
                                                                            "Caso previo del foco",
                                                                            "Caso aislado"),
                                                                 stops = c("#B2182B", "#F4A582",
                                                                           "#4D4D4D"),
                                                                 default = "gray"),
                                circle_stroke_color = "white",
                                circle_stroke_width = 0.8,
                                popup = "popup") |>
        mapgl::add_categorical_legend(
            legend_title = paste0("Focos activos al ", format(ref_date, "%d/%m/%Y")),
            values = c("Caso en foco",
                       if(method == "st-dbscan") "Caso previo del foco",
                       "Caso aislado",
                       if(is.null(knox) || is.null(foci)) paste0("Zona de transmisi\u00f3n (", spatial_scale, " m)")
                       else names(col_knox),
                       paste0("Zona de control (+", buffer_m, " m)")),
            colors = c("#B2182B",
                       if(method == "st-dbscan") "#F4A582",
                       "#4D4D4D",
                       if(is.null(knox) || is.null(foci)) "#D7191C" else unname(col_knox),
                       "#FDAE61"),
            patch_shape = "circle",
            position = "top-left",
            draggable = TRUE) |>
        mapgl::add_fullscreen_control(position = "top-left") |>
        mapgl::add_navigation_control() |>
        mapgl::add_globe_control() |>
        mapgl::add_scale_control(position = "bottom-left") |>
        # basemaps and layers in one collapsible window
        mapgl::add_layers_control(
            position = "top-right",
            collapsible = TRUE,
            layers = c(
                # basemaps (each label = its layer id)
                as.list(stats::setNames(names(basemaps), names(basemaps))),
                # layers of the function, with readable labels
                list("L\u00edmite de la localidad" = "ciudad-borde"),
                if(!is.null(foci)) list(
                    "Zona de control"      = c("control-fill", "control-borde"),
                    "Zona de transmisi\u00f3n"  = "transmision-fill",
                    "Manzanas a controlar" = "manzanas-borde"),
                list("Casos" = "casos-puntos")))

    message(if(is.null(foci)) 0 else nrow(foci), " active foci, ",
            nrow(zf), " of ", nrow(z), " cases in foci, ",
            if(is.null(blocks_sel)) 0 else nrow(blocks_sel), " blocks to control ",
            "(", win_ini, " to ", ref_date, ", method = ", method, ").",
            if(!is.null(knox)) paste0(
                " Knox test (", knox$n_sim, " permutations, ", knox$n_casos,
                " cases from ", per_ini, "): global ", knox$knox, " pairs, expected ",
                round(knox$esperados, 1), ", RR = ", round(knox$rr, 2),
                ", p = ", signif(knox$p_value, 2), ".",
                if(!is.null(foci)) paste0(" ", sum(foci$significativo), " of ",
                                          nrow(foci), " foci with Holm p < ",
                                          alpha, ".")))
    message("Diagn\u00f3stico: ", diagnostico$escenario, "\n",
            paste0("  ", seq_along(diagnostico$detalle), ". ",
                   diagnostico$detalle, collapse = "\n"),
            "\nAcci\u00f3n: ", diagnostico$accion)

    if(print_map) print(m)
    invisible(list(map = m,
                   foci = foci,
                   transmission = transmission,
                   blocks = blocks_sel,
                   cases = cases,
                   knox = knox,
                   diagnostico = diagnostico))
}
