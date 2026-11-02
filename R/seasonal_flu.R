# Seasonal flu indicators use the Census workbook and the analysis' recorded
# cutoff. Only aggregated results are retained or passed to the dashboard.
FSM_FLU_GEOGRAPHIES <- c("FSM", "Chuuk", "Kosrae", "Pohnpei", "Yap")
FSM_FLU_AGES <- c("6 months - 4 years", "5 - 12 years", "13 - 17 years", "18 - 64 years", "65+ years")

fsm_flu_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  text <- trimws(as.character(x))
  result <- as.Date(rep(NA_character_, length(text)))
  for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")) {
    pending <- is.na(result) & !is.na(text) & nzchar(text)
    result[pending] <- suppressWarnings(as.Date(text[pending], format = fmt))
  }
  serial <- is.na(result) & !is.na(text) & grepl("^[0-9]+(\\.[0-9]+)?$", text)
  result[serial] <- as.Date(as.numeric(text[serial]), origin = "1899-12-30")
  result
}

read_fsm_flu_population <- function(path) {
  raw <- readxl::read_excel(path, sheet = "FSM", col_names = FALSE,
                            col_types = "text", .name_repair = "minimal")
  if (ncol(raw) < 2L) stop("The FSM Census sheet needs age labels and population values.")
  geography <- NULL
  rows <- list()
  totals <- numeric()
  for (i in seq_len(nrow(raw))) {
    label <- toupper(trimws(as.character(raw[[1]][i])))
    if (is.na(label) || !nzchar(label)) next
    geo <- match(label, toupper(FSM_FLU_GEOGRAPHIES))
    if (!is.na(geo)) { geography <- FSM_FLU_GEOGRAPHIES[[geo]]; next }
    if (is.null(geography)) next
    number <- suppressWarnings(as.numeric(gsub(",", "", raw[[2]][i])))
    if (label == "TOTAL") { totals[geography] <- number; next }
    age <- if (grepl("^6\\s*MONTH", label)) 1L else if (grepl("^5\\s*YEAR", label)) 2L else
      if (grepl("^13\\s*YEAR", label)) 3L else if (grepl("^18\\s*YEAR", label)) 4L else
      if (grepl("^65", label)) 5L else NA_integer_
    if (!is.na(age)) rows[[length(rows) + 1L]] <- data.frame(
      geography = geography, age_group = FSM_FLU_AGES[[age]], population = number)
  }
  result <- do.call(rbind, rows)
  if (is.null(result) || nrow(result) != 25L ||
      anyDuplicated(paste(result$geography, result$age_group)) ||
      any(!is.finite(result$population) | result$population <= 0)) {
    stop("Census_Flu_2026.xlsx must contain five positive age-group populations for FSM and each state.")
  }
  for (geo in FSM_FLU_GEOGRAPHIES) {
    if (!geo %in% result$geography) stop("Missing Census population for ", geo)
    if (geo %in% names(totals) && is.finite(totals[[geo]]) &&
        abs(sum(result$population[result$geography == geo]) - totals[[geo]]) > 1) {
      stop("Census age-group populations do not match the total for ", geo)
    }
  }
  result
}

latest_fsm_flu_file <- function(directory) {
  files <- list.files(directory, pattern = "^Dataset 4_Seasonal Flu_DeID_[0-9]{6}\\.xlsx$", full.names = TRUE)
  dates <- as.Date(sub(".*_([0-9]{6})\\.xlsx$", "\\1", files), "%m%d%y")
  valid <- !is.na(dates)
  if (!any(valid)) stop("No seasonal flu extract was found in Analytic Code Output.")
  files <- files[valid]
  dates <- dates[valid]
  files[order(dates, file.info(files)$mtime, decreasing = TRUE)[[1]]]
}

fsm_flu_settings <- function(source, path) {
  settings <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  required <- c("source_file", "season_start", "analysis_as_of_date")
  if (!all(required %in% names(settings))) stop("Seasonal flu settings are missing required dates.")
  row <- which(settings$source_file == basename(source))
  if (length(row) != 1L) stop("Record the analysis as-of date and season start for ", basename(source), " in seasonal_flu_settings.csv.")
  start <- as.Date(settings$season_start[row])
  cutoff <- as.Date(settings$analysis_as_of_date[row])
  # A future extract can retain its own cutoff in an Analysis_Metadata sheet.
  sheets <- readxl::excel_sheets(source)
  if ("Analysis_Metadata" %in% sheets) {
    metadata <- readxl::read_excel(source, sheet = "Analysis_Metadata")
    for (field in c("coverage_as_of_date", "analysis_as_of_date")) {
      if (field %in% names(metadata) && nrow(metadata) == 1L) {
        cutoff <- as.Date(as.character(metadata[[field]][[1]]))
        break
      }
    }
  }
  if (is.na(start) || is.na(cutoff) || start > cutoff || format(start, "%d") != "01") {
    stop("Seasonal flu dates must contain a month-start season date and a valid analysis cutoff.")
  }
  list(season_start = start, as_of_date = cutoff)
}

aggregate_fsm_flu <- function(data, population, season_start, as_of_date) {
  date_cols <- grep("^flu_date[0-9]+$", names(data), value = TRUE)
  if (!all(c("state", "agegroup") %in% names(data)) || !length(date_cols)) {
    stop("Seasonal flu data need state, agegroup and at least one flu_date column.")
  }
  month_start <- as.Date(format(as_of_date, "%Y-%m-01"))
  month_end <- seq(month_start, by = "month", length.out = 2L)[[2]] - 1
  through <- if (as_of_date == month_end) as_of_date else month_start - 1
  if (through < season_start) stop("There is no completed reporting month in the selected season.")
  season_end <- seq(season_start, by = "year", length.out = 2L)[[2]] - 1
  through <- min(through, season_end)
  months <- seq(season_start, as.Date(format(through, "%Y-%m-01")), by = "month")
  age_id <- suppressWarnings(as.integer(data$agegroup))
  ages <- FSM_FLU_AGES[match(age_id, 1:5)]
  states <- FSM_FLU_GEOGRAPHIES[match(toupper(trimws(as.character(data$state))), toupper(FSM_FLU_GEOGRAPHIES))]
  states[states == "FSM" & !is.na(states)] <- NA_character_
  events <- do.call(rbind, lapply(date_cols, function(column) data.frame(
    person = seq_len(nrow(data)), geography = states, age_group = ages,
    date = fsm_flu_date(data[[column]]), stringsAsFactors = FALSE)))
  events <- events[!is.na(events$age_group) & !is.na(events$date) &
                     events$date >= season_start & events$date <= through, , drop = FALSE]
  events <- unique(events)
  events <- events[order(events$date), , drop = FALSE]
  first <- events[!duplicated(events$person), , drop = FALSE]
  curves <- expand.grid(month = months, age_group = FSM_FLU_AGES,
                         geography = FSM_FLU_GEOGRAPHIES, stringsAsFactors = FALSE)
  key <- paste(population$geography, population$age_group)
  curves$population <- population$population[match(paste(curves$geography, curves$age_group), key)]
  if (anyNA(curves$population) || any(curves$population <= 0)) stop("A geography/age population denominator is missing.")
  curves$people <- vapply(seq_len(nrow(curves)), function(i) {
    end <- seq(curves$month[i], by = "month", length.out = 2L)[[2]] - 1
    in_geo <- if (curves$geography[[i]] == "FSM") rep(TRUE, nrow(first)) else
      !is.na(first$geography) & first$geography == curves$geography[[i]]
    as.integer(sum(in_geo & first$age_group == curves$age_group[[i]] & first$date <= end))
  }, integer(1))
  curves$coverage <- 100 * curves$people / curves$population
  doses <- expand.grid(age_group = FSM_FLU_AGES, geography = FSM_FLU_GEOGRAPHIES, stringsAsFactors = FALSE)
  doses$doses <- vapply(seq_len(nrow(doses)), function(i) {
    in_geo <- if (doses$geography[[i]] == "FSM") rep(TRUE, nrow(events)) else
      !is.na(events$geography) & events$geography == doses$geography[[i]]
    as.integer(sum(in_geo & events$age_group == doses$age_group[[i]]))
  }, integer(1))
  list(curves = curves, doses = doses, population = population,
       season_start = season_start, as_of_date = as_of_date, through = through,
       records = nrow(data), vaccinated_without_state = sum(is.na(first$geography)))
}

load_fsm_seasonal_flu <- function(directory, population_file, settings_file) {
  tryCatch({
    source <- latest_fsm_flu_file(directory)
    dates <- fsm_flu_settings(source, settings_file)
    population <- read_fsm_flu_population(population_file)
    headers <- names(readxl::read_excel(source, n_max = 0))
    keep <- c("state", "agegroup", grep("^flu_date[0-9]+$", headers, value = TRUE))
    data <- readxl::read_excel(source, col_types = ifelse(headers %in% keep, "text", "skip"))
    result <- aggregate_fsm_flu(data, population, dates$season_start, dates$as_of_date)
    result$source <- basename(source)
    result$population_source <- basename(population_file)
    result$error <- NULL
    result
  }, error = function(e) list(error = conditionMessage(e), as_of_date = as.Date(NA_character_)))
}

fsm_flu_cards <- function(result, geographies, ages) {
  # FSM already contains every state. Count national values once when selected.
  card_geographies <- if ("FSM" %in% geographies) "FSM" else geographies
  p <- result$population
  d <- result$doses
  list(population = sum(p$population[p$geography %in% card_geographies & p$age_group %in% ages]),
       doses = sum(d$doses[d$geography %in% card_geographies & d$age_group %in% ages]))
}

fsm_flu_selected_curves <- function(result, geography, ages) {
  x <- result$curves[result$curves$geography == geography & result$curves$age_group %in% ages, , drop = FALSE]
  if (!nrow(x)) return(x)
  total <- stats::aggregate(cbind(people, population) ~ month + geography, data = x, FUN = sum)
  total$age_group <- "Total"
  total$coverage <- 100 * total$people / total$population
  rbind(x, total[names(x)])
}

fsm_flu_plot <- function(result, geography, ages, ymax) {
  x <- fsm_flu_selected_curves(result, geography, ages)
  colors <- setNames(c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#003E73"), c(FSM_FLU_AGES, "Total"))
  p <- plotly::plot_ly()
  for (age in c(ages, "Total")) {
    line <- x[x$age_group == age, , drop = FALSE]
    p <- plotly::add_trace(p, data = line, x = ~month, y = ~coverage,
      type = "scatter", mode = "lines+markers", name = age,
      line = list(color = colors[[age]], width = if (age == "Total") 4 else 2,
                  dash = if (age == "Total") "dash" else "solid"),
      marker = list(color = colors[[age]]),
      text = ~paste0(geography, "<br>", age_group, "<br>", format(month, "%B %Y"),
        "<br>", format(people, big.mark = ","), " people / ",
        format(round(population, 1), big.mark = ","), " population<br>", sprintf("%.2f%%", coverage)),
      hovertemplate = "%{text}<extra></extra>")
  }
  p <- plotly::layout(p,
    xaxis = list(title = "Month", tickvals = unique(x$month), tickformat = "%b %Y", fixedrange = TRUE),
    yaxis = list(title = "Cumulative vaccination coverage (%)", range = c(0, ymax), ticksuffix = "%", fixedrange = FALSE),
    dragmode = "zoom", legend = list(orientation = "h", y = -0.25),
    margin = list(b = 140, l = 75), hovermode = "closest")
  plotly::config(p, scrollZoom = TRUE, displaylogo = FALSE,
                 modeBarButtonsToRemove = c("select2d", "lasso2d"))
}
