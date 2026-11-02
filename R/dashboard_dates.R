# Dates come from analysis metadata, never file names, timestamps or today's date.
parse_dashboard_date <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(as.Date(NA_character_))
  tryCatch(as.Date(as.character(x)), error = function(e) as.Date(NA_character_))
}

extract_dashboard_analysis_date <- function(path, dataset) {
  absent <- as.Date(NA_character_)
  if (!file.exists(path) || !tolower(tools::file_ext(path)) %in% c("xlsx", "xls")) return(absent)
  sheets <- readxl::excel_sheets(path)
  if ("Analysis_Metadata" %in% sheets) {
    metadata <- readxl::read_excel(path, sheet = "Analysis_Metadata")
    field <- if (dataset == "adolescent") "coverage_as_of_date" else "analysis_as_of_date"
    if (field %in% names(metadata) && nrow(metadata) == 1L) return(parse_dashboard_date(metadata[[field]][[1]]))
  }
  # Existing adolescent extracts already record the coverage cutoff in the
  # hpv_count definition. Do not use age_years, whose age date may differ.
  if (dataset == "adolescent" && "Data_Dictionary" %in% sheets) {
    dictionary <- readxl::read_excel(path, sheet = "Data_Dictionary")
    if (ncol(dictionary) >= 2L) {
      rows <- which(tolower(as.character(dictionary[[1]])) == "hpv_count")
      if (length(rows) == 1L) {
        description <- as.character(dictionary[[2]][rows])
        match <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", description)
        if (match[[1]] > 0L) return(parse_dashboard_date(regmatches(description, match)))
      }
    }
  }
  absent
}

read_dashboard_dates <- function(path) {
  defaults <- data.frame(dataset = c("child", "adolescent"),
                         data_as_of_date = as.Date(c(NA_character_, NA_character_)),
                         source_patient_file = c("", ""), stringsAsFactors = FALSE)
  if (!file.exists(path)) return(defaults)
  metadata <- utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE)
  if (!all(names(defaults) %in% names(metadata))) return(defaults)
  for (i in seq_len(nrow(defaults))) {
    row <- which(metadata$dataset == defaults$dataset[[i]])
    if (length(row) == 1L) {
      defaults$data_as_of_date[i] <- parse_dashboard_date(metadata$data_as_of_date[row])
      defaults$source_patient_file[i] <- metadata$source_patient_file[row]
    }
  }
  defaults
}

write_dashboard_dates <- function(path, child_source, adolescent_source, child_analysis_date = NULL) {
  saved <- read_dashboard_dates(path)
  result <- saved
  sources <- c(child = child_source, adolescent = adolescent_source)
  for (i in seq_len(nrow(result))) {
    scope <- result$dataset[[i]]
    source <- sources[[scope]]
    date <- if (scope == "child" && !is.null(child_analysis_date)) {
      parse_dashboard_date(child_analysis_date)
    } else {
      extract_dashboard_analysis_date(source, scope)
    }
    # Preserve a previously recorded date only for the same source extract.
    if (is.na(date) && identical(basename(source), saved$source_patient_file[[i]])) {
      date <- saved$data_as_of_date[i]
    }
    result$data_as_of_date[i] <- date
    result$source_patient_file[i] <- basename(source)
  }
  utils::write.csv(result, path, row.names = FALSE, na = "")
  result
}

dashboard_date_label <- function(metadata, dataset) {
  date <- metadata$data_as_of_date[match(dataset, metadata$dataset)]
  if (length(date) != 1L || is.na(date)) return("Data as of date not recorded")
  paste0("Data as of ", format(date, "%B"), " ", as.integer(format(date, "%d")), ", ", format(date, "%Y"))
}
