# Apply program review rules after the numerical score and base priority level
# have been calculated. Scores and score-based ranks are deliberately retained.
apply_community_priority_review <- function(communities) {
  required <- c("state", "county_join", "child_population", "priority_group",
                "latitude", "longitude", "census_total", "area_km2",
                "population_density_km2")
  missing <- setdiff(required, names(communities))
  if (length(missing)) stop("Missing priority review fields: ", paste(missing, collapse = ", "))
  if (anyNA(communities$child_population) || any(communities$child_population < 0)) {
    stop("Community child populations must be non-missing and non-negative.")
  }
  county_key <- toupper(trimws(as.character(communities$county_join)))
  words <- gsub("[^A-Z0-9]+", " ", county_key)
  unknown <- !is.na(county_key) & (
    grepl("(^| )UNKNOWN( |$)", words) |
      county_key %in% c("MISSING", "NA", "N/A", "")
  )
  state_key <- if ("state_join" %in% names(communities)) communities$state_join else communities$state
  state_key <- toupper(trimws(as.character(state_key)))
  if (anyNA(state_key) || any(state_key == "")) stop("Priority review requires a state for each community.")
  unknown_totals <- tapply(ifelse(unknown, communities$child_population, 0), state_key, sum)
  unknown_children <- as.numeric(unknown_totals[state_key])
  small <- communities$child_population < 11
  base <- if ("priority_group_before_review" %in% names(communities)) {
    as.character(communities$priority_group_before_review)
  } else {
    as.character(communities$priority_group)
  }
  level <- base
  level[!unknown & small & !is.na(base) & base == "High"] <- "Moderate"
  level[unknown & unknown_children < 15] <- "Low"
  level[unknown & unknown_children >= 15 & unknown_children <= 30] <- "Moderate"
  level[unknown & unknown_children > 30] <- "High"
  communities$priority_group_before_review <- factor(base, levels = c("Low", "Moderate", "High"), ordered = TRUE)
  communities$priority_group <- factor(level, levels = c("Low", "Moderate", "High"), ordered = TRUE)
  communities$county_unknown <- unknown
  communities$unknown_county_children_in_state <- ifelse(unknown, unknown_children, NA_real_)

  missing_coordinates <- is.na(communities$latitude) | is.na(communities$longitude)
  unresolved <- unknown | county_key %in% c("MORTLOCK", "NORTHWEST", "MISSING", "UNKNOWN")
  review_required <- unknown & unknown_children > 30
  # Each applicable flag is retained, in a stable order, separated by semicolons.
  communities$data_quality_flag <- vapply(seq_len(nrow(communities)), function(i) {
    flags <- c(
      if (missing_coordinates[[i]]) "Missing coordinates",
      if (unresolved[[i]]) "Geography requires resolution",
      if (is.na(communities$census_total[[i]])) "Missing Census match",
      if (is.na(communities$area_km2[[i]])) "Missing land area",
      if (is.na(communities$population_density_km2[[i]])) "Missing population density",
      if (small[[i]]) "Small denominator",
      if (review_required[[i]]) "Data quality review required"
    )
    if (length(flags)) paste(flags, collapse = "; ") else "Ready"
  }, character(1))
  communities$routing_ready <- !missing_coordinates & !unresolved
  communities
}
