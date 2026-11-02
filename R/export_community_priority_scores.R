# Export the dashboard's final community results using the analysis' existing
# openxlsx dependency. No patient-level records are written to this workbook.
export_community_priority_scores <- function(community_summary, output_dir,
                                             analysis_date = Sys.Date(),
                                             source_file = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Install the 'openxlsx' package before exporting community priority scores.")
  }
  required <- c(
    "state", "county", "utd_coverage", "mmr_coverage", "utd_no_mmr_coverage",
    "median_months_since_vax", "population_density_km2", "priority_score",
    "priority_group", "priority_rank", "child_population", "children_not_utd",
    "data_quality_flag"
  )
  missing <- setdiff(required, names(community_summary))
  if (length(missing)) stop("Missing community export columns: ", paste(missing, collapse = ", "))
  if (!nrow(community_summary)) stop("No communities are available for the priority export.")
  analysis_date <- as.Date(analysis_date)
  if (length(analysis_date) != 1L || is.na(analysis_date)) stop("analysis_date must be one valid date.")

  ordered <- community_summary[order(community_summary$priority_rank,
                                    community_summary$state, community_summary$county,
                                    na.last = TRUE), , drop = FALSE]
  export <- data.frame(
    State = ordered$state,
    County = ordered$county,
    `UTD (%)` = ordered$utd_coverage / 100,
    `MMR UTD (%)` = ordered$mmr_coverage / 100,
    `UTD excluding MMR (%)` = ordered$utd_no_mmr_coverage / 100,
    `Median time since vaccination (months)` = ordered$median_months_since_vax,
    `Population Density` = ordered$population_density_km2,
    `Priority Score` = ordered$priority_score,
    `Priority Level` = as.character(ordered$priority_group),
    `Priority Rank` = ordered$priority_rank,
    `Number of Children` = ordered$child_population,
    `Children Not UTD` = ordered$children_not_utd,
    `Data Quality Flag` = ordered$data_quality_flag,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0("Community Priority Score_",
                                              format(analysis_date, "%m%d%y"), ".xlsx"))
  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10, fontColour = "#243247")
  openxlsx::addWorksheet(wb, "Community Priorities", gridLines = FALSE, tabColour = "#203864", zoom = 85)
  openxlsx::addWorksheet(wb, "Definitions", gridLines = FALSE, zoom = 90)
  title_style <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#203864")
  header_style <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#203864",
                                        textDecoration = "bold", halign = "center",
                                        valign = "center", wrapText = TRUE)
  number_style <- openxlsx::createStyle(numFmt = "0.0", halign = "right", valign = "center")
  percent_style <- openxlsx::createStyle(numFmt = "0.0%", halign = "right", valign = "center")
  integer_style <- openxlsx::createStyle(numFmt = "#,##0", halign = "right", valign = "center")
  text_style <- openxlsx::createStyle(halign = "left", valign = "center")
  sheet <- "Community Priorities"
  openxlsx::writeData(wb, sheet, "Community Priority Score", startRow = 2, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, title_style, rows = 2, cols = 1)
  openxlsx::writeData(wb, sheet, "Export date", startRow = 3, colNames = FALSE)
  openxlsx::writeData(wb, sheet, analysis_date, startRow = 3, startCol = 2, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "mmm d, yyyy"), rows = 3, cols = 2)
  openxlsx::writeData(wb, sheet, "Highest priority first. Definitions and source details are on the Definitions worksheet.",
                      startRow = 4, colNames = FALSE)
  openxlsx::writeDataTable(wb, sheet, export, startRow = 6, tableName = "CommunityPriorityScores",
                           tableStyle = "TableStyleLight9", withFilter = TRUE, keepNA = FALSE)
  openxlsx::addStyle(wb, sheet, header_style, rows = 6, cols = seq_len(ncol(export)), gridExpand = TRUE, stack = TRUE)
  rows <- seq.int(7, nrow(export) + 6)
  openxlsx::addStyle(wb, sheet, text_style, rows = rows, cols = c(1, 2, 9, 13), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, percent_style, rows = rows, cols = 3:5, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, number_style, rows = rows, cols = c(6, 7, 8), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, integer_style, rows = rows, cols = 10:12, gridExpand = TRUE, stack = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = 1:13,
                        widths = c(14, 25, 14, 16, 22, 30, 20, 16, 16, 15, 20, 20, 42))
  openxlsx::setRowHeights(wb, sheet, rows = 6, heights = 43)
  # Compound data-quality flags must remain readable in the exported table.
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(wrapText = TRUE, valign = "center"),
                     rows = rows, cols = 13, gridExpand = TRUE, stack = TRUE)
  flag_lines <- vapply(export$`Data Quality Flag`, function(x) max(1L, length(strwrap(x, width = 40))), integer(1))
  openxlsx::setRowHeights(wb, sheet, rows = rows, heights = pmax(22, 14 * flag_lines + 6))
  openxlsx::freezePane(wb, sheet, firstActiveRow = 7, firstActiveCol = 3)
  for (level in c("High", "Moderate", "Low")) {
    style <- switch(level,
                    High = openxlsx::createStyle(fgFill = "#FCE3DF", fontColour = "#9C3428"),
                    Moderate = openxlsx::createStyle(fgFill = "#FFF0D5", fontColour = "#8A5A12"),
                    Low = openxlsx::createStyle(fgFill = "#E8F0EC", fontColour = "#37654C"))
    openxlsx::conditionalFormatting(wb, sheet, cols = 9, rows = rows, type = "expression",
                                    rule = paste0('$I7="', level, '"'), style = style)
  }
  openxlsx::conditionalFormatting(wb, sheet, cols = 13, rows = rows, type = "expression",
                                  rule = '$M7<>"Ready"',
                                  style = openxlsx::createStyle(fgFill = "#FFF0D5", fontColour = "#8A5A12"))

  definitions <- data.frame(
    Field = c(names(export), "Source", "Run behavior", "Missing values", "Individual score", "Community score", "Priority classification"),
    Definition = c(
      "FSM state of the planning community.",
      "Operational county / atoll grouping used by the dashboard. Unresolved geography records are retained.",
      "Percentage of analytic children meeting all age-appropriate vaccine requirements, including MMR when eligible.",
      "Percentage UTD for age-appropriate MMR doses, among MMR-eligible children. Children under 12 months are excluded from this denominator.",
      "Percentage meeting age-appropriate vaccine requirements when MMR is excluded.",
      "Median months since the last vaccination among children who are NOT UTD. Days are divided by 30.4375 and capped at 36 months, matching the dashboard.",
      "People per square kilometre: aggregated Census total population divided by aggregated land area. A missing density remains blank in this export.",
      "Community priority score on a 0-100 scale, rounded to one decimal. Higher values indicate higher priority.",
      "High, Moderate or Low after score/coverage rules and the unknown-county and small-denominator overrides described below.",
      "Descending rank of the numerical score, before program review overrides. Rank 1 is highest; ties share the minimum rank. Priority Level can differ from the numerical ranking.",
      "Number of analytic children in this planning community. This is the coverage denominator, not Census population.",
      "Number of analytic children not UTD for all age-appropriate vaccines.",
      "All applicable data-quality flags, separated by semicolons. Small denominator means 10 or fewer children. Unknown county totals above 30 require data-quality review. Flags do not change the numerical score.",
      if (is.null(source_file)) "Current dashboard community summary, calculated by R/prepare_data.R." else paste0("Patient input: ", basename(source_file), ". Community scores and Census matches calculated by R/prepare_data.R."),
      "A new dated workbook is written after each completed analysis or data-preparation run. Re-running on the same date replaces that date's workbook; other dates are retained.",
      "Unavailable values are blank, not zero. Scoring retains the existing neutral density of 50 for missing density and maximum time contribution for missing vaccination dates.",
      "20 points for non-MMR overdue, 55 for MMR overdue, plus up to 25 for time since vaccination over 36 months. MMR not yet eligible contributes zero.",
      "Patient risk = 60% median child score + 20% child-score 75th percentile + 20% percentage scoring >=70. Final score = 90% community patient risk + 5% high-density transmission potential + 5% high-risk child burden.",
      "Base level: High if above the upper score tertile, overall UTD <30%, or MMR UTD <50%; otherwise Moderate above the lower tertile, else Low. Overrides: Unknown county total per state <15 is Low, 15-30 is Moderate, >30 is High for data-quality review. A known county with 10 or fewer children cannot be High and is changed to Moderate. Numerical scores and ranks are retained."
    ), check.names = FALSE, stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "Definitions", "Community Priority Score definitions", startRow = 2, colNames = FALSE)
  openxlsx::addStyle(wb, "Definitions", title_style, rows = 2, cols = 1)
  openxlsx::writeData(wb, "Definitions", definitions, startRow = 4)
  openxlsx::addStyle(wb, "Definitions", header_style, rows = 4, cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Definitions", openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                     rows = 5:(nrow(definitions) + 4), cols = 1:2, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Definitions", cols = 1:2, widths = c(38, 116))
  openxlsx::setRowHeights(wb, "Definitions", rows = 5:(nrow(definitions) + 4), heights = 45)
  openxlsx::setRowHeights(wb, "Definitions", rows = 23:24, heights = 78)
  openxlsx::freezePane(wb, "Definitions", firstActiveRow = 5)
  saved <- openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE, returnValue = TRUE)
  if (!isTRUE(saved)) {
    stop("Could not update the community priority workbook. Close it in Excel and rerun: ", output_file)
  }
  message("Community priority workbook created: ", output_file)
  invisible(output_file)
}
