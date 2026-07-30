# ==============================================================================
# Shore to Shot Dashboard
# Prepare patient-, community-, Census-, and vaccine-level dashboard datasets
#
# This version:
#   1. Applies the same operational atoll groupings to patient and Census data.
#   2. Aggregates Census population and land area across grouped communities.
#   3. Recalculates population density after aggregation.
#   4. Uses population-weighted Census coordinates for grouped atolls.
#   5. Retains QA outputs for duplicate and unmatched Census keys.
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(readxl)
library(tibble)

# ==============================================================================
# 1. File paths and analytic settings
# ==============================================================================

patient_file <- file.path(
  "data",
  "Dataset 1_FSM_2-83 mos_Full Dataset_06252026_DeID.csv"
)

census_file <- file.path(
  "data",
  "FSM Counties_Census_ChatGPT_11Jun2026.xlsx"
)

census_sheet <- "FSM Villages"

community_output_file <- file.path("data", "community_summary.csv")
vaccine_output_file <- file.path("data", "community_vaccine_needs.csv")
patient_output_file <- file.path("data", "patient_dashboard.csv")

unmatched_census_output_file <- file.path(
  "data",
  "qa_unmatched_census_communities.csv"
)

census_duplicates_output_file <- file.path(
  "data",
  "qa_census_duplicate_keys.csv"
)

census_atoll_members_output_file <- file.path(
  "data",
  "qa_census_atoll_members.csv"
)

# Threshold used to identify children with elevated individual risk.
high_individual_risk_threshold <- 70


# ==============================================================================
# 2. Verify required files
# ==============================================================================

required_files <- c(
  patient_file,
  census_file
)

missing_required_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_required_files) > 0) {
  stop(
    paste0(
      "The following required file(s) were not found relative to the current ",
      "working directory (", getwd(), "):\n",
      paste0("  - ", missing_required_files, collapse = "\n"),
      "\n\nOpen the ShoreToShot_R_Prototype RStudio project and rerun the script."
    )
  )
}


# ==============================================================================
# 3. Helper functions
# ==============================================================================

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  median(x, na.rm = TRUE)
}


safe_quantile <- function(x, probability = 0.75) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  as.numeric(
    quantile(
      x,
      probs = probability,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
  )
}


safe_weighted_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  if (!any(valid)) {
    return(safe_mean(x))
  }

  weighted.mean(
    x[valid],
    w = w[valid],
    na.rm = TRUE
  )
}


rescale_100 <- function(x) {
  output <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (!any(valid)) {
    return(output)
  }

  valid_range <- range(x[valid])

  if (diff(valid_range) == 0) {
    output[valid] <- 50
    return(output)
  }

  output[valid] <- 100 * (
    x[valid] - valid_range[1]
  ) / diff(valid_range)

  output
}


percentile_100 <- function(x) {
  output <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (!any(valid)) {
    return(output)
  }

  valid_values <- x[valid]

  if (
    length(valid_values) == 1 ||
    length(unique(valid_values)) == 1
  ) {
    output[valid] <- 50
    return(output)
  }

  output[valid] <- 100 * percent_rank(valid_values)

  output
}


normalize_join_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\u00A0", " ") %>%
    str_squish() %>%
    str_to_upper()
}


clean_display_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\u00A0", " ") %>%
    str_squish() %>%
    str_to_lower() %>%
    str_to_title()
}


clean_numeric <- function(x) {
  parse_number(
    str_replace_all(
      as.character(x),
      "\u00A0",
      ""
    )
  )
}


# ==============================================================================
# 4. Operational atoll crosswalk
# ==============================================================================
#
# source_county_join is the normalized name found in either the patient or
# Census data. operational_county is the combined dashboard geography.
#
# Alternate spellings in the Census workbook are included:
#   - MURILLO -> Murilo Atoll
#   - POLLAP  -> Pulap Atoll
#
# PIIS PANEU and PIIS-PANEU are both included because punctuation can vary.
# ==============================================================================

atoll_crosswalk <- tribble(
  ~source_county_join, ~operational_county,

  # Losap Atoll
  "LOSAP",             "Losap Atoll",
  "PIIS PANEU",        "Losap Atoll",
  "PIIS-PANEU",        "Losap Atoll",
  "NAMA",              "Losap Atoll",

  # Lukunor Atoll
  "ONEOP",             "Lukunor Atoll",
  "LEKINIOCH",         "Lukunor Atoll",

  # Murilo Atoll
  "MURILO",            "Murilo Atoll",
  "MURILLO",           "Murilo Atoll",
  "PIISEMWAR",         "Murilo Atoll",
  "RUO",               "Murilo Atoll",

  # Nomwin Atoll
  "FANANU",            "Nomwin Atoll",
  "NOMWIN",            "Nomwin Atoll",

  # Pulap Atoll
  "PULAP",             "Pulap Atoll",
  "POLLAP",            "Pulap Atoll",
  "TAMATAM",           "Pulap Atoll",

  # Satawan Atoll
  "KUTTU",             "Satawan Atoll",
  "MOCH",              "Satawan Atoll",
  "SATAWAN",           "Satawan Atoll",
  "TA",                "Satawan Atoll",

  # Namonuito Atoll
  "PIHARAR",           "Namonuito Atoll",
  "UNANU",             "Namonuito Atoll",
  "ONOUN",             "Namonuito Atoll",
  "MAKUR",             "Namonuito Atoll"
) %>%
  distinct(
    source_county_join,
    .keep_all = TRUE
  )


apply_operational_county <- function(data, county_variable) {
  county_variable <- rlang::ensym(county_variable)

  data %>%
    mutate(
      county_original = clean_display_text(!!county_variable),
      county_source_join = normalize_join_text(!!county_variable)
    ) %>%
    left_join(
      atoll_crosswalk,
      by = c(
        "county_source_join" = "source_county_join"
      )
    ) %>%
    mutate(
      county = coalesce(
        operational_county,
        county_original
      ),
      county_join = normalize_join_text(county)
    ) %>%
    select(-operational_county)
}


# ==============================================================================
# 5. Import and prepare Census data
# ==============================================================================

census_lookup_raw <- read_excel(
  census_file,
  sheet = census_sheet
)


census_prepared <- census_lookup_raw %>%
  transmute(
    state = clean_display_text(STATE),
    region = clean_display_text(REGION),
    source_county = as.character(COUNTY),

    census_total = clean_numeric(Census_Total),
    census_under5 = clean_numeric(Census_Under5),

    latitude_census = clean_numeric(GAM_LATITUDE),
    longitude_census = clean_numeric(GAM_LONGITUDE),

    area_m2 = clean_numeric(Area_m2)
  ) %>%
  apply_operational_county(source_county) %>%
  mutate(
    state_join = normalize_join_text(state),
    region_join = normalize_join_text(region)
  ) %>%
  filter(
    !is.na(state_join),
    state_join != "",
    !is.na(county_join),
    county_join != ""
  )


# Save the individual Census records that contribute to each combined atoll.
census_atoll_members <- census_prepared %>%
  filter(county != county_original) %>%
  select(
    state,
    region,
    county,
    county_original,
    county_source_join,
    census_total,
    census_under5,
    area_m2,
    latitude_census,
    longitude_census
  ) %>%
  arrange(
    state,
    county,
    county_original
  )


write_csv(
  census_atoll_members,
  census_atoll_members_output_file,
  na = ""
)


# ==============================================================================
# 6. Aggregate Census data to operational community/atoll level
# ==============================================================================
#
# Population density is recalculated as:
#
#   summed population / summed land area
#
# Existing density values are not averaged.
# ==============================================================================

census_lookup <- census_prepared %>%
  group_by(
    state,
    state_join,
    county,
    county_join
  ) %>%
  summarise(
    census_region = {
      valid_regions <- region[
        !is.na(region) &
          region != ""
      ]

      if (length(valid_regions) == 0) {
        NA_character_
      } else {
        names(
          sort(
            table(valid_regions),
            decreasing = TRUE
          )
        )[1]
      }
    },

    census_total = if (
      all(is.na(census_total))
    ) {
      NA_real_
    } else {
      sum(census_total, na.rm = TRUE)
    },

    census_under5 = if (
      all(is.na(census_under5))
    ) {
      NA_real_
    } else {
      sum(census_under5, na.rm = TRUE)
    },

    area_m2 = if (
      all(is.na(area_m2))
    ) {
      NA_real_
    } else {
      sum(area_m2, na.rm = TRUE)
    },

    latitude_census = safe_weighted_mean(
      latitude_census,
      census_total
    ),

    longitude_census = safe_weighted_mean(
      longitude_census,
      census_total
    ),

    census_source_communities = paste(
      sort(unique(county_original)),
      collapse = "; "
    ),

    census_source_n = n_distinct(
      county_original
    ),

    .groups = "drop"
  ) %>%
  mutate(
    area_km2 = case_when(
      !is.na(area_m2) &
        area_m2 > 0 ~
        area_m2 / 1e6,

      TRUE ~ NA_real_
    ),

    population_density_km2 = case_when(
      !is.na(census_total) &
        !is.na(area_km2) &
        area_km2 > 0 ~
        census_total / area_km2,

      TRUE ~ NA_real_
    ),

    under5_density_km2 = case_when(
      !is.na(census_under5) &
        !is.na(area_km2) &
        area_km2 > 0 ~
        census_under5 / area_km2,

      TRUE ~ NA_real_
    )
  )


# ==============================================================================
# 7. Check aggregated Census keys for duplicates
# ==============================================================================

census_duplicate_keys <- census_lookup %>%
  count(
    state_join,
    county_join,
    name = "records_per_key"
  ) %>%
  filter(records_per_key > 1)


write_csv(
  census_duplicate_keys,
  census_duplicates_output_file,
  na = ""
)


if (nrow(census_duplicate_keys) > 0) {
  warning(
    paste0(
      "Duplicate state/county keys remain after Census aggregation. Review: ",
      census_duplicates_output_file
    )
  )
}


census_lookup_join <- census_lookup %>%
  arrange(
    state_join,
    county_join,
    desc(!is.na(population_density_km2)),
    desc(!is.na(census_total)),
    desc(!is.na(latitude_census)),
    desc(!is.na(longitude_census))
  ) %>%
  distinct(
    state_join,
    county_join,
    .keep_all = TRUE
  )


# ==============================================================================
# 8. Import and prepare patient-level data
# ==============================================================================

patient <- read_csv(
  patient_file,
  show_col_types = FALSE
) %>%
  mutate(
    state = clean_display_text(state),
    county_input = county
  ) %>%
  apply_operational_county(county_input) %>%
  mutate(
    state_join = normalize_join_text(state),

    community_id = paste(
      state_join,
      county_join,
      sep = "__"
    ),

    months_since_last_vax = case_when(
      is.na(days_since_last_vax) ~ NA_real_,

      TRUE ~ pmin(
        days_since_last_vax / 30.4375,
        36
      )
    ),

    not_utd = case_when(
      is.na(utd) ~ NA_integer_,
      utd == 0 ~ 1L,
      TRUE ~ 0L
    ),

    not_utd_no_mmr = case_when(
      is.na(utd_no_mmr) ~ NA_integer_,
      utd_no_mmr == 0 ~ 1L,
      TRUE ~ 0L
    ),

    mmr_not_utd = case_when(
      is.na(mmr_utd) ~ NA_integer_,
      mmr_utd == 0 ~ 1L,
      TRUE ~ 0L
    ),

    high_individual_risk = case_when(
      is.na(individual_risk_score) ~ NA_integer_,

      individual_risk_score >=
        high_individual_risk_threshold ~ 1L,

      TRUE ~ 0L
    )
  )


# ==============================================================================
# 9. Operational remoteness classifications
# ==============================================================================

remote_names <- c(
  "Eauripik",
  "Elato Island",
  "Fais",
  "Faraulep",
  "Ifalik",
  "Lamotrek",
  "Ngulu",
  "Pikelot",
  "Satawal",
  "Sorol",
  "Ulithi",
  "Woleai",
  "Kapingamarangi",
  "Mokil",
  "Nukuoro",
  "Pingelap",
  "Sapwuahfik",
  "Losap Atoll",
  "Lukunor Atoll",
  "Murilo Atoll",
  "Nomwin Atoll",
  "Pulap Atoll",
  "Satawan Atoll",
  "Namonuito Atoll",
  "Namoluk",
  "Nukuoro Atoll",
  "Houk",
  "Polowat"
)


moderate_names <- c(
  "Wonei",
  "Uman",
  "Fefan",
  "Eot Island",
  "Parem",
  "Paata",
  "Polle"
)


remote_names_join <- normalize_join_text(remote_names)
moderate_names_join <- normalize_join_text(moderate_names)


# ==============================================================================
# 10. Create patient-derived community summary
# ==============================================================================

community_summary <- patient %>%
  filter(
    !is.na(state_join),
    state_join != "",
    !is.na(county_join),
    county_join != ""
  ) %>%
  group_by(
    community_id,
    state,
    state_join,
    county,
    county_join
  ) %>%
  summarise(
    child_population = n(),

    source_patient_communities = paste(
      sort(unique(county_original)),
      collapse = "; "
    ),

    source_patient_community_n = n_distinct(
      county_original
    ),

    children_not_utd = sum(
      not_utd == 1,
      na.rm = TRUE
    ),

    proportion_not_utd = 100 * safe_mean(not_utd),

    utd_coverage = 100 * safe_mean(utd),

    utd_no_mmr_coverage =
      100 * safe_mean(utd_no_mmr),

    mmr_eligible_n = sum(
      !is.na(mmr_utd)
    ),

    mmr_not_utd_n = sum(
      mmr_not_utd == 1,
      na.rm = TRUE
    ),

    mmr_coverage = 100 * safe_mean(mmr_utd),

    median_months_since_vax = safe_median(
      if_else(
        not_utd == 1,
        months_since_last_vax,
        NA_real_
      )
    ),

    mean_individual_risk =
      safe_mean(individual_risk_score),

    median_individual_risk =
      safe_median(individual_risk_score),

    p75_individual_risk =
      safe_quantile(
        individual_risk_score,
        probability = 0.75
      ),

    high_risk_n = sum(
      high_individual_risk == 1,
      na.rm = TRUE
    ),

    high_risk_percent =
      100 * safe_mean(high_individual_risk),

    latitude_patient = safe_mean(latitude),
    longitude_patient = safe_mean(longitude),

    .groups = "drop"
  )


# ==============================================================================
# 11. Join aggregated Census data to community summary
# ==============================================================================

community_summary <- community_summary %>%
  left_join(
    census_lookup_join %>%
      select(
        state_join,
        county_join,
        census_region,
        census_total,
        census_under5,
        latitude_census,
        longitude_census,
        area_m2,
        area_km2,
        population_density_km2,
        under5_density_km2,
        census_source_communities,
        census_source_n
      ),
    by = c(
      "state_join",
      "county_join"
    )
  ) %>%
  mutate(
    region = census_region,

    # Use patient-derived coordinates when available.
    # Use the population-weighted Census center as a fallback.
    latitude = coalesce(
      latitude_patient,
      latitude_census
    ),

    longitude = coalesce(
      longitude_patient,
      longitude_census
    )
  )


# ==============================================================================
# 12. Create risk-score components
# ==============================================================================

community_summary <- community_summary %>%
  mutate(
    remoteness = case_when(
      county_join %in% remote_names_join ~ 1,
      county_join %in% moderate_names_join ~ 0.5,
      TRUE ~ 0
    ),

    access_component =
      100 * remoteness,

    median_risk_component = pmin(
      pmax(median_individual_risk, 0),
      100
    ),

    p75_risk_component = pmin(
      pmax(p75_individual_risk, 0),
      100
    ),

    high_risk_percent_component = pmin(
      pmax(high_risk_percent, 0),
      100
    ),

    # Community patient-risk summary.
    #
    # The individual score already incorporates:
    #   1. UTD status excluding MMR
    #   2. MMR UTD status
    #   3. Time since last vaccination
    
    community_patient_risk_component =
      0.50 * median_risk_component +
      0.30 * p75_risk_component +
      0.20 * high_risk_percent_component,

    # Log transformation reduces the influence of very high-density places.
    log_population_density = case_when(
      !is.na(population_density_km2) &
        population_density_km2 >= 0 ~
        log1p(population_density_km2),

      TRUE ~ NA_real_
    )
  )


# ==============================================================================
# 13. Convert density and burden to comparable 0–100 components
# ==============================================================================

community_summary <- community_summary %>%
  mutate(
    # Higher percentile means greater population density.
    density_percentile_component =
      percentile_100(log_population_density),
    
    # Low density represents operational difficulty.
    low_density_component = case_when(
      !is.na(density_percentile_component) ~
        100 - density_percentile_component,
      TRUE ~ NA_real_
    ),
    
    # High density represents potential for greater transmission.
    high_density_component = case_when(
      !is.na(density_percentile_component) ~
        density_percentile_component,
      TRUE ~ NA_real_
    ),
    
    population_density_missing =
      is.na(population_density_km2),
    
    # Neutral values when density is unavailable.
    low_density_component_for_score =
      coalesce(low_density_component, 50),
    
    high_density_component_for_score =
      coalesce(high_density_component, 50),
    
    # High density matters most when vaccination risk is also high.
    transmission_component =
      high_density_component_for_score *
      community_patient_risk_component / 100,
    
    # Number of high-risk children represents potential outreach impact.
    burden_component =
      rescale_100(log1p(high_risk_n))
  )

# ==============================================================================
# 14. Calculate community priority score
# ==============================================================================

community_summary <- community_summary %>%
  mutate(
    priority_score = round(
      0.75 * community_patient_risk_component +
        0.05 * access_component +
        0.05 * low_density_component_for_score +
        0.10 * transmission_component +
        0.05 * burden_component,
      digits = 1
    ),
    
    priority_rank =
      min_rank(desc(priority_score))
  )


priority_cutpoints <- quantile(
  community_summary$priority_score,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE,
  names = FALSE
)


community_summary <- community_summary %>%
  mutate(
    priority_group = factor(
      case_when(
        is.na(priority_score) ~ NA_character_,
        
        # Highest score tertile.
        priority_score > priority_cutpoints[2] ~ "High",
        
        # Severe coverage gaps are always High.
        utd_coverage < 30 |
          mmr_coverage < 50 ~ "High",
        
        # Remote communities below the program target
        # cannot be classified as Low.
        remoteness == 1 &
          (
            utd_coverage < 50 |
              mmr_coverage < 50
          ) ~ "Moderate",
        
        # Middle score tertile.
        priority_score > priority_cutpoints[1] ~ "Moderate",
        
        TRUE ~ "Low"
      ),
      levels = c(
        "Low",
        "Moderate",
        "High"
      ),
      ordered = TRUE
    )
  )

# ==============================================================================
# 15. Create data-quality and routing-readiness indicators
# ==============================================================================

unresolved_geographies <- normalize_join_text(
  c(
    "Mortlock",
    "Northwest",
    "Missing",
    "Unknown"
  )
)


community_summary <- community_summary %>%
  mutate(
    data_quality_flag = case_when(
      is.na(latitude) | is.na(longitude) ~
        "Missing coordinates",

      county_join %in% unresolved_geographies ~
        "Geography requires resolution",

      is.na(census_total) ~
        "Missing Census match",

      is.na(area_km2) ~
        "Missing land area",

      is.na(population_density_km2) ~
        "Missing population density",

      child_population < 5 ~
        "Small denominator",

      TRUE ~
        "Ready"
    ),

    routing_ready = case_when(
      is.na(latitude) | is.na(longitude) ~ FALSE,
      county_join %in% unresolved_geographies ~ FALSE,
      TRUE ~ TRUE
    )
  ) %>%
  arrange(
    priority_rank,
    state,
    county
  )


# ==============================================================================
# 16. Create Census-join quality-assurance table
# ==============================================================================

unmatched_census_communities <- community_summary %>%
  filter(is.na(census_total)) %>%
  select(
    community_id,
    state,
    county,
    state_join,
    county_join,
    source_patient_communities,
    child_population,
    latitude,
    longitude
  ) %>%
  arrange(
    state,
    county
  )


write_csv(
  unmatched_census_communities,
  unmatched_census_output_file,
  na = ""
)


if (nrow(unmatched_census_communities) > 0) {
  warning(
    paste0(
      nrow(unmatched_census_communities),
      " operational communities did not match the Census lookup. Review: ",
      unmatched_census_output_file
    )
  )
}


# ==============================================================================
# 17. Create vaccine-needs summary
# ==============================================================================

vaccine_map <- c(
  DTaP = "dtap_utd",
  IPV = "ipv_utd",
  MMR = "mmr_utd",
  HepB = "hepb_utd",
  Hib = "hib_utd",
  PCV = "pcv_utd"
)


missing_vaccine_variables <- setdiff(
  unname(vaccine_map),
  names(patient)
)


if (length(missing_vaccine_variables) > 0) {
  warning(
    paste0(
      "The following vaccine variables were not found and will be excluded: ",
      paste(
        missing_vaccine_variables,
        collapse = ", "
      )
    )
  )
}


available_vaccine_map <- vaccine_map[
  vaccine_map %in% names(patient)
]


community_vaccine_needs <- lapply(
  names(available_vaccine_map),
  function(vaccine_name) {

    vaccine_variable <-
      available_vaccine_map[[vaccine_name]]

    patient %>%
      filter(
        !is.na(state_join),
        state_join != "",
        !is.na(county_join),
        county_join != ""
      ) %>%
      group_by(
        community_id,
        state,
        county
      ) %>%
      summarise(
        eligible_children = sum(
          !is.na(.data[[vaccine_variable]])
        ),

        children_due = sum(
          .data[[vaccine_variable]] == 0,
          na.rm = TRUE
        ),

        children_utd = sum(
          .data[[vaccine_variable]] == 1,
          na.rm = TRUE
        ),

        vaccine_coverage = case_when(
          eligible_children > 0 ~
            100 * children_utd / eligible_children,

          TRUE ~ NA_real_
        ),

        .groups = "drop"
      ) %>%
      mutate(
        vaccine = vaccine_name,

        # Initial planning assumption: one dose per child due.
        doses_needed = children_due
      )
  }
) %>%
  bind_rows() %>%
  select(
    community_id,
    state,
    county,
    vaccine,
    eligible_children,
    children_utd,
    children_due,
    vaccine_coverage,
    doses_needed
  ) %>%
  arrange(
    state,
    county,
    vaccine
  )


# ==============================================================================
# 18. Add aggregated Census variables to patient dashboard data
# ==============================================================================

patient_dashboard <- patient %>%
  left_join(
    census_lookup_join %>%
      select(
        state_join,
        county_join,
        census_region,
        census_total,
        census_under5,
        area_m2,
        area_km2,
        population_density_km2,
        under5_density_km2,
        census_source_communities,
        census_source_n
      ),
    by = c(
      "state_join",
      "county_join"
    )
  )


# ==============================================================================
# 19. Write dashboard-ready files
# ==============================================================================

write_csv(
  community_summary,
  community_output_file,
  na = ""
)


write_csv(
  community_vaccine_needs,
  vaccine_output_file,
  na = ""
)


write_csv(
  patient_dashboard,
  patient_output_file,
  na = ""
)


# ==============================================================================
# 20. Print preparation summary
# ==============================================================================

preparation_summary <- community_summary %>%
  summarise(
    communities = n(),

    analytic_children =
      sum(child_population, na.rm = TRUE),

    combined_operational_communities =
      sum(source_patient_community_n > 1, na.rm = TRUE),

    census_matches =
      sum(!is.na(census_total)),

    census_unmatched =
      sum(is.na(census_total)),

    density_available =
      sum(!is.na(population_density_km2)),

    density_missing =
      sum(is.na(population_density_km2)),

    coordinates_available =
      sum(
        !is.na(latitude) &
          !is.na(longitude)
      ),

    routing_ready_communities =
      sum(routing_ready, na.rm = TRUE),

    high_priority =
      sum(
        priority_group == "High",
        na.rm = TRUE
      ),
    
    moderate_priority =
      sum(
        priority_group == "Moderate",
        na.rm = TRUE
      ),
    
    low_priority =
      sum(
        priority_group == "Low",
        na.rm = TRUE
      )
  )


print(preparation_summary)


message(
  paste0(
    "\nData preparation complete.\n\n",
    "Created:\n",
    "  - ", community_output_file, "\n",
    "  - ", vaccine_output_file, "\n",
    "  - ", patient_output_file, "\n",
    "  - ", unmatched_census_output_file, "\n",
    "  - ", census_duplicates_output_file, "\n",
    "  - ", census_atoll_members_output_file, "\n"
  )
)
