library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(readxl)


safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
rescale_100 <- function(x) {
  if (all(is.na(x)) || diff(range(x, na.rm = TRUE)) == 0) return(rep(50, length(x)))
  100 * (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
}

#Calculate population density

census_lookup <- read_excel(
  "data/FSM Counties_Census_ChatGPT_11Jun2026.xlsx",
  sheet = "FSM Villages"
) %>%
  transmute(
    state = str_to_upper(str_squish(STATE)),
    region = str_to_upper(str_squish(REGION)),
    county = str_to_upper(str_squish(COUNTY)),
    
    census_total = as.numeric(Census_Total),
    census_under5 = as.numeric(Census_Under5),
    
    latitude = as.numeric(
      str_replace_all(as.character(GAM_LATITUDE), "\u00A0", "")
    ),
    longitude = as.numeric(
      str_replace_all(as.character(GAM_LONGITUDE), "\u00A0", "")
    ),
    
    area_m2 = as.numeric(Area_m2),
    
    # Always derive this directly from Area_m2.
    area_km2 = area_m2 / 1e6,
    
    population_density_km2 = if_else(
      !is.na(census_total) &
        !is.na(area_km2) &
        area_km2 > 0,
      census_total / area_km2,
      NA_real_
    ),
    
    under5_density_km2 = if_else(
      !is.na(census_under5) &
        !is.na(area_km2) &
        area_km2 > 0,
      census_under5 / area_km2,
      NA_real_
    )
  )

census_lookup_join <- census_lookup %>%
  select(
    state,
    region,
    county,
    census_total,
    census_under5,
    latitude_census = latitude,
    longitude_census = longitude,
    area_m2,
    area_km2,
    population_density_km2,
    under5_density_km2
  ) %>%
  distinct(state, county, .keep_all = TRUE)

# ! Replace the path when new IIS data are generated !

patient <- read_csv(
  "data/Dataset 1_FSM_2-59 mos_Full Dataset_06182026_DeID.csv",
  show_col_types = FALSE
) %>%
  mutate(
    state = str_to_title(str_squish(state)),
    county = str_to_title(str_squish(county)),
    
    state_join = str_to_upper(str_squish(state)),
    county_join = str_to_upper(str_squish(county)),
    
    community_id = paste(state_join, county_join, sep = "__"),
    
    months_since_last_vax = pmin(days_since_last_vax / 30.4375,36,na.rm = FALSE),
    
    not_utd = as.integer(utd == 0),
    not_utd_no_mmr = as.integer(utd_no_mmr == 0),
    
    mmr_not_utd = if_else(
      is.na(mmr_utd),
      NA_integer_,
      as.integer(mmr_utd == 0)
    ),
    
    high_individual_risk = as.integer(
      individual_risk_score >= 70
    )
  )

community_summary <- patient %>%
  filter(
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
    
    children_not_utd = sum(not_utd, na.rm = TRUE),
    proportion_not_utd = 100 * safe_mean(not_utd),
    
    utd_coverage = 100 * safe_mean(utd),
    utd_no_mmr_coverage = 100 * safe_mean(utd_no_mmr),
    
    mmr_eligible_n = sum(!is.na(mmr_utd)),
    mmr_not_utd_n = sum(mmr_not_utd, na.rm = TRUE),
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
    
    p75_individual_risk = if (
      all(is.na(individual_risk_score))
    ) {
      NA_real_
    } else {
      as.numeric(
        quantile(
          individual_risk_score,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        )
      )
    },
    
    high_risk_n = sum(
      high_individual_risk,
      na.rm = TRUE
    ),
    
    high_risk_percent =
      100 * safe_mean(high_individual_risk),
    
    latitude_patient = safe_mean(latitude),
    longitude_patient = safe_mean(longitude),
    
    .groups = "drop"
  ) %>%
  left_join(
    census_lookup_join,
    by = c(
      "state_join" = "state",
      "county_join" = "county"
    )
  )

community_summary <- community_summary %>%
  mutate(
    latitude = coalesce(
      latitude_patient,
      latitude_census
    ),
    
    longitude = coalesce(
      longitude_patient,
      longitude_census
    )
  )

#Log transform pop density

log_population_density =
  log1p(population_density_km2)

# Operational remoteness lookup. Edit this file as local classifications improve.
remote_names <- c("Eauripik", "Elato Island", "Fais", "Faraulep", "Ifalik", "Lamotrek",
                  "Ngulu", "Pikelot", "Satawal", "Sorol", "Ulithi", "Woleai", "Kapingamarangi",
                  "Mokil", "Nukuoro", "Pingelap", "Sapwuahfik", "Losap Atoll", "Namoluk",
                  "Nukuoro Atoll", "Satawan Atoll", "Houk", "Onoun", "Onou", "Pollap", "Polowat")
moderate_names <- c("Wonei", "Uman", "Fefan", "Eot Island", "Parem", "Piis-Paneu", "Paata", "Polle")

community_summary <- patient %>%
  filter(!is.na(county), county != "") %>%
  group_by(community_id, state, county) %>%
  summarise(
    child_population = n(),
    children_not_utd = sum(not_utd, na.rm = TRUE),
    proportion_not_utd = 100 * safe_mean(not_utd),
    utd_coverage = 100 * safe_mean(utd),
    utd_no_mmr_coverage = 100 * safe_mean(utd_no_mmr),
    mmr_eligible_n = sum(!is.na(mmr_utd)),
    mmr_not_utd_n = sum(mmr_not_utd, na.rm = TRUE),
    mmr_coverage = 100 * safe_mean(mmr_utd),
    median_months_since_vax = safe_median(if_else(not_utd == 1, months_since_last_vax, NA_real_)),
    median_individual_risk = safe_median(individual_risk_score),
    high_risk_n = sum(high_individual_risk, na.rm = TRUE),
    latitude = safe_mean(latitude),
    longitude = safe_mean(longitude),
    .groups = "drop"
  ) %>%
  mutate(
    remoteness = case_when(
      county %in% remote_names ~ 1,
      county %in% moderate_names ~ 0.5,
      TRUE ~ 0
    ),
    # Until verified land-area/population data are supplied, this is explicitly a
    # child-count concentration proxy and is NOT labeled population density.
    child_concentration_component = rescale_100(log1p(child_population)),
    
    individual_risk_component = pmin(pmax(median_individual_risk, 0), 100),
    
    access_component = 100 * remoteness,
    
    service_gap_component = pmin(pmax(proportion_not_utd, 0), 100),
    
    mmr_gap_component = if_else(is.na(mmr_coverage), 0, 100 - mmr_coverage),
    
    priority_score = round(
      0.45 * individual_risk_component +
      0.20 * access_component +
      0.10 * service_gap_component +
      0.10 * mmr_gap_component +
      0.15 * child_concentration_component, 1
    ),
    priority_rank = min_rank(desc(priority_score)),
    priority_group = case_when(
      priority_score >= 70 ~ "Very high",
      priority_score >= 55 ~ "High",
      priority_score >= 35 ~ "Moderate",
      TRUE ~ "Lower"
    ),
    data_quality_flag = case_when(
      is.na(latitude) | is.na(longitude) ~ "Missing coordinates",
      county %in% c("Mortlock", "Northwest", "Missing", "Unknown") ~ "Geography requires resolution",
      child_population < 5 ~ "Small denominator",
      TRUE ~ "Ready"
    )
  ) %>%
  arrange(priority_rank)

vaccine_map <- c(
  DTaP = "dtap4utd", IPV = "ipv3utd", MMR = "mmr_utd", HepB = "hepb3utd",
  Hib = "hib_utd", PCV = "pcv_utd", Rotavirus = "rota3utd"
)
community_vaccine_needs <- lapply(names(vaccine_map), function(vax) {
  var <- vaccine_map[[vax]]
  patient %>%
    filter(!is.na(county), county != "") %>%
    group_by(community_id, state, county) %>%
    summarise(children_due = sum(.data[[var]] == 0, na.rm = TRUE), .groups = "drop") %>%
    mutate(vaccine = vax, doses_needed = children_due)
}) %>% bind_rows()

write_csv(community_summary, "data/community_summary.csv", na = "")
write_csv(community_vaccine_needs, "data/community_vaccine_needs.csv", na = "")
write_csv(patient, "data/patient_dashboard.csv", na = "")

