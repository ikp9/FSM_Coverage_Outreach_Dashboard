library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(DT)
library(leaflet)
library(plotly)

required_project_files <- c(
  "R/route_functions.R",
  "R/prepare_data.R"
)

missing_project_files <- required_project_files[
  !file.exists(required_project_files)
]

if (length(missing_project_files) > 0) {
  stop(
    paste0(
      "The app is not running from the FSM_Coverage_Outreach_Dashboard project root.\n",
      "Current working directory: ", getwd(), "\n\n",
      "Missing project file(s):\n  - ",
      paste(missing_project_files, collapse = "\n  - "),
      "\n\nOpen FSM_Coverage_Outreach_Dashboard.Rproj and rerun the app."
    ),
    call. = FALSE
  )
}

source("R/route_functions.R")

# All generated dashboard datasets are stored under data/ in the NEW project.
dashboard_files <- c(
  community = file.path("data", "community_summary.csv"),
  vaccine_needs = file.path("data", "community_vaccine_needs.csv"),
  product_needs = file.path("data", "community_product_needs.csv"),
  patient = file.path("data", "patient_dashboard.csv")
)

if (any(!file.exists(dashboard_files))) {
  source("R/prepare_data.R", local = FALSE)
}

missing_dashboard_files <- dashboard_files[
  !file.exists(dashboard_files)
]

if (length(missing_dashboard_files) > 0) {
  stop(
    paste0(
      "Dashboard data preparation did not create the required file(s):\n  - ",
      paste(missing_dashboard_files, collapse = "\n  - "),
      "\n\nRun source(\"R/prepare_data.R\") from the project root and review the first error."
    ),
    call. = FALSE
  )
}

community <- read_csv(
  dashboard_files[["community"]],
  show_col_types = FALSE
)

vax_needs <- read_csv(
  dashboard_files[["vaccine_needs"]],
  show_col_types = FALSE
)

product_needs <- read_csv(
  dashboard_files[["product_needs"]],
  show_col_types = FALSE
)

patient <- read_csv(
  dashboard_files[["patient"]],
  locale = locale(encoding = "windows-1252"),
  show_col_types = FALSE
)

# Dashboard coverage components mirror the age-specific requirements used in
# the analytic UTD-for-age definition. Defining them here also corrects the
# previously generated age-group-9 UTD values without requiring a data rebuild.
flag_or <- function(x, y) {
  as.numeric(coalesce(x, 0) == 1 | coalesce(y, 0) == 1)
}

patient <- patient %>%
  mutate(
    coverage_dtap = case_when(
      between(age_months, 2, 3) ~ coalesce(as.numeric(dtap1utd), 0),
      between(age_months, 4, 5) ~ coalesce(as.numeric(dtap2utd), 0),
      between(age_months, 6, 11) ~ coalesce(as.numeric(dtap3utd), 0),
      between(age_months, 12, 47) ~ coalesce(as.numeric(dtap4utd), 0),
      between(age_months, 48, 83) ~ coalesce(as.numeric(dtap54utd), 0),
      TRUE ~ NA_real_
    ),
    coverage_ipv = case_when(
      between(age_months, 2, 3) ~ coalesce(as.numeric(ipv1utd), 0),
      between(age_months, 4, 5) ~ coalesce(as.numeric(ipv2utd), 0),
      between(age_months, 6, 47) ~ coalesce(as.numeric(ipv3utd), 0),
      between(age_months, 48, 83) ~ coalesce(as.numeric(ipv43utd), 0),
      TRUE ~ NA_real_
    ),
    coverage_mmr = case_when(
      age_months < 12 ~ NA_real_,
      between(age_months, 12, 47) ~ coalesce(as.numeric(mmr1utd), 0),
      between(age_months, 48, 83) ~ coalesce(as.numeric(mmr2utd), 0),
      TRUE ~ NA_real_
    ),
    coverage_hib = case_when(
      between(age_months, 2, 3) ~ coalesce(as.numeric(hib1utd), 0),
      between(age_months, 4, 11) ~ coalesce(as.numeric(hib2utd), 0),
      between(age_months, 12, 18) ~ coalesce(as.numeric(hib3utd), 0),
      between(age_months, 19, 83) ~ flag_or(hib3utd, hib_utd),
      TRUE ~ NA_real_
    ),
    coverage_hepb = case_when(
      between(age_months, 2, 5) ~ coalesce(as.numeric(hepb2utd), 0),
      between(age_months, 6, 83) ~ coalesce(as.numeric(hepb3utd), 0),
      TRUE ~ NA_real_
    ),
    coverage_pcv = case_when(
      between(age_months, 2, 3) ~ coalesce(as.numeric(pcv1utd), 0),
      between(age_months, 4, 5) ~ coalesce(as.numeric(pcv2utd), 0),
      between(age_months, 6, 11) ~ coalesce(as.numeric(pcv3utd), 0),
      between(age_months, 12, 18) ~ coalesce(as.numeric(pcv4utd), 0),
      between(age_months, 19, 83) ~ flag_or(pcv4utd, pcv_utd),
      TRUE ~ NA_real_
    ),
    coverage_utd = case_when(
      between(age_months, 2, 11) ~ as.numeric(
        coverage_dtap == 1 & coverage_ipv == 1 & coverage_hib == 1 &
          coverage_hepb == 1 & coverage_pcv == 1
      ),
      between(age_months, 12, 83) ~ as.numeric(
        coverage_dtap == 1 & coverage_ipv == 1 & coverage_mmr == 1 &
          coverage_hib == 1 & coverage_hepb == 1 & coverage_pcv == 1
      ),
      TRUE ~ NA_real_
    )
  )

priority_levels <- c(
  "High",
  "Moderate",
  "Low"
)

priority_colors <- c(
  "#E03F4F",  # High: red
  "#F8c463",  # Moderate: yellow
  "#81912F"   # Low: green
)

pal_priority <- colorFactor(
  palette = priority_colors,
  levels = priority_levels,
  ordered = TRUE,
  na.color = "#BDBDBD"
)

# Discrete vaccination coverage colors used consistently on both coverage maps:
#   <30%:    red 
#   40 - <50% dark orange
#   50–<80%: light orange
#   80-<90: yellow
#   90-<95: light green
#   >=95%:   green
coverage_colors <- c(
  "#FF0D0D",
  "#FF4E11",
  "#FF8E15",
  "#FAB733",
  "#ACB334",
  "#69B34C"
)

coverage_color <- function(x) {
  values <- suppressWarnings(as.numeric(x))
  
  dplyr::case_when(
    is.na(values) ~ "#BDBDBD",
    values < 30 ~ coverage_colors[[1]],
    values < 50 ~ coverage_colors[[2]],
    values < 80 ~ coverage_colors[[3]],
    values < 90 ~ coverage_colors[[4]],
    values < 95 ~ coverage_colors[[5]],
    TRUE ~ coverage_colors[[6]]
  )
}

coverage_legend_labels <- c(
  "<30%",
  "30–<50%",
  "50–<80%",
  "80–<90%",
  "90–<95%",
  "≥95%"
)

# Stable marker colors for the reminder/recall map. The dashboard's `county`
# field already contains the operational atoll grouping for relevant Chuuk
# communities, so all villages in the same county or atoll share one color.
reminder_color_lookup <- patient %>%
  filter(
    !is.na(state),
    str_trim(state) != "",
    !is.na(county),
    str_trim(county) != ""
  ) %>%
  transmute(
    state = str_squish(state),
    color_group = str_squish(county),
    color_key = paste(state, color_group, sep = " — ")
  ) %>%
  distinct(state, color_group, color_key) %>%
  arrange(state, color_group)

reminder_group_colors <- grDevices::hcl.colors(
  n = nrow(reminder_color_lookup),
  palette = "Dynamic"
)

pal_reminder_group <- colorFactor(
  palette = reminder_group_colors,
  domain = reminder_color_lookup$color_key,
  na.color = "#6c757d"
)

coverage_age_choices <- c(
  "2–3 months" = "2_3",
  "4–5 months" = "4_5",
  "6–11 months" = "6_11",
  "12–18 months" = "12_18",
  "19–35 months" = "19_35",
  "3 years" = "3_years",
  "4 years" = "4_years",
  "5 years" = "5_years",
  "6 years" = "6_years",
  "2–59 months" = "2_59",
  "4–6 years" = "4_6_years"
)

coverage_age_ranges <- list(
  "2_3" = c(2, 3),
  "4_5" = c(4, 5),
  "6_11" = c(6, 11),
  "12_18" = c(12, 18),
  "19_35" = c(19, 35),
  "3_years" = c(36, 47),
  "4_years" = c(48, 59),
  "5_years" = c(60, 71),
  "6_years" = c(72, 83),
  "2_59" = c(2, 59),
  "4_6_years" = c(48, 83)
)

coverage_age_labels <- setNames(
  names(coverage_age_choices),
  unname(coverage_age_choices)
)

coverage_indicator_choices <- c(
  "DTaP UTD" = "coverage_dtap",
  "IPV UTD" = "coverage_ipv",
  "MMR UTD" = "coverage_mmr",
  "Hib UTD" = "coverage_hib",
  "HepB UTD" = "coverage_hepb",
  "PCV UTD" = "coverage_pcv"
)

coverage_definition_notes <- c(
  "2_3" = paste0(
    "Up to date for 2–3 months includes: ≥1 DTaP, ≥1 IPV, ",
    "≥1 Hib, ≥2 HepB, and ≥1 PCV."
  ),
  "4_5" = paste0(
    "Up to date for 4–5 months includes: ≥2 DTaP, ≥2 IPV, ",
    "≥2 Hib, ≥2 HepB, and ≥2 PCV."
  ),
  "6_11" = paste0(
    "Up to date for 6–11 months includes: ≥3 DTaP, ≥3 IPV, ",
    "≥2 Hib, ≥3 HepB, and ≥3 PCV."
  ),
  "12_18" = paste0(
    "Up to date for 12–18 months includes: ≥4 DTaP, ≥3 IPV, ",
    "≥1 MMR, ≥3 Hib, ≥3 HepB, and ≥4 PCV."
  ),
  "19_35" = paste0(
    "Up to date for 19–35 months includes: ≥4 DTaP, ≥3 IPV, ",
    "≥1 MMR, Hib UTD (or ≥3 Hib), ≥3 HepB, and PCV UTD ",
    "(or ≥4 PCV)."
  ),
  "3_years" = paste0(
    "Up to date for 3 years includes: ≥4 DTaP, ≥3 IPV, ≥1 MMR, ",
    "Hib UTD (or ≥3 Hib), ≥3 HepB, and PCV UTD (or ≥4 PCV)."
  ),
  "4_years" = paste0(
    "Up to date for 4 years includes: DTaP UTD (≥5 doses, or the ",
    "4th dose at age ≥4 years), IPV UTD (≥4 doses, or the 3rd dose ",
    "at age ≥4 years), ≥2 MMR, Hib UTD (or ≥3 Hib), ≥3 HepB, ",
    "and PCV UTD (or ≥4 PCV)."
  ),
  "5_years" = paste0(
    "Up to date for 5 years includes: DTaP UTD (≥5 doses, or the ",
    "4th dose at age ≥4 years), IPV UTD (≥4 doses, or the 3rd dose ",
    "at age ≥4 years), ≥2 MMR, Hib UTD (or ≥3 Hib), ≥3 HepB, ",
    "and PCV UTD (or ≥4 PCV)."
  ),
  "6_years" = paste0(
    "Up to date for 6 years includes: DTaP UTD (≥5 doses, or the ",
    "4th dose at age ≥4 years), IPV UTD (≥4 doses, or the 3rd dose ",
    "at age ≥4 years), ≥2 MMR, Hib UTD (or ≥3 Hib), ≥3 HepB, ",
    "and PCV UTD (or ≥4 PCV)."
  ),
  "2_59" = paste0(
    "Up to date for 2–59 months uses each child's exact age-specific ",
    "dose requirements. MMR is included only for children aged ",
    "≥12 months."
  ),
  "4_6_years" = paste0(
    "Up to date for 4–6 years includes: DTaP UTD (≥5 doses, or the ",
    "4th dose at age ≥4 years), IPV UTD (≥4 doses, or the 3rd dose ",
    "at age ≥4 years), ≥2 MMR, Hib UTD (or ≥3 Hib), ≥3 HepB, ",
    "and PCV UTD (or ≥4 PCV)."
  )
)

filter_coverage_age <- function(data, age_group) {
  age_range <- coverage_age_ranges[[age_group]]
  data %>%
    filter(
      age_months >= age_range[[1]],
      age_months <= age_range[[2]]
    )
}

ui <- page_navbar(
  title = "From Shore to Shot",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#0056a6", navbar_bg = "#003e73"),
  header = tags$head(tags$link(rel="stylesheet", href="styles.css")),
  nav_panel(
    "Situation overview",
    
    layout_sidebar(
      fillable = FALSE,
      
      sidebar = sidebar(
        selectInput(
          "state",
          "State",
          choices = c(
            "All",
            sort(unique(community$state))
          ),
          selected = "All"
        ),
        
        sliderInput(
          "min_priority",
          "Minimum priority score",
          0,
          100,
          0
        ),
        
        checkboxInput(
          "ready_only",
          "Only geographies ready for routing",
          FALSE
        ),
        
        p(
          class = "small-note",
          paste0(
            "Priority score: 90% community patient risk, ",
            "1% access/remoteness, ",
            "1% low-density access difficulty, ",
            "4% high-density transmission potential, and ",
            "4% high-risk child burden."
          )
        )
      ),
      
      layout_columns(
        value_box(
          title = "Children",
          value = textOutput("n_children"),
          showcase = bsicons::bs_icon("people"),
          class = "overview-value-box"
        ),
        
        value_box(
          title = "Not UTD",
          value = textOutput("n_not_utd"),
          showcase = bsicons::bs_icon("exclamation-triangle"),
          class = "overview-value-box"
        ),
        
        value_box(
          title = "High-priority communities",
          value = textOutput("n_high"),
          showcase = bsicons::bs_icon("geo-alt"),
          class = "overview-value-box"
        ),
        
        value_box(
          title = "MMR doses potentially needed",
          value = textOutput("n_mmr"),
          showcase = bsicons::bs_icon("shield-plus"),
          class = "overview-value-box"
        ),
        
        col_widths = c(3, 3, 3, 3),
        class = "overview-metrics"
      ),
      
      layout_columns(
        card(
          full_screen = TRUE,
          class = "overview-map-card",
          card_header("Community priority map"),
          
          div(
            class = "overview-map",
            leafletOutput(
              "overview_map",
              height = "430px"
            )
          )
        ),
        
        card(
          full_screen = TRUE,
          class = "overview-map-card",
          card_header(
            "Vaccination coverage: up to date for age"
          ),
          
          tags$p(
            class = "small-note map-note",
            paste0(
              "Community coverage is the percentage of children meeting ",
              "all age-appropriate vaccine requirements in the analytic ",
              "dataset. Marker size represents the number of children ",
              "in the community."
            )
          ),
          
          div(
            class = "overview-map",
            leafletOutput(
              "coverage_map",
              height = "430px"
            )
          )
        ),
        
        col_widths = c(6, 6),
        class = "overview-map-grid"
      ),
      
      card(
        class = "overview-table-card",
        card_header("Ranked community priorities"),
        DTOutput("priority_table")
      )
    )
  ),
  nav_panel(
    "Vaccination coverage",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectInput(
          "coverage_jurisdiction",
          "Geography",
          choices = c(
            "FSM",
            sort(unique(patient$state))
          ),
          selected = "FSM"
        ),
        selectInput(
          "coverage_age_group",
          "Age group",
          choices = coverage_age_choices,
          selected = "2_59"
        ),
        selectInput(
          "coverage_map_indicator",
          "Coverage map indicator",
          choices = coverage_indicator_choices,
          selected = "coverage_dtap"
        ),
        tags$p(
          class = "small-note",
          paste0(
            "The coverage chart and coverage map use the selected age group. ",
            "The reminder/recall map includes all children aged 2 months ",
            "through 6 years in the selected geography."
          )
        )
      ),
      layout_columns(
        value_box(
          title = "Children aged 2 months–6 years",
          value = textOutput("coverage_children"),
          showcase = bsicons::bs_icon("people"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Percent UTD for age",
          value = textOutput("coverage_utd_percent"),
          showcase = bsicons::bs_icon("shield-check"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Children with active reminder/recall",
          value = textOutput("coverage_reminder_count"),
          showcase = bsicons::bs_icon("bell"),
          class = "overview-value-box"
        ),
        col_widths = c(4, 4, 4),
        class = "overview-metrics"
      ),
      card(
        class = "coverage-chart-card",
        card_header("Vaccination coverage by indicator"),
        plotlyOutput("coverage_bar", height = "350px"),
        uiOutput("coverage_definition_note")
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          class = "overview-map-card",
          card_header("Vaccination coverage map"),
          tags$p(
            class = "small-note map-note",
            paste0(
              "Community-level coverage for the selected age group and ",
              "indicator. Marker size represents the eligible denominator."
            )
          ),
          div(
            class = "overview-map",
            leafletOutput("vaccination_coverage_map", height = "430px")
          )
        ),
        card(
          full_screen = TRUE,
          class = "overview-map-card",
          card_header("Children on active reminder/recall"),
          tags$p(
            class = "small-note map-note",
            paste0(
              "Village-level counts for all children aged 2 months through ",
              "6 years. Marker size increases with the number of children. ",
              "Marker color denotes county; in Chuuk, color denotes the operational atoll where applicable."
            )
          ),
          div(
            class = "overview-map",
            leafletOutput("reminder_recall_map", height = "430px")
          )
        ),
        col_widths = c(6, 6),
        class = "overview-map-grid"
      )
    )
  ),
  nav_panel("Community risk & access",
            layout_sidebar(
              sidebar=sidebar(selectInput("community_id", "Community", choices=setNames(community$community_id, paste(community$state, community$county, sep=" — ")))),
              layout_columns(
                card(card_header("Community profile"), uiOutput("community_profile")),
                card(card_header("Priority-score components"), plotlyOutput("components_plot", height=330)),
                col_widths=c(5,7)
              ),
              card(
                card_header("Estimated Vaccine Needs"),
                
                layout_columns(
                  div(
                    class = "vaccine-needs-table",
                    DTOutput("vax_table")
                  ),
                  
                  div(
                    class = "vaccine-needs-table",
                    DTOutput("product_table")
                  ),
                  
                  col_widths = c(6, 6)
                )
              )
            )
  ),
  nav_panel("Outreach planner",
            layout_sidebar(
              sidebar=sidebar(
                selectInput("plan_state", "State", choices=sort(unique(community$state))),
                selectizeInput("plan_communities", "Communities to visit", choices=NULL, multiple=TRUE),
                numericInput("hub_lat", "Hub latitude", value=6.982, step=.001),
                numericInput("hub_lon", "Hub longitude", value=158.204, step=.001),
                numericInput("teams", "Number of teams", value=1, min=1, max=10),
                numericInput("speed", "Average travel speed (km/hour)", value=25, min=2),
                numericInput("workday", "Workday length (hours)", value=8, min=1),
                numericInput("setup", "Setup time per community (minutes)", value=30, min=0),
                numericInput("service", "Minutes per child", value=4, min=.5, step=.5),
                actionButton("optimize", "Generate outreach plan", class="btn-primary"),
                downloadButton("download_plan", "Export itinerary")
              ),
              layout_columns(
                value_box(title="Communities", value=textOutput("plan_n")),
                value_box(title="Children targeted", value=textOutput("plan_children")),
                value_box(title="Travel hours", value=textOutput("plan_travel")),
                value_box(title="Distance (km)", value=textOutput("plan_distance")),
                col_widths=c(3,3,3,3)
              ),
              card(full_screen=TRUE, card_header("Proposed route"), leafletOutput("route_map", height=540)),
              card(card_header("Operational itinerary"), DTOutput("itinerary")),
              card(card_header("Estimated vaccine products needed for selected outreach"), DTOutput("plan_product_table"))
            )
  ),
  nav_panel("Scenario comparison",
            card(card_header("Scenario calculator"),
                 layout_columns(
                   numericInput("scen_children", "Children targeted", 300, min=1),
                   numericInput("scen_communities", "Communities", 8, min=1),
                   numericInput("scen_distance", "Route distance (km)", 250, min=0),
                   col_widths=c(4,4,4)
                 ),
                 DTOutput("scenario_table")
            )
  ),
  nav_panel("Data quality & methods",
            card(card_header("Data readiness"), DTOutput("quality_table")),
            card(card_header("Prototype methods"),
                 tags$p("This version uses patient-level immunization data aggregated to planning communities. Route planning uses a transparent nearest-neighbor heuristic over geodesic distances."),
                 tags$ul(
                   tags$li("Update patient_file_stem in R/prepare_data.R when a newer deidentified analytic dataset is available, then rerun the preparation script."),
                   tags$li("Replace the provisional remoteness lookup with a validated community lookup."),
                   tags$li("Add verified Census population/land-area fields before labeling any measure population density."),
                   tags$li("Replace geodesic travel estimates with validated route-edge travel times, schedules, fuel, and weather constraints for production use.")
                 )
            )
  )
)

server <- function(input, output, session) {
  coverage_scope <- reactive({
    x <- patient %>%
      filter(between(age_months, 2, 83))
    
    if (input$coverage_jurisdiction != "FSM") {
      x <- x %>%
        filter(state == input$coverage_jurisdiction)
    }
    
    x
  })
  
  coverage_age_data <- reactive({
    filter_coverage_age(
      coverage_scope(),
      input$coverage_age_group
    )
  })
  
  output$coverage_children <- renderText({
    format(
      n_distinct(coverage_scope()$patient_id),
      big.mark = ","
    )
  })
  
  output$coverage_utd_percent <- renderText({
    x <- coverage_scope()
    eligible <- !is.na(x$coverage_utd)
    
    if (!any(eligible)) {
      return("—")
    }
    
    paste0(
      formatC(
        100 * mean(x$coverage_utd[eligible]),
        format = "f",
        digits = 1
      ),
      "%"
    )
  })
  
  output$coverage_reminder_count <- renderText({
    format(
      sum(coverage_scope()$onreminder == 1, na.rm = TRUE),
      big.mark = ","
    )
  })
  
  observeEvent(
    input$coverage_age_group,
    {
      choices <- coverage_indicator_choices
      
      if (input$coverage_age_group %in% c("2_3", "4_5", "6_11")) {
        choices <- choices[names(choices) != "MMR UTD"]
      }
      
      selected <- isolate(input$coverage_map_indicator)
      
      if (is.null(selected) || !selected %in% unname(choices)) {
        selected <- "coverage_dtap"
      }
      
      updateSelectInput(
        session,
        "coverage_map_indicator",
        choices = choices,
        selected = selected
      )
    },
    ignoreInit = FALSE
  )
  
  coverage_bar_data <- reactive({
    x <- coverage_age_data()
    indicator_map <- coverage_indicator_choices
    
    if (input$coverage_age_group %in% c("2_3", "4_5", "6_11")) {
      indicator_map <- indicator_map[names(indicator_map) != "MMR UTD"]
    }
    
    bind_rows(
      lapply(
        names(indicator_map),
        function(indicator_label) {
          indicator_variable <- indicator_map[[indicator_label]]
          values <- x[[indicator_variable]]
          eligible_children <- sum(!is.na(values))
          children_utd <- sum(values == 1, na.rm = TRUE)
          
          tibble(
            indicator = indicator_label,
            eligible_children = eligible_children,
            children_utd = children_utd,
            coverage = if_else(
              eligible_children > 0,
              100 * children_utd / eligible_children,
              NA_real_
            )
          )
        }
      )
    ) %>%
      mutate(
        indicator = factor(
          indicator,
          levels = names(indicator_map)
        )
      )
  })
  
  output$coverage_bar <- renderPlotly({
    d <- coverage_bar_data() %>%
      filter(!is.na(coverage))
    
    validate(
      need(nrow(d) > 0, "No eligible children are available for this selection.")
    )
    
    plot_ly(
      d,
      x = ~indicator,
      y = ~coverage,
      type = "bar",
      marker = list(color = "#0056a6"),
      text = ~paste0(round(coverage, 1), "%"),
      textposition = "outside",
      hovertext = ~paste0(
        "<b>", indicator, "</b><br>",
        "Coverage: ", sprintf("%.1f", coverage), "%<br>",
        "Children UTD: ", format(children_utd, big.mark = ","), "<br>",
        "Eligible children: ", format(eligible_children, big.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(
          title = "Coverage (%)",
          range = c(0, 105),
          ticksuffix = "%"
        ),
        margin = list(l = 70, r = 25, t = 20, b = 70),
        showlegend = FALSE
      )
  })
  
  output$coverage_definition_note <- renderUI({
    tagList(
      tags$p(
        class = "small-note coverage-definition-note",
        coverage_definition_notes[[input$coverage_age_group]]
      ),
      if (input$coverage_age_group == "2_59") {
        tags$p(
          class = "small-note coverage-definition-note",
          "The MMR denominator excludes children younger than 12 months."
        )
      }
    )
  })
  
  coverage_map_data <- reactive({
    req(input$coverage_map_indicator)
    indicator_variable <- input$coverage_map_indicator
    
    coverage_age_data() %>%
      filter(
        !is.na(community_id),
        community_id != ""
      ) %>%
      mutate(
        selected_indicator = .data[[indicator_variable]]
      ) %>%
      group_by(
        community_id,
        state,
        county
      ) %>%
      summarise(
        eligible_children = sum(!is.na(selected_indicator)),
        children_utd = sum(selected_indicator == 1, na.rm = TRUE),
        coverage = if_else(
          eligible_children > 0,
          100 * children_utd / eligible_children,
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      left_join(
        community %>%
          select(
            community_id,
            latitude,
            longitude,
            data_quality_flag
          ),
        by = "community_id"
      )
  })
  
  output$vaccination_coverage_map <- renderLeaflet({
    x <- coverage_map_data() %>%
      filter(
        eligible_children > 0,
        !is.na(latitude),
        !is.na(longitude)
      )
    
    validate(
      need(nrow(x) > 0, "No geocoded coverage data are available for this selection.")
    )
    
    indicator_label <- names(coverage_indicator_choices)[
      match(input$coverage_map_indicator, coverage_indicator_choices)
    ]
    
    m <- leaflet(x) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmax(5, sqrt(eligible_children)),
        color = ~coverage_color(coverage),
        fillColor = ~coverage_color(coverage),
        fillOpacity = 0.85,
        opacity = 1,
        weight = 1,
        label = ~paste0(
          county,
          ": ",
          round(coverage, 1),
          "%"
        ),
        popup = ~paste0(
          "<b>", county, "</b><br>",
          "State: ", state, "<br>",
          indicator_label, ": ", round(coverage, 1), "%<br>",
          "Children UTD: ", children_utd, " of ", eligible_children, "<br>",
          "Data readiness: ", data_quality_flag
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = coverage_colors,
        labels = coverage_legend_labels,
        opacity = 0.9,
        title = paste0(indicator_label, " coverage")
      )
    
    if (nrow(x) == 1) {
      m <- m %>%
        setView(
          lng = x$longitude[[1]],
          lat = x$latitude[[1]],
          zoom = 9
        )
    } else {
      m <- m %>%
        fitBounds(
          lng1 = min(x$longitude),
          lat1 = min(x$latitude),
          lng2 = max(x$longitude),
          lat2 = max(x$latitude)
        )
    }
    
    m
  })
  
  reminder_map_data <- reactive({
    coverage_scope() %>%
      filter(
        onreminder == 1,
        !is.na(city),
        str_trim(city) != "",
        !is.na(latitude),
        !is.na(longitude),
        !is.na(county),
        str_trim(county) != ""
      ) %>%
      mutate(
        village = str_to_title(str_squish(city)),
        color_group = str_squish(county),
        color_key = paste(str_squish(state), color_group, sep = " — ")
      ) %>%
      group_by(
        state,
        county,
        color_group,
        color_key,
        village
      ) %>%
      summarise(
        active_reminder = n_distinct(patient_id),
        latitude = median(latitude, na.rm = TRUE),
        longitude = median(longitude, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        marker_radius = pmin(
          24,
          pmax(5, 3 + 2.2 * sqrt(active_reminder))
        )
      )
  })
  
  output$reminder_recall_map <- renderLeaflet({
    x <- reminder_map_data()
    
    validate(
      need(nrow(x) > 0, "No geocoded reminder/recall records are available for this selection.")
    )
    
    m <- leaflet(x) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~marker_radius,
        color = ~pal_reminder_group(color_key),
        fillColor = ~pal_reminder_group(color_key),
        fillOpacity = 0.72,
        opacity = 0.95,
        weight = 1.5,
        label = ~paste0(
          village,
          ": ",
          active_reminder,
          " children"
        ),
        popup = ~paste0(
          "<b>", village, "</b><br>",
          "County / atoll: ", color_group, "<br>",
          "State: ", state, "<br>",
          "Active reminder/recall: ", active_reminder
        )
      )
    
    if (nrow(x) == 1) {
      m <- m %>%
        setView(
          lng = x$longitude[[1]],
          lat = x$latitude[[1]],
          zoom = 11
        )
    } else {
      m <- m %>%
        fitBounds(
          lng1 = min(x$longitude),
          lat1 = min(x$latitude),
          lng2 = max(x$longitude),
          lat2 = max(x$latitude)
        )
    }
    
    m
  })
  
  filtered <- reactive({
    x <- community %>% filter(priority_score >= input$min_priority)
    if (input$state != "All") x <- x %>% filter(state == input$state)
    if (input$ready_only) x <- x %>% filter(data_quality_flag == "Ready")
    x
  })
  coverage_filtered <- reactive({
    x <- community
    if (input$state != "All") x <- x %>% filter(state == input$state)
    if (input$ready_only) x <- x %>% filter(data_quality_flag == "Ready")
    x
  })
  
  output$n_children <- renderText(format(sum(filtered()$child_population), big.mark=","))
  output$n_not_utd <- renderText(format(sum(filtered()$children_not_utd), big.mark=","))
  output$n_high <- renderText(
    sum(
      filtered()$priority_group == "High",
      na.rm = TRUE
    ))
  output$n_mmr <- renderText(format(sum(filtered()$mmr_not_utd_n), big.mark=","))
  
  output$overview_map <- renderLeaflet({
    x <- filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    m <- leaflet(x) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~longitude, ~latitude, radius=~pmax(5, sqrt(child_population)),
                       color=~pal_priority(priority_group), fillOpacity=.8, stroke=TRUE,
                       label = ~paste0(
                         county,
                         ": ",
                         priority_group,
                         " priority (",
                         priority_score,
                         ")"
                       ),
                       popup=~paste0("<b>", county, "</b><br>Children: ", child_population,
                                     "<br>Not UTD: ", children_not_utd,
                                     "<br>Priority score: ", priority_score,
                                     "<br>Priority group: ", priority_group,
                                     "<br>Readiness: ", data_quality_flag)) %>%
      addLegend(
        position = "bottomright",
        colors = priority_colors,
        labels = priority_levels,
        opacity = 0.9,
        title = "Community priority"
      )
    m
  })
  output$coverage_map <- renderLeaflet({
    x <- coverage_filtered() %>%
      filter(!is.na(latitude), !is.na(longitude))
    
    leaflet(x) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmax(5, sqrt(child_population)),
        color = ~coverage_color(utd_coverage),
        fillColor = ~coverage_color(utd_coverage),
        fillOpacity = 0.85,
        opacity = 1,
        weight = 1,
        label = ~paste0(county, ": ", round(utd_coverage, 1), "% UTD"),
        popup = ~paste0(
          "<b>", county, "</b><br>",
          "State: ", state, "<br>",
          "UTD for age: ", round(utd_coverage, 1), "%<br>",
          "Children UTD: ", child_population - children_not_utd, " of ", child_population, "<br>",
          "Children not UTD: ", children_not_utd, "<br>",
          "Data readiness: ", data_quality_flag
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = coverage_colors,
        labels = coverage_legend_labels,
        opacity = 0.9,
        title = "UTD for age coverage"
      )
  })
  
  output$priority_table <- renderDT(
    datatable(
      filtered() %>%
        transmute(
          `Priority Rank` = priority_rank,
          `State` = state,
          `Community` = county,
          `Child Population` = child_population,
          `Children Not UTD` = children_not_utd,
          `UTD Coverage Excluding MMR (%)` = round(utd_no_mmr_coverage, 1),
          `MMR Coverage (%)` = round(mmr_coverage, 1),
          `Median Months Since Vaccination` = round(median_months_since_vax, 1),
          `Priority Score` = priority_score,
          `Priority Group` = priority_group,
          `Data Quality Flag` = data_quality_flag
        ),
      options = list(
        pageLength = 8,
        lengthMenu = c(5, 8, 10, 15, 25),
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  )
  
  selected_comm <- reactive(community %>% filter(community_id == input$community_id) %>% slice(1))
  output$community_profile <- renderUI({x<-selected_comm(); req(nrow(x)); tagList(h3(x$county), h5(x$state), tags$hr(), tags$p(strong("Priority: "),x$priority_score," (",x$priority_group,")"), tags$p(strong("Children: "),x$child_population), tags$p(strong("Not UTD: "),x$children_not_utd," (",round(x$proportion_not_utd,1),"%)"), tags$p(strong("MMR coverage: "),round(x$mmr_coverage,1),"%"), tags$p(strong("Median months since vaccination among not UTD: "),round(x$median_months_since_vax,1)), tags$p(strong("Routing readiness: "),x$data_quality_flag))})
  output$components_plot <- renderPlotly({
    x <- selected_comm()
    req(nrow(x) == 1)
    
    d <- tibble::tibble(
      component = c(
        "Community patient risk",
        "Access / remoteness",
        "Low-density access difficulty",
        "High-density transmission potential",
        "High-risk child burden"
      ),
      
      component_score = c(
        x$community_patient_risk_component,
        x$access_component,
        x$low_density_component_for_score,
        x$transmission_component,
        x$burden_component
      ),
      
      weight = c(
        0.90,
        0.01,
        0.01,
        0.04,
        0.04
      )
    ) %>%
      mutate(
        weighted_points = component_score * weight,
        
        component = factor(
          component,
          levels = rev(component)
        )
      )
    
    plot_ly(
      d,
      x = ~component_score,
      y = ~component,
      type = "bar",
      orientation = "h",
      customdata = ~weighted_points,
      
      text = ~paste0(
        "<b>", component, "</b>",
        "<br>Component score: ",
        round(component_score, 1),
        " / 100",
        "<br>Weight: ",
        round(weight * 100),
        "%",
        "<br>Contribution to priority score: ",
        round(weighted_points, 1),
        " points"
      ),
      
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(
          title = "Component score (0–100)",
          range = c(0, 100)
        ),
        
        yaxis = list(
          title = "",
          automargin = TRUE
        ),
        
        margin = list(
          l = 235,
          r = 25,
          t = 20,
          b = 55
        ),
        
        showlegend = FALSE
      )
  })
  output$vax_table <- renderDT({
    
    vaccine_order <- c(
      "DTaP",
      "IPV",
      "MMR",
      "Hib",
      "HepB",
      "PCV"
    )
    
    vaccine_table_data <- vax_needs %>%
      filter(
        community_id == input$community_id
      ) %>%
      select(
        vaccine,
        children_due
      ) %>%
      complete(
        vaccine = vaccine_order,
        fill = list(
          children_due = 0
        )
      ) %>%
      mutate(
        vaccine = factor(
          vaccine,
          levels = vaccine_order
        )
      ) %>%
      arrange(vaccine) %>%
      transmute(
        `Vaccine type` = as.character(vaccine),
        `Number of Children Due` = children_due
      )
    
    datatable(
      vaccine_table_data,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            className = "dt-left",
            targets = 0
          ),
          list(
            className = "dt-center",
            targets = 1
          )
        )
      )
    )
  })
  
  output$product_table <- renderDT({
    
    product_order <- c(
      "PedVaxHib",
      "Vaxelis",
      "Pediarix",
      "DTaP_Single",
      "IPV_Single",
      "MMR_Single",
      "Prevnar",
      "RotaTeq"
    )
    
    product_table_data <- product_needs %>%
      filter(
        community_id == input$community_id
      ) %>%
      select(
        product,
        doses_needed
      ) %>%
      complete(
        product = product_order,
        fill = list(
          doses_needed = 0
        )
      ) %>%
      mutate(
        product = factor(
          product,
          levels = product_order
        )
      ) %>%
      arrange(product) %>%
      transmute(
        `Vaccine product type` = as.character(product),
        `Number of Doses` = doses_needed
      )
    
    datatable(
      product_table_data,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            className = "dt-left",
            targets = 0
          ),
          list(
            className = "dt-center",
            targets = 1
          )
        )
      )
    )
  })
  
  observe({
    opts <- community %>% filter(state==input$plan_state, data_quality_flag=="Ready") %>% arrange(priority_rank)
    updateSelectizeInput(session,"plan_communities",choices=setNames(opts$community_id,paste0(opts$county," (",opts$priority_score,")")),selected=head(opts$community_id,8),server=TRUE)
    if(input$plan_state=="Chuuk") {updateNumericInput(session,"hub_lat",value=7.458); updateNumericInput(session,"hub_lon",value=151.841)}
    if(input$plan_state=="Pohnpei") {updateNumericInput(session,"hub_lat",value=6.982); updateNumericInput(session,"hub_lon",value=158.204)}
    if(input$plan_state=="Yap") {updateNumericInput(session,"hub_lat",value=9.499); updateNumericInput(session,"hub_lon",value=138.082)}
    if(input$plan_state=="Kosrae") {updateNumericInput(session,"hub_lat",value=5.357); updateNumericInput(session,"hub_lon",value=162.958)}
  })
  plan <- eventReactive(input$optimize, {
    stops <- community %>% filter(community_id %in% input$plan_communities)
    nearest_neighbor_route(stops,input$hub_lon,input$hub_lat,input$speed,input$service,input$setup,input$workday,input$teams)
  }, ignoreInit=FALSE)
  output$plan_n <- renderText(if(nrow(plan()$summary)) plan()$summary$communities else 0)
  output$plan_children <- renderText(if(nrow(plan()$summary)) plan()$summary$children_targeted else 0)
  output$plan_travel <- renderText(if(nrow(plan()$summary)) plan()$summary$total_travel_hours else 0)
  output$plan_distance <- renderText(if(nrow(plan()$summary)) plan()$summary$total_distance_km else 0)
  output$route_map <- renderLeaflet({it<-plan()$itinerary; m<-leaflet()%>%addProviderTiles(providers$CartoDB.Positron)%>%addMarkers(input$hub_lon,input$hub_lat,label="Departure hub"); if(nrow(it)){for(tm in unique(it$team)){z<-it[it$team==tm,]; m<-m%>%addPolylines(lng=c(input$hub_lon,z$longitude),lat=c(input$hub_lat,z$latitude),weight=3,label=paste("Team",tm)); m<-m%>%addCircleMarkers(data=z,lng=~longitude,lat=~latitude,radius=7,label=~paste0("Team ",team," stop ",stop_sequence,": ",county),popup=~paste0("<b>",county,"</b><br>Children targeted: ",children_not_utd,"<br>Day: ",day))}};m})
  output$plan_product_table <- renderDT({
    product_order <- c(
      "PedVaxHib",
      "Vaxelis",
      "Pediarix",
      "DTaP_Single",
      "IPV_Single",
      "MMR_Single",
      "Prevnar",
      "RotaTeq"
    )
    
    product_table_data <- product_needs %>%
      filter(
        community_id %in% input$plan_communities
      ) %>%
      group_by(product) %>%
      summarise(
        doses_needed = sum(doses_needed, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(
        product = product_order,
        fill = list(doses_needed = 0)
      ) %>%
      mutate(
        product = factor(product, levels = product_order)
      ) %>%
      arrange(product) %>%
      transmute(
        `Vaccine Product Type` = as.character(product),
        `Number of Doses Needed` = doses_needed
      )
    
    datatable(
      product_table_data,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-left", targets = 0),
          list(className = "dt-center", targets = 1)
        )
      )
    )
  })
  output$itinerary <- renderDT({
    it <- plan()$itinerary
    
    itinerary_table_data <- it %>%
      transmute(
        `Team` = team,
        `Day` = day,
        `Stop Sequence` = stop_sequence,
        `State` = state,
        `Community` = county,
        `Priority Score` = round(priority_score, 1),
        `Children Targeted` = children_not_utd,
        `Leg Distance (km)` = round(leg_distance_km, 1),
        `Travel Minutes` = round(travel_minutes),
        `Service Minutes` = round(service_minutes),
        `Cumulative Hours` = round(cumulative_hours, 1)
      )
    
    datatable(
      itinerary_table_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  output$download_plan <- downloadHandler(filename=function() paste0("shore_to_shot_itinerary_",Sys.Date(),".csv"),content=function(file) write_csv(plan()$itinerary,file))
  
  output$scenario_table <- renderDT({
    scenarios <- data.frame(
      Scenario = c(
        "Lean resources",
        "Base plan",
        "Expanded response",
        "High-capacity response"
      ),
      Teams = c(1, 2, 3, 4),
      Speed_kmh = c(18, 25, 30, 30),
      Workday_hours = c(8, 8, 10, 10)
    ) %>%
      mutate(
        Estimated_travel_hours = round(input$scen_distance / Speed_kmh, 1),
        Estimated_service_hours = round(input$scen_communities * .5 + input$scen_children * 4 / 60, 1),
        Estimated_team_days = ceiling((Estimated_travel_hours + Estimated_service_hours) / (Teams * Workday_hours)),
        Children_per_team_day = round(input$scen_children / Estimated_team_days / Teams)
      ) %>%
      transmute(
        `Scenario` = Scenario,
        `Teams` = Teams,
        `Speed (km/hour)` = Speed_kmh,
        `Workday Hours` = Workday_hours,
        `Estimated Travel Hours` = Estimated_travel_hours,
        `Estimated Service Hours` = Estimated_service_hours,
        `Estimated Team Days` = Estimated_team_days,
        `Children per Team Day` = Children_per_team_day
      )
    
    datatable(
      scenarios,
      rownames = FALSE,
      options = list(
        dom = 't',
        autoWidth = TRUE
      )
    )
  })
  output$quality_table <- renderDT(datatable(community %>% count(data_quality_flag,state,name="communities") %>% arrange(data_quality_flag,state),rownames=FALSE,options=list(dom='t')))
}

shinyApp(ui, server)
