# Run from the project root after installing the dashboard's usual packages.
source(file.path("R", "seasonal_flu.R"))
population <- expand.grid(age_group = FSM_FLU_AGES, geography = FSM_FLU_GEOGRAPHIES,
                          stringsAsFactors = FALSE)
population$population <- 1000
patients <- data.frame(state = c("CHUUK", "YAP", "CHUUK", "POHNPEI", NA, "CHUUK"),
                       agegroup = c(1, 2, 3, 4, 5, 1),
                       flu_date1 = as.Date(c("2026-07-01", "2026-09-30", "2026-10-01", "2026-06-30", "2026-07-02", "2026-07-03")),
                       flu_date2 = as.Date(c("2026-08-05", "2026-09-30", NA, NA, NA, "2026-07-03")))
result <- aggregate_fsm_flu(patients, population, as.Date("2026-07-01"), as.Date("2026-09-30"))
cards <- fsm_flu_cards(result, c("FSM", "Chuuk"), FSM_FLU_AGES)
stopifnot(cards$population == 5000, cards$doses == 5)
states <- fsm_flu_cards(result, c("Chuuk", "Yap"), FSM_FLU_AGES)
stopifnot(states$population == 10000, states$doses == 4)
national <- fsm_flu_selected_curves(result, "FSM", FSM_FLU_AGES)
total <- national[national$age_group == "Total", ]
stopifnot(identical(total$people, c(3, 3, 4)), all(total$population == 5000))
stopifnot(all(abs(total$coverage - c(.06, .06, .08)) < 1e-12))
stopifnot(result$vaccinated_without_state == 1)
partial <- aggregate_fsm_flu(patients, population, as.Date("2026-07-01"), as.Date("2026-10-15"))
stopifnot(partial$through == as.Date("2026-09-30"),
          fsm_flu_cards(partial, "FSM", FSM_FLU_AGES)$doses == 5)
stopifnot(all(fsm_flu_date(c("2026-07-01", "07/01/2026", "46204"))[1:2] == as.Date("2026-07-01")))
empty <- fsm_flu_cards(result, character(), FSM_FLU_AGES)
stopifnot(empty$population == 0, empty$doses == 0)
missing_population <- try(aggregate_fsm_flu(patients, population[-1, ], as.Date("2026-07-01"), as.Date("2026-09-30")), silent = TRUE)
stopifnot(inherits(missing_population, "try-error"))
cat("PASS: person/dose distinction, repeat-date deduplication, season boundaries, partial-month cutoff and geography union.\n")
