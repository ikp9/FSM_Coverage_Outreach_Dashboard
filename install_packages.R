packages <- c(
  "shiny", "bslib", "bsicons", "dplyr", "readr", "readxl", "tidyr",
  "stringr", "tibble", "DT", "leaflet", "plotly", "here"
)
new <- packages[!packages %in% rownames(installed.packages())]
if(length(new)) install.packages(new)


# Run these interactively only when a new Shiny version is needed:
# install.packages("shiny")
# library(shiny)
# packageVersion("shiny")
