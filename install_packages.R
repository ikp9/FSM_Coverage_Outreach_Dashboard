packages <- c("shiny","bslib","bsicons","dplyr","readr","tidyr","stringr","DT","leaflet","plotly")
new <- packages[!packages %in% rownames(installed.packages())]
if(length(new)) install.packages(new)
