packages <- c("shiny","bslib","bsicons","dplyr","readr","tidyr","stringr","DT","leaflet","plotly")
new <- packages[!packages %in% rownames(installed.packages())]
if(length(new)) install.packages(new)


#Run this only new version shiny is needed:
install.packages("shiny")
library(shiny)
packageVersion("shiny")