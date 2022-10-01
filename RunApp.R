if (! require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
  library("tidyverse")
}
if (! require("lubridate")) {
  install.packages("lubridate", dependencies = TRUE)
  library("lubridate")
}
if (! require("readxl")) {
  install.packages("readxl", dependencies = TRUE)
  library("readxl")
}
if (! require("TTR")) {
  install.packages("TTR", dependencies = TRUE)
  library("TTR")
}
if (! require("quantmod")) {
  install.packages("quantmod", dependencies = TRUE)
  library("quantmod")
}
if (! require("plotly")) {
  install.packages("plotly", dependencies = TRUE)
  library("plotly")
}
if (! require("shiny")) {
  install.packages("shiny", dependencies = TRUE)
  library("shiny")
}

runApp("EventsStockPrices")
