library(tidyverse)
library(dplyr)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
this_folder <- "."
location <-"confirmedDeathSA.csv"
linkRaw <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, file.path(this_folder, location))


ReportedDeaths <- as.data.frame(data) %>%
mutate(dailyConfrimedDeath= c(1, diff(total)))

ReportedDeaths$date <- lubridate::dmy(ReportedDeaths$date)


ggplot(ReportedDeaths, aes(date, dailyConfrimedDeath)) +
  geom_point()






