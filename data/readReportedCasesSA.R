setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
this_folder <- "."
location <- "reportedCasesSA.csv"
linkRaw <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/nicd_daily_national_report.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, file.path(this_folder, location))

