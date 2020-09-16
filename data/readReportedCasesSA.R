location <- "data/reportedCasesSA.csv"
linkRaw <- "https://github.com/dsfsi/covid19za/blob/master/data/nicd_daily_national_report.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, file.path(this_folder, location))
