# plot total tests done each day
# Jeremy Bingham
# July 2020
library(lubridate)

testing_data_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv"
dd <- read_csv(url(testing_data_url))

first_day_tests <- as.numeric(dd[1,"cumulative_tests"])

dd_test <- dd %>%
  mutate(daily_tests = c(first_day_tests, diff(cumulative_tests)),
         date = ymd(YYYYMMDD)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), fill = list(0)) %>%
  select(date,daily_tests)

dd_test$day = 1:nrow(dd_test)