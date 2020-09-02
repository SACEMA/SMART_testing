# plot total tests done each day
# Jeremy Bingham
# July 2020
library(lubridate)
dd <- read_csv("C:/Users/jem/Documents/Covid19za/data/covid19za_timeline_testing.csv")

first_day_tests <- as.numeric(dd[1,"cumulative_tests"])

dd_test <- dd %>%
  mutate(daily_tests = c(first_day_tests, diff(cumulative_tests)),
         date = ymd(YYYYMMDD)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), fill = list(0)) %>%
  select(date,daily_tests)
