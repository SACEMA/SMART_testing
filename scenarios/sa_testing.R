# plot total tests done each day
# Jeremy Bingham
# July 2020
library(lubridate)
library(zoo)

testing_data_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv"
dd <- read_csv(url(testing_data_url))

first_day_tests <- as.numeric(dd[1,"cumulative_tests"])


## raw data 
dd_test_raw <- dd %>%
  mutate(daily_tests_raw = c(first_day_tests, diff(cumulative_tests)),
         date = ymd(YYYYMMDD)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), fill = list(0)) %>%
  select(date,daily_tests_raw)

dd_test_raw$day = 1:nrow(dd_test_raw)

dd_test_raw$daily_tests_raw <- replace_na(dd_test_raw$daily_tests_raw, 1)




###rolling average 
## see https://www.storybench.org/how-to-calculate-a-rolling-average-in-r/
#k=7 - calculates a simple moving average (over 7 days)

dd_test <- dd_test_raw %>%
  mutate(daily_tests = (rollmean(daily_tests_raw, k=7, fill=1)),  ###fill=1, means NA=1
          date= date) %>%
  select(date,daily_tests)

dd_test$day = 1:nrow(dd_test) ###need to sort out first and last 3



## visualization 

raw_testingdata_plot <- dd_test_raw %>%
  ggplot(aes(x = date, y = daily_tests_raw)) +
  geom_line() +
  theme(legend.position = "none", legend.title = element_blank()) +
  labs(x = "Date", y = "Daily tests raw") 
raw_testingdata_plot
  


ave_testingdata_plot <- dd_test %>%
  ggplot(aes(x = date, y = daily_tests)) +
  geom_line() +
  theme(legend.position = "none", legend.title = element_blank()) +
  labs(x = "Date", y = "Daily tests rolling average")
ave_testingdata_plot

