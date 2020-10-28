#import SA data
library(tidyverse)
library(lubridate)
library(zoo)

case_data_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"
sa_cases <- read_csv(url(case_data_url))

death_data_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv"
sa_deaths <- read_csv(url(death_data_url))

testing_data_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv"
sa_tests <- read_csv(url(testing_data_url))
