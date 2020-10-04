library(tidyverse)
library(dplyr)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
this_folder <- "."
location <- "reportedCasesSA.csv"
linkRaw <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"
data <- readr::read_csv(linkRaw)
readr::write_csv(data, file.path(this_folder, location))


caseReport <- as.data.frame(data) %>%
mutate(dailyCases= total-lag(total, default=0))

caseReport$date <- lubridate::dmy(caseReport$date)


ggplot(caseReport, aes(date, dailyCases)) +
  geom_point()


# mutate(totalCases = male_total_cases + unknown_sex_total_cases + female_total_cases) %>% 
# mutate(DailyPos = (totalCases - lag(totalCases, default = 0))) %>%
# mutate(DailyTestsCat_1 = (testing_passive - lag(testing_passive, default = 0))+
#       (testing_community_screening - lag(testing_community_screening, default = 0)))%>%
# mutate(DailyTestsCat_2 = (testing_public - lag(testing_public, default = 0)) +
#       (testing_private - lag(testing_private, default = 0))) %>%
# mutate(publicTesting = (testing_public - lag(testing_public
#                                                , default = 0))) %>%
# mutate(privateTesting = (testing_private - lag(testing_private
#                                                  ,default = 0)))%>%
# mutate(communityTesting = (testing_community_screening - lag(testing_community_screening
#                                                                , default = 0)))%>%
# mutate(passiveTesting = (testing_passive - lag(testing_passive, default = 0))) %>%
# mutate(positivityRate= DailyPos/DailyTestsCat_1)%>%
# mutate(Date = as.Date(date, format = "%d-%m-%Y")) 
# 
# NICD_caseReport <- NICD_caseReport[-1,]




