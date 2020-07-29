# tests-conducted: linear approximation to SA

# import SA testing data
dd_tests <- read_csv()
# fit linear prediction
lm(tests ~ day_since_first_case)
# initial number of cases? 3 cases, on day of first detection.

# tests_conducted <- **formula of lm manually written out, or lm.predict(days)
tests_conducted = rep(1000, length(timesteps))


max_daily_test_supply = rep(20000, length(timesteps))

testing_demand_feedback_strength = 0
testing_demand_lag = 4 #number of days