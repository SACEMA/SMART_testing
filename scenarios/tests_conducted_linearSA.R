# tests-conducted: linear approximation to SA

# import SA testing data
source('./scenarios/sa_testing.R')

# fit linear prediction
linear_fit <- lm(daily_tests ~ 0 + day, data = dd_test)
# initial number of cases? 3 cases, on day of first detection.

tests_conducted = (1:length(timesteps)) * linear_fit$coefficients[1]
# tests_conducted <- **formula of lm manually written out, or lm.predict(days)

max_daily_test_supply = rep(2e5, length(timesteps))

testing_demand_feedback_strength = 0
testing_demand_lag = 4 #number of days