# constant tests


tests_conducted = rep(10000, length(timesteps))
# tests_conducted <- **formula of lm manually written out, or lm.predict(days)

max_daily_test_supply = rep(2e5, length(timesteps))

testing_demand_feedback_strength = 0
testing_demand_lag = 4 #number of days
