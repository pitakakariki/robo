
expect_silent(
  cows_sl <- classify_sl(cows, "Y", -0.65)
)

expect_silent(
  smooth_behaviour(cows_sl)  
)
