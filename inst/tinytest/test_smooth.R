
expect_silent(
  cows_sl <- classify_sl(cows, "Y", -0.65)
)

expect_silent(
  smooth_behaviour(cows_sl)
)

expect_silent(
  smooth_behaviour(cows_sl, min_run=1)
)

## tests for smooth_bouts are in test_bouts.R
