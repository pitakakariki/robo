
expect_silent(
  goats_sl <- goats |> dplyr::group_by(Animal) |> classify_sl("X", -0.60, min_run=3)
)

expect_silent(goats_daily <- group_daily(goats_sl))
  
expect_silent(goats_daily_summary <- summarise_time(goats_daily))

