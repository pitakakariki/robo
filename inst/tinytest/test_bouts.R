
expect_silent(
  test <- cows |> dplyr::group_by(Animal) |> classify_sl("Y", -0.65) |> group_daily() |> bouts()
)
