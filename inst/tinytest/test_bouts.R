
expect_silent(
  cows_daily <- cows |> dplyr::group_by(Animal) |> classify_sl("Y", -0.65) |> group_daily()
)

expect_silent(
  cows_bouts <- bouts(cows_daily)
)

expect_error(
  cows_bouts_badbvr <- bouts(cows_daily, behaviour="a")
)

expect_error(
  cows_bouts_badtime <- bouts(cows_daily, time="a")
)

expect_silent(
  summarise_bouts(cows_bouts)
)

#
# Custom behaviour name to trigger certain errors
#

expect_silent(
  cows_daily_bvr <- cows |> dplyr::group_by(Animal) |> classify_sl("Y", -0.65, name="Bvr") |> group_daily()
)

expect_silent(
  cows_bouts_bvr <- bouts(cows_daily_bvr, behaviour="Bvr")
)

expect_error(
  cows_bouts_nobvr <- bouts(cows_daily_bvr)
)

expect_error(
  cows_bouts_nottime <- bouts(cows_daily_bvr, behaviour="Bvr", time="Bvr")
)

#
# Smoothing
#

expect_silent(smooth_bouts(cows_bouts))

#
# no examples of high-frequency data for autogap so "volume" test directly
#
expect_equal(robo:::autogap(1:20), 30)
