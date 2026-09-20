
##
## classify_sl
##

## note that some test have been moved to other files
## to avoid redundancy in testing setup code

expect_silent(
  goats_sl <- goats |> dplyr::group_by(Animal) |> classify_sl("X", -0.60)
)

expect_false(is.null(goats_sl$Behaviour))

expect_error(
  goats_sl_bad <- goats |> dplyr::group_by(Animal) |> classify_sl(list("X", "Y", "Z"), -0.60)
)

expect_silent({
  test1 <- with(goats_sl, ifelse(X < -0.60, "S", "L")) |> factor(c("S", "L"))
  test2 <- xtabs(~ test1 + goats_sl$Behaviour)
})

expect_equal(sum(test2), sum(diag(test2)))
expect_equal(sum(diag(test2)), nrow(goats))

expect_silent(
  goats_sl2 <- goats |> dplyr::group_by(Animal) |> classify_sl("X", -0.60, min_run=2)
)

expect_error(
 cows_sl_bad <- cows |> dplyr::group_by(Animal) |> classify_sl("X", -0.60)
)

expect_error(
 cows_sl_bad2 <- cows |> dplyr::group_by(Animal) |> classify_sl("Y", -0.65, comparison="a")
)

##
## classify_slr
##

expect_silent(
  goats_slr <- goats |> dplyr::group_by(Animal) |> classify_slr("X", -0.60, "Z", -0.65)
)

expect_silent(
  goats_slr2 <- goats |> dplyr::group_by(Animal) |> classify_slr("X", -0.60, "Z", -0.65, min_run=2)
)

expect_error(
 goats_slr_bad <- goats |> dplyr::group_by(Animal) |> classify_slr("X", -0.60, "Z", -0.65, comparison1="a")
)

