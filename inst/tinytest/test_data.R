
expect_true(dir.exists(hobo_example("cows")))
expect_true(dir.exists(hobo_example("goats")))

cows_path <- hobo_example("cows")
cow01_file <- file.path(cows_path, "cow01.csv")

expect_silent(cow01 <- read_hobo(cow01_file))
expect_true(is(cow01, "data.frame"))
expect_identical(nrow(cow01), 29625L)

expect_silent(cows_raw <- read_hobo_dir(cows_path))
expect_true(is(cows_raw, "data.frame"))
expect_identical(nrow(cows_raw), 266643L)

