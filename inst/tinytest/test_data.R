
expect_true(dir.exists(hobo_example("cows")))
expect_true(dir.exists(hobo_example("goats")))

expect_silent(cows_path <- hobo_example("cows"))
expect_silent(cow01_file <- file.path(cows_path, "cow01.csv"))
expect_error(horse_path <- hobo_example("horses"))

expect_silent(cow01 <- read_hobo(cow01_file))
expect_true(is(cow01, "data.frame"))
expect_identical(nrow(cow01), 29625L)

expect_silent(cows_raw <- read_hobo_dir(cows_path))
expect_true(is(cows_raw, "data.frame"))
expect_identical(nrow(cows_raw), 266643L)

expect_error(horse_raw <- read_hobo_dir(tempdir()))

expect_error(hobo_bad1 <- read_hobo("files/fail_plot_title.csv"))
expect_warning(hobo_bad2 <- read_hobo("files/fail_hash_column.csv"))
expect_error(hobo_bad3 <- read_hobo("files/fail_datetime_column.csv"))
expect_error(hobo_bad4 <- read_hobo("files/fail_timezone.csv"))
expect_warning(hobo_bad5 <- read_hobo("files/fail_extra_column.csv"))
