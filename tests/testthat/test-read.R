testthat::context('Testing read.R')
setwd(here::here(''))  # workspace is reset per file



testthat::test_that('cols_char()', {
	example_csv <- readr::readr_example('mtcars.csv')

	testthat::expect_identical(
		readr::read_csv(example_csv, T, cols_char()) %>%
			sapply(class) %T>% {names(.) <- NULL},
		rep('character', 11)
	)
	testthat::expect_identical(
		readr::read_csv(example_csv, T, cols_char(mpg = 'd')) %>%
			sapply(class) %T>% {names(.) <- NULL},
		c('numeric', rep('character', 10))
	)
	testthat::expect_identical(
		readr::read_csv(example_csv, T, cols_char(.default = readr::col_double())) %>%
			sapply(class) %T>% {names(.) <- NULL},
		rep('numeric', 11)
	)
});




testthat::test_that('rm_problematic_row()', {
	testthat::expect_true(dplyr::all_equal(
        # hope dplyr::all_equal() not get superseded
        # former has `$spec` and its `$class` include spec_tbl_df
		suppressWarnings(readr::read_tsv('a\tb\tc\n1\n2\t3\n1\t2\t3')) %>%
			rm_problematic_row(),
		tibble::tibble(a = 1, b = 2, c = 3)
	))
})
