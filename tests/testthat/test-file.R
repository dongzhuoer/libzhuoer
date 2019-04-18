testthat::context('Testing main.R')
setwd(here::here(''))  # workspace is reset per file


testthat::test_that("gzip_integrity()", {
	testthat::expect_identical(
		gzip_integrity('inst/extdata/good.gz'),
		TRUE
	)
	testthat::expect_identical(
		suppressWarnings(gzip_integrity('inst/extdata/bad.gz')),
		FALSE
	)
})


testthat::test_that("check_gzip()", {
	testthat::expect_identical(
		check_gzip('inst/extdata'),
		"inst/extdata/bad.gz"
	);
})

