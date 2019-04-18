testthat::context('Testing main.R')
setwd(here::here(''))  # workspace is reset per file


testthat::test_that("print_or_T() on vector", {
	testthat::expect_true(print_or_T(logical()));

	testthat::expect_output(
		testthat::expect_false(print_or_T(letters)),
		'x'
	);
})


testthat::test_that("print_or_T() on tibble", {
	testthat::expect_true(print_or_T(tibble::tibble()));

	testthat::expect_output(
		testthat::expect_false(print_or_T(tibble::tibble(x = 1:5))),
		'x'
	);
})


testthat::test_that("run nested_element()", {
	testthat::expect_true(T);

	nested_element(list(1, list(1)))
})
