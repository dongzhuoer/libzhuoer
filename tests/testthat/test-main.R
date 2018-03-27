testthat::context('Testing main.R')


testthat::test_that("print_or_T() on vector", {
	testthat::expect_true(print_or_T(logical()));

	testthat::expect_false(print_or_T(letters));
	testthat::expect_output(print_or_T(letters), 'x');
});


testthat::test_that("print_or_T() on tibble", {
	testthat::expect_true(print_or_T(tibble::tibble()));

	testthat::expect_false(print_or_T(tibble::tibble(x = 1:5)));
	testthat::expect_output(print_or_T(tibble::tibble(x = 1:5)), 'x');
});
