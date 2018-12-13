testthat::context('Testing read-char.R')
if (basename(getwd()) == 'testthat') setwd('../..')  # workspace is reset per file


testthat::test_that('basic usage', {
    testthat::expect_equal(read_char_csv('a,b\n1.0,2.0'), tibble::tibble(a = '1.0', b = '2.0'));
	testthat::expect_equal(read_char_csv2('a;b\n1,0;2,0'), tibble::tibble(a = '1,0', b = '2,0'));
	testthat::expect_equal(read_char_tsv('a\tb\n1.0\t2.0'), tibble::tibble(a = '1.0', b = '2.0'));
});


testthat::test_that('read_char_*() empty file', {
	testthat::expect_identical(read_char_tsv('a\tb'), tibble::tibble())
	testthat::expect_identical(
		read_char_tsv('a\tb\n'),
		readr::read_tsv('a\tb\n',T, readr::cols(.default = readr::col_character()))
	)
});


testthat::test_that('read_char_*() discard rows with problems', {
    testthat::expect_equal(read_char_tsv('a\tb\tc\n1\n2\t3\n1\t2\t3'), tibble::tibble(a = '1', b = '2', c = '3'));
});


testthat::test_that('rm_problematic_row()', {
	testthat::expect_equal(
		suppressWarnings(read_char_tsv('a\tb\tc\n1\n2\t3\n1\t2\t3')) %>%
			rm_problematic_row(),
		tibble::tibble(a = '1', b = '2', c = '3')
	)
})
