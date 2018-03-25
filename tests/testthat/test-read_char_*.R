context('Testing read_char_*()')

test_that("basic usage", {
    expect_equal(read_char_csv('a,b\n1.0,2.0'), tibble::tibble(a = '1.0', b = '2.0'));
	expect_equal(read_char_csv2('a;b\n1,0;2,0'), tibble::tibble(a = '1,0', b = '2,0'));
	expect_equal(read_char_tsv('a\tb\n1.0\t2.0'), tibble::tibble(a = '1.0', b = '2.0'));
});


test_that("read_char_*() discard rows with problems", {
    expect_equal(read_char_tsv('a\tb\tc\n1\n2\t3\n1\t2\t3'), tibble::tibble(a = '1', b = '2', c = '3'));
});


