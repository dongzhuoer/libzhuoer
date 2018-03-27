#' @name read_char_delim
#'
#' @title read a delimited file, assuming that every column is character
#'
#' @details the first line must specify column names. It is used to determine
#'   columns number as it is unlikely to contain `delim` (cell value may contain
#'   it in quote).
#'
#' @param file. see [readr::read_file()]. for literal input, character will be
#'   collapse into a single string, since using large character for `file`
#'   parameter may casue 'C stack usage 7969182 is too close to the limit' for
#'   readr::read_*()
#' @param delim. string. passed on to `fun`
#' @param fun. function. usually read_* from readr
#'
#' @return [tibble::tibble()]
#'
#' @section  to do: unittest
#' @examples
#' read_char_csv("a,b\n1.0,2.0")
#' read_char_tsv("a\tb\n1.0\t2.0")
NULL

#' @rdname read_char_delim
#'
#' @export
read_char_csv <- function(file) {
	readr_char_impl(file, ',', readr::read_csv);
}

#' @rdname read_char_delim
#'
#' @export
read_char_csv2 <- function(file) {
	readr_char_impl(file, ';', readr::read_csv2);
}

#' @rdname read_char_delim
#'
#' @export
read_char_tsv <- function(file) {
	readr_char_impl(file, '\t', readr::read_tsv);
}

readr_char_impl <- function(file, delim, fun) {
	if (length(file) > 1L) file %<>% paste0(collapse = '\n');
    content <- readr::read_file(file);
    header <- stringr::str_extract(content, '^[^\n]+');
	n_column <- stringr::str_count(header, stringr::fixed(delim)) + 1;
	col_types <- stringr::str_dup('c', n_column);

	data <- suppressWarnings(fun(content, T, col_types));
	problematic_row <- unique(readr::problems(data)$row);
	if (length(problematic_row) == 0L) data else dplyr::slice(data, -problematic_row);
}