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
	if (stringr::str_count(file, '\n') == 0L) return(tibble::tibble());

	col_types = readr::cols(.default = readr::col_character())
    file %>% readr::read_file() %>%
    	{suppressWarnings(fun(., T, col_types))} %>% rm_problematic_row();
}


#' @title remove problematic row after you read_*() a file
#'
#' @param data tibble
#'
#' @return tibble
#'
#' @examples
#'
#' @export
rm_problematic_row <- function(data) {
	problematic_row <- unique(readr::problems(data)$row);
	if (length(problematic_row) > 0L) data %<>% dplyr::slice(-problematic_row);
	data %T>% {attr(., c('problems')) <- NULL}
}







