#' @name read_char_delim
#'
#' @title read a delimited file, assuming that every column is character
#'
#' @details the first line must specify column names. It is used to determine
#'   columns number as it is unlikely to contain `delim` (cell value may contain
#'   it in quote)
#'
#' @param file. see [readr::read_file()]. for literal input, string is preferred to character, the latter may casue 'C stack usage 7969182 is too close to the limit' for read_tsv
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
read_char_tsv <- function(file) {
	readr_char_impl(file, '\t', readr::read_tsv);
}



readr_char_impl <- function(file, delim, fun) {
    content <- readr::read_file(file);
    header <- stringr::str_extract(content, '^[^\n]*');
	n_column <- stringr::str_count(header, delim) + 1;
	col_types <- stringr::str_dup('c', n_column);

	fun(content, T, col_types);
}









#' Title
#'
#' @return
#' @export
#'
#' @examples
update_packages <- function() {

}











#' @title get nested elements
#'
#' @description get an element (usually a list) itself and elments of it, elements of elments of it, ...
#'
#' @details though `get_element` restrict the type of elemnt (e.g. the default value force element must be a list), the outermost elment isn't affected. The elements of `nested_element(list(1))` are all granted to be lists, but the first (and only) elment `nested_element(1)` is numeric, NOT list.
#'
#' @param element usually list. the outermost element.
#' @param get_element function. recieve an element and return a list of element.
#'
#' @return list of elements.
#'
#' @section legacy: when I support `max_level`, `nested_list(l, fun)` should return all levels as before
#'
#' @examples
#'
#' @export
nested_element <- function(element, get_element = function(e){e[sapply(e, is.list)]}) {
	first_level <- list(element)
	levels <- list(first_level);

	while (T) {
		next_level <- unlist(lapply(levels[[1]], get_element), F)

		if (length(next_level) == 0L) {
			break
		} else
			levels %<>% append(list(next_level), .)
	}

	unlist(levels, F)
}













