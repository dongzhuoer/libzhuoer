

#' @title print a variable if not empty
#'
#' @description print a variable if not empty, otherwise return `T`
#'
#' @details extremely useful with `testthat::expect_true()`
#' @param x
#'
#' @return logical scalar
#' @export
#'
#' @examples
#' print_or_T(logical())
#' print_or_T(letters)
#'
#' print_or_T(tibble::tibble())
#' print_or_T(tibble::tibble(x = 1:5))
print_or_T <- function(x) {
	if (identical(length(x), 0L) || identical(nrow(x), 0L))
		T
	else {
		print(x);
		F
	}
}



#' @title get nested elements
#'
#' @description get an element (usually a list) itself and elments of it, elements of elments of it, ...
#'
#' @details Although `get_element` restrict the type of elemnt (e.g. the default value force element must be a list), the outermost elment isn't affected. The elements of `nested_element(list(1))` are all granted to be lists, but the first (and only) elment `nested_element(1)` is numeric, NOT list.
#'
#' @param element usually list. the outermost element.
#' @param get_element function. recieve an element and return a list of element.
#'
#' @return list of elements.
#'
#' @section legacy: when I support `max_level`, `nested_list(l, fun)` should return all levels as before
#'
#' @examples
#' nested_element(list(1, list(1)))
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













