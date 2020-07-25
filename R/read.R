#' @title Set default column type to character for **readr**
#'
#' @description Sometimes most column types are character, but **readr** use
#'   [readr::col_guess()] by default, thus I write this toy function.
#'
#' @param ..., .default  passed on to [readr::cols()]
#'
#' @examples
#' example_csv <- readr::readr_example('mtcars.csv')
#'
#' # default column type is guessed
#' readr::read_csv(example_csv, T)
#'
#' # sometimes we want to set default column type to character.
#' readr::read_csv(example_csv, T, cols_char())
#'
#' # in addition, we can explicitly sepcify some colunmns' type by name
#' readr::read_csv(example_csv, T, cols_char(mpg = 'd'))
#'
#' # rather, you can also set default column type to other type (NO abbreviation)
#' readr::read_csv(example_csv, T, cols_char(.default = readr::col_double()))
#'
#' @export
cols_char <- function(..., .default = readr::col_character()) {
     readr::cols(..., .default = .default)
}


#' @title remove problematic rows after you [readr::read_*()][readr::read_delim()] a file
#'
#' @param data tibble
#'
#' @return tibble
#'
#' @examples
#' problematic_df <- readr::read_tsv('a\tb\tc\n1\n2\t3\n1\t2\t3')
#'
#' problematic_df
#'
#' rm_problematic_row(problematic_df)
#'
#' @export
rm_problematic_row <- function(data) {
	problematic_row <- unique(readr::problems(data)$row);
	if (length(problematic_row) > 0L) data %<>% dplyr::slice(-problematic_row);
	data %T>% {attr(., c('problems')) <- NULL}
}







