#' @title check integrity of `.gz` files in a directory
#'
#' @param path character. path to the folder(s) containing `.gz` files.
#'
#' @return character. path of broken `.gz` files.
#'
#' @examples
#' dir(system.file('extdata', package = 'libzhuoer'), full = T)
#'
#' check_gzip(system.file('extdata', package = 'libzhuoer'))
#'
#' @export
check_gzip <- function(path) {
	files <- dir(path, '\\.gz$', full.names = T);
	okay <- parallel::mclapply(files, gzip_integrity) %>% sapply(as.logical);
	files[!okay];
}

#' @title check `.gz` file integrity
#'
#' @param path character. path to the `.gz` file to be checked.
#'
#' @return logical scalar. whether the `.gz` file is integrated.
#'
#' @examples
#' gzip_integrity(system.file('extdata/good.gz', package = 'libzhuoer'))
#' gzip_integrity(system.file('extdata/bad.gz', package = 'libzhuoer'))
#'
#' @export
# path = 'inst/extdata/bad.gz'
gzip_integrity <- function(path) {
	suppressWarnings( system2('gzip', c('-t', path), T, F) ) %>%
		attr("status") %>% identical(NULL)
}
