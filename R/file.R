#' @title check integrity of gzip files in a directory
#'
#' @param path character. path to the folder(s) containing gzip files
#'
#' @return character. path of broken gzip files
#' @export
#'
#' @examples NULL
check_gzip <- function(path) {
	files <- dir(path, '\\.gz$', full.names = T);
	okay <- parallel::mclapply(files, gzip_integrity) %>% sapply(as.logical);
	files[!okay];
}

#' @title test gzip compressed file integrity
gzip_integrity <- function(path) {
	system2('gzip', c('-t', path), T) %>% attr("status") %>% identical(NULL);
}


#' create a new directory and make sure it's empty
#' export
initiate_dir <- function(path) {
    if (dir.exists(path))
    	unlink(path, recursive = T)
	else
    	dir.create(path, recursive = T);
}