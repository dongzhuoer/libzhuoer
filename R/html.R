#' @title open multiple urls sequentially
#'
#' @details The sequence maybe unstable when file number become large
#'
#' @details It depends on external command (`system('sleep 0.2')`).
#'
#' @param urls character.
#' @param t numeric scalar. intervel between opening adjacent webpages, unit:
#'   second. The longer, the more likely your webpage would be opened in the
#'   same order as that of `urls`
#'
#' @return `NULL`
#'
#' @examples
#' \dontrun{
#'     browse_url(c('https://www.bing.com/', 'https://dongzhuoer.com/'), 0.4)
#' }
#'
#' @export
browse_url <- function(urls, t = 0.2) {
	plyr::l_ply(urls, . %>% {browseURL(.); system(paste('sleep', t))});

	return(NULL);
}