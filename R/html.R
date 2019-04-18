#' @title open multiple urls sequentially
#'
#' @details the sequence maybe unstable when file number become large
#'
#' @details It depends on external command (`system('sleep 0.2')`).
#'
#' @param urls character.
#' @param t numeric scalar. intervel between opening adjacent webpages, unit:
#'   second. The longer, the more likely your webpage would be opened in the
#'   same order as that of `urls`
#'
#' @return `NULL`
#' @export
#'
#' @section demo: `libzhuoer::browse_url(c('https://www.bing.com/',
#'   'https://zhuoer.netlify.com/'), 0.4)`
browse_url <- function(urls, t = 0.2) {
	plyr::l_ply(urls, . %>% {browseURL(.); system(paste('sleep', t))});

	return(NULL);
}