#' @title memoised lapply
#' @description
#' \code{memoised lapply} is the memoised version of lapply. Any other arguments remains the same as the original \code{lapply}.
#' @seealso [lapply()] for the arguments.
#' @details
#' Technically, it uses a memoised function; therefore,
#' as long as the contents of the input object remains the same, the return is remembered for 15 minutes and then automatically
#' deleted from the user's temp file.
#' @returns a list.
#' @family netABM_misc
#' @author Keiichi Satoh
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @export

# memoised mms
lapply_mms <- memoise(lapply,
                      cache = cache_mem(max_age = 900))
