#' @title Selecting All Agents in netABM objects
#' @description
#' \code{selectAgent_all} returns all agents in the netABM objects.
#' @param D a \code{netABM} object.
#'
#' @details
#' A typicall usage of \code{selectAgent_all} is setting this function as an argument of \code{runABM_network},
#' so that all agents acts simultaneously per time. Technically, it uses a memoised function; therefore,
#' as long as the original D object remains the same, the return is remembered for 15 minutes and then automatically
#' deleted from the user's temp file.
#' @returns  a vector of the IDs of agents in the \code{netABM} class object D.
#' @family selectAgent
#' @author Keiichi Satoh
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @export
#' @examples
#' D <- setABM_network(n = 5, .act = list(NULL))
#' selectAgent_all(D = D)

selectAgent_all <- memoise(
  function(D){names(D$agent)},
  cache = cache_mem(max_age = 900))
