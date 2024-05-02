#' @title Agent does nothing
#' @description
#' \code{actAgent_nothing} returns NULL regardless of any D.
#' @param D The D object. Note that this argument is internally supplied within
#' runABM and should not be modified by the user.
#' @param self self of the R6 class. Note that this argument is internally supplied within
#' runABM and should not be modified by the user.
#' @returns  NULL
#' @details
#' \code{actAgent_nothing} is intended to express the agent's non-action.
#' While this may seem like a trivial function, it is functionally necessary for \code{runABM}
#' because it mandates setting an action even if the user does not intend to specify any action.
#' The primary purpose of this function is to assign a non-action state to certain agents while allowing others to act.
#' @family actAgent
#' @author Keiichi Satoh
#' @export
#' @examples
#' # This function is supposed to be used in tandem with setABM family function.
#'
#' setABM_network(n = 5,
#'                .act = actAgent_nothing)

f_nothing <- function(D){NULL}

