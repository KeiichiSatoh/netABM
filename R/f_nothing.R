#' @title Agent does nothing
#' @description
#' \code{f_nothing} returns \code{NULL} regardless of any \code{D}.
#' @returns  NULL
#' @details
#' \code{f_nothing} is intended to express the agent's non-action.
#' While this may seem like a trivial function, it is functionally necessary
#' for \code{runABM} because it mandates setting an action
#' even if the user does not intend to specify any action.
#' The primary purpose of this function is to assign a non-action state to
#' certain agents while allowing others to act.
#' @family f_tools
#' @author Keiichi Satoh
#' @export
#' @examples
#' D <- setABM(agent_n = 3, agent_f = f_nothing)
#' D$agent$ID1$.f(D = D)

f_nothing <- function(){return(NULL)}
