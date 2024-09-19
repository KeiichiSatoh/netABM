#' @title Execute an Expression Multiple Times
#' @description
#' This function takes an R expression and executes it a specified number of times.
#'
#' @param expr An R expression to be evaluated. It should be a valid R code without quotes.
#' @param times An integer indicating how many times to execute the expression. Default is 10.
#'
#' @return A list containing the results of each execution of the expression.
#' @examples
#' multi(sample(x = 1:10, size = 2))
#'
#' multi({
#'   P <- sample(x = 1:10, size = 2)
#'   mean(P)
#' }, times = 5)
#'
#' @export
multi <- function(expr, times = 10) {
  expr <- substitute(expr)
  FUN <- function(X = X) {}
  body(FUN) <- as.call(expr)
  lapply(X = 1:times, FUN = FUN)
}


