#' @title Extract and Process Log Data from ABM_G Objects
#' @description
#' This function extracts log data from objects of class \code{ABM_G} and
#' optionally applies a processing function or combines the results.
#'
#' @param expr An expression that specifies the log data to extract.
#' Must include a `$` operator.
#' @param G An `ABM_G` class object. If \code{NULL}, the value is automatically obtained
#' from \code{expr} within the \code{parent.frame()} environment.
#' The default is \code{NULL}.
#' @param log A numeric vector specifying which logs to extract.
#' If \code{NULL}, all logs are extracted.
#' @param FUN An optional function to apply to each extracted log entry.
#' @param ... Additional arguments passed to \code{FUN}.
#' @param combine_FUN An optional function to combine the results of \code{FUN}.
#' @return A list of extracted (and optionally processed or combined) log entries.
#' @importFrom stringr str_locate str_sub
#' @export
#' @examples
#' # Prepare a simple ABM_G object
#' increase_x <- function() { G$x <- G$x + 1 }
#' G <- setABM(stage = list(x = 1), global_FUN = increase_x)
#' G <- runABM(G, schedule = "increase_x", times = 3, save_log = TRUE)
#'
#' # Get the value of x over time
#' get_log(G$x)
#'
#' # Get the result as a vector
#' get_log(G$x, combine_FUN = unlist)
#'
#' # Retrieve each log, divide by four, and return them as a vector
#' get_log(G$x, FUN = function(x) { x / 4 }, combine_FUN = unlist)#' @export
#'
#' # prepare a simple ABM_G object
#' increase_x <- function(){G$x <- G$x + 1}
#' G <- setABM(stage = list(x = 1), global_FUN = increase_x)
#' G <- runABM(G, schedule = "increase_x", times = 3, save_log = TRUE)
#' # get the value of x over the time
#' get_log(G$x)
#' # get the result as vector
#' get_log(G$x, combine_FUN = unlist)
#' # get the each log and devide by four and return them as vector
#' get_log(G$x, FUN = function(x){x/4}, combine_FUN = unlist)

get_log <- function(expr, G = NULL, log = NULL, FUN = NULL, ...,
                    combine_FUN = NULL, combine_FUN_args = NULL) {
  # 投入部分を確認する
  expr <- substitute(expr)
  expr_str <- paste(deparse(expr), collapse = "")
  loc <- stringr::str_locate(expr_str, pattern = "\\$")

  if (is.na(loc[1, 1])) {
    stop("The input expression must contain a '$'.")
  }

  expr_base <- stringr::str_sub(expr_str, end = loc[1, 1] - 1)
  expr_rest <- stringr::str_sub(expr_str, start = loc[1, 1] + 1)

  # base_obs
  if(!is.null(G)){
    base_obs <- G
  }else{
    base_obs <- eval(parse(text = expr_base), parent.frame())
  }

  # ベースをチェック
  if (!inherits(base_obs, "ABM_G")) {
    warning("This function is designed for objects of class 'ABM_G'. The output may not be as expected.")
  }

  # どのlogを取得するか
  if (is.null(log)) {
    log <- seq_along(base_obs$log)
  }

  if (any(log < 1 | log > length(base_obs$log))) {
    stop("Some indices in 'log' are out of range.")
  }

  # logを取得する
  result <- lapply(log, function(i) {
    eval(parse(text = paste0(substitute(base_obs), "$log", "[[", i, "]]$", expr_rest)))
  })

  # FUN
  if (!is.null(FUN)) {
    if (!is.function(FUN)) stop("'FUN' must be a function.")
    result <- lapply(result, FUN, ...)
  }

  # combine
  if (!is.null(combine_FUN)) {
    if (!is.function(combine_FUN)) stop("'combine_FUN' must be a function.")
    result <- combine_FUN(result)
  }

  # output
  result
}
