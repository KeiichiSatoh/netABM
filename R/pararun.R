#' @title Parallel Execution of an Expression
#' @description
#' This function executes a given R expression in parallel across multiple cores using the socket approach via parLapply.
#'
#' @param X A list or vector of input values to be processed in parallel.
#' @param expr An R expression to be evaluated in parallel. It should be valid R code without quotes.
#' @param ncores An integer specifying the number of cores to use for parallel execution.
#'               If \code{NULL}, all available cores will be used.
#' @param LB A logical indicating whether to use load balancing. Default is \code{FALSE}.
#' @param export_var A character vector of variable names to be exported to the cluster environment.
#'                   Default is all variables in the global environment.
#' @param evalQ_expr An optional expression to be evaluated on each worker before executing the main expression.
#'
#' @return A list of results from the execution of the expression for each element in X.
#' @details
#' This is a wrapper function of \code{parLapply} in the \code{parallel} package.
#' @import parallel
#' @examples
#' agent_attr <- data.frame(age = rep(0, 2))
#' f <- function(){self$age <- self$age + 1}
#' D <- setABM(agent_n = 2, agent_attr = agent_attr, agent_f = f)
#' pararun(X = 1:3, expr = runABM(D = D), ncores = 2)
#' pararun(X = 1:3,
#'         expr = {
#'           D_res <- runABM(D = D)
#'           D_res$agent$ID1
#'         }, ncores = 2)
#' @export

pararun <- function(X, expr, ncores = NULL, LB = FALSE,
                    export_var = ls(envir = globalenv()),
                    evalQ_expr = NULL) {
  # expressionに基づいて新しい関数を作成する
  FUN <- function(X = X) {}　　　# 空の関数
  expr <- substitute(expr)
  body(FUN) <- as.call(expr)　　# 空の関数の中身を置き換える

  # clusterを作成する
  if(is.null(ncores)) { ncores <- parallel::detectCores() }   # コア数が不明の場合には最大値に
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl)) # 関数終了時に自動的にclusterを閉じる

  # クラスターに必ず必要な関数・パッケージをエクスポート
  parallel::clusterExport(cl, varlist = c("FUN"), envir = environment(FUN))
  parallel::clusterEvalQ(cl, library(netABM))

  # 追加的に必要なオブジェクト、関数群をエクスポート
  parallel::clusterExport(cl, varlist = export_var)

  # evalQが指定されている場合にはexport
  evalQ_expr <- substitute(evalQ_expr)

  if(!is.null(evalQ_expr)) {
    # clusterCallで各クラスターにeval()を使って式を評価させる
    parallel::clusterCall(cl, function(){eval(evalQ_expr, envir = .GlobalEnv)})
  }

  # lapply
  if(LB == FALSE) {
    parallel::parLapply(cl = cl, X = X, fun = FUN)
  } else {
    parallel::parLapplyLB(cl = cl, X = X, fun = FUN)
  }
}
