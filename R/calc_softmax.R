#' package
#' library(rlang)
#'
#'
#' # EXAMPLE
#' n <- 10
#' dat <- data.frame(
#'   Y = LETTERS[1:10],
#'   X1 = rnorm(n),
#'   X2 = rnorm(n)
#' )
#'
#' calc_softmax(Y ~ X1 * X2, data = dat, standardize = FALSE)
#' calc_softmax(  ~ X1 * X2, data = dat, standardize = FALSE)

calc_softmax <- function(formula, data = NULL, standardize = FALSE){
  # データを標準化する場合
  # 標準化する場合
  if(standardize == TRUE){
    numeric_x <- unlist(lapply(data, is.numeric))
    data[ ,numeric_x] <- lapply(data[ ,numeric_x],
                                function(X){
                                  X <- scale(X)
                                  attributes(X) <- NULL
                                  X
                                })
  }
  # 式を分解
  rhf <- f_rhs(formula)
  lhf <- f_lhs(formula)
  # Xを計算する
  X <- eval(rhf, envir = data)
  # softmax関数をもとに確率を計算する
  X_exp <- exp(X)
  u <- sum(X_exp)
  p <- X_exp/u
  # 名前を付ける(lhfがある場合)
  if(is.null(lhf)){
    names(p) <- 1:length(p)
  }else{
    names(p) <- eval(lhf, envir = data)
  }
  # 確率をリターン
  p
}



choose_softmax <- function(formula, data = NULL, standardize = FALSE, n = 1){
  p <- calc_softmax(formula = formula, data = data, standardize = standardize)
  decision <- sample(names(p), size = n, prob = p)
  decision
}

#' Example
#' choose_softmax(Y ~ X1 * X2, data = dat, standardize = FALSE, n = 2)


