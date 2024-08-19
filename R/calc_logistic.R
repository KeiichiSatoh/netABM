#' @title Calculate Probabilities Using Logistic Function
#'
#' @description
#'  This function calculates the probabilities of choosing the dependent variable
#' based on the logistic function for a given formula and dataset.
#'
#' @param formula A formula specifying the model.
#' The left-hand side (LHS) can optionally specify the labels,
#' and the right-hand side (RHS) specifies the predictors.
#' @param data A data frame containing the variables in the formula.
#' @param standardize A logical value indicating whether to standardize numeric predictors.
#' Default is \code{FALSE}.
#' @param base_prob A constant probability that will be added as a intercept into the formula.
#'  The default \code{NULL} adds no intercept into the formula.
#' @param randomness A funtion that generates random number.
#' The default \code{rnorm(sd = 0.01)} adds around 1% randomness into
#' the probability in addition to specified formula.
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @details
#' \code{calc_logistic}  evaluates the specified formula in the context of the
#' provided data.
#' It computes the probabilities for the predictors on the RHS of the formula.
#' If standardize is \code{TRUE}, numeric predictors are standardized
#' before calculating the probabilities.
#'
#' \code{choose_logistic} samples from the choices based on these probabilities.
#'
#' @return \code{calc_logistic} returns a probability.
#' \code{choose_softmax} returns TRUE or FALSE based on the logistic probability.
#'
#' @author Keiichi Satoh
#' @family f_tools
#'
#' @examples
#' # Create sample data
#' n <- 10
#' dat <- data.frame(
#'    Y = LETTERS[1:10],
#'    X1 = rnorm(n),
#'    X2 = rnorm(n)
#'  )
#'
#' # The left-hand formula is used to index the result
#' calc_softmax(Y ~ X1 * X2, data = dat, standardize = FALSE)
#' # Without the left-hand specification, the result is returned with the row index
#' calc_softmax(~ X1 * X2, data = dat, standardize = FALSE)
#'
#' # Choose based on softmax probabilities
#' choose_softmax(Y ~ X1 * X2, data = dat, standardize = FALSE, n = 2)
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @export
#' @examples
#' dat <- data.frame(
#' Y = LETTERS[1:3],
#' X1 = rnorm(n = 3),
#' X2 = rnorm(n = 3)
#' )
#'
#' calc_logistic(Y ~ X1 + X2, data = dat, base_prob = 0.1)
#' calc_logistic(~ X1 + X2, data = dat, base_prob = 0.1)
#' calc_logistic(~ 0, data = dat, base_prob = 0.05)
#' calc_logistic(~ 0, data = dat, base_prob = 0.05, randomness = rnorm(sd = 0.01))
#' choose_logistic(~ 0, data = dat, base_prob = 0.05, randomness = rnorm(sd = 0.01))

# 関数
calc_logistic <- function(
    formula, data = NULL, standardize = FALSE,
    base_prob = NULL,
    randomness = rnorm(sd = 0.01)){
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

  # 最低基準確率
  if(!is.null(base_prob)){
    const <- log((1-base_prob)/base_prob)*(-1)
  }else{
    const <- 0
  }

  # randomness
  if(!is.null(substitute(randomness))){
    randomness_args <- c(call_args(substitute(randomness)), n = NROW(data), mean = 0)
    x_random <- do.call(what = call_name(substitute(randomness)), randomness_args)
  }else{
    x_random <- 0
  }

  # 式を分解
  rhf <- f_rhs(formula)
  lhf <- f_lhs(formula)

  # Xを計算する
  fx <- eval(rhf, envir = data) + x_random + const
  # ロジスティック関数に基づき確率を計算
  p <- 1/(1 + exp((-1)*fx))

  # 名前を付ける(lhfがある場合)
  if(is.null(lhf)){
    names(p) <- 1:length(p)
  }else{
    names(p) <- eval(lhf, envir = data)
  }
  # 確率をリターン
  p
}

#' @rdname calc_logistic
#' @export

# choose関数
choose_logistic <- function(formula, data = NULL, standardize = FALSE,
                            base_prob = NULL, randomness = rnorm(sd = 0.01)){
  p <- calc_logistic(formula = formula, data = data, standardize = standardize)
  decision <- unlist(lapply(1:length(p), FUN = function(i){
    sample(c(TRUE,FALSE), size = 1, prob = c(p[i], 1-p[i]))
  }))
  names(decision) <- names(p)
  decision
}


