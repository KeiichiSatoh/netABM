#' @title Calculate Probabilities Using Softmax Function
#'
#' @description
#'  This function calculates the probabilities of choosing the dependent variable
#' based on the softmax probabilities for a given formula and dataset.
#'
#' @param formula A formula specifying the model.
#' The left-hand side (LHS) can optionally specify the labels,
#' and the right-hand side (RHS) specifies the predictors.
#' @param data A data frame containing the variables in the formula.
#' @param standardize A logical value indicating whether to standardize numeric predictors.
#' Default is \code{FALSE}.
#' @param n An integer specifying the number of items to choose. Default is {1}. Only Used for the \code{choose_softmax}.
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @details
#' \code{calc_softmax}  evaluates the specified formula in the context of the provided data frame.
#' It computes the probabilities for the predictors on the RHS of the formula.
#' If standardize is \code{TRUE}, numeric predictors are standardized
#' before calculating the probabilities.
#'
#' \code{choose_softmax} samples from the choices based on these probabilities.
#'
#' This function can be used to calculate the probabilities according to which agent choose the options.
#'
#' @return \code{calc_softmax} returns A named vector of softmax probabilities.
#' If the LHS of the formula is specified, the names of the vector are derived from the LHS.
#' Otherwise, the names are row indices.
#'
#' \code{choose_softmax} returns A vector of chosen items based on the softmax probabilities,
#' with length equal to the specified \code{n}.
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

calc_softmax <- function(formula, data = NULL, standardize = FALSE){
  # データを標準化する場合
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
  rhf <- rlang::f_rhs(formula)
  lhf <- rlang::f_lhs(formula)
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

#' @rdname calc_softmax
#' @export
choose_softmax <- function(formula, data = NULL, standardize = FALSE, n = 1, replace = TRUE){
  p <- calc_softmax(formula = formula, data = data, standardize = standardize)
  decision <- sample.int(n = length(p), size = n, prob = p, replace = replace)
  names(p[decision])
}
