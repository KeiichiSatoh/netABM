#' @title Extract neighbors from a matrix
#' @description
#' This function extracts the values of the neighboring cells from a given matrix
#' for specified positions. The neighbors can include or exclude the center cell (ego).
#' An optional function (\code{FUN}) can be applied to the extracted neighbors.
#'
#' @param mat A numeric matrix from which neighbors will be extracted.
#' @param posit A numeric vector representing linear indices of the positions whose
#' neighbors should be extracted. Either \code{posit} or \code{posit_ind} must be provided.
#' @param posit_ind An integer matrix with two columns, where each row specifies the
#' row and column indices of the positions whose neighbors should be extracted.
#' Either \code{posit} or \code{posit_ind} must be provided.
#' @param include_ego A logical flag indicating whether to include the center cell (ego)
#' in the output. Default is \code{FALSE}.
#' @param FUN An optional function to be applied to the extracted neighbors for each
#' position. The function must take a numeric vector as input and return a single value
#' or a modified vector. If \code{NULL}, the raw neighbors are returned.
#' @param ... Additional arguments to be passed to \code{FUN}.
#' @return A numeric matrix where each row corresponds to the neighbors of a position
#' specified in \code{posit} or \code{posit_ind}. If \code{FUN} is provided, the result
#' is a transformed matrix or vector depending on \code{FUN}.
#' @examples
#' mat <- matrix(1:16, nrow = 4)
#' posit <- c(6, 11)  # Linear indices
#' neib8(mat, posit, include_ego = TRUE)
#'
#' # Using posit_ind
#' posit_ind <- matrix(c(2, 2, 3, 3), ncol = 2, byrow = TRUE)
#' neib8(mat, posit_ind = posit_ind, include_ego = FALSE)
#'
#' # Applying a custom function
#' neib8(mat, posit, include_ego = TRUE, FUN = sum)
#' @export

neib8 <- function(mat, posit = NULL, posit_ind = NULL, include_ego = FALSE,
                  FUN = NULL, ...) {
  if (is.null(posit) && is.null(posit_ind)) {
    stop("Either 'posit' or 'posit_ind' must be provided.")
  }

  mat_rows <- nrow(mat)
  mat_cols <- ncol(mat)

  # posit を posit_ind に変換
  if (!is.null(posit)) {
    posit_ind <- cbind(
      ((posit - 1) %% mat_rows) + 1,
      ((posit - 1) %/% mat_rows) + 1
    )
  }

  # 入力データの検証
  if (any(posit_ind[, 1] < 1 | posit_ind[, 1] > mat_rows |
          posit_ind[, 2] < 1 | posit_ind[, 2] > mat_cols)) {
    stop("'posit_ind' contains indices outside the matrix bounds.")
  }

  # Rcpp関数を呼び出す
  result <- neib8_rcpp(mat, posit_ind, include_ego)

  # FUNがある場合の扱い
  if (!is.null(FUN)) {
    result <- apply(result, 1, FUN, ...)
  } else {
    # 列名をつける
    if (ncol(result) == 8) {
      colnames(result) <- c("LU", "L", "LD", "U", "D", "RU", "R", "UD")
    } else if (ncol(result) == 9) {
      colnames(result) <- c("LU", "L", "LD", "U", "ego", "D", "RU", "R", "UD")
    } else {
      warning("Unexpected column count in the result.")
    }
  }

  # リターン
  return(result)
}
