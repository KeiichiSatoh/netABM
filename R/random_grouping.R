#' @title Randomly group IDs into specified group sizes
#' @description
#' This function takes a vector of IDs, shuffles them randomly, and divides them into groups of a specified size.
#' If the total number of IDs is not evenly divisible by the group size, the behavior for handling the remainder
#' can be controlled via the `remainder_meth` argument.
#'
#' @param id A vector of IDs to be grouped.
#' @param size An integer specifying the size of each group. Defaults to 2.
#' @param remainder_meth A character string indicating how to handle IDs that do not fit evenly into groups.
#'   Options are:
#'   \itemize{
#'     \item `"omit"`: Discard the remainder IDs.
#'     \item `"keep"`: Add the remainder IDs as a single group, padded with `NA` if necessary.
#'     \item `"isolate"`: Place each remainder ID in its own group, padded with `NA` to match the group size.
#'   }
#'   Defaults to `"omit"`.
#'
#' @return A matrix where each row represents a group, and columns contain IDs or `NA` if the group is smaller than the specified size.
#'
#' @examples
#' random_grouping(id = 1:8, size = 2)
#' random_grouping(id = 1:8, size = 3, remainder_meth = "omit")
#' random_grouping(id = 1:8, size = 3, remainder_meth = "keep")
#' random_grouping(id = 1:8, size = 3, remainder_meth = "isolate")
#'
#' @export
random_grouping <- function(id, size = 2,
                            remainder_meth = c("omit", "keep", "isolate")){
  # Validate inputs
  remainder_meth <- match.arg(remainder_meth)

  # Calculate n, quotient, and remainder
  n <- length(id)
  quotient <- n %/% size
  remainder <- n %% size

  # Group numbers
  group <- rep(1:quotient, each = size)

  # Shuffle IDs
  shuffled_id <- sample(id)

  if (remainder == 0) {
    # Split without remainder
    result <- split(shuffled_id, f = group)
  } else {
    # Handle remainder based on method
    main_ids <- shuffled_id[1:(n - remainder)]
    remainder_ids <- shuffled_id[(n - remainder + 1):n]

    result <- split(main_ids, f = group)
    # omit:の場合には何もしない。
    # その他の場合で場合分け
    if (remainder_meth == "keep") {
      # Add remainder as a single group
      result <- c(result, list(c(remainder_ids, rep(NA, size - remainder))))
    } else if (remainder_meth == "isolate") {
      # Add remainder as individual groups
      isolate_groups <- lapply(remainder_ids, function(x) c(x, rep(NA, size - 1)))
      result <- c(result, isolate_groups)
    }
  }

  # マトリクスとしてまとめる
  result <- do.call(what = rbind, args = result)
  # Return result
  return(result)
}
