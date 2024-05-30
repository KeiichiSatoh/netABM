#' @title Convert a 2D Cellular Automaton (CA) to 3D
#' @description \code{ca_2d_to_3d} converts a 2D cellular automaton (CA) to
#' a 3D array by replicating the 2D grid along a third dimension.
#'
#' @param CA A 2D matrix representing the cellular automaton.
#' @param height An integer specifying the number of layers to replicate the 2D grid along the third dimension.
#' @details
#' The function checks if the input CA is a 2D matrix. If it is, the function replicates this 2D matrix along the third dimension
#' to create a 3D array. The resulting 3D array has dimensions equal to the original 2D matrix dimensions plus the specified height.
#' @return A 3D array where the 2D CA grid is replicated along the third dimension.
#' @author Keiichi Satoh
#' @family ca_tools
#' @family f_tools
#' @examples
#' # Create a 2D CA grid
#' CA_2d <- matrix(1:9, nrow = 3, ncol = 3)
#' # Convert the 2D CA grid to a 3D array with height 4
#' CA_3d <- ca_2d_to_3d(CA_2d, height = 4)
#'
#' @export
ca_2d_to_3d <- function(CA, height){
  # 次元を取得
  CA_dim <- dim(CA)
  # 2次元でないときには停止
  stopifnot("The number of dimension of CA must be 2." = length(CA_dim)==2)
  # 3D化したもの
  CA_3d <- array(rep(as.vector(CA), height), dim = c(CA_dim, height))
  # アウトプット
  CA_3d
}

