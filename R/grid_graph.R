#' @title Generate a grid graph
#' @description
#' This function generates a grid graph with the given number of vertices and degree.
#' @param n The number of vertices
#' @param k The number of neighboring vertices connected to each vertex. Note that the resulting neighboring vertices will be 2*k.
#' @return The grid graph represented in adjacency matrix format
#' @description
#' This function generates a special case of grid graph where each vertices are corrected
#' to their given number of neighboring vertices and there is no "end" vertices because the
#' the vertices of the last ID again connects to the vertices with the first ID.
#' This graph can be useful to set a initial condition of the spatial networks where
#' agent on the given vertices can move to the neighboring vertices.
#' This graph can be seen as a generalization of the cellular automaton
#' to the given dimension. Note that each vertices has 2*k neighbors.
#' Concretely, by setting the following number of k,
#' the movable direction of the agents on a vertices changes from 2 (k = 1), 4 (k = 2),
#' 6 (k = 3), and 8 (k = 4).
#'
#' @export
#'
#' @examples
#' # k = 1:cicle
#' grid_graph(10, 1)
#'
#' # k = 2: A square Generate a 4x4 grid graph
#' grid_graph(10, 2)

grid_graph <- function(n, k){
  # Create labels
  act_label <- paste0(1:n)
  # Create an empty matrix
  mat <- matrix(0, n, n, dimnames = list(act_label, act_label))

  # Double the labels vector
  act_label2 <- rep(act_label, 2)
  for(i in 1:n){
    mat[i, act_label2[(i+1):(i+k)]] <- 1
  }
  # Symmetrize
  mat <- symmetrize(mat)
  dimnames(mat) <- list(act_label, act_label)
  # Output
  mat
}
