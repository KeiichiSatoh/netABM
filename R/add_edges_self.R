#' @title Add Edges from Self to Alters
#' @description
#' \code{add_edges_self} adds edges from a self node to specified alter nodes
#' in a network object.
#' @param D A network object of class 'netABM_network'.
#' @param self self of the R6 object. For writing the .act function, simply write self to this argument.
#' Alternatively, users can supply the character of the agent ID.
#' @param new_alters Character vector specifying the IDs of the new alter nodes to add edges to.
#' @param value Numeric value or vector specifying the value(s) of the edge(s) to be added (default is 1).
#' @param which_net The index or the name of the network in the network list to use (default is 1).
#' @param duplicated_edges Function specifying how to handle duplicated edges (default is sum).
#' @param ... Additional arguments to be passed to duplicated_edges function.
#'
#' @return Updated network object with added edges.
#'
#' @export
#' @family act_tools
#'
#' @details \code{add_edges_self} is a utility function designed to assist users
#' in defining agent actions of adding edges from the agent to specified alter nodes.
#'
#' By default, the value \code{1} is assigned to the newly created edges.
#' Users can customize this value by modifying the \code{value} argument.
#' Additionally, users can assign different values to each alter by providing a
#' vector of values to be added to the new edges for each alter.
#'
#' When additional edges are added and there are existing edges from the agent to
#' an alter node, the default behavior is to sum up the values of the edges.
#' Users can customize this calculation by changing the \code{duplicated_edges} function
#' to another function (e.g. \code{mean}, \code{max}, or \code{min}).
#'
#' Important note: when using this function for writing the agent's .act function,
#' be sure to supply self to the self argument (i.e., add_edges_self(D, self,...)).
#' See Example 3.
#'
#' @examples
#' # Create a sample D
#' D <- setABM_network(n = 5)
#'
#' # Example 1: add edges with "ID2" and "ID3" to "ID1"
#' D$agents$ID1$e
#' add_edges_self(D, self = "ID1", new_alters = c("ID2", "ID3"))
#' D$agents$ID1$e
#'
#' # Example 2: Calculate the mean value for the duplicated edges.
#' add_edges_self(D, self = "ID2", new_alters = "ID1")
#' D$agents$ID2$e  # add the edge with ID2 with the value 1 to ID1
#'
#' add_edges_self(D, self = "ID2", value = 2,
#'                duplicated_edges = mean)   # add the edge with ID2 with the value 2 to ID1
#' D$agents$ID2$e  # 1.5 because (1 + 2)/2
#'
#' # Example 3: A more real usage
#'
#' # Write an action that the agent adds an edge to ID2 agent.
#' form_edges_with_ID2 <- function(D){
#'   add_edges_self(D, self, new_alters = "ID2")
#' }
#'
#' D <- setABM_network(n = 5, .act = form_edges_with_ID2)
#' D$agents$ID1$e          # no edge exists
#' D$agents$ID1$.act(D)    # ID1's action
#' D$agents$ID1$e          # An edge added with ID2

add_edges_self <- function(D, self, new_alters, value = 1, which_net = 1,
                           duplicated_edges = sum, ...){
  # selfIDがNULLの場合
  if(any(class(self)=="R6")){
    selfID <- self$a$ID
  }else{
    selfID <- self
  }

  # valueがaltersの人数分ない場合にはコピーする
  if(length(value) < length(new_alters)){
    new_value <- rep(value, length(new_alters))
  }else{
    new_value <- value
  }
  # 新しい値に名前を付ける
  names(new_value) <- new_alters
  # 新しい値を統合
  new_edges <- c(D$agents[[selfID]]$e[[which_net]], new_value)
  # 重複の値も含めて処理
  D$agents[[selfID]]$e[[which_net]] <- tapply(new_edges, names(new_edges), duplicated_edges, ...)
}
