#' @title Symmetrize Edges in a Network
#' @description
#' This function symmetrizes the edges of the agents in focus. For example, if
#' the agent in focus is ID1 and ID1 has tie with ID2, then this function
#' creates the ties from ID2 to ID1 so that the edge between ID1 and ID2.
#'
#' @param D A \code{netABM} object.
#' @param self self of the R6 object. For writing the .act function, simply write self to this argument.
#' Alternatively, users can supply the character of the agent ID.
#' @param which_net The index or the name of the network in the network list to use (default is 1).
#' @return Updated netABM_network object with symmetrized edges.
#' @details
#' In some network agent-based modeling scenarios, the network is undirected.
#' In such cases, both edges from agent A to B and from B to A need to be
#' in the same state. This function can be utilized to write .act functions
#' to ensure that networks remain symmetric.
#' @export
#'
#' @examples
#' # Example 1: add an edge from ID3 to ID1 to symmetrize
#' network1 <- matrix(c(0, 1, 1, 0, 0,
#'                     1, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0), 5, 5, byrow = TRUE)
#' D <- setABM_network(n = 5, networks = network1)
#' get_net(D)
#' symmetrize_edges(D, self = "ID1")
#' get_net(D)        # symmetrized
#'
#' # Example 2: delete an edge from ID3 to ID1 to symmetrize
#' network2 <- matrix(c(0, 1, 0, 0, 0,
#'                     1, 0, 0, 0, 0,
#'                     1, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0), 5, 5, byrow = TRUE)
#' D <- setABM_network(n = 5, networks = network2)
#' get_net(D)
#' symmetrize_edges(D, self = "ID1")
#' get_net(D)        # symmetrized
#'
#' # Example 3: nothing occurs because the network is already symmetric
#' network3 <- matrix(c(0, 1, 1, 0, 0,
#'                      1, 0, 0, 0, 0,
#'                      1, 0, 0, 0, 0,
#'                      0, 0, 0, 0, 0,
#'                      0, 0, 0, 0, 0), 5, 5, byrow = TRUE)
#' D <- setABM_network(n = 5, networks = network3)
#' get_net(D)
#' symmetrize_edges(D, self = "ID1")
#' get_net(D)        # symmetrized
#'
#' # Example 1: add an edge from ID3 to ID1 to symmterize
#' network1 <- matrix(c(0, 1, 1, 0, 0,
#'                     1, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0), 5, 5, byrow = T)
#' # Create the netABM_network object
#' D <- setABM_network(n = 5, networks = network1)
#' get_net(D)
#' # symmetrize
#' symmetrize_edges(D, self = "ID1")
#' get_net(D)        # symmetrized
#'
#' # Example 2: delete an edge from ID3 to ID1 to symmterize
#' network2 <- matrix(c(0, 1, 0, 0, 0,
#'                     1, 0, 0, 0, 0,
#'                     1, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0,
#'                     0, 0, 0, 0, 0), 5, 5, byrow = T)
#' # Create the netABM_network object
#' D <- setABM_network(n = 5, networks = network2)
#' get_net(D)
#' # symmetrize
#' symmetrize_edges(D, self = "ID1")
#' get_net(D)        # symmetrized
#'
#' # Example 3: nothing occurs because the network is already symmetric
#' network3 <- matrix(c(0, 1, 1, 0, 0,
#'                      1, 0, 0, 0, 0,
#'                      1, 0, 0, 0, 0,
#'                      0, 0, 0, 0, 0,
#'                      0, 0, 0, 0, 0), 5, 5, byrow = T)
#' # Create the netABM_network object
#' D <- setABM_network(n = 5, networks = network3)
#' get_net(D)
#' # symmetrize
#' symmetrize_edges(D, self = "ID1")
#' get_net(D)        # symmetrized


# function
symmetrize_edges <- function(D, self, which_net = 1){
  # selfIDがNULLの場合
  if(any(class(self)=="R6")){
    selfID <- self$a$ID
  }else{
    selfID <- self
  }

  # networkを取得する
  net <- netABM::get_net(D, which_net = which_net)
  ego_vect <- net[selfID, ]
  alt_vect <- net[ ,selfID]

  # 両者を比較する
  diff_edge <- ego_vect != alt_vect
  diff_alt_name <- colnames(diff_edge)[diff_edge]

  # 変更箇所を修正する
  ## もしも修正するものが何もない場合
  if(length(diff_alt_name)==0){
    return(NULL)
  }else{
    ## 修正する場合
    for(i in 1:length(diff_alt_name)){
      new_value <- unlist(ego_vect[diff_alt_name[i]])
      if(new_value == 0){
        temp_vec <- D$agents[[names(new_value)]]$e[[which_net]]
        D$agents[[names(new_value)]]$e[[which_net]] <- temp_vec[-which(names(temp_vec) == selfID)]
      }else{
        D$agents[[names(new_value)]]$e[[which_net]][selfID] <- new_value
      }
    }
  }
}
