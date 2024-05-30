#' @title Move an Agent in a Cellular Automaton (CA)
#' @description This function moves an agent from its current location
#' to a specified new location within a cellular automaton (CA).
#'
#' @param D A \code{netABM_ca} class object set by \code{setABM_ca} function.
#' @param ID The ID of the agent to be moved.
#' @param where_to An integer specifying the new location for the agent within the CA.
#' @param which_ca An index or name specifying which CA to use from the \code{D} list. Default is \code{1}.
#'
#' @details
#' The function identifies the current location of the specified agent ID in the CA.
#' It then attempts to move the agent to the specified new location.
#' If the destination is already occupied by another agent, the move is not performed,
#' and a message is displayed. Otherwise, the agent is moved,
#' and its original location is cleared.
#'
#' @return The function updates the CA in place and does not return a value.
#' @family ca_tools
#' @family f_tools
#' @examples
#' # prepare an example dataset
#' set.seed(1)
#' ca <- init_ca(dim = c(3, 3), agent_n = 5)
#' D <- setABM_ca(agent_n = 5, ca = ca)
#'
#' # ID1 locates at the place 9
#' D$ca$ca
#'
#' # ID1 moves
#' ca_move(D, ID = 1, where_to = 5, which_ca = "ca")
#'
#' # ID1 now moved to the place 5
#' D$ca$ca
#'
#' @export
ca_move <- function(D, ID, where_to, which_ca = 1){
  # CAを特定する
  if(is.numeric(which_ca)){
    ca_namelist <- names(D$ca)[!names(D$ca) %in% c(".__enclos_env__","clone","print")]
    which_ca <- ca_namelist[which_ca]
  }

  ## 対象とするIDのあるアドレスを特定
  loc <- which(D$ca[[which_ca]] == ID, arr.ind = TRUE)

  ## 移動先の値を置き換える
  if(D$ca[[which_ca]][where_to] != 0){
    # すでに他のエージェントがいる場合には、もとの場所にとどまる
    message("The destination is already occupied by another agent. Hence, the agent remains in the original place.")
  }else{
    # 移動可能な場合には移動し、もとの場所の情報を消す
    D$ca[[which_ca]][where_to] <- ID
    D$ca[[which_ca]][loc] <- 0
  }
}

