#' @title Move an Agent in a Cellular Automaton (CA)
#' @description This function moves an agent from its current location
#' to a specified new location within a cellular automaton (CA).
#'
#' @param which_ca A name specifying which CA to use from the \code{D}.
#' The Default is \code{NULL}, resulting in selecting the first CA found by
#' \code{D$.field_type()} method.
#' @param where_to An integer of vector index or matrix of array index
#' specifying the new location for the agent within the CA.
#' @param D A \code{ABM_D} class object set by \code{setABM} function.
#' The default \code{NULL} will result in selecting the \code{D} object
#' from the parent.frame, which may be the usual usage.
#' @param ID The ID of the agent. The default \code{NULL} will result in
#' selecting the current agent from the parent.frame,
#' which may be the usual usage.
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
#' @export
#' @examples
#' # Example 1
#' # prepare an example dataset
#' set.seed(1)
#' ca <- init_ca(dim = c(3, 3), agent_n = 5)
#' D <- setABM(agent_n = 5, ca = ca)
#' # ID1 locates at the place 9
#' D$ca
#' ca_move(which_ca = "ca", where_to = 5, D = D, ID = 1)
#' # ID1 moves to the place 5 (i.e. row = 2, col = 2)
#' D$ca
#'
#' # Example 2: a more practical usage
#'
#' f <- function(){
#'  ca_move(which_ca = "ca", where_to = 5)
#'}
#'
#' set.seed(1)
#' ca <- init_ca(dim = c(3, 3), agent_n = 1)
#' D <- setABM(agent_n = 1, ca = ca, agent_f = f)
#' D$ca
#' D <- runABM(D = D)
#' D$ca

ca_move <- function(
    which_ca = NULL,
    where_to,
    ID = NULL,
    D = NULL){
  # 準備
  ## DとIDが入っていない場合にはparent.frameから取得する
  if(is.null(D)){D <- parent.frame()$D}
  if(is.null(ID)){ID <- parent.frame()$self$ID}
  ## which_caが直接指定されている場合にはそちらを取る。
  ## そうでない場合にはDの中の1つめのcaを取得
  if(is.null(which_ca)){
    field_type <- D$.field_type()
    which_ca <- names(field_type)[field_type=="ca"][1]
  }

  ## 対象とするIDのあるアドレスを特定
  loc <- which(D[[which_ca]] == ID)

  ## 移動先の値を置き換える
  if(D[[which_ca]][where_to] != 0){
    # すでに他のエージェントがいる場合には、もとの場所にとどまる
    message("The destination is already occupied by another agent, so the agent ",
    ID, " remains in its original position.")
  }else{
    # 移動可能な場合には移動し、もとの場所の情報を消す
    D[[which_ca]][where_to] <- ID
    D[[which_ca]][loc] <- 0
  }
}

