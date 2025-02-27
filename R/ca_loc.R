#' @title Get Location of an Agent in a Cellular Automaton (CA)
#' @description
#' This function retrieves the location of a specified agent ID in a cellular automaton (CA).
#'
#' @param ID The ID of the agent whose location is to be found.
#' If this is remain to be the default \code{NULL},
#' the current agent's ID is automatically retrieved.
#' @param which_ca A name specifying which CA to use from the \code{D} list.
#' If \code{NULL}, the first ca field found in the \code{D} is automatically found.
#' @param CA An optional matrix or array representing the CA.
#' If provided, this CA is used directly. Default is \code{NULL}.
#' @param arr.ind A logical value indicating whether to return the array indices
#' (row and column) of the agent's location. If FALSE, returns the position as
#' an integer-valued index. Default is \code{TRUE}.
#'
#' @details
#' The function first checks if a CA is directly provided through
#' the \code{CA} parameter. If not, it retrieves the CA from the \code{D} list
#' using the specified \code{which_ca} index.
#' It then finds and returns the location of the specified agent ID in the CA.
#'
#' @return
#' The function returns the location of the specified agent ID in the CA.
#' The format of the return value depends on the \code{arr.ind} parameter:
#' \itemize{
#'   \item If \code{arr.ind = TRUE}, returns the row and column indices of the agent's location.
#'   \item If \code{arr.ind = FALSE}, returns the position as an integer-valued index.
#' }
#' @family ca_tools
#' @family f_tools
#' @author Keiichi Satoh
#' @export
#' @examples
#'
#' # Example 1: use get_ca_loc as an action of an agent
#' f <- function(){
#'   get_ca_loc()
#' }
#' D <- setABM(agent_n = 3, ca = 1, agent_f = f)
#' D$agent$ID1$.f(D = D)
#'
#' # Example 2: retireve the agent location directly
#' get_ca_loc(CA = D$ca, ID = 1)

ca_loc <- function(
    which_ca = NULL,
    arr.ind = TRUE,
    ID = NULL,
    D = NULL){
  ## DとIDが入っていない場合にはparent.frameから取得する
  if(is.null(D)){D <- parent.frame()$D}
  if(is.null(ID)){ID <- parent.frame()$self$ID}
  ## which_caが直接指定されている場合にはそちらを取る。
  ## そうでない場合にはDの中の1つめのcaを取得
  if(is.null(which_ca)){
    field_type <- D$.field_type()
    which_ca <- names(field_type)[field_type=="ca"][1]
  }

  # 場所を取得する
  which(D[[which_ca]]==ID, arr.ind = arr.ind)
}


