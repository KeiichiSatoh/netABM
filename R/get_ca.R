#' @title Get CA Represented By An Attribute
#' @description This function returns a cellular automaton (CA)
#' in which each cell shows agent's attribute.
#'
#' @param which_ca A name specifying which CA to use from the \code{D}.
#' The Default is \code{NULL}, resulting in selecting the first CA found by
#' \code{D$.field_type()} method.
#' @param by_which_attr A character of the agent's attribute name.
#' @param D A \code{ABM_D} class object set by \code{setABM} function.
#' The default \code{NULL} will result in selecting the \code{D} object
#' from the parent.frame, which may be the usual usage.
#' @param from_log Should the CA retrieved from \code{log}. Default is \code{FALSE}.
#'
#' @details
#' CA in \code{D} usually shows the agent ID. This function searches the relevant
#' agent's attribute and returns a new CA in which each cell is replaced by these
#' attributes.
#'
#' @return A matrix or array of a CA or list of them if \code{from_log} is \code{TRUE}.
#' @family ca_tools
#' @family f_tools
#' @export
#' @examples
#' D <- setABM(agent_n = 6, agent_attr = data.frame(sex = c(1,1,1,2,2,2)), ca = 1)
#' D <- runABM(D = D, save_log = TRUE)
#' get_ca(which_ca = "ca", by_which_attr = "sex")
#' get_ca(which_ca = "ca", by_which_attr = "sex", from_log = TRUE)

get_ca <- function(which_ca = NULL, by_which_attr = NULL, D = NULL, from_log = FALSE){
  # DとIDが入っていない場合にはparent.frameから取得する
  if(is.null(D)){D <- parent.frame()$D}
  # which_caが直接指定されていない場合には
  # そうでない場合にはDの中の1つめのcaを取得
  if(is.null(which_ca)){
    field_type <- D$.field_type()
    which_ca <- names(field_type)[field_type=="ca"][1]
  }
  # by_which_attr:NULLの場合にはIDを入れる。
  if(is.null(by_which_attr)){by_which_attr <- "ID"}

  # 現在のDからの取得
  if(from_log==FALSE){
    # CAを取得
    CA <- D[[which_ca]]
    # attrのベクトルを取得
    attr <- sapply(1:length(D$agent), function(i){
      D$agent[[i]][[by_which_attr]]
    })
    # CAをコピーする
    CA2 <- CA
    # CAの値を置き換える
    CA_id <- CA[CA > 0]
    if(is.numeric(attr)){
      CA2[CA > 0] <- attr[CA_id]
    }else{
      CA2_vect <- as.vector(CA2)
      CA2_vect[CA2_vect > 0] <- attr[CA_id]
      CA2_vect[CA2_vect == 0] <- ""
      CA2 <- array(CA2_vect, dim = dim(CA))
    }
 }else{
 # logからの取得
   CA <- lapply(1:length(D$log), function(t){
     D$log[[t]][[which_ca]]
   })

   attr <- lapply(1:length(D$log), function(t){
     sapply(1:length(D$log[[t]]$agent), function(i){
       D$log[[t]]$agent[[i]][[by_which_attr]]
     })
   })
   # CAをコピーする
   CA2 <- CA
   # IDを取得する
   CA_id <- lapply(1:length(D$log), function(t){
     CA[[t]][CA[[t]] > 0]})

   # CAの値を書き換える
   ## attrがnumericの場合
   if(is.numeric(attr[[1]])){
     CA2 <- lapply(1:length(D$log), function(t){
       CA_temp <- CA2[[t]]
       CA_temp[CA_temp > 0] <- attr[[t]][CA_id[[t]]]
       CA_temp
     })
   }else{
   ## attrがnumeric以外の場合
     CA2 <- lapply(1:length(D$log), function(t){
       CA_temp <- CA2[[t]]
       CA_temp_vect <- as.vector(CA_temp)
       CA_temp_vect[CA_temp_vect > 0] <- attr[[t]][CA_id[[t]]]
       CA_temp_vect[CA_temp_vect == 0] <- ""
       CA_temp <- array(CA_temp_vect, dim = dim(CA_temp))
       CA_temp
     })
   }
   names(CA2) <- names(D$log)
 } # logから：ここまで
 # リターン
CA2
}
