
#' D <- setABM_ca(agent_n = 5)
#' ca_move(D, ID = 1, where_to = 5, which_ca = 1)
#' D$ca

ca_move <- function(D, ID, where_to, which_ca = 1){
  # CAを特定する
  if(is.numeric(which_ca)){
    ca_namelist <- names(D$ca)[!names(D$ca) %in% c(".__enclos_env__","clone","print")]
    which_ca <- ca_namelist[which_ca]
  }

  ## 対象とするIDのあるアドレスを特定
  loc <- which(D$ca[[which_ca]]==ID, arr.ind = T)

  ## 移動先の値を置き換える
  if(D$ca[[which_ca]][where_to]!=0){
    # すでに他のエージェントがいる場合には、もとの場所にとどまる
    message("The destination is already occupied by another agent. Hence, the agent remains to the original place.")
  }else{
    # 移動可能な場合には移動し、もとの場所の情報を消す
    D$ca[[which_ca]][where_to] <- ID
    D$ca[[which_ca]][loc] <- 0
  }
}

