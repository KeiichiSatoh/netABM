
ca_neib <- function(
    D = NULL,
    ID = NULL,
    loc = NULL,
    which_ca = 1,
    CA = NULL,
    neib_type = "Moore",
    torus = FALSE,
    attr = NULL,
    FUN = NULL,
    ...){
  ## CAがそのまま定義されていればそちらを取得
  ## そうでない場合にはDの指定から取得
  if(is.null(CA)){
    ## CAの名前を取得し、対象とするCAを特定する
    if(is.numeric(which_ca)){
      ca_namelist <- names(D$ca)[!names(D$ca) %in% c(".__enclos_env__","clone","print")]
      which_ca <- ca_namelist[which_ca]
    }
    CA <- D$ca[[which_ca]]
  }

  # locが直接入力されていればそれを採用
  # そうでなければIDのいるアドレスを特定
  if(is.null(loc)){
    loc <- which(CA==ID, arr.ind = TRUE)
  }

  ## CAのdimを取得
  CA_dim <- dim(CA)

  ## torusに対応させてCAをアップデートする
  if(torus){
    # 行に関して
    if(loc[1]==1){
      CA <- CA[c(CA_dim[1],(1:(CA_dim[1]-1))),]
      loc[1] <- loc[1] + 1
    }else if(loc[1]==CA_dim[1]){
      CA <- CA[c((2:(CA_dim[1])),1),]
      loc[1] <- loc[1] - 1
    }
    # 列に関して
    if(loc[2]==1){
      CA <- CA[ ,c(CA_dim[1],(1:(CA_dim[1]-1)))]
      loc[2] <- loc[2] + 1
    }else if(loc[2]==CA_dim[2]){
      CA <- CA[ ,c((2:(CA_dim[1])),1)]
      loc[2] <- loc[2] - 1
    }
    row_min <- loc[1] - 1
    row_max <- loc[1] + 1
    col_min <- loc[2] - 1
    col_max <- loc[2] + 1
  }else{
    # 取得範囲を決定する
    row_min <- ifelse(loc[1]==1, 1, loc[1]-1)
    row_max <- ifelse(loc[1]==CA_dim[1], CA_dim[1], loc[1]+1)
    col_min <- ifelse(loc[2]==1, 1, loc[2]-1)
    col_max <- ifelse(loc[2]==CA_dim[2], CA_dim[2], loc[2]+1)
   }

  ## 回りのIDを取得する
  neib_ID <- switch(neib_type,
                    "Moore" = {
                      neib_ID <- as.vector(CA[row_min:row_max, col_min:col_max])
                      neib_ID <- setdiff(neib_ID, c(0, ID))
                      neib_ID
                    },
                    "Neumann" = {
                      neib_ID_col <- as.vector(CA[row_min:row_max, loc[2]])
                      neib_ID_row <- as.vector(CA[loc[1]         , col_min:col_max])
                      neib_ID <- setdiff(c(neib_ID_col,neib_ID_row), c(0, ID))
                      neib_ID
                    },
                    stop("No neib_type option found.")
  )

  # 何も周りにいない場合にはNULLを返す
  if(is_empty(neib_ID)){
    return(NULL)
  }

  # attrがない場合にはneib_IDを返す
  if(is.null(attr)){
    return(neib_ID)
  }else{
    # neib_attrを取得する
    neib <- D$agent[neib_ID]
    neib_attr <- data.frame(t(data.frame(lapply(1:length(neib), function(i){
      unlist(neib[[i]]$a[attr])
    }))))
    rownames(neib_attr) <- neib_ID

    if(is.null(FUN)){
      # FUNがない場合にはneib_attrを返す
      return(neib_attr)
    }else{
      ## 隣人のattributeからさらに計算を行う
      calc_out <- apply(neib_attr, MARGIN = 2, FUN = FUN, ...)
      ## calc_outを戻す
      calc_out
    }
  }
}
