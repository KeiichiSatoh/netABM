#'
#' get_ca_by_attr(D)
#' get_ca_by_attr(D, from_log = TRUE)
#' get_ca_by_attr(CA = D$ca$room, D = D)
#' get_ca_by_attr(CA = D$ca$room, attr_vec_list = list(c("F","F","F","J","J")))

get_ca_by_attr <- memoise::memoise(
  function(
    D = NULL, which_ca = 1, which_attr = 1, from_log = FALSE,
    CA = NULL, attr_list = NULL, exclude_value = 0){
  # CAが直接投入されている場合にはそちらを利用
  # そうでない場合にはDとwhich_caから取得
  if(is.null(CA)){
    # Dが必要となるため、チェックをかける
    stopifnot("D must be supplied, if you do not supply CA directly." = is.null(D)==FALSE)
    # CAを特定する
    if(is.numeric(which_ca)){
      ca_namelist <- names(D$ca)[!names(D$ca) %in% c(".__enclos_env__","clone","print")]
      which_ca <- ca_namelist[which_ca]
    }
    if(from_log == FALSE){
      # from_log==TRUE:現状のデータからCAを構成
      CA <- D$ca[[which_ca]]
    }else{
      # from_log==FALSE:logからCAを構成
      CA <- lapply(1:length(D$log), function(t){
        D$log[[t]]$ca[[which_ca]]
      })
    }
  }

  # すべてのCAをリスト形式に揃える
  if(is.list(CA)==FALSE){
    CA <- list(CA)
  }

  # attr
  if(!is.null(attr_list)){
    # attr_vecが投入されている場合にはそちらを使用
    stopifnot("attr_list must be a list. If you want to supply a vector, set imput as list(vector)."=is.list(attr_list))
    for(t in 1:length(attr_list)){
      if(is.vector(attr_list[[t]])==FALSE){
        attr_list[[t]] <- attr_list[[t]][[which_attr]]
      }
    }
  }else{
    stopifnot("D must be supplied, if you do not supply attr_list directly." = is.null(D)==FALSE)
    # それ以外の場合にはwhich_attrから取得
    if(from_log==FALSE){
      # logから取らない場合
      attr_list <- list(unlist(lapply(D$agent, function(X){X$a[[which_attr]]})))
    }else{
      # logからとる場合
      attr_list <- lapply(D$log, function(X){
        unlist(lapply(X$agent, function(Y){Y$a[[which_attr]]}))
      })
    }
  }

  # CAとattr_vec_listの次元が会うかどうかをテスト
  stopifnot("The length of CA and attr_vec_list does not match." = length(CA)==length(attr_list))

  # CAのIDをattr_vec_listに置き換える
  out <- mapply(function(X, Y){
    X_dim <- dim(X)
    X_vec <- as.vector(X)
    X_vec[X_vec %in% exclude_value] <- NA
    array(Y[X_vec], dim = X_dim)
  }, X = CA, Y = attr_list, SIMPLIFY = FALSE)

  # アウトプットのリスト数が1ならば、リストを外す
  if(length(out)==1){
    out <- out[[1]]
  }
  # アウトプット
  out
}, cache = cachem::cache_mem(max_age = 900))
