################################################################################
# .shape_agent
################################################################################

.shape_agent <- function(agents_sbs){
  # NULLかどうかを判定
  if(is.null(agents_sbs)){
    return(NULL)
  }

  # evalで解凍する
  agents <- eval(agents_sbs)

  # オブジェクトラベル
  if(is.symbol(agents_sbs)){
    agents_label <- deparse(agents_sbs)
  }else{
    agents_label <- "agents"
  }

  # classを確認し、番号の場合にはagentを作成する
  agents <- if(!all(lapply(unlist(agents), function(x){class(x)[[1]]})=="ABM_Agent")){
    ## "ABM_Agent" classではない場合
    lapply(unlist(agents), function(y){
      if(is.numeric(y)){
        init_agent(agent_n = y)
      }else{
        stop("agents must be either ABM_Agent class object or a positive integer.")
      }
    })
  }else{
    agents
  }

  # インプットが複数のエージェントタイプを含むものかで場合分け
  if(is.list(agents[[1]])==FALSE){
    agent_formatted <- list(agents)
  }else{
    agent_formatted <- agents
  }

  # 名前がついているかを確認
  if(is.null(names(agent_formatted))){
    ## 名前がついていない場合
    if(length(agent_formatted)==1){
      names(agent_formatted) <- agents_label
    }else{
      names(agent_formatted) <- paste0(agents_label, 1:length(agent_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(agent_formatted)=="")){
    stop("Put names to each agent object.")
  }

  # field_typeを定義する
  for(m in 1:length(agent_formatted)){
    attr(agent_formatted[[m]], "field_type") <- "agent"
  }

  # アウトプット
  agent_formatted
}


#---------------------------------------------------------
# .shape_net
#---------------------------------------------------------

#' @import rlang
.shape_net <- function(net_sbs = NULL){
  # NULLかどうかを判定
  if(is.null(net_sbs)){
    return(NULL)
  }

  # sbsを解凍する
  net <- eval(net_sbs)

  # オブジェクトラベル
  if(is.symbol(net_sbs)){
    net_label <- deparse(net_sbs)
  }else{
    net_label <- "net"
  }

  # リスト形式にすべて統一する
  if(!is.list(net)){
    net <- list(net)
  }

  # 要素ごとに操作を変える
  net_formatted <- lapply(net, function(X){
    if(is.matrix(X)){
      if(is.null(dimnames(X))){
        message("Warning: The object provided in the 'net' argument of setABM lacks dimension names, which may cause issues when running the ABM.")
      }
      X
    }else if(rlang::is_scalar_atomic(X)){
      temp_mat <- matrix(0, X, X)
      dimnames(temp_mat) <- list(1:X, 1:X)
      temp_mat
    }else{
      stop("net must be either matrix or an integer or list of them.")
    }
  })

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(net_formatted))){
    if(length(net_formatted)==1){
      names(net_formatted) <- net_label
    }else{
      names(net_formatted) <- paste0(net_label, 1:length(net_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(net_formatted)=="")){
    stop("Put names to each net object.")
  }

  # field_typeを定義する
  for(m in 1:length(net_formatted)){
    attr(net_formatted[[m]], "field_type") <- "net"
  }

  # アウトプット
  net_formatted
}

#---------------------------------------------------------
# .shape_mat
#---------------------------------------------------------

#' @import rlang
#' @import Matrix

.shape_mat <- function(mat_sbs = NULL){
  # NULLかどうかを判定
  if(is.null(mat_sbs)){
    return(NULL)
  }

  # sbsを解凍する
  mat <- eval(mat_sbs)

  # オブジェクトラベル
  if(is.symbol(mat_sbs)){
    mat_label <- deparse(mat_sbs)
  }else{
    mat_label <- "mat"
  }

  # リスト形式にすべて統一する
  if(!is.list(mat)){
    mat <- list(mat)
  }

  # 要素ごとに操作を変える
  mat_formatted <- lapply(mat, function(X){
    if(is.matrix(X)|is.array(X)){
      X
    }else if(rlang::is_scalar_atomic(X)){
      temp_mat <- matrix(0, X, X)
      dimnames(temp_mat) <- list(1:X, 1:X)
      temp_mat
    }else{
      stop("net must be either, array, matrix or an integer or list of them.")
    }
  })

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(mat_formatted))){
    if(length(mat_formatted)==1){
      names(mat_formatted) <- mat_label
    }else{
      names(mat_formatted) <- paste0(mat_label, 1:length(mat_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(mat_formatted)=="")){
    stop("Put names to each mat object.")
  }

  # field_typeを定義する
  for(m in 1:length(mat_formatted)){
    if(is.matrix(mat_formatted[[m]])){
      mat_formatted[[m]] <- as(mat_formatted[[m]], "sparseMatrix")
    }
    attr(mat_formatted[[m]], "field_type") <- "mat"
  }

  # アウトプット
  mat_formatted
}


#---------------------------------------------------------
# .shape_euc
#---------------------------------------------------------

#' @import rlang
.shape_euc <- function(euc_sbs = NULL){
  # NULLかどうかを判定
  if(is.null(euc_sbs)){
    return(NULL)
  }

  # sbsを解凍する
  euc <- eval(euc_sbs)

  # オブジェクトラベル
  if(is.symbol(euc_sbs)){
    euc_label <- deparse(euc_sbs)
  }else{
    euc_label <- "euc"
  }

  # リスト形式にすべて統一する
  if(is.data.frame(euc)){
    euc <- list(euc)
  }else if(!is.list(euc)){
    euc <- list(euc)
  }

  # 要素ごとに操作を変える
  euc_formatted <- lapply(euc, function(X){
    if(is.data.frame(X)){
      if(is.null(colnames(X))){
        message("Warning: colnames lack in euc, which can cause problems when running the ABM.")
      }
      X
    }else if(rlang::is_scalar_atomic(X)){
      data.frame(x = rnorm(n = X, mean = 0, sd = 1),
                 y = rnorm(n = X, mean = 0, sd = 1))
    }else{
      stop("euc must be a data.frame or list of them.")
    }
  }
  )

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(euc_formatted))){
    if(length(euc_formatted)==1){
      names(euc_formatted) <- euc_label
    }else{
      names(euc_formatted) <- paste0(euc_label, 1:length(euc_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(euc_formatted)=="")){
    stop("Put names to each euc object.")
  }

  # field_typeを定義する
  for(m in 1:length(euc_formatted)){
    attr(euc_formatted[[m]], "field_type") <- "euc"
  }

  # アウトプット
  euc_formatted
}

#---------------------------------------------------------
# .shape_df
#---------------------------------------------------------

#' @import rlang
.shape_df <- function(df_sbs = NULL){
  # NULLかどうかを判定
  if(is.null(df_sbs)){
    return(NULL)
  }

  # sbsを解凍する
  df <- eval(df_sbs)

  # オブジェクトラベル
  if(is.symbol(df_sbs)){
    df_label <- deparse(df_sbs)
  }else{
    df_label <- "df"
  }

  # リスト形式にすべて統一する
  if(is.data.frame(df)){
    df <- list(df)
  }else if(!is.list(df)){
    df <- list(df)
  }

  # 要素ごとに操作を変える
  df_formatted <- lapply(df, function(X){
    if(is.data.frame(X)){
      if(is.null(colnames(X))){
        message("Warning: colnames lack in df, which can cause problems when running the ABM.")
      }
      X
    }else if(rlang::is_scalar_atomic(df)){
      data.frame(x = rnorm(n = X, mean = 0, sd = 1))
    }else{
      stop("df must be a data.frame or list of them.")
    }
  }
  )

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(df_formatted))){
    if(length(df_formatted)==1){
      names(df_formatted) <- df_label
    }else{
      names(df_formatted) <- paste0(df_label, 1:length(df_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(df_formatted)=="")){
    stop("Put names to each df object.")
  }

  # field_typeを定義する
  for(m in 1:length(df_formatted)){
    attr(df_formatted[[m]], "field_type") <- "df"
  }

  # アウトプット
  df_formatted
}

#---------------------------------------------------------
# .shape_other
#---------------------------------------------------------

#' @import rlang
.shape_other <- function(other_sbs = NULL){
  # NULLかどうかを判定
  if(is.null(other_sbs)){
    return(NULL)
  }

  # sbsを解凍する
  other <- eval(other_sbs)

  # オブジェクトラベル
  if(is.symbol(other_sbs)){
    other_label <- deparse(other_sbs)
  }else{
    other_label <- "other"
  }

  # リスト形式にすべて統一する
  if(!is.list(other)){
    other <- list(other)
  }
  other_formatted <- other

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(other_formatted))){
    if(length(other_formatted)==1){
      names(other_formatted) <- other_label
    }else{
      names(other_formatted) <- paste0(other_label, 1:length(other_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(other_formatted)=="")){
    stop("Put names to each other_field object.")
  }

  # field_typeを定義する
  for(m in 1:length(other_formatted)){
    attr(other_formatted[[m]], "field_type") <- "other"
  }

  # アウトプット
  other_formatted
}



#---------------------------------------------------------
# .shape_active_binding_field
#---------------------------------------------------------

#' @import rlang
.shape_active_binding_field <- function(active_binding_field_sbs = NULL){
  # NULLかどうかを判定
  if(is.null(active_binding_field_sbs)){
    return(NULL)
  }

  # sbsを解凍する
  active_binding_field <- eval(active_binding_field_sbs)

  # オブジェクトラベル
  if(is.symbol(active_binding_field_sbs)){
    active_binding_field_label <- deparse(active_binding_field_sbs)
  }else{
    active_binding_field_label <- "active_binding"
  }

  # リスト形式にすべて統一する
  if(!is.list(active_binding_field)){
    active_binding_field <- list(active_binding_field)
  }
  active_binding_field_formatted <- active_binding_field

  ## すべての形をfunction型に揃える
  for(i in 1:length(active_binding_field_formatted)){
    if(is.character(active_binding_field_formatted[[i]])){
      parsed_FUN <- parse(text = active_binding_field_formatted[[i]])[[1]]
      if(is.name(parsed_FUN)){
        retrieved_FUN <- get(parsed_FUN)
        stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
        active_binding_field_formatted[[i]] <- retrieved_FUN
      }else if(is.call(parsed_FUN)){
        retrieved_FUN <- get(call_name(parsed_FUN))
        stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
        formals(retrieved_FUN) <- call_args(parsed_FUN)
        active_binding_field_formatted[[i]] <- retrieved_FUN
      }
    }
  }

  # すべて関数型になっているか確認
  stopifnot("active_binding_field must be a function." = all(unlist(lapply(active_binding_field_formatted, is.function))))

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(active_binding_field_formatted))){
    if(length(active_binding_field_formatted)==1){
      names(active_binding_field_formatted) <- active_binding_field_label
    }else{
      names(active_binding_field_formatted) <- paste0(active_binding_field_label, 1:length(active_binding_field_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(active_binding_field_formatted)=="")){
    stop("Put names to each active_binding_field object.")
  }

  # アウトプット
  active_binding_field_formatted
}

#------------------------------------------------
# assign_func_envs: active_binding用
#------------------------------------------------

assign_func_envs <- function(objs, target_env) {
  if (is.null(target_env)) return(objs)

  lapply(objs, function(x) {
    if (is.function(x)) environment(x) <- target_env
    x
  })
}

#-------------------------------------------------------------------------------
# .get_FUN
#-------------------------------------------------------------------------------

#' @import rlang

.get_FUN <- function(FUN){
  # FUNが関数かclosureの場合にはそのままFUNを返す
  if(is.function(FUN)|rlang::is_closure(FUN)){
    FUN
  }else if(is.character(FUN)){
    # FUNがcharacter型の場合には当該関数を環境から取得する
    if(exists(FUN)){
      ## FUNが環境に存在するならば、そちらから取得
      FUN <- get(FUN)
      stopifnot("The retrieved object in global_FUN is not a function." = is.function(FUN))
      FUN
    }else{
      ## FUNが存在しないならば、callとして取得できないか試す
      parsed_FUN <- parse(text = FUN)[[1]]
      func_name <- rlang::call_name(parsed_FUN)
      if(exists(func_name)){
        ### func_nameが存在している場合、処理を行う
        func_args <- rlang::call_args(parsed_FUN)
        assign("temp_func", get(func_name))
        #### 元の関数のデフォルト値を新しい値に置き換える
        if(length(func_args) > 0){
          for(k in 1:length(func_args)){
            formals(temp_func)[names(func_args[k])] <- func_args[k]
          }
        }
        ### temp_funcを戻す
        temp_func
      }else{
        ### func_nameが存在しない場合
        stop("FUN does not exists in the environment.")
      }
    }
    # FUNがcharacter型の場合：ここまで----------------------
  }else{
    stop("The retrieved object is not a function.")
  }
}


#-------------------------------------------------------------------------------
# .shape_global_FUN
#-------------------------------------------------------------------------------
#' @import rlang

.shape_global_FUN <- function(global_FUN, global_FUN_sbs){
  if(is.null(global_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(global_FUN_sbs)){
    global_FUN_label <- deparse(global_FUN_sbs)
  }else{
    global_FUN_label <- "global_FUN"
  }

  # リストかどうか
  if(!is.list(global_FUN)){
    global_FUN_list <- list(global_FUN)
  }else{
    global_FUN_list <- global_FUN
  }

  # それぞれのFUNについて、functionを取得する
  global_FUN_formatted <- lapply(global_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にDを追加する
  global_FUN_formatted <- lapply(global_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってDを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="D")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(D = D, E = E), current_formals) # D=D,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(global_FUN_formatted))){
    if(length(global_FUN_formatted)==1){
      names(global_FUN_formatted) <- global_FUN_label
    }else{
      names(global_FUN_formatted) <- paste0(global_FUN_label, 1:length(global_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(global_FUN_formatted)=="")){
    stop("Put names to each global_FUN object.")
  }

  # field_typeを定義する
  for(m in 1:length(global_FUN_formatted)){
    attr(global_FUN_formatted[[m]], "field_type") <- "global_FUN"
  }

  # アウトプット
  global_FUN_formatted
}


#-------------------------------------------------------------------------------
# .shape_select_FUN
#-------------------------------------------------------------------------------
#' @import rlang

.shape_select_FUN <- function(select_FUN, select_FUN_sbs){
  if(is.null(select_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(select_FUN_sbs)){
    select_FUN_label <- deparse(select_FUN_sbs)
  }else{
    select_FUN_label <- "select_FUN"
  }

  # リストかどうか
  if(!is.list(select_FUN)){
    select_FUN_list <- list(select_FUN)
  }else{
    select_FUN_list <- select_FUN
  }

  # それぞれのFUNについて、functionを取得する
  select_FUN_formatted <- lapply(select_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にDを追加する
  select_FUN_formatted <- lapply(select_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってDを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="D")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(D = D, E = E), current_formals) # D=D,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(select_FUN_formatted))){
    if(length(select_FUN_formatted)==1){
      names(select_FUN_formatted) <- select_FUN_label
    }else{
      names(select_FUN_formatted) <- paste0(select_FUN_label, 1:length(select_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(select_FUN_formatted)=="")){
    stop("Put names to each select_FUN object.")
  }

  # field_typeを定義する
  for(m in 1:length(select_FUN_formatted)){
    attr(select_FUN_formatted[[m]], "field_type") <- "select_FUN"
  }

  # アウトプット
  select_FUN_formatted
}

#-------------------------------------------------------------------------------
# .shape_stop_FUN
#-------------------------------------------------------------------------------

.shape_stop_FUN <- function(stop_FUN, stop_FUN_sbs){
  if(is.null(stop_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(stop_FUN_sbs)){
    stop_FUN_label <- deparse(stop_FUN_sbs)
  }else{
    stop_FUN_label <- "stop_FUN"
  }

  # リストかどうか
  if(!is.list(stop_FUN)){
    stop_FUN_list <- list(stop_FUN)
  }else{
    stop_FUN_list <- stop_FUN
  }

  # それぞれのFUNについて、functionを取得する
  stop_FUN_formatted <- lapply(stop_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にDを追加する
  stop_FUN_formatted <- lapply(stop_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってDを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="D")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(D = D, E = E), current_formals) # D=D,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(stop_FUN_formatted))){
    if(length(stop_FUN_formatted)==1){
      names(stop_FUN_formatted) <- stop_FUN_label
    }else{
      names(stop_FUN_formatted) <- paste0(stop_FUN_label, 1:length(stop_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(stop_FUN_formatted)=="")){
    stop("Put names to each stop_FUN object.")
  }

  # field_typeを定義する
  for(m in 1:length(stop_FUN_formatted)){
    attr(stop_FUN_formatted[[m]], "field_type") <- "stop_FUN"
  }

  # アウトプット
  stop_FUN_formatted
}

#-------------------------------------------------------------------------------
# .shape_update_FUN
#-------------------------------------------------------------------------------


.shape_update_FUN <- function(update_FUN, update_FUN_sbs){
  if(is.null(update_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(update_FUN_sbs)){
    update_FUN_label <- deparse(update_FUN_sbs)
  }else{
    update_FUN_label <- "update_FUN"
  }

  # リストかどうか
  if(!is.list(update_FUN)){
    update_FUN_list <- list(update_FUN)
  }else{
    update_FUN_list <- update_FUN
  }

  # それぞれのFUNについて、functionを取得する
  update_FUN_formatted <- lapply(update_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にDを追加する
  update_FUN_formatted <- lapply(update_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってDを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="D")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(D = D, E = E), current_formals) # D=D,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(update_FUN_formatted))){
    if(length(update_FUN_formatted)==1){
      names(update_FUN_formatted) <- update_FUN_label
    }else{
      names(update_FUN_formatted) <- paste0(update_FUN_label, 1:length(update_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(update_FUN_formatted)=="")){
    stop("Put names to each update_FUN object.")
  }

  # field_typeを定義する
  for(m in 1:length(update_FUN_formatted)){
    attr(update_FUN_formatted[[m]], "field_type") <- "update_FUN"
  }

  # アウトプット
  update_FUN_formatted
}

#-------------------------------------------------------------------------------
# .shape_partial_update_FUN_body
#-------------------------------------------------------------------------------

.shape_partial_update_FUN_body <- function(
    partial_update_FUN_body,
    partial_update_FUN_body_sbs){
  if(is.null(partial_update_FUN_body)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(partial_update_FUN_body_sbs)){
    partial_update_FUN_body_label <- deparse(partial_update_FUN_body_sbs)
  }else{
    partial_update_FUN_body_label <- "partial_update_FUN_body"
  }

  # リストかどうか
  if(!is.list(partial_update_FUN_body)){
    partial_update_FUN_body_list <- list(partial_update_FUN_body)
  }else{
    partial_update_FUN_body_list <- partial_update_FUN_body
  }

  # それぞれのリスト要素について整形し、call型に変換する
  partial_update_FUN_body_formatted <- lapply(partial_update_FUN_body_list, function(X){
    if(is.expression(X)){
      X[[1]]
    }else if(is.character(X)){
      parse(text = X)[[1]]
    }else{
      stop("partial_update_FUN_body must be either an expression, a character string, or a list of these types.")
    }
  })

  # listに名前がついていない場合
  if(is.null(names(partial_update_FUN_body_formatted))){
    if(length(partial_update_FUN_body_formatted)==1){
      names(partial_update_FUN_body_formatted) <-partial_update_FUN_body_label
    }else{
      names(partial_update_FUN_body_formatted) <- paste0(partial_update_FUN_body_label, 1:length(partial_update_FUN_body_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(partial_update_FUN_body_formatted)=="")){
    stop("Put names to each partial_update_FUN_body object.")
  }

  # field_typeを定義する
  for(m in 1:length(partial_update_FUN_body_formatted)){
    attr(partial_update_FUN_body_formatted[[m]], "field_type") <- "partial_update_FUN_body"
  }

  # アウトプット
  partial_update_FUN_body_formatted
}
