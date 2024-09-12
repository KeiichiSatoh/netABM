
#---------------------------------------------------------
# .shape_agent_attr
#---------------------------------------------------------
.shape_agent_attr <- function(agent_attr_sbs = NULL, agent_n){
  # substituteされているオブジェクト名
  obs_name <- deparse(agent_attr_sbs)
  # substituteを評価して中身を取り出す
  agent_attr <- eval(agent_attr_sbs)
  # data.frameに変換する
  if(is.null(agent_attr)){
    return(NULL)
  }else{
    agent_attr <- as.data.frame(agent_attr)
    if(colnames(agent_attr)[1]=="agent_attr"){
      colnames(agent_attr) <- obs_name
    }
  }
  # agent_nとの整合性をチェック
  stopifnot("Number of the rows and agent_n does not match" = NROW(agent_attr)==agent_n)
  # アウトプット
  agent_attr
}


#---------------------------------------------------------
# .shape_net
#---------------------------------------------------------

#' @import Matrix

.shape_net <- function(net_sbs = NULL, agent_n = NULL){
  # ひとまずnet_labelにNULLを設定
  net_label <- NULL

  # net_sbsがNULLかどうかを判定
  if(is.null(net_sbs)){
    return(NULL)
  }else if(is.symbol(net_sbs)){
    # symbolかどうか
    net_label <- deparse(net_sbs)
    net <- eval(net_sbs)
  }else{
    # それ以外
    net <- eval(net_sbs)
  }

  # 数字のみの場合には、その個数分のマトリクスを作成するトリクスを作る
  if(class(net)[1]=="numeric"){
    net_list <- lapply(1:net, function(X){matrix(0, nrow = agent_n, ncol = agent_n, dimnames = list(1:agent_n, 1:agent_n))})
  }else if(is.list(net)){
  # NULLでない場合、リストの場合にはそのまま
    net_list <- net
  }else{
    # リストではない場合には、リストとして括る
    net_list <- list(net)
  }

  # 名前を処理する
  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(net_list))){
    ## netラベルはありの場合
    if(!is.null(net_label)){
      ### netラベルの長さがlistの長さと同じ
      if(length(net_list)==length(net_label)){
        names(net_list) <- net_label
      }else{
        ### netラベルの長さがlistの長さと異なる
        names(net_list) <- paste0(net_label, 1:length(net_list))
      }
      ## netラベルもない場合
    }else{
      ### net_listの長さが1
      if(length(net_list)==1){
        names(net_list) <- "net"
      }else{
        ### net_listの長さが1より大きい
        names(net_list) <- paste0("net", 1:length(net_list))
      }
    }
  }

  # リストの要素ごとに処理を変える
  for(i in 1:length(net_list)){
    switch(class(net_list[[i]])[1],
           "numeric" = {
             temp_net <- matrix(net_list[[i]][1], agent_n, agent_n,
                                     dimnames = list(1:agent_n, 1:agent_n))
             net_list[[i]] <- temp_net
             },
           "matrix" = {
             stopifnot("Number of row and the number of agent_n does not match" = NROW(net_list[[i]])==agent_n)
             temp_net <- net_list[[i]]
             if(is.null(dimnames(temp_net))){
               dimnames(temp_net) <- list(1:agent_n, 1:agent_n)
              }
             net_list[[i]] <- temp_net
             },
           "array" = {
             net_list[[i]] <- net_list[[i]]
             },
           stop("net must be either a matrix or list of them.")
           )
  }

  # sparseMatrixに要素を変更する
  # field_typeを定義する
  for(m in 1:length(net_list)){
    if(is.matrix(net_list[[m]])){
      net_list[[m]] <- as(net_list[[m]], "sparseMatrix")
    }
    attr(net_list[[m]], "field_type") <- "net"
  }

  # アウトプット
  net_list
}


#---------------------------------------------------------
# .shape_ca
#---------------------------------------------------------

#' @import Matrix

.shape_ca <- function(ca_sbs = NULL, agent_n = NULL){
  # ひとまずca_labelにNULLを設定
  ca_label <- NULL

  # ca_sbsがNULLかどうかを判定
  if(is.null(ca_sbs)){
    return(NULL)
  }else if(is.symbol(ca_sbs)){
  # symbolかどうか
    ca_label <- deparse(ca_sbs)
    ca <- eval(ca_sbs)
  }else{
  # それ以外
    ca <- eval(ca_sbs)
  }

  # 数字のみの場合には、その個数分のマトリクスを作成するトリクスを作る
  # エージェントはランダムに分布させる
  if(class(ca)[1]=="numeric"){
    ca_list <- vector("list", ca)
    ca_dim <- ceiling(sqrt(agent_n + 1))
    for(m in 1:length(ca_list)){
      ca_list[[m]] <- matrix(sample(c(1:agent_n, rep(0, ca_dim*ca_dim - agent_n))),
                             nrow = ca_dim, ncol = ca_dim)}
  }else if(is.list(ca)){
  # リストの場合にはそのまま
    ca_list <- ca
  }else{
    # リストではない場合には、リストとして括る
    ca_list <- list(ca)
    names(ca_list) <- names(ca)
  }

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(ca_list))){
    ## caラベルはありの場合
    if(!is.null(ca_label)){
      ### caラベルの長さがlistの長さと同じ
      if(length(ca_list)==length(ca_label)){
        names(ca_list) <- ca_label
      }else{
      ### caラベルの長さがlistの長さと異なる
        names(ca_list) <- paste0(ca_label, 1:length(ca_list))
      }
    ## caラベルもない場合
    }else{
      ### ca_listの長さが1
      if(length(ca_list)==1){
        names(ca_list) <- "ca"
      }else{
      ### ca_listの長さが1より大きい
        names(ca_list) <- paste0("ca", 1:length(ca_list))
      }
    }
  }

  # リストの要素ごとに処理を変える
  for(i in 1:length(ca_list)){
    switch(class(ca_list[[i]])[1],
           "numeric" = {
             ca_dim <- ca_list[[i]]
             stopifnot("Number of cells in the ca is smaller than agent_n" = ca_dim*ca_dim >= agent_n)
             temp_ca <- matrix(sample(c(1:agent_n, rep(0, ca_dim*ca_dim - agent_n))),
                               nrow = ca_dim, ncol = ca_dim)
             ca_list[[i]] <- temp_ca
           },
           "matrix" = {
             ca_list[[i]] <- ca_list[[i]]
           },
           "array" = {
             ca_list[[i]] <- ca_list[[i]]
           },
           stop("ca must be either a matrix or list of them.")
    )
  }

  # sparseMatrixに要素を変更する
  # field_typeを定義する
  for(m in 1:length(ca_list)){
    if(is.matrix(ca_list[[m]])){
      ca_list[[m]] <- as(ca_list[[m]], "sparseMatrix")
    }
    attr(ca_list[[m]], "field_type") <- "ca"
  }

  # アウトプット
  ca_list
}

#-------------------------------------------------------------------------------
# .shape_euc
#-------------------------------------------------------------------------------

.shape_euc <- function(euc_sbs = NULL, agent_n = NULL){
  # ひとまずeuc_labelにNULLを設定
  euc_label <- NULL

  # euc_sbsがNULLかどうかを判定
  if(is.null(euc_sbs)){
    return(NULL)
  }else if(is.symbol(euc_sbs)){
    # symbolかどうか
    euc_label <- deparse(euc_sbs)
    euc <- eval(euc_sbs)
  }else{
    # それ以外
    euc <- eval(euc_sbs)
  }

  # 数字のみの場合には、その個数分のdata.frameを作成する
  if(class(euc)[1]=="numeric"){
    euc_list <- lapply(1:euc, function(X){
      data.frame(x = rnorm(n = agent_n, mean = 0, sd = 1),
                 y = rnorm(n = agent_n, mean = 0, sd = 1))})
  }else if(is.data.frame(euc)){
    euc_list <- list(euc)
  }else if(is.list(euc)){
    euc_list <- euc
  }else{
  # リストではない場合には、リストとして括る
    euc_list <- list(euc)
  }

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(euc_list))){
    ## eucラベルはありの場合
    if(!is.null(euc_label)){
      ## eucラベルの長さがlistの長さと同じ
      if(length(euc_list)==length(euc_label)){
        names(euc_list) <- euc_label
      }else{
        ### eucラベルの長さがlistの長さと異なる
        names(euc_list) <- paste0(euc_label, 1:length(euc_list))
      }
      ## eucラベルもない場合
    }else{
      ### euc_listの長さが1
      if(length(euc_list)==1){
        names(euc_list) <- "euc"
      }else{
        ### euc_listの長さが1より大きい
        names(euc_list) <- paste0("euc", 1:length(euc_list))
      }
    }
  }

  # リストの要素ごとに処理を変える
  for(i in 1:length(euc_list)){
    switch(class(euc_list[[i]])[1],
           "numeric" = {
             vec <- lapply(1:euc_list[[i]], function(X){rnorm(n = agent_n, mean = 0, sd = 1)})
             temp_euc <- data.frame(vec)
             if(euc_list[[i]]<=3){
               colnames(temp_euc) <- LETTERS[24: (23 + euc_list[[i]])]
             }else{
               colnames(temp_euc) <- paste0("X", 1:length(euc_list[[i]]))
             }
             euc_list[[i]] <- temp_euc
           },
           "matrix" = {
             temp_euc <- data.frame(euc_list[[i]])
             if(is.null(names(temp_euc))){
               if(ncol(temp_euc)<=3){
                 colnames(temp_euc) <- LETTERS[24:(23 + ncol(temp_euc))]
               }else{
                 colnames(temp_euc) <- paste0("X", 1:ncol(temp_euc))
               }
             }
             stopifnot("Number of the row does not match the number of agent_n" = NROW(temp_euc)==agent_n)
             euc_list[[i]] <- temp_euc
           },
           "data.frame" = {
             temp_euc <- euc_list[[i]]
             if(is.null(names(temp_euc))){
               if(ncol(temp_euc)<=3){
                 colnames(temp_euc) <- LETTERS[24:(23 + ncol(temp_euc))]
               }else{
                 colnames(temp_euc) <- paste0("X", 1:ncol(temp_euc))
               }
             }
             stopifnot("Number of the row does not match the number of agent_n" = NROW(temp_euc)==agent_n)
           },
           stop("euc must be either a data.frame or list of them.")
    )
  }

  # field_typeを入れる
  for(m in 1:length(euc_list)){
    attr(euc_list[[m]], "field_type") <- "euc"
  }

  # アウトプット
  euc_list
}



#-------------------------------------------------------------------------------
# .shape_other_field
#-------------------------------------------------------------------------------

.shape_other_field <- function(other_field_sbs = NULL){
  # ひとまずother_field_labelにNULLを設定
  other_field_label <- NULL

  # other_field_sbsがNULLかどうかを判定
  if(is.null(other_field_sbs)){
    return(NULL)
  }else if(is.symbol(other_field_sbs)){
    # symbolかどうか
    other_field_label <- deparse(other_field_sbs)
    other_field <- eval(euc_sbs)
  }else{
    # それ以外
    other_field <- eval(other_field_sbs)
  }

  # リストに揃える
  if(!is.list(other_field)){
    other_field_list <- list(other_field)
  }else{
    other_field_list <- other_field
  }

  # namesをつける
  # listに名前がついていない場合
  if(is.null(names(other_field_list))){
    ## other_fieldラベルはありの場合
    if(!is.null(other_field_label)){
      ## other_fieldラベルの長さがlistの長さと同じ
      if(length(other_field_list)==length(other_field_label)){
        names(other_field_list) <- other_field_label
      }else{
        ### other_fieldラベルの長さがlistの長さと異なる
        names(other_field_list) <- paste0(other_field_label, 1:length(other_field_list))
      }
      ## other_fieldラベルもない場合
    }else{
      ### other_field_listの長さが1
      if(length(other_field_list)==1){
        names(other_field_list) <- "field"
      }else{
        ### euc_listの長さが1より大きい
        names(other_field_list) <- paste0("field", 1:length(other_field_list))
      }
    }
  }

  # matrixはsparseMatrixに置き換える
  # field_typeを設定する
  for(m in 1:length(other_field_list)){
    if(is.matrix(other_field_list[[m]])){
      other_field_list[[m]] <- as(other_field_list[[m]], "sparseMatrix")
    }
    attr(other_field_list[[m]], "field_type") <- "other"
  }
  # return
  other_field_list
}


#--------------------------------------------------
# .shape_agent_f改善版
#--------------------------------------------------

.shape_agent_f <- function(agent_f_sbs, agent_n){
  ### NULLやNAが単体で投入された場合
  if(is.null(agent_f_sbs)){
    agent_f_label <- rep("NULL", agent_n)
  }else{
    temp_label <- as.character(agent_f_sbs)
    #### agent_fの要素の数ごとに：人数分コピーする
    if(temp_label[1]=="list"){
      # リストの場合listをまずは外す
      agent_f_label <- temp_label[-1]
      ### もしもlistに挟まれているが1個しか入っていない場合agent_n分コピー
      if(length(agent_f_label)==1){
        agent_f_label <- rep(agent_f_label, agent_n)
      }
      ### それ以外の場合には、agent_n分、agent_fが入っている必要があるので、確認
      stopifnot("The number of functions within a list should correspond to the number of agents." = length(agent_f_label)==agent_n)
    }else if(any(length(temp_label)==0|is.na(temp_label))){
      # NULL/NAの場合
      agent_f_label <- rep("NULL", agent_n)
    }else{
      # fに単体で設定されているものをn個分コピー
      agent_f_label <- rep(deparse(agent_f_sbs, width.cutoff = 500), agent_n)
    }
  }

  ### 各agentごとにfunctionを作るようにまずは空のリストを作成
  agent_f_list <- vector("list", agent_n)
  names(agent_f_list) <- agent_f_label
  ### 各agentごとにfunctionを付与する
  for(i in 1:agent_n){
    # NULL/NAの場合
    if(agent_f_label[i] %in% c("NA", "NULL")){
      agent_f_list[[i]] <- f_nothing
    }else if(exists(agent_f_label[i])){
      # Objectは存在している場合
      retrieved_object <- get(agent_f_label[i])
      # 取得されたのがfunctionの場合
      if(is.function(retrieved_object)){
        agent_f_list[[i]] <- retrieved_object
      }else if(is.call(retrieved_object)){
        # 取得されたのがcallの場合
        func_name <- rlang::call_name(retrieved_object)
        func_args <- rlang::call_args(retrieved_object)
        assign("temp_func", get(func_name))
        # 元の関数のデフォルト値を新しい値に置き換える
        if(length(func_args) > 0){
          for(k in 1:length(func_args)){
            formals(temp_func)[names(func_args[k])] <- func_args[k]
          }
        }
        # 新しい関数を貼り付ける
        agent_f_list[[i]] <- temp_func
      }else{
        agent_f_list[[i]] <- get(agent_f_label[i])
        warnings("The content within agent_f list seems to be not a function. Please check.")
      }
    }else{
      # objectが存在しない場合
      # 文字列から元の関数名と指定されているargを取り出す
      parsed_expr <- rlang::parse_expr(agent_f_label[i])
      func_name <- rlang::call_name(parsed_expr)
      func_args <- rlang::call_args(parsed_expr)
      assign("temp_func", get(func_name))
      # 元の関数のデフォルト値を新しい値に置き換える
      if(length(func_args)>0){
        for(k in 1:length(func_args)){
          formals(temp_func)[names(func_args[k])] <- func_args[k]
        }
      }
      # 新しい関数を貼り付ける
      agent_f_list[[i]] <- temp_func
    }
  }
  ### すべてのact_listがfunction型か確認
  stopifnot("All agent_f objects need to be function." = all(unlist(lapply(agent_f_list, is.function))))
  ### すべての関数がDを引数に取るようにする
  for(i in 1:length(agent_f_list)){
    current_formals <- formals(agent_f_list[[i]])
    # すでにユーザーが誤ってDを引数に書いていたらひとまず消す
    current_formals[which(names(current_formals)=="D")] <- NULL
    formals(agent_f_list[[i]]) <- c(alist(D = D), current_formals)
  }
  ### アウトプット
  agent_f_list
}

#--------------------------------------------------
# .modify_R6_field
#--------------------------------------------------
.modify_R6_field <- function(R6object, field = NULL){
  if(is.null(field)){
    return(NULL)
  }
  field_rev <- field[rev(names(field))]
  list2env(field_rev, envir = R6object$.__enclos_env__$self)
  invisible()
}

#---------------------------------------------------
# .generator_D
#---------------------------------------------------
.generator_D <- function(field = NULL, time = 1, log = NULL, notes = NULL){
  D <- ABM_D$new()
  if(!is.null(notes)){
    .modify_R6_field(R6object = D, field = list(notes = NULL))
    D$notes <- notes
  }
  .modify_R6_field(R6object = D, field = list(log = log))
  .modify_R6_field(R6object = D, field = list(time = time))
  if(!is.null(field)){
    .modify_R6_field(R6object = D, field = field)
  }
  .modify_R6_field(R6object = D, field = list(agent = NULL))
  D
}

#-------------------------------------------------
# .generator_Agent
#-------------------------------------------------
.generator_Agent <- function(agent_n, agent_attr = NULL, agent_f = NULL){
  # agentを生成する
  agent <- vector("list", agent_n)
  names(agent) <- paste0("ID", 1:agent_n)
  for(i in 1:agent_n){
    agent[[i]] <- ABM_Agent$new()
  }

  # agent_fを修正する
  if(!is.null(agent_f)){
    for(i in 1:agent_n){
      agent[[i]]$.f <- agent_f[[i]]
      environment(agent[[i]][[".f"]]) <- agent[[i]]$.__enclos_env__
      .modify_R6_field(R6object = agent[[i]], field = list(f_label = names(agent_f)[i]))
    }
  }

  # attributeを貼り付ける
  if(!is.null(agent_attr)){
    if(ncol(agent_attr)==1){
      for(i in 1:agent_n){
        temp <- as.list(agent_attr[i, ])
        names(temp) <- colnames(agent_attr)
        .modify_R6_field(R6object = agent[[i]], field = temp)
      }
    }else{
      for(i in 1:agent_n){
        .modify_R6_field(R6object = agent[[i]], field = agent_attr[i, ])
      }
    }
  }

  # IDをつける
  for(i in 1:agent_n){
    .modify_R6_field(R6object = agent[[i]], field = list(ID = i))
  }
  # リターン
  agent
}

#-------------------------------------------------------------------------------
# .insert_line_to_function
#-------------------------------------------------------------------------------

.insert_line_to_function <- function(FUN, expr = expression(self <- self), after=1) {
  body(FUN)<-as.call(append(as.list(body(FUN)), expr, after=after))
  FUN
}
