#-------------------------------------------------------------------------------
# .shape_act_FUN: 投入された関数基づいて、第一階層にアクション名、第二階層に、
# 各エージェントのアクションがネストされたリストを返す。
#
# 投入値がベクトルの場合には、各エージェントに関するものとみなす。もしも一つしかなければ、
# エージェント分コピーする。
# リストは、異なるアクションとみなす。
#
# 投入は、クロージャ―、関数、関数名の文字型、関数名の文字型に修正値が付いたものの
# 四つを受け入れる。
#
# リストの名前は、アクション名として扱う。名前がない場合には、act1, act2と順につける。
#-------------------------------------------------------------------------------


.shape_act_FUN <- function(act_FUN, act_FUN_sbs, n){
  if(is.null(act_FUN)){
    return(NULL)
  }
  # リストかどうか
  if(!is.list(act_FUN)){
    ## リストではない場合
    ## 第一階層が1、第二階層がnとなるlistを作成するようにlapplyを用いる
    out <- vector("list", 1)

    ## act_FUNの長さに応じてoutに関数の中身を貼り付ける
    if(length(act_FUN)==1){
      ### act_FUNの長さが1の場合
      out[[1]] <- lapply(1:n, function(x){.get_act_FUN(FUN = act_FUN)})
    }else if(length(act_FUN) >= 2){
      ### act_FUNの長さが2以上の場合
      stopifnot("Length of act_FUN does not match n." = length(act_FUN)==n)
      out[[1]] <- lapply(act_FUN, function(x){.get_act_FUN(FUN = x)})
    }else{
      ### それ以外の場合
      stop("The length of act_FUN must be equal to 1 or more.")
    }
    ## リストではない場合:ここまで------
  }else{
    ## リストである場合
    out <- lapply(X = act_FUN, FUN = function(X){
      if(length(X)==1){
        lapply(1:n, function(i){.get_act_FUN(FUN = X)})
      }else if(length(X) >= 2){
        stopifnot("Length of the element within list of act_FUN must match n." = length(x)==n)
        lapply(X, function(y){.get_act_FUN(FUN = y)})
      }else{
        stop("Length of the element within list of act_FUN must match n.")
      }
    })
  } ## リストである場合：ここまで-------

  # 名前をoutの第一階層に付加する
  if(is.null(names(act_FUN))){
    ## namesがact_FUNについていない場合
    ## act_labelをsymbol名から取得する
    if(is.symbol(act_FUN_sbs)){
      act_label <- as.character(act_FUN_sbs)
    }else{
      act_label <- "act"
    }
    ## act_labelを付与する
    if(length(out)==1){
      ### outの長さが１の場合、actをつける
      names(out) <- act_label
    }else{
      ### outの長さが2以上の場合、act1から順に番号をつける
      names(out) <- paste0(act_label, 1:length(out))
    }
    ## namesがact_FUNについていない場合：ここまで--
  }else{
    ## namesがact_FUNについている場合
    stopifnot("Names must be provided for each list element." = length(names(act_FUN))==length(out))
    names(out) <- names(act_FUN)
  }

  # アウトプット
  out
}


#-------------------------------------------------------------------------------
# .get_act_FUN: FUNとして入ってきたものをすべて関数型で返す
#-------------------------------------------------------------------------------

.get_act_FUN <- function(FUN){
  # FUNが関数かclosureの場合にはそのままFUNを返す
  if(is.function(FUN)|rlang::is_closure(FUN)){
    FUN
  }else if(is.character(FUN)){
    # FUNがcharacter型の場合には当該関数を環境から取得する
    if(exists(FUN)){
      ## FUNが環境に存在するならば、そちらから取得
      FUN <- get(FUN)
      stopifnot("The retrieved object in act_FUN is not a function." = is.function(FUN))
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
    stop("The retrieved object in act_FUN is not a function.")
  }
}


#-------------------------------------------------------------------------------
# .shape_agent_attr
#-------------------------------------------------------------------------------

.shape_agent_attr <- function(attr_sbs){
  # attr_sbs
  attr <- eval(attr_sbs)

  # 場合分け
  if(is.null(attr)){
    return(NULL)
  }else if(is.vector(attr) & !is.list(attr)){
    df <- as.data.frame(attr)
    if(is.symbol(attr_sbs)){
      colnames(df) <- deparse(attr_sbs)
    }else{
      colnames(df) <- "X"
    }
  }else if(is.data.frame(attr)){
    df <- attr
  }else if(is.list(attr)){
    df <- as.data.frame(attr)
  }else{
    stop("attr must be either vector or data.frame.")
  }

  # リターン
  df
}

#-------------------------------------------------------------------------------
# .shape_active_binding_field
#-------------------------------------------------------------------------------

# see: setABM_helper
