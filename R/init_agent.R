#' @title Create a set of ABM_Agent class agents
#' @description \code{init_agent} create ABM_Agent class agents, which can be
#' an input to setABM.
#' @param
#' @details
#' The function checks if the input CA is a 2D matrix. If it is, the function replicates this 2D matrix along the third dimension
#' to create a 3D array. The resulting 3D array has dimensions equal to the original 2D matrix dimensions plus the specified height.
#' @return A 3D array where the 2D CA grid is replicated along the third dimension.
#' @author Keiichi Satoh
#' @family init
#' @examples
#' @export

init_agent <- function(n = NULL, attr = NULL, act_FUN = NULL,
                       active_binding_field = NULL, ID_start = 1){
  # attrを処理する
  attr_sbs <- substitute(attr)
  attr_df <- .shape_agent_attr(attr_sbs = attr_sbs)

  # nが投入されている場合には、数値かどうかを判定
  if(!is.null(n)){
    stopifnot("n must be numeric." = is.numeric(n)==TRUE)
  }

  # nとattrの投入状況ごとに整理
  if(!is.null(n) & !is.null(attr)){
    ## nとattrが両方投入されている場合：両者が同じかをチェック
    stopifnot("n must be the same to nrow in each attr." = n == NROW(attr_df))
  }else if(is.null(n) & !is.null(attr)){
    ## nはなし、attrは投入あり：nをattrから取得
    n <- NROW(attr_df)
  }else if(!is.null(n) & is.null(attr)){
    ## nはあり、attrはなし：nはそのまま
    n <- n
  }else{
    ## nもattrもなし：エラーを出す
    stop("Either n or attr must be inputted.")
  }

  # act_FUNを整理
  act_FUN_sbs <- substitute(act_FUN)
  act_FUN_list <- .shape_act_FUN(act_FUN = act_FUN,
                                 act_FUN_sbs = act_FUN_sbs,
                                 n = n)

  # active_binding_fieldを整理
  if(!is.null(active_binding_field)){
    active_binding_field_sbs <- substitute(active_binding_field)
    active_binding_field_formatted <- .shape_active_binding_field(active_binding_field_sbs = active_binding_field_sbs)
  }else{
    active_binding_field_formatted <- NULL
  }

  # act_FUN_listの中身において、引数にD、中身のすべての1行目にselfを追加する
  act_FUN_list <- lapply(act_FUN_list, function(FUN_vec){
    lapply(FUN_vec, function(FUN){
      # すでにユーザーが誤ってDを引数に書いていたらひとまず消す
      current_formals <- formals(FUN)
      current_formals[which(names(current_formals)=="D")|which(names(current_formals)=="E")] <- NULL
      formals(FUN) <- c(alist(D = D, E = E), current_formals) # D=D, E = Eを足す
      # self <- selfを1行目に足す
      body(FUN) <- as.call(append(as.list(body(FUN)), expression(self <- self), after=1))
      FUN
    })
  })

  # agentID
  agentID <- ID_start:(ID_start + n - 1)

  # agentのR6クラスを作成
  act_FUN_len <- length(act_FUN_list)
  act_FUN_i <- vector("list", act_FUN_len)
  names(act_FUN_i) <- names(act_FUN_list)

  out <- lapply(1:n, function(i){
    ID_i <- c(ID = agentID[i])
    if(ncol(attr_df)==1 && !is.null(attr_df)){
      attr_i <- attr_df[i, ]
      names(attr_i) <- colnames(attr_df)
    }else{
      attr_i <- attr_df[i, ]
    }
    if(act_FUN_len==0){
      act_FUN_i <- NULL
    }else{
      for(p in 1:act_FUN_len){
        act_FUN_i[[p]] <- act_FUN_list[[p]][[i]]
      }
    }
    # 付加する
    Agent <- ABM_Agent$new(fields = c(ID_i, attr_i), methods = act_FUN_i)

    # active_binding_fieldを処理する
    if(!is.null(active_binding_field_formatted)){
      active <- assign_func_envs(active_binding_field_formatted, Agent$.__enclos_env__)
      for(name in names(active)){
        makeActiveBinding(name, active[[name]], Agent$.__enclos_env__$self)
      }
      Agent$.__enclos_env__$.__active__ <- active
    }
    # リターン
    Agent
  })

  # IDを名前として付与する
  names(out) <- paste0("ID", agentID)
  # リターン
  out
}

