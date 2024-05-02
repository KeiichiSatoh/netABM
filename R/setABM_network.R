#' @title Setting netABM_network Objects
#' @description
#' \code{setABM_network} constructs a \code{netABM_network} object for running ABM.
#' @param agent_n integer. Number of agents
#' @param agent_attr vector/data.frame/list of attributes of agents (default: \code{NULL})
#' @param agent_network Single n times n matrix or dataframe, or list of multiple matrices. The default value \code{NULL} will result in creating a n times n matrix without any edges.
#' @param agent_f a user-defined or built-in function object or list of them representing agent's actions.
#' If user supplies only one action, it will then copied to all actors. User can also supply different actions for each agent as list.
#'
#' @details
#' \code{setAgent_network} is a constructor of \code{netABM_network} object (D)
#' which has \code{agent}, \code{stage}, \code{time}(set as 1), and \code{log} (set as NA)
#' as a list format.
#' Each agent in the \code{agent} has their attribute listed under \code{a} (i.e. "attributes"),
#' list of the names of agent partners, and the action as \code{.f} (i.e., "function").
#' The each agent under \code{agent} and networks under \code{stage} are \code{R6} class objects from package \code{R6}.
#' Object \code{D} also has a class \code{netABM} which is the parent class of \code{netABM_network}.
#'
#' Each agent automatically get its \code{ID} and \code{act_label} and store them as their attributes.
#' The latter \code{act_label} is taken from the supplied object name of \code{agent_f}.
#'
#' There are two ways to set \code{agnet_f}.
#' The first way is to write the user's own function of agent's
#' actions and attach it to an object and supply this object to \code{agent_f}.
#' Do not write the function directly to \code{agent_f} because this will not be properly parsed.
#' Upon writing an original function, be sure to set \code{D} as the first argument without any default;
#' otherwise agent's action does not reflect dynamically to the changing \code{D} object during the simulation.
#' \code{self} is a reserved for indicating the agent themselves.
#'
#' The second way of setting \code{agent_f} is to use a built-in function of this package.
#' This second way actually has further three variations. First, the easiest one,
#' just supply the function object to \code{agent_f} (e.g., agent_f = function_name).
#' Second, if user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
#' Third, if user wants to put another name to this modified function object, assign it with substitute().
#' Then supply this substituted object to \code{agnet_f}. The last method may be useful when the modification
#' of the function is very long.
#' For getting the ideas more concretely about how to supply a function to \code{agent_f},
#' see the examples below.
#'
#' @returns  a \code{netABM_network} class object D (see Details)
#' @family setABM
#' @author Keiichi Satoh
#' @importFrom R6 R6Class
#' @importFrom rlang parse_expr
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @import Matrix
#' @export
#' @examples
#' # Example 1: Save the user-defined action object as "agent_get_older"
#'agent_attr <- data.frame(
#'  age = c(0, 1, 2, 3, 4),
#'  sex = c("m","m","m","f","f"))
#'agent_network <- matrix(1, 5, 5)
#'
#' # Example of the user-defined action
#'agent_get_older <- function(D){self$a$age <- self$a$age + 1}
#'
#'
#'D <- setABM_network(agent_n = 5,
#'                    agent_attr = agent_attr,
#'                    agent_f = agent_get_older,
#'                    agent_network = agent_network)
#'
#'# Example 2: Set agent_f directly with an modified built-in function.
#'D <- setABM_network(
#'   n = 5,
#'   .act = actAgent_addEdges_random(.valueFunction = rnorm(n = 1, mean = 0, sd = 1)))
#'
#' # Example 3: Set .act via a substituted object.
#' random2 <- substitute(actAgent_addEdges_random(.valueFunction = rnorm(n = 1, mean = 0, sd = 1)))
#'
#' D <- setABM_network(
#'    n = 5,
#'    .act = random2)
#'
#' # Example 4: Set diffenret actions for each agent.
#'agent_get_older2 <- function(D, b = 1){self$a$age <- b*self$a$age + 1}
#'
#' D <- setABM_network(
#'    n = 5,
#'    .act = list(agent_get_older2(b = 1),
#'                agent_get_older2(b = 2),
#'                agent_get_older2(b = 3),
#'                agent_get_older2(b = 4),
#'                agent_get_older2(b = 5)))
#'

setABM_network <- function(
    agent_n,
    agent_attr = NULL,
    agent_f = list(NULL),
    agent_network = NULL){
  # インプットの形態を確認する------------
  # Agentに関して=========================
  ## agent_n: 数値データであることを確認
  stopifnot("agent_n must be numeric." = is.numeric(agent_n))
  ## agent_attr:data.frameの形に揃える
  ### リストの場合
  if(is.list(agent_attr)){
    if(is.null(names(agent_attr))){
      names(agent_attr) <- paste0("X", 1:length(agent_attr))
    }
    agent_attr <- as.data.frame(agent_attr, col.names = names(agent_attr))
  }else if(is.vector(agent_attr)){
    ### ベクトルの場合
    object_name_vector <- as.character(substitute(agent_attr))
    agent_attr <- data.frame(agent_attr)
    colnames(agent_attr) <- object_name_vector
  }
  ## agenet_network: リストの形に揃える
  ### Nullの場合
  if(is.null(agent_network)){
    agent_network <- list(matrix(0, agent_n, agent_n))
    names(agent_network) <- "net"
  }else if(is.data.frame(agent_network)){
    object_name_network <- as.character(substitute(agent_network))
    agent_network <- list(as.matrix(agent_network))
    names(agent_network) <- object_name_network
  }else if(is.matrix(agent_network)){
    ### matrixの場合
    object_name_network <- as.character(substitute(agent_network))
    agent_network <- list(agent_network)
    names(agent_network) <- object_name_network
  }
  ### ここまでの処理でlist以外のデータの場合にはストップ
  stopifnot("agent_network must be either matrix, data.frame or list." = is.list(agent_network))
  ### 名前を付ける(ついていない場合)
  if(is.null(names(agent_network))){
    names(agent_network) <- paste0("net", 1:length(agent_network))
  }

  ## agent_f:
  #### agent_fの要素の数ごとに：人数分コピーする
  temp_label <- as.character(substitute(agent_f))
  if(temp_label[1]=="list"){
    # リストの場合
    agent_f_label <- as.character(substitute(agent_f))[-1]
    ### もしもlistに挟まれているが1個しか入っていない場合
    if(length(agent_f_label)==1){
      agent_f_label <- rep(agent_f_label, agent_n)
    }
    stopifnot("The number of functions within a list should correspond to the number of agents." = length(agent_f_label)==agent_n)
  }else if(any(length(temp_label)==0|is.na(temp_label))){
    # NULL/NAの場合
    agent_f_label <- rep("NULL", agent_n)
  }else{
    # fに単体で設定されているものをn個分コピー
    agent_f_label <- rep(deparse(substitute(agent_f), width.cutoff = 500), agent_n)
  }

  ### 各agentごとにfunctionが存在しない場合にfunctionに変更処理
  agent_f_list <- vector("list", agent_n)
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

  # node_attribute, networkがnと整合するかをテスト-------
  ## node_attrとn
  if(!is.null(agent_attr)){
    stopifnot(nrow(agent_attr)==agent_n)
  }
  ## netとn
  for(m in 1:length(agent_network)){
    stopifnot(nrow(agent_network[[m]])==agent_n)
    stopifnot(ncol(agent_network[[m]])==agent_n)
  }


  ## IDを付与する----------------------------
  agent_ID <- paste0("A", 1:agent_n)
  ## agent_attrに"ID"という名前の列がある場合には、"ID_user"と変更する
  if(any(colnames(agent_attr)=="ID")){
    colnames(agent_attr)[which(colnames(agent_attr)=="ID")] <- "ID_user"
  }
  ## IDをnetworkに振る
  for(m in 1:length(agent_network)){
    dimnames(agent_network[[m]]) <- list(agent_ID, agent_ID)
  }

  ## fieldを作成--------------------------
  ### 各フィールドの数をカウントする
  #### agent
  num_agent_attr <- length(colnames(agent_attr)) + 1            # agent_f_label分を足す
  num_agent_network <- length(agent_network)

  ### フィールドの空の入れ物を作成
  #### agent
  field_agent_attr <- vector("list", length = num_agent_attr)
  names(field_agent_attr) <- c(names(agent_attr), "f_label")
  field_agent_network <- vector("list", length = num_agent_network)
  names(field_agent_network) <- names(agent_network)

  ## アクティブフィールドの前準備をする----------
  ### agent_network
  temp_func_agent <- function(env = D$stage$agent$.__enclos_env__){
    temp <- env$self[[net_name]][self$ID, ]
    names(temp[temp != 0])
  }
  active_agent_network <- vector("list", num_agent_network)
  names(active_agent_network) <- names(agent_network)
  for(i in 1:num_agent_network){
    active_agent_network[[i]] <- temp_func_agent
    body(active_agent_network[[i]])[[2]][[3]][[2]][[3]] <- names(agent_network)[i]
  }

  ### R6 classを作る
  #### Agentクラス
  network_Agent <- R6::R6Class(
    "network_Agent",
    public = c(
      ID = NA,
      a = list(field_agent_attr),
      .f = NA,
      print = function(...){
        # attributes(ある場合)
        if(!is.null(agent_attr)){
          for(m in 1:ncol(agent_attr)){
            cat(colnames(agent_attr)[m],": ", self$a[[colnames(agent_attr)[m]]], "\n", sep = "")
          }
        }
        # f_label
        cat("f_label: ", self$a$f_label, "\n", sep = "")
        # agent network
        for(m in 1:length(agent_network)){
          cat(paste0(names(agent_network)[m],":"),
              self[[names(agent_network)[m]]], "\n", sep = " ")
        }
      }),
    active = c(active_agent_network),
    lock_objects = F, cloneable = T)

  # stageの各R6クラス
  ## agent_network
  network_stage_agentNetwork <- R6::R6Class(
    "network_stage_agentNetwork",
    public = c(field_agent_network,
               print = function(...){
                 # agent network
                 for(m in 1:length(agent_network)){
                   temp <- self[[names(agent_network)[m]]]
                   cat(paste0("$", names(agent_network)[m]),"\n")
                   print(temp)
                   cat("\n")
                 }
               }
    ),
    lock_objects = F, cloneable = T)

  ## Dとして入れ物を用意し、IDを貼り付ける
  D <- list()
  D$agent <- vector(mode = "list", agent_n)
  names(D$agent) <- agent_ID

  ## Agent、Placeを新たにインスタンス化する-------------------
  for(i in 1:agent_n){
    D$agent[[i]] <- network_Agent$new()
  }
  D$stage$agent <- spatNetwork_stage_agentNetwork$new()

  # Agentの情報付与----------------------------------
  ## 当該agentのメタ情報を付与する
  for(i in 1:agent_n){
    D$agent[[i]]$ID <- agent_ID[i]
    D$agent[[i]]$a["f_label"] <- agent_f_label[i]
  }
  ## 当該agentのfを貼り付ける
  for(i in 1:agent_n){
    D$agent[[i]][[".f"]] <- agent_f_list[[i]]
    environment(D$agent[[i]][[".f"]]) <- D$agent[[i]]$.__enclos_env__
  }
  ## 当該agentのattr(nullでない場合)
  if(!is.null(agent_attr)){
    for(i in 1:agent_n){
      for(m in 1:ncol(agent_attr)){
        D$agent[[i]]$a[colnames(agent_attr)[m]] <- agent_attr[i,m]
      }
    }
  }

  ## stageに各情報を付与する---------------------------
  for(i in 1:length(field_agent_network)){
    D$stage$agent[[names(field_agent_network)[i]]] <- as(agent_network[[i]], "sparseMatrix")
  }

  # DATAオブジェクトを返す---------------------
  ## Time, logをつける
  D$time <- 1
  D$log  <- NA
  ## class名を付与する
  class(D) <- c("netABM", "netABM_network")

  ## DATAを返却
  D
}
