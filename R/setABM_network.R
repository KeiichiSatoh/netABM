#' @title Setting netABM_network Objects
#' @description
#' \code{setABM_network} constructs a \code{netABM_network} object for running ABM.
#' @param n integer. Number of agents
#' @param node_attr vector/data.frame/list of attributes of agents (default: \code{NULL})
#' @param networks Single n times n matrix or dataframe, or list of multiple matrices. The default value \code{NULL} will result in creating a n times n matrix without any edges.
#' @param .act list that contains user-defined or built-in function object representing agent's actions.
#' If user supplies only one action, it will then copied to all actors; Otherwise, user can also supply different actions for each agent.
#' Be sure to supply .act encapseled with list, even when user supplies only one agent action.
#'
#' @details
#' \code{setAgent_network} is a constructor of \code{netABM_network} object (D)
#' which has \code{agents}, \code{time}(set as 1), and \code{log} (set as NA) as a list format.
#' \code{agents} contains each agent's attribute listed under \code{a} (i.e. "attributes"),
#' networks listed under \code{e} (i.e. "edges"), and actions as \code{.act}.
#' The each agent under \code{agents} is \code{R6} class object from package \code{R6}.
#' Object \code{D} also has a class \code{netABM} which is the parent class of \code{netABM_network}.
#'
#' Each agent automatically get its \code{ID} and \code{act_label} and store them as their attributes.
#' The latter \code{act_label} is taken from the supplied object name of \code{.act}.
#'
#' There are two ways to set \code{.act}.
#' The first way is to write the user's own function of agent's actions.
#' Upon writing an original function, be sure to set \code{D} as the first argument without any default;
#' otherwise agent's action does not reflect dynamically to the changing \code{D} object during the simulation.
#' \code{self} is a reserved for indicating the agent themselves.
#'
#' The second way of setting \code{.act} is to use a built-in function of this package.
#' This second way actually has further three variations. First, the easiest one,
#' just supply the function object to \code{.act} (e.g., .act = function_name).
#' Second, if user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
#' Third, if user wants to put another name to this modified function object, assign it with substitute().
#' Then supply this substituted object to \code{.act}. The last method may be useful when the modification
#' of the function is very long. For getting the ideas more concretely about how to supply a function to \code{.act},
#' see the examples below.
#'
#' For now, \code{setABM_network} only supports directed networks.
#'
#' @returns  a \code{netABM_network} class object D (see Details)
#' @family setABM
#' @author Keiichi Satoh
#' @importFrom R6 R6Class
#' @importFrom rlang parse_expr
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @export
#' @examples
#' node_attr <- data.frame(
#' age = c(0, 1, 2, 3, 4),
#' sex = c("m","m","m","f","f"))
#' network <- matrix(1, 5, 5)
#'
#' # Example of the user-defined action
#' agent_get_older <- function(D){self$age <- self$age + 1}
#'
#' # Example 1
#' D <- setABM_network(n = 5,
#'                    node_attr = node_attr,
#'                    networks = network,
#'                    .act = list(agent_get_older))
#'
#'# Example 2: Set .act supplied directly with an modified built-in function.
#' D <- setABM_network(
#'   n = 5,
#'   .act = list(actAgent_addEdges_random(.valueFunction = rnorm(n = 1, mean = 0, sd = 1))))
#'
#' # Example 3: Set .act via a substituted object.
#' random2 <- substitute(actAgent_addEdges_random(.valueFunction = rnorm(n = 1, mean = 0, sd = 1)))
#'
#' D <- setABM_network(
#'   n = 5,
#'   .act = list(actAgent_addEdges_random(.valueFunction = rnorm(n = 1, mean = 0, sd = 1))))
#'




setABM_network <- function(
    n,
    node_attr = NULL,
    .act = list(NULL),
    networks = NULL){
  # インプットの形態を確認する------------
  ## n: 数値データであることを確認
  stopifnot("n must be numeric." = is.numeric(n))
  ## node_attr:data.frameの形に揃える
  ### リストの場合
  if(is.list(node_attr)){
    if(is.null(names(node_attr))){
      names(node_attr) <- paste0("X", 1:length(node_attr))
    }
    node_attr <- as.data.frame(node_attr, col.names = names(node_attr))
  }else if(is.vector(node_attr)){
    ### ベクトルの場合
    object_name_vector <- as.character(substitute(node_attr))
    node_attr <- data.frame(node_attr)
    colnames(node_attr) <- object_name_vector
  }
  ## networks: リストの形に揃える
  ### Nullの場合
  if(is.null(networks)){
    networks <- list(matrix(0, n, n))
    names(networks) <- "net"
  }else if(is.data.frame(networks)){
    object_name_networks <- as.character(substitute(networks))
    networks <- list(as.matrix(networks))
    names(networks) <- object_name_networks
  }else if(is.matrix(networks)){
    ### matrixの場合
    object_name_networks <- as.character(substitute(networks))
    networks <- list(networks)
    names(networks) <- object_name_networks
  }
  ### ここまでの処理でlist以外のデータの場合にはストップ
  stopifnot("networks must be either matrix, data.frame or list." = is.list(networks))
  ### 名前を付ける(ついていない場合)
  if(is.null(names(networks))){
    names(networks) <- paste0("net", 1:length(networks))
  }

  ## .act:
  #### .actの要素の数ごとに：人数分コピーする
  temp_label <- as.character(substitute(.act))[-1]
  if(length(temp_label)==0){
    act_label <- rep("NULL", n)
    warning("The agents' actions are set as NULL. If this is not what you want, please check if you correctly set the agent's action as a list (e.g., list(actAgent_addEdges_random))")
  }else if(length(temp_label)==1){
    act_label <- rep(temp_label, n)
  }else{
    stopifnot("The number of actions within a list should correspond to the number of agents." = length(temp_label)==n)
    act_label <- temp_label
  }

  ### 各agentごとにfunctionが存在しない場合にfunctionに変更処理
  .act_list <- vector("list", n)
  for(i in 1:n){
    # NULL/NAの場合
    if(act_label[i] %in% c("NA", "NULL")){
      .act_list[[i]] <- actAgent_nothing
    }else if(exists(act_label[i])){
      # Objectは存在している場合
      retrieved_object <- get(act_label[i])
      # 取得されたのがfunctionの場合
      if(is.function(retrieved_object)){
        .act_list[[i]] <- retrieved_object
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
        .act_list[[i]] <- temp_func
      }else{
        .act_list[[i]] <- get(act_label[i])
        warnings("The content with in .act list seems to be not function. Please check.")
      }
    }else{
      # objectが存在しない場合
      # 文字列から元の関数名と指定されているargを取り出す
      parsed_expr <- rlang::parse_expr(act_label[i])
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
      .act_list[[i]] <- temp_func
    }
  }
  ### すべての.act_listがfunction型か確認
  stopifnot("All .act objects need to be function." = all(unlist(lapply(.act_list, is.function))))

  # node_attribute, networkがnと整合するかをテスト-------
  ## node_attrとn
  if(!is.null(node_attr)){
    stopifnot(nrow(node_attr)==n)
  }
  ## netとn
  for(m in 1:length(networks)){
    stopifnot(nrow(networks[[m]])==n)
    stopifnot(ncol(networks[[m]])==n)
  }

  # Agentのクラス（network_Agent)を作成する--------------
  ## ID
  node_ID <- paste0("ID", 1:n)
  ## node_attrに"ID"という名前の列がある場合には、"ID_user"と変更する
  if(any(colnames(node_attr)=="ID")){
    colnames(node_attr)[which(colnames(node_attr)=="ID")] <- "ID_user"
  }

  ## IDをnetworkに振る
  for(m in 1:length(networks)){
    colnames(networks[[m]]) <- node_ID
  }

  ## fieldを作成
    ### 各フィールドの数をカウントする
    num_attr <- length(colnames(node_attr)) + 2            # IDとact_label分を足す
    num_networks <- length(networks)

    ### attribute, networkごとにフィールドを作成
    field_attr <- vector("list", length = num_attr)
    names(field_attr) <- c("ID", names(node_attr), "act_label")
    field_networks <- vector("list", length = num_networks)
    names(field_networks) <- names(networks)

  ## network_Agentクラスを作る
  network_Agent <- R6::R6Class(
    "network_Agent",
    public = c(a = list(field_attr),
               e = list(field_networks),
               .act = NA,
               print = function(...){
                 # attributes(ある場合)
                 if(!is.null(node_attr)){
                   for(m in 1:ncol(node_attr)){
                     cat(colnames(node_attr)[m],": ", self$a[[colnames(node_attr)[m]]], "\n", sep = "")
                   }
                 }
                 # network
                 for(m in 1:length(networks)){
                   net_temp <- paste0(names(self$e[[names(networks)[m]]]), sep = " ")
                   cat(names(networks)[m], ": ", net_temp, "\n", sep = "")
                 }
                 # act_label
                 cat("act_label: ", self$a$act_label, "\n", sep = "")
               }),
    lock_objects = F, cloneable = T)

  ## Dとして入れ物を用意し、IDを貼り付ける
  D <- list()
  D$agents <- vector(mode = "list", n)
  names(D$agents) <- node_ID

  ## Agentを新たにインスタンス化する
  for(i in 1:n){
    D$agents[[i]] <- network_Agent$new()
  }

  ## 当該agentのメタ情報を付与する
  for(i in 1:n){
    D$agents[[i]]$a["ID"] <- node_ID[i]
    D$agents[[i]]$a["act_label"] <- act_label[i]
  }

  ## 当該agentの.actを貼り付ける
  for(i in 1:n){
    D$agents[[i]][[".act"]] <- .act_list[[i]]
    environment(D$agents[[i]][[".act"]]) <- D$agents[[i]]$.__enclos_env__
  }


  ## 当該agentのネットワークを貼り付ける
  for(i in 1:n){
    for(p in 1:length(networks)){
      ego_net <- networks[[p]][i, ]
      ego_net_active <- ego_net[ego_net != 0]
      # すべて0の場合
      if(rlang::is_empty(ego_net_active)==F){
        # なんらかのつながりを持つ場合
        D$agents[[i]]$e[[names(networks)[p]]] <- ego_net_active
      }
    }
  }

  ## 当該agentのnode_attrをつける(nullではない場合)
  if(!is.null(node_attr)){
    for(i in 1:n){
      for(m in 1:ncol(node_attr)){
        D$agents[[i]]$a[colnames(node_attr)[m]] <- node_attr[i,m]
      }
    }
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

