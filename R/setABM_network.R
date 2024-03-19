#' @title Setting netABM_network Objects
#' @description
#' \code{setABM_network} constructs a \code{netABM_network} object for running ABM.
#' @param n Number of agents
#' @param node_attr vector/data.frame/list of attributes of agents (default: \code{NULL})
#' @param networks Single n times n matrix or dataframe, or list of multiple matrices. The default value \code{NULL} will result in creating a n times n matrix without any edges.
#' @param .act A user-defined or built-in function object representing agent's actions or list of functions for each agent. The default value \code{NULL} will result in setting \code{actAgent_nothing} to all agents.
#' @details
#' \code{setAgent_network} is a constructor of \code{netABM_network} object (D)
#' which has \code{agents}, \code{time}(set as 1), and \code{log} (set as NA) as a list format.
#' \code{agents} contains each agent's attribute, networks, and actions as a \code{R6} class object from package \code{R6}.
#'
#' Each agent automatically get its \code{ID} and \code{act_label} and store them as their attributes.
#' The latter \code{act_label} is put from the supplied object name of \code{.act}.
#'
#' There are two ways to set \code{.act}.
#' The first way is to write the user's own function of agent's actions.
#' Upon writing an original function, be sure to set \code{D} as the first argument without any default;
#' otherwise agent's action does not reflect dynamically to the changing \code{D} object during the simulation.
#' \code{self} is a reserved for indicating the agent themselves.
#'
#' The second way of setting \code{.act} is to use a built-in function of this package.
#'  If user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
#'
#' For now, \code{setABM_network} only supports directed networks.
#'
#' @returns  a D object (see Details)
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
#' agent_get_older <- function(D, b = 1){self$age <- b*self$age + 1}
#'
#' D <- setABM_network(n = 5,
#'                    node_attr = node_attr,
#'                    networks = network,
#'                    .act = agent_get_older)

setABM_network <- function(
    n,
    node_attr = NULL,
    .act = NULL,
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
    names(networks) <- "network"
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
  ### いったんひとまず人数分コピーする
  if(is.null(.act)){
    act_label <- rep("actAgent_nothing", n)
  }else if(length(deparse(substitute(.act)))==1){
    # .actの名前を取得
    act_label <- rep(deparse(substitute(.act)), n)
  }else{
    act_label <- as.character(substitute(.act))[-1]
    stopifnot("The length of .act should be n." = length(act_label)==n)
  }
  ### 各agentごとにfunctionが存在しない場合にfunctionに変更処理
  .act_list <- vector("list", n)
  for(i in 1:n){
    if(act_label[i] %in% c("NA", "NULL")){
      .act_list[[i]] <- actAgent_nothing
    }else if(exists(act_label[i])){
      .act_list[[i]] <- get(act_label[i])
    }else{
      # 文字列から元の関数名と指定されているargを取り出す
      parsed_expr <- rlang::parse_expr(act_label[i])
      call_name <- rlang::call_name(parsed_expr)
      call_args <- rlang::call_args(parsed_expr)
      assign("temp_func", get(call_name))
      # 元の関数のデフォルト値を新しい値に置き換える
      if(length(call_args)>0){
        for(k in 1:length(call_args)){
          formals(temp_func)[names(call_args[k])] <- call_args[k]
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

  ## IDをnetworkに振る
  for(m in 1:length(networks)){
    colnames(networks[[m]]) <- node_ID
  }

  ## fieldを作成
  if(is.null(node_attr)){
    num_fields <- length(networks) + 2                      # IDとact_label分を足す
  }else{
    num_fields <- ncol(node_attr) + length(networks) + 2    # IDとact_label分を足す
  }
  node_fields <- vector(mode = "list", length = num_fields)
  names(node_fields) <- c("ID", names(node_attr), "act_label", names(networks))

  ## network_Agentクラスを作る
  network_Agent <- R6::R6Class(
    "network_Agent",
    public = c(node_fields, .act = NA,
               print = function(...){
                 # attributes(ある場合)
                 if(!is.null(node_attr)){
                   for(m in 1:ncol(node_attr)){
                     cat(colnames(node_attr)[m],": ", self[[colnames(node_attr)[m]]], "\n", sep = "")
                   }
                 }
                 # network
                 for(m in 1:length(networks)){
                   net_temp <- paste0(names(self[[names(networks)[m]]]), sep = " ")
                   cat(names(networks)[m], ": ", net_temp, "\n", sep = "")
                 }
                 # act_label
                 cat("act_label: ", self$act_label, "\n", sep = "")
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
    D$agents[[i]][["ID"]] <- node_ID[i]
    D$agents[[i]][["act_label"]] <- act_label[i]
  }

  ## 当該agentの.actを貼り付ける(個別のアクターごとにNULLの可能性あり)
  for(i in 1:n){
    if(!is.null(.act_list[[i]])){
      # .act_listがNULLでない場合のみ
      D$agents[[i]][[".act"]] <- .act_list[[i]]
      environment(D$agents[[i]][[".act"]]) <- D$agents[[i]]$.__enclos_env__
    }
  }

  ## 当該agentのネットワークを貼り付ける
  for(i in 1:n){
    for(p in 1:length(networks)){
      ego_net <- networks[[p]][i, ]
      ego_net_active <- ego_net[ego_net != 0]
      # すべて0の場合
      if(rlang::is_empty(ego_net_active)){
        D$agents[[i]][[names(networks)[p]]] <- NULL
      }else{
        # なんらかのつながりを持つ場合
        D$agents[[i]][[names(networks)[p]]] <- networks[[p]][i, ]
      }
    }
  }

  ## 当該agentのnode_attrをつける(nullではない場合)
  if(!is.null(node_attr)){
    for(i in 1:n){
      for(m in 1:ncol(node_attr)){
        D$agents[[i]][[colnames(node_attr)[m]]] <- node_attr[i,m]
      }
    }
  }

  # DATAオブジェクトを返す---------------------
  ## variable nameをつける
  if(is.null(node_attr)){
    variable_type <- c(rep("network", length(networks)),
                       "ID",
                       "act_label")
  }else{
    variable_type <- c(rep("node_attr", ncol(node_attr)),
                       rep("network", length(networks)),
                       "ID",
                       "act_label")
  }
  names(variable_type) <- c(colnames(node_attr),
                            names(networks),
                            "ID", "act_label")
  attr(D$agents, "variable_type") <- variable_type

  ## Time, logをつける
  D$time <- 1
  D$log  <- NA
  ## class名を付与する
  attr(D, "class") <- "ABM_network"

  ## DATAを返却
  D
}

