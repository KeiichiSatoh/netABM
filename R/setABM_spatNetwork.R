#' @title Setting netABM_spatNetwork Objects
#' @description
#' \code{setABM_spatNetwork} constructs a \code{netABM_spatNetwork} object for running ABM.
#' @param agent_n integer. Number of agents
#' @param agent_attr vector, data.frame, or list of attributes of agents (default: \code{NULL})
#' @param agent_f a user-defined or built-in function object or a list of them representing agent's actions.
#' If user supplies only one action, it will then copied to all actors. User can also supply different actions for each agent as a list.
#' @param agent_network Single n times n matrix or data.frame,
#' or list of multiple matrices. The default value \code{NULL} will result in creating a n times n matrix without any edges.
#' @param agent_location vector, data.frame, matrix or list of agent location.
#' See the details for how to create each type of location object. The default \code{NULL} will allocate
#' each agents randomly to each place.
#' @param place_n integer. Number of places
#' @param place_attr vector, data.frame, or list of attributes of agents (default: \code{NULL})
#' @param place_network Single n times n matrix or data.frame,
#' or list of multiple matrices. The default value \code{NULL} will result in creating a n times n matrix wherein places are fully connected each other.
#'
#' @details
#' \code{setABM_spatNetwork} is a constructor of \code{netABM_spatNetwork} object (D)
#' which has \code{agent}, \code{place}, \code{stage}, \code{time}(set as 1), and \code{log} (set as NA) as a list format.
#' each agent in the \code{agent} has their attribute listed under \code{a} (i.e. "attributes"),
#' list of names of the agent partners, agent's current location on the places,
#' and agent actions as \code{.f} (i.e., "function").
#' Similarly, \code{place} contains each place's attribute (under \code{a}),
#' the ID of the agents who occupy the relevant place, and list of other places
#' that is connected from the current place.
#' \code{stage} contains the three different networks. \code{agent} is the agent network(s),
#' \code{location} is network(s) of places, and \code{location} is the two-mode
#' network in that each entry is \code{1} if an agent in the row locates at the place in
#' the column, and \code{0} otherwise.
#'
#' Each agent automatically get its \code{ID} and \code{act_label} and store them as their attributes.
#' The latter \code{act_label} is taken from the supplied object name of \code{agent_f}.
#'
#' There are two ways to set \code{agent_f}.
#' The first way is to write the user's own function of agent's
#' actions and attach it to an object and supply this object to \code{agent_f}.
#' Do not write the function directly to \code{agent_f} because this will not be properly parsed.
#' Upon writing an original function, be sure to set \code{D} as the first argument without any default;
#' otherwise agent's action does not reflect dynamically to the changing \code{D} object during the simulation.
#' \code{self} is a reserved for indicating the agent themselves.
#'
#' The second way of setting \code{agent_f} is to use a built-in function of this package.
#' This second way actually has further three variations. First, the easiest one,
#' just supply the function object to \code{agent_f} (e.g., .act = function_name).
#' Second, if user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
#' Third, if user wants to put another name to this modified function object, assign it with substitute().
#' Then supply this substituted object to \code{agent_f}. The last method may be useful when the modification
#' of the function is very long.
#' For getting the ideas more concretely about how to supply a function to \code{agent_f},
#' see the examples below.
#'
#' Incidentally, the each agent under \code{agents} is \code{R6} class object from package \code{R6}.
#' For getting more information about this class, see the R6's vignettes (https://r6.r-lib.org/articles/Introduction.html).
#'
#' There are several options for setting \code{agent_location}. The simplest way is
#' supplying it as vector. For example, c(1,5,4) defines that first, second and third agent locate at first, fifth, and
#' fourth place, respectively. If users supplies agent_location as data.frame, each column will be
#' treated as a different dimension of location information. (e.g., location of living and location of working.)
#' Furthermore, users can also define agent places with a matrix wherein a agent in the row locates at place(s) in the column.
#' The last method will be useful if users want to let agents locates different places simultaneously.
#' Lastly, the the multiple matrices of agent_location can be supplied via list so that each matrices
#' will be treated as a different dimension of places.
#'
#' Note that as explained above, agents can locates multiple places if defined as matrix format.
#' However, this may be rather a rare usage. Accordingly, in the default of the random allocation of
#' agents (if \code{NULL} was supplied to \code{agent_location}), each place is allowed to be occupied only
#' an agent. Hence, if number of agents are more than number of places, an error will be returned.
#'
#' Incidentally, object \code{D} also has a class \code{netABM} which is the parent class of \code{netABM_network}.
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
#' Example 1
#' set.seed(1)
#' move_to_P1 <- function(D){
#'   D$stage$location$loc[self$a$ID, names(self$loc)] <- 0
#'   D$stage$location$loc[self$a$ID, 1] <- 1
#' }
#' D <- setABM_spatNetwork(agent_n = 10, place_n = 15, agent_f = move_to_P1)
#' D$agent$A1$loc
#' D$agent$A1$.f(D)
#' D$agent$A1$loc      # A1 moved to P1

setABM_spatNetwork <- function(
    agent_n,
    agent_attr = NULL,
    agent_f = list(NULL),
    agent_network = NULL,
    agent_location = NULL,
    place_n = NULL,
    place_attr = NULL,
    place_network = NULL
  ){
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

  # placeに関して=========================
  ## place_n: 数値データであることを確認
  stopifnot("place_n must be numeric." = is.numeric(place_n))
  ## place_attr:data.frameの形に揃える
  ### リストの場合
  if(is.list(place_attr)){
    if(is.null(names(place_attr))){
      names(place_attr) <- paste0("X", 1:length(place_attr))
    }
    place_attr <- as.data.frame(place_attr, col.names = names(place_attr))
  }else if(is.vector(place_attr)){
    ### ベクトルの場合
    place_object_name_vector <- as.character(substitute(place_attr))
    place_attr <- data.frame(place_attr)
    colnames(place_attr) <- place_object_name_vector
  }
  ## place_network: リストの形に揃える
  ### Nullの場合
  if(is.null(place_network)){
    place_network <- list(matrix(1, place_n, place_n)) # すべてつながっている状態
    names(place_network) <- "plc"
  }else if(is.data.frame(place_network)){
    place_object_name_network <- as.character(substitute(place_network))
    place_network <- list(as.matrix(place_network))
    names(place_network) <- place_object_name_network
  }else if(is.matrix(place_network)){
    ### matrixの場合
    place_object_name_network <- as.character(substitute(place_network))
    place_network <- list(place_network)
    names(place_network) <- place_object_name_network
  }
  ### ここまでの処理でlist以外のデータの場合にはストップ
  stopifnot("place_network must be either matrix, data.frame or list." = is.list(place_network))
  ### 名前を付ける(ついていない場合)
  if(is.null(names(place_network))){
    names(place_network) <- paste0("plc", 1:length(place_network))
  }

  # node_attribute, networkがnと整合するかをテスト-------
  ## node_attrとn
  if(!is.null(place_attr)){
    stopifnot(nrow(place_attr)==place_n)
  }
  ## netとn
  for(m in 1:length(place_network)){
    stopifnot(nrow(place_network[[m]])==place_n)
    stopifnot(ncol(place_network[[m]])==place_n)
  }

  # agent_locationをlocation_networkに整形===================
  if(is.null(agent_location)){
    stopifnot("The number of location is fewer than the number of agents" = place_n >= agent_n)
    agent_location <- sample(x = 1:place_n, size = agent_n)
    temp <- matrix(0, agent_n, place_n)
    for(i in 1:agent_n){
      temp[i, agent_location[i]] <- 1
      diag(temp) <- 0
    }
    location_network <- list()
    location_network$loc <- temp
  }else if(is.vector(agent_location)){
    stopifnot("The length of agent_location must be the same as the number of agents." = length(agent_location)==agent_n)
    location_network <- matrix(0, agent_n, place_n)
    for(i in 1:agent_n){
      temp[i, agent_location[i]] <- 1
    }
    location_network <- list()
    location_network$loc <- temp
  }else if(is.data.frame(agent_location)){
    stopifnot("The row length of agent_location must be the same as the number of agents." = nrow(agent_location)==agent_n)
    warnings("Each column of agent_location is treated as a different location indicator.")
    if(is.null(names(colnames(agent_location)))){
      colnames(agent_location) <- paste0("loc", 1:ncol(agent_location))
    }
    location_network <- vector(mode = "list", ncol(agent_location))
    names(location_network) <- colnames(agent_location)
    for(m in 1:ncol(agent_location)){
      location_network[[m]] <- matrix(0, agent_n, place_n)
      for(i in 1:nrow(agent_location)){
        location_network[[m]][i, agent_location[i,m]] <- 1
      }
    }
  }else if(is.matrix(agent_location)){
    location_network <- list()
    location_network$loc <- agent_location
  }else if(is.list(agent_location)){
    location_networks <- vector(mode = "list", length = length(agent_location))
    if(is.null(names(agent_location))){
      names(location_network) <- paste0("loc", 1:length(agent_location))
    }
    for(i in length(agent_location)){
      stopifnot("The row length of agent_location must be the same as the number of agents." = nrow(agent_location[[i]])==agent_n)
      stopifnot("The col length of agent_location must be the same as the number of places." = ncol(agent_location[[i]])==place_n)
      location_network <- as.matrix(agent_location[[i]])
    }
  }else{
    stop("agent_location must be either NULL, vector, data.frame, matrix, or list")
  }

  ## IDを付与する----------------------------
  agent_ID <- paste0("A", 1:agent_n)
  place_ID <- paste0("P", 1:place_n)
  ## agent_attrに"ID"という名前の列がある場合には、"ID_user"と変更する
  if(any(colnames(agent_attr)=="ID")){
    colnames(agent_attr)[which(colnames(agent_attr)=="ID")] <- "ID_user"
  }
  ## place_attrに"ID"という名前の列がある場合には、"ID_user"と変更する
  if(any(colnames(place_attr)=="ID")){
    colnames(place_attr)[which(colnames(place_attr)=="ID")] <- "ID_user"
  }
  ## IDをnetworkに振る
  for(m in 1:length(agent_network)){
    dimnames(agent_network[[m]]) <- list(agent_ID, agent_ID)
  }
  ### place
  for(m in 1:length(place_network)){
    dimnames(place_network[[m]]) <- list(place_ID, place_ID)
  }
  ### location
  for(m in 1:length(location_network)){
    dimnames(location_network[[m]]) <- list(agent_ID, place_ID)
  }

  ## fieldを作成--------------------------
    ### 各フィールドの数をカウントする
    #### agent
    num_agent_attr <- length(colnames(agent_attr)) + 1            # agent_f_label分を足す
    num_agent_network <- length(agent_network)
    #### place
    num_place_attr <- length(colnames(place_attr))
    num_place_network <- length(place_network)
    #### location
    num_location_network <- length(location_network)

    ### フィールドの空の入れ物を作成
    #### agent
    field_agent_attr <- vector("list", length = num_agent_attr)
    names(field_agent_attr) <- c(names(agent_attr), "f_label")
    field_agent_network <- vector("list", length = num_agent_network)
    names(field_agent_network) <- names(agent_network)
    #### place
    field_place_attr <- vector("list", length = num_place_attr)
    names(field_place_attr) <- c(names(place_attr))
    field_place_network <- vector("list", length = num_place_network)
    names(field_place_network) <- names(place_network)
    #### location
    field_location_network <- vector("list", length = num_location_network)
    names(field_location_network) <- names(location_network)


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
  ### place_network
  temp_func_place <- function(env = D$stage$place$.__enclos_env__){
    temp <- env$self[[net_name]][self$ID, ]
    names(temp[temp != 0])
  }
  active_place_network <- vector("list", num_place_network)
  names(active_place_network) <- names(place_network)
  for(i in 1:num_place_network){
    active_place_network[[i]] <- temp_func_place
    body(active_place_network[[i]])[[2]][[3]][[2]][[3]] <- names(place_network)[i]
  }
  ### location_network(agent)
  temp_func_location <- function(env = D$stage$location$.__enclos_env__){
    temp <- env$self[[location_name]][self$ID, ]
    names(temp[temp != 0])
  }
  active_location_network <- vector("list", num_location_network)
  names(active_location_network) <- names(location_network)
  for(i in 1:num_location_network){
    active_location_network[[i]] <- temp_func_location
    body(active_location_network[[i]])[[2]][[3]][[2]][[3]] <- names(location_network)[i]
  }
  ### location_network(place)
  temp_func_location_placed <- function(env = D$stage$location$.__enclos_env__){
    temp <- env$self[[location_name]][ ,self$ID]
    names(temp[temp != 0])
  }
  active_location_network_placed <- vector("list", num_location_network)
  names(active_location_network_placed) <- names(location_network)
  for(i in 1:num_location_network){
    active_location_network_placed[[i]] <- temp_func_location_placed
    body(active_location_network_placed[[i]])[[2]][[3]][[2]][[3]] <- names(location_network)[i]
  }

  ### R6 classを作る
  #### Agentクラス
  spatNetwork_Agent <- R6::R6Class(
    "spatNetwork_Agent",
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
        # location
        for(m in 1:length(location_network)){
          temp <- self[[names(location_network)[m]]]
          cat(paste0(names(location_network)[m], ":"),
              temp, "\n", sep = " ")}
      }),
    active = c(active_agent_network, active_location_network),
    lock_objects = F, cloneable = T)

  ## spatNetwork_Placeクラスを作る
  spatNetwork_Place <- R6::R6Class(
    "spatNetwork_Place",
    public = c(
      ID = NA,
      a = list(field_place_attr),
      print = function(...){
        # attributes(ある場合)
        if(!is.null(place_attr)){
          for(m in 1:ncol(place_attr)){
            cat(colnames(place_attr)[m],": ", self$a[[colnames(place_attr)[m]]], "\n", sep = "")
          }
        }
        # place network
        for(m in 1:length(place_network)){
          cat(paste0(names(place_network)[m], ":"),
              self[[names(place_network)[m]]],
              "\n", sep = " ")
        }
        # location
        for(m in 1:length(location_network)){
          temp <- self[[names(location_network)[m]]]
          cat(paste0(names(location_network)[m], ":"),
              temp, "\n", sep = " ")}
      }),
    active = c(active_place_network, active_location_network_placed),
    lock_objects = F, cloneable = T)


  # stageの各R6クラス
  ## agent_network
  spatNetwork_stage_agentNetwork <- R6::R6Class(
    "spatNetwork_stage_agentNetwork",
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
  ## place_network
  spatNetwork_stage_placeNetwork <- R6::R6Class(
    "spatNetwork_stage_placexNetwork",
    public = c(field_place_network,
               print = function(...){
                 # place network
                 for(m in 1:length(place_network)){
                   temp <- self[[names(place_network)[m]]]
                   cat(paste0("$", names(place_network)[m]),"\n")
                   print(temp)
                   cat("\n")
                 }
               }
    ),
    lock_objects = F, cloneable = T)
  ## location_network
  spatNetwork_stage_locationxNetwork <- R6::R6Class(
    "spatNetwork_stage_locationNetwork",
    public = c(field_location_network,
               print = function(...){
                 # place network
                 for(m in 1:length(location_network)){
                   temp <- self[[names(location_network)[m]]]
                   cat(paste0("$", names(location_network)[m]),"\n")
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
  D$place <- vector(mode = "list", place_n)
  names(D$place) <- place_ID
  D$stage <- vector(mode = "list", 3)
  names(D$stage) <- c("agent","place","location")

  ## Agent、Placeを新たにインスタンス化する-------------------
  for(i in 1:agent_n){
    D$agent[[i]] <- spatNetwork_Agent$new()
  }
  for(i in 1:place_n){
    D$place[[i]] <- spatNetwork_Place$new()
  }
  D$stage$agent <- spatNetwork_stage_agentNetwork$new()
  D$stage$place <- spatNetwork_stage_placeNetwork$new()
  D$stage$location <- spatNetwork_stage_locationxNetwork$new()

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

  # Placeの情報付与----------------------------------
  ## Placeのメタ情報
  for(i in 1:place_n){
    D$place[[i]]$ID <- place_ID[i]
  }
  ## 当該agent, placeのattrをつける(nullではない場合)
  if(!is.null(place_attr)){
    for(i in 1:place_n){
      for(m in 1:ncol(place_attr)){
        D$place[[i]]$a[colnames(place_attr)[m]] <- place_attr[i,m]
      }
    }
  }

  ## stageに各情報を付与する---------------------------
  for(i in 1:length(field_agent_network)){
    D$stage$agent[[names(field_agent_network)[i]]] <- as(agent_network[[i]], "sparseMatrix")
  }
  for(i in 1:length(field_place_network)){
    D$stage$place[[names(field_place_network)[i]]] <- as(place_network[[i]], "sparseMatrix")
  }
  for(i in 1:length(field_location_network)){
    D$stage$location[[names(field_location_network)[i]]] <- as(location_network[[i]], "sparseMatrix")
  }


  # DATAオブジェクトを返す---------------------
  ## Time, logをつける
  D$time <- 1
  D$log  <- NA
  ## class名を付与する
  class(D) <- c("netABM", "netABM_spatNetwork")

  ## DATAを返却
  D
}
