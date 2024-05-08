#' @title Setting netABM_ca (Cellular Automaton) Objects
#' @description
#' \code{setABM_ca} constructs a \code{netABM_ca} object for running ABM.
#' @param agent_n integer. Number of agents
#' @param agent_attr vector/data.frame/list of attributes of agents (default: \code{NULL})
#' @param agent_f a user-defined or built-in function object or list of them representing agent's actions.
#' @param agent_location vector, data.frame, matrix or list of agent location.
#' See the details for how to create each type of location object. The default \code{NULL} will allocate
#' each agents randomly to each place.
#' @param ca A matrix or list of matrices of cellular automaton. The default \code{NULL} will result in creating a
#' \code{agent_n}*\code{agent_n} square matrix except for the case that \code{agent_location} is provided as a matrix or list format:
#' In such case \code{ca} is overwritten by \code{agent_location}.
#'
#' @details
#' \code{setAgent_cax} is a constructor of \code{netABM_cax} object (D)
#' which has \code{agent}, \code{stage}, \code{time}(set as 1), and \code{log} (set as NA)
#' as a list format.
#' Each agent in the \code{agent} has their attribute listed under \code{a} (i.e. "attributes"),
#' the current location of the agent in the CA, and the action as \code{.f} (i.e., "function").
#' The each agent under \code{agent} and \code{ca} under \code{stage} are \code{R6} class objects from package \code{R6}.
#' Object \code{D} also has a class \code{netABM} which is the parent class of \code{netABM_ca}.
#'
#' Each agent automatically get its \code{ID} and \code{f_label} and store them as their attributes.
#' The latter \code{f_label} is taken from the supplied object name of \code{agent_f}.
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
#' There are several options for setting \code{agent_location}. The simplest way is
#' supplying it as vector. For example, c(1,5,4) defines that first, second and third agent locate at first, fifth, and
#' fourth place, respectively. If users supplies agent_location as data.frame, each column will be
#' treated as a different dimension of location information. (e.g., location of living and location of working.)
#' Furthermore, users can also define agent places with a matrix wherein a agent's ID is directly placed at CA(s).
#'
#' Agent's location in the CA can be got in two ways. Agent's ca_adr indicates the address of the location in the CA.
#' The address in the CA is allocated in the order of the upper-left cell to right-bottom corner.
#' Agent's ca_rc indicates the agent's location as the row-column format in the CA matrix.
#'
#' @returns  a \code{netABM_ca} class object D (see Details)
#' @family setABM
#' @author Keiichi Satoh
#' @importFrom R6 R6Class
#' @importFrom rlang parse_expr
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @import Matrix
#' @export
#' @examples
#' # Data for the agent attributes and agent behavior
#'agent_attr <- data.frame(
#'  age = c(0, 1, 2, 3, 4),
#'  sex = c("m","m","m","f","f"))
#' move_to_agr1 <- function(D){
#'  D$stage$ca$ca[self$ca_adr] <- 0
#'  D$stage$ca$ca[1] <- self$ID
#'}
#'
#' # Example 1: A simple example
#' set.seed(seed = 2)
#' D <- setABM_ca(agent_n = 5, agent_attr = agent_attr, agent_f = move_to_agr1)
#' D$stage$ca$ca          # Agent 1 locates at the upper-right corner
#' D$agent$A1$.f(D)       # Agent 1's action to move to 1
#' D$stage$ca$ca          # Agent 1 now locates at the upper-left corner
#'
#' # Example 2: setting the agent's location manually using the vector
#' # that indicates the location of the each agent's address.
#' D <- setABM_ca(agent_n = 5, agent_attr = agent_attr,
#'               agent_f = move_to_agr1, agent_location = 1:5)
#' D$stage$ca$ca          # Agent 1 locates at the upper-right corner
#'
#' # Example 3: setting the agent's location manually using the ca matrix.
#' ca_mat <- matrix(0, 4, 5)
#' ca_mat[1,] <- 1:5
#' D <- setABM_ca(agent_n = 5, agent_attr = agent_attr,
#'               agent_f = move_to_agr1, agent_location = ca_mat)
#' D$stage$ca$ca          # Agent 1 locates at the upper-right corner
#'
#' # Example 4: setting the multiple CAs agent's location manually using the ca matrix.
#' ca_mat1 <- matrix(0, 4, 5)
#' ca_mat1[1,] <- 1:5
#' ca_mat2 <- t(ca_mat1)
#' D <- setABM_ca(agent_n = 5, agent_attr = agent_attr,
#'               agent_f = move_to_agr1, agent_location = list(ca_mat1, ca_mat2))
#'
#' # Example 5: setting the size of CA and let agents locate randomly.
#' ca1 <- matrix(0, nrow = 5, ncol = 6)
#' D <- setABM_ca(agent_n = 5, agent_attr = agent_attr,
#'               agent_f = move_to_agr1, ca = ca1)

setABM_ca <- function(
    agent_n,
    agent_attr = NULL,
    agent_f = list(NULL),
    agent_location = NULL,
    ca = NULL){
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
  ## ca: リストの形に揃える
  ### Nullの場合
  if(is.null(ca)){
    if(is.null(agent_location)){
      # agent_locationにもデータがない場合
      ca <- list(matrix(0, agent_n, agent_n))
      names(ca) <- "ca"
    }else{
      # agent_locationには何らかのデータがある場合
      if(is.list(agent_location)){
        ca <- vector("list", length(agent_location))
        names(ca) <- paste0("ca", 1:length(agent_location))
      }else{
        ca <- vector("list", 1)
        names(ca) <- "ca"
      }
    }
  }else if(is.data.frame(ca)){
    object_name_ca <- substitute(ca)
    if(class(object_name_ca)=="call"){
      object_name_ca <- deparse(object_name_ca)
    }else{
        object_name_ca <- as.character(object_name_ca)
    }
    ca <- list(as.matrix(ca))
    names(ca) <- object_name_ca
  }else if(is.matrix(ca)){
    ### matrixの場合
    object_name_ca <- as.character(substitute(ca))
    ca <- list(ca)
    names(ca) <- object_name_ca
  }
  ### ここまでの処理でlist以外のデータの場合にはストップ
  stopifnot("ca must be either matrix, data.frame or list." = is.list(ca))
  ### 名前を付ける(ついていない場合)
  if(is.null(names(ca))){
    names(ca) <- paste0("ca", 1:length(ca))
  }

  ## agent_location
  if(is.null(agent_location)){
    # agent_location: NULLの時
    m <- length(ca)
    agent_location <- vector(mode = "list", m)
    for(k in 1:m){
      agent_location[[k]] <- sample(1:(nrow(ca[[k]])*ncol(ca[[k]])), size = agent_n, replace = FALSE)
      ca[[k]][agent_location[[k]]] <- 1:agent_n
    }
  }else{
    # agent_locationに入力はある場合
    if(is.integer(agent_location)){
      # vectorの場合
      m <- length(ca)
      for(k in 1:m){
        ca[[k]][agent_location] <- 1:agent_n
      }
    }else if(is.data.frame(agent_location)){
      # data.frameの場合
      m <- length(ca)
      if(ncol(agent_location)!=m){warnings("Number of the columns in agent_location does not match with the number of ca.")}
      for(k in 1:m){
        ca[[k]][agent_location[ ,k]] <- 1:agent_n
      }
    }else{
      warnings("ca is overwritten by agent_location. Make sure if this is your intention.")
      m <- length(ca)
      if(is.list(agent_location)==FALSE){agent_location <- list(agent_location)}
      for(k in 1:m){
        ca[[k]] <- agent_location[[k]]
      }
    }
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

  ## IDを付与する----------------------------
  agent_ID <- 1:agent_n
  ## agent_attrに"ID"という名前の列がある場合には、"ID_user"と変更する
  if(any(colnames(agent_attr)=="ID")){
    colnames(agent_attr)[which(colnames(agent_attr)=="ID")] <- "ID_user"
  }

  ## fieldを作成--------------------------
  ### 各フィールドの数をカウントする
  #### agent
  num_agent_attr <- length(colnames(agent_attr)) + 1            # agent_f_label分を足す
  num_agent_ca <- length(ca)

  ### フィールドの空の入れ物を作成
  #### agent
  field_agent_attr <- vector("list", length = num_agent_attr)
  names(field_agent_attr) <- c(names(agent_attr), "f_label")
  field_agent_ca <- vector("list", length = num_agent_ca)
  names(field_agent_ca) <- names(ca)

  ## アクティブフィールドの前準備をする----------
  ### agent location in ca
  f_agent_ca_address <- function(env = D$stage$ca$.__enclos_env__){
    which(env$self[[ca_name]]==self$ID)
  }
  f_agent_ca_rowcol <- function(env = D$stage$ca$.__enclos_env__){
    which(env$self[[ca_name]]==self$ID, arr.ind = TRUE)
  }
  active_agent_ca_address <- vector("list", num_agent_ca)
  names(active_agent_ca_address) <- paste0(names(ca), "_adr")
  active_agent_ca_rowcol <- vector("list", num_agent_ca)
  names(active_agent_ca_rowcol) <- paste0(names(ca), "_rc")

  for(i in 1:num_agent_ca){
    active_agent_ca_address[[i]] <- f_agent_ca_address
    body(active_agent_ca_address[[i]])[[2]][[2]][[2]][[3]] <- names(ca)[i]
    active_agent_ca_rowcol[[i]] <- f_agent_ca_rowcol
    body(active_agent_ca_rowcol[[i]])[[2]][[2]][[2]][[3]] <- names(ca)[i]
  }

  ### R6 classを作る
  #### Agentクラス
  network_Agent <- R6::R6Class(
    "ca_Agent",
    public = c(
      ID = NA,
      a = list(field_agent_attr),
      .f = NA,
      print = function(...){
        # ID
        cat("ID: ", self$ID, "\n", sep = "")
        # attributes(ある場合)
        if(!is.null(agent_attr)){
          for(m in 1:ncol(agent_attr)){
            cat(colnames(agent_attr)[m],": ", self$a[[colnames(agent_attr)[m]]], "\n", sep = "")
          }
        }
        # f_label
        cat("f_label: ", self$a$f_label, "\n", sep = "")
        # agent_ca
        for(m in 1:length(ca)){
          temp <- self[[names(active_agent_ca_address)[m]]]
          cat(paste0(names(active_agent_ca_address)[m], ":"),
              temp, "\n", sep = " ")
          }
      }),
    active = c(active_agent_ca_address, active_agent_ca_rowcol),
    lock_objects = F, cloneable = T)

  # stageの各R6クラス
  ## agent_network
  ca_stage_ca <- R6::R6Class(
    "ca_stage_ca",
    public = c(field_agent_ca,
               print = function(...){
                 # agent network
                 for(m in 1:length(ca)){
                   temp <- self[[names(ca)[m]]]
                   cat(paste0("$", names(ca)[m]),"\n")
                   print(temp)
                   cat("\n")
                 }
               }
    ),
    lock_objects = F, cloneable = T)

  ## Dとして入れ物を用意し、IDを貼り付ける
  D <- list()
  D$agent <- vector(mode = "list", agent_n)
  names(D$agent) <- paste0("A", agent_ID)
  ## Agent、Placeを新たにインスタンス化する-------------------
  for(i in 1:agent_n){
    D$agent[[i]] <- network_Agent$new()
  }

  ## stage:agent network
  D$stage$ca <- ca_stage_ca$new()

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
  for(i in 1:length(field_agent_ca)){
    D$stage$ca[[names(field_agent_ca)[i]]] <- as(ca[[i]], "sparseMatrix")
  }

  # DATAオブジェクトを返す---------------------
  ## Time, logをつける
  D$time <- 1
  D$log  <- NA
  ## class名を付与する
  class(D) <- c("netABM", "netABM_ca")

  ## DATAを返却
  D
}
