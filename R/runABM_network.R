#' @title Run the simulation based on the netABM_network object
#' @description
#' \code{runABM_network} let agents their action defined by \code{.act} in the
#' \code{netABM_network} object.
#' @param D a \code{netABM} class object.
#' @param .stopCondition A user-defined or built-in function object that determines when the simulation to stop.
#' The default value \code{NULL} will result in running one simulation.
#' @param .selectAgent A user-defined or built-in function object about which agents to select.
#' The default value \code{NULL} will result in selecting all agents, meaning that all agents do their action each time.
#' @param save_log logical; if the log of each run shold be saved. The default is \code{FALSE}.
#' @details
#' \code{runAgent_network} is a simulator based on \code{netABM_network} object (D).
#' In each run the selected agents act their action defined \code{.act} of \code{netABM_network}
#' object.
#'
#' Because the counting system of \code{R} starts from 1, \code{runABM} counts the initial time starts from 1,
#' which means that the one run of the simulation corresponds to time 2.
#'
#' For setting \code{.stopCondition} and \code{.selectAgent} condition, there are two different ways.
#' The first way is to write the user's own function.
#' Upon writing an original function, be sure to set \code{D} as the first argument without any default;
#' otherwise agent's action does not reflect dynamically to the changing \code{D} object during the simulation.
#' \code{self} is a reserved for indicating the agent themselves.
#' In addition, each function should returns the following value:
#' #' - \code{.stopCondition}: Returns \code{TRUE} if the condition of \code{D} reaches the desired condition
#' - \code{.selectAgent}: Returns the character vector of the agent IDs (e.g., \code{"ID1"}, \code{"ID2"}...)
#' In addition, be sure to write the function in such a way that it takes should \code{D} as the first argument without any default value.
#'
#' The second way is to use a built-in function of this package.
#' This second way actually has further two variations. First, the easiest one,
#' just supply the function object to \code{.stopCondition} and \code{.selectAgent} (e.g., .act = function_name).
#' Second, if user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
#' See the examples below.
#'
#' @returns  a \code{netABM_network} class object
#' @family runABM
#' @author Keiichi Satoh
#' @importFrom R6 R6Class
#' @importFrom rlang parse_expr
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @export
#' @examples
#' node_attr <- data.frame(
#'  age = c(0, 0, 0, 0, 0),
#'  sex = c("m","m","m","f","f"))
#' network <- matrix(0, 5, 5)
#' agent_get_older <- function(D){self$a$age <- self$a$age + 1}
#'
#' # Create the netABM_network object
#' D <- setABM_network(n = 5,
#'                     node_attr = node_attr,
#'                     .act = list(agent_get_older))
#' # run the simulation
#' D <- runABM_network(D = D,
#'                     .stopCondition = stopABM_times(simTimes = 10))
#' # result
#' D


runABM_network <- function(D,
                           .stopCondition = NULL,
                           .selectAgent = NULL,
                           save_log = FALSE){
  # Dをチェック(ABM_networkクラスかどうかでwaningを出す)
  if(!any(class(D) == "netABM")){
    warning("D is supposed to be the class of: netABM")
  }

  # .stopCondition
  temp_stopCondition <- substitute(.stopCondition)
  switch(class(temp_stopCondition),
         ## NULLの場合：
         "NULL" = {
           .stopCondition_name <- "stopABM_times"
           .stopCondition_args <- list(simTimes = 1)
           stopCondition_label <- "stopABM_times"
           },
         "name" = {
           stopifnot(".stopCondition object does not exists in the environment" = exists(as.character(temp_stopCondition)))
           stopifnot(".stopCondition object is not the class of function" = is.function(get(as.character(temp_stopCondition))))
           stopCondition_label <- as.character(temp_stopCondition)
           .stopCondition_name <- as.character(temp_stopCondition)
           .stopCondition_args <- list()
         },
         "call" = {
           stopCondition_label <- temp_stopCondition
           .stopCondition_name <- call_name(temp_stopCondition)
           .stopCondition_args <- call_args(temp_stopCondition)
           # 無名関数の場合には修正
           if(.stopCondition_name=="function"){
             anonymous_stopFunc <- .stopCondition
             stopCondition_label <- "anonymous_stopFunc"
             .stopCondition_name <- "anonymous_stopFunc"
             .stopCondition_args <- as.list(formals(anonymous_stopFunc))
           }
         },
         stop("invalid .stopCondition type.")
  )
  ## Dに関するargumentが入っているとコンフリクトが起こるためNULLに
  .stopCondition_args["D"] <- NULL

  # .selectAgent
  temp_selectAgent <- substitute(.selectAgent)
  switch(class(temp_selectAgent),
         ## NULLの場合：
         "NULL" = {
           selectAgent_label <- "selectAgent_all"
           .selectAgent_name <- "selectAgent_all"
           .selectAgent_args <- list()
         },
         "name" = {
           stopifnot(".selectAgnet object does not exists in the environment" = exists(as.character(temp_selectAgent)))
           stopifnot(".selectAgnet object is not the class of function" = is.function(get(as.character(temp_selectAgent))))
           selectAgent_label <- as.character(temp_selectAgent)
           .selectAgent_name <- as.character(temp_selectAgent)
           .selectAgent_args <- list()
         },
         "call" = {
           selectAgent_label <- temp_selectAgent
           .selectAgent_name <- call_name(temp_selectAgent)
           .selectAgent_args <- call_args(temp_selectAgent)
           # 無名関数の場合には修正
           if(.selectAgent_name=="function"){
             anonymous_selectAgentFunc <- .selectAgent
             selectAgent_label <- "anonymous_selectAgentFunc"
             .selectAgent_name <- "anonymous_selectAgentFunc"
             .selectAgent_args <- as.list(formals(anonymous_selectAgentFunc))
           }
         },
         stop("invalid .selectAgent type.")
  )
  ## Dに関するargumentが入っているとコンフリクトが起こるためNULLに
  .selectAgent_args["D"] <- NULL

  # 時間を設定する
  ## D$timeがNULLの場合には1を代入
  if(is.null(D$time)){
    D$time <- 1
  }
  time <- D$time
  cat(paste0("time: ", time, " "))

  # シミュレーション用の環境を設定する
  ## .stopConditionと.selectAgentの関数のそれぞれコピーを作成
  .stopConditionFunc <- get(.stopCondition_name)
  .selectAgentFunc <- get(.selectAgent_name)
  ## 新しい環境を設定
  sim_env <- rlang::new_environment(data = c(D = list(D),
                                             .stopConditionFunc = .stopConditionFunc,
                                             .stopCondition_args = list(.stopCondition_args),
                                             .selectAgentFunc = .selectAgentFunc,
                                             .selectAgent_args = list(.selectAgent_args)))
  # AgentについてはDEEP cloneする
  for(i in 1:length(sim_env$D$agents)){
    sim_env$D$agents[[i]] <- sim_env$D$agents[[i]]$clone(deep = TRUE)
  }

  # variable_labelを取得
  variable_type <- attr(D$agents, "variable_type")
  sim_env$variable_type <- variable_type

  ## log = Fの場合
  if(save_log == FALSE){
    repeat{
      # 時間を更新
      sim_env$D$time <- sim_env$D$time + 1
      cat(paste0(sim_env$D$time, " "))

      # 対象Agentを選定
      selected_agent <- do.call(".selectAgentFunc",
                                args = c(sim_env$.selectAgent_args,
                                         D = list(sim_env$D)),
                                envir = sim_env)

      # 対象Agentに行為させる
      netABM::lapply_mms(selected_agent, function(i) {
        sim_env$D$agents[[i]]$.act(D = sim_env$D)
      })

      # 終了条件かどうかをチェック
      if(do.call(what = ".stopConditionFunc",
                 args = c(sim_env$.stopCondition_args,
                          D = list(sim_env$D)),
                 envir = sim_env)){
        break()
      }
    } # repeat終わり
  }else{
    ## save_logありの場合
    ### 現状のものを保存
    if(is.na(sim_env$D$log)|is.null(sim_env$D$log)){
      sim_env$new_log <- list()
      sim_env$new_log$agents <- lapply(1:length(sim_env$D$agents), function(i){
        as.list(sim_env$D$agents[[i]])[c("a","e")]
      })
      names(sim_env$new_log$agents) <- names(sim_env$D$agents)
      sim_env$new_log$time <- sim_env$D$time
      sim_env$D$log <- list(sim_env$new_log)
    }else{
      sim_env$new_log <- list()
      sim_env$new_log$agents <- lapply(1:length(sim_env$D$agents), function(i){
        as.list(sim_env$D$agents[[i]])[c("a", "e")]
      })
      names(sim_env$new_log$agents) <- names(sim_env$D$agents)
      sim_env$new_log$time <- sim_env$D$time
      sim_env$D$log <- c(sim_env$D$log, list(sim_env$new_log))
    }

    ### リピート
    repeat{
      # 時間を更新
      sim_env$D$time <- sim_env$D$time + 1
      cat(paste0(sim_env$D$time, " "))

      # 対象Agentを選定
      selected_agent <- do.call(".selectAgentFunc",
                                args = c(sim_env$.selectAgent_args,
                                         D = list(sim_env$D)),
                                envir = sim_env)

      # 対象Agentに行為させる
      netABM::lapply_mms(selected_agent, function(i) {
        sim_env$D$agents[[i]]$.act(D = sim_env$D)
      })

      # logをつける
      sim_env$new_log <- list()
      sim_env$new_log$agents <- netABM::lapply_mms(1:length(sim_env$D$agents), function(i){
        as.list(sim_env$D$agents[[i]])[c("a", "e")]
      })
      names(sim_env$new_log$agents) <- names(sim_env$D$agents)
      sim_env$new_log$time <- sim_env$D$time
      sim_env$D$log <- c(sim_env$D$log, list(sim_env$new_log))

      # 終了条件かどうかをチェック
      if(do.call(what = ".stopConditionFunc",
                 args = c(sim_env$.stopCondition_args,
                          D = list(sim_env$D)),
                 envir = sim_env)){
        break()
      }
    }# リピートを終わり
    ### logの名前をつける
    names(sim_env$D$log) <- paste0("t",
                                   unlist(lapply(sim_env$D$log, function(X){X$time})))
  }

  # 結果を返す
  ## sim_envのものをもとのDにコピーする
  D <- sim_env$D
  ## メモリーリークを防ぐためsim_envを削除する
  rm(sim_env)
  ## 結果であるDを戻す
  D
}
