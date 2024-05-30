#' @title Run the simulation based on the netABM_ca object
#' @description
#' \code{runABM_spatNetwork} let agents their action defined by \code{.} in the
#' \code{netABM_spatNetwork} object.
#' @param D a \code{netABM_spatNetwork} class object.
#' @param .stopCondition A user-defined or built-in function object that
#' determines when the simulation to stop.
#' The default value \code{NULL} will result in running one simulation.
#' @param .selectAgent A user-defined or built-in function object about
#' which agents to select. The default value \code{NULL} will result in
#' selecting all agents, meaning that all agents do their action each time.
#' @param save_log logical; if the log of each run should be saved.
#' The default is \code{FALSE}.
#' @details
#' \code{runAgent_spatNetwork} is a simulator based on \code{netABM_spatNetwork} object (D).
#' In each run the selected agents act their action defined \code{.f} of \code{netABM_spatNetwork}
#' object.
#'
#' Because the counting system of \code{R} starts from 1, \code{runABM} counts
#' the initial time starts from 1,
#' which means that the one run of the simulation corresponds to time 2.
#'
#' For setting \code{.stopCondition} and \code{.selectAgent} condition, there are two different ways.
#' The first way is to write the user's own function.
#' Upon writing an original function, be sure to set \code{D} as the first argument without any default;
#' otherwise agent's action does not reflect dynamically to the changing \code{D} object during the simulation.
#' \code{self} is a reserved for indicating the agent themselves.
#' In addition, each function should returns the following value:
#' #' - \code{.stopCondition}: Returns \code{TRUE} if the condition of \code{D} reaches the desired condition
#' - \code{.selectAgent}: Returns the character vector of the agent IDs (e.g., \code{"A1"}, \code{"A2"}...)
#' In addition, be sure to write the function in such a way that it takes \code{D} as the first argument without any default value.
#'
#' The second way is to use a built-in function of this package.
#' This second way actually has further two variations. First, the easiest one,
#' just supply the function object to \code{.stopCondition} and \code{.selectAgent} (e.g., .stopCondition = function_name).
#' Second, if user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
#' See the examples below.
#'
#' @returns  a \code{netABM_spatNetwork} class object
#' @family runABM
#' @author Keiichi Satoh
#' @importFrom R6 R6Class
#' @importFrom rlang parse_expr
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @import Matrix
#' @export
#' @examples
#' # Example
#' move_to_vacant_place <- function(D){
#'   D$stage$ca$ca[self$ca_adr] <- 0
#'   vacant_place <- which(D$stage$ca$ca == 0)
#'   D$stage$ca$ca[sample(vacant_place, 1)] <- self$ID
#' }
#' D <- setABM_ca(agent_n = 5,  agent_f = move_to_vacant_place)
#' D$stage$ca$ca
#' D <- runABM_ca(D = D, save_log = TRUE)
#' D$log$t1$stage$ca$ca
#' D$log$t2$stage$ca$ca

runABM_ca <- function(D,
                      .stopCondition = NULL,
                      .selectAgent = NULL,
                      save_log = FALSE){
  # Dをチェック(ABM_networkクラスかどうかでwaningを出す)
  if(!any(class(D) == "netABM_ca")){
    warning("D is supposed to be the class of: netABM_ca")
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
  for(i in 1:length(sim_env$D$agent)){
    sim_env$D$agent[[i]] <- sim_env$D$agent[[i]]$clone(deep = TRUE)
  }

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
      lapply(selected_agent, function(i) {
        sim_env$D$agent[[i]]$.f(D = sim_env$D)
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
      #### agent
      sim_env$new_log$agent <- lapply(1:length(sim_env$D$agent), function(i){
        as.list(sim_env$D$agent[[i]])[c("ID","a")]
      })
      names(sim_env$new_log$agent) <- names(sim_env$D$agent)
      #### stage
      sim_env$new_log$ca <- as.list(sim_env$D$ca)[!names(sim_env$D$ca) %in% c(".__enclos_env__","clone","print")]
      #### time
      sim_env$new_log$time <- sim_env$D$time
      #### logを付与
      sim_env$D$log <- list(sim_env$new_log)
    }else{
      ### log = NAの場合と途中まで同じだが、最後に既存のlogに追加する所だけ異なる
      sim_env$new_log <- list()
      #### agent
      sim_env$new_log$agent <- lapply(1:length(sim_env$D$agent), function(i){
        as.list(sim_env$D$agent[[i]])[c("ID","a")]
      })
      names(sim_env$new_log$agent) <- names(sim_env$D$agent)
      #### stage
      sim_env$new_log$ca <- as.list(sim_env$D$ca)[!names(sim_env$D$ca) %in% c(".__enclos_env__","clone","print")]
      #### time
      sim_env$new_log$time <- sim_env$D$time
      #### log
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
      lapply(selected_agent, function(i) {
        sim_env$D$agent[[i]]$.f(D = sim_env$D)
      })

      # logをつける
      ### log = NAの場合と途中まで同じだが、最後に既存のlogに追加する所だけ異なる
      sim_env$new_log <- list()
      #### agent
      sim_env$new_log$agent <- lapply(1:length(sim_env$D$agent), function(i){
        as.list(sim_env$D$agent[[i]])[c("ID","a")]
      })
      names(sim_env$new_log$agent) <- names(sim_env$D$agent)
      #### stage
      sim_env$new_log$ca <- as.list(sim_env$D$ca)[!names(sim_env$D$ca) %in% c(".__enclos_env__","clone","print")]
      #### time
      sim_env$new_log$time <- sim_env$D$time
      #### log
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
