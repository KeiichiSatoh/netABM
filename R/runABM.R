#' @title Run the simulation based on the ABM_D object
#' @description
#' \code{runABM} let agents their action defined by \code{.f} of each agent
#' in the \code{ABM_D} object.
#' @param D a \code{ABM_D} class object.
#' @param selectFUN either a character of \code{"all"} or \code{"one"} or
#' a user-defined function object about which agents to select for each time.
#' @param stopFUN An integer or a user-defined function that
#' determines when the simulation to stop. The default value \code{1} will
#' result in running one simulation.
#' @param updateFUN A user-defined function that
#' determines how the update of the field will be done.
#' The default value \code{NULL} will result in letting the selected agents
#' defined in \code{selectFUN} act based on their \code{.f} of each agent.
#' @param save_log logical; if the log of each run should be saved.
#' The default is \code{FALSE}.
#' @details
#' \code{runABM} is a simulator based on \code{ABM_D} object.
#' In each run the selected agents act their action defined in \code{.f} of each agent.
#'
#' Because the counting system of \code{R} starts from 1, \code{runABM} counts
#' the initial time starts from 1, which means that the one run of the
#' simulation corresponds to time 2.
#'
#' The built-in \code{.selectFUN} are as follows:
#' \list{
#' - \code{"all"}: selecting all agents.
#' - \code{"one"}: Randomly selecting one agent in each time.
#' }
#' If user supplies its own stop function, the function should return the
#' integer of each agent ID.
#'
#' If user supplied an original function to \code{stopFUN}, it should return
#' \code{TRUE} when the condition to stop is satisfied.
#'
#' If user supplied an own function to \code{updateFUN}, it should define
#' how to update the object \code{D}.
#' Notably, the default behavior of this function is that
#' it internally creates a new function into which the supplied selectFUN
#' and updateFUN is integrated in this order. Accordingly, if the user-defined
#' updateFUN includes a procedure to select agents, be sure to set \code{selectFUN}
#' as \code{NULL},
#'
#' @returns  a \code{ABM_D} class object
#' @author Keiichi Satoh
#' @importFrom rlang parse_expr
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @import Matrix
#' @import R6
#' @export
#' @examples
#' Data for the agent attributes and agent behavior
#' agent_attr <- data.frame(
#' age = c(0, 1, 2, 3, 4),
#' sex = c("m","m","m","f","f"))
#'
#' # Behavior 1: Agent gets older by beta in each time in a simulation.
#' agent_get_older <- function(beta = 1){self$age <- self$age + beta}
#' Set the default behavior
#' D1 <- setABM(agent_n = 3, agent_f = agent_get_older)
#'
#' # Example 1: Select all agents in each run
#' D1_1 <- runABM(D = D1, selectFUN = "all", save_log = TRUE)
#'
#' # Example 2: Select one agent in each run
#' D1_2 <- runABM(D = D1, selectFUN = "one", save_log = TRUE)

runABM <- function(
    D, selectFUN = "all", stopFUN = 1, updateFUN = NULL, save_log = FALSE){
  # Dをdeep clone
  D <- D$clone(deep = TRUE)
  # DがABM_Dかをチェック
  stopifnot("D must be the class of ABM_D" = class(D)[1]=="ABM_D")
  # selectFUN
  selectFUN_sbs <- substitute(selectFUN)
  shape_selectFUN_out <- .shape_selectFUN(selectFUN_sbs = selectFUN_sbs)
  selectFUN_shaped <- shape_selectFUN_out$selectFUN
  selectFUN_label <- shape_selectFUN_out$selectFUN_label
  # stopFUN
  stopFUN_sbs <- substitute(stopFUN)
  shape_stopFUN_out <- .shape_stopFUN(stopFUN_sbs = stopFUN_sbs, init_time = D$time)
  stopFUN_shaped <- shape_stopFUN_out$stopFUN
  stopFUN_label <- shape_stopFUN_out$stopFUN_label
  # repeatFUN
  updateFUN_sbs <- substitute(updateFUN)
  shape_updateFUN_out <- .shape_updateFUN(updateFUN_sbs = updateFUN_sbs)
  updateFUN_shaped <- shape_updateFUN_out$updateFUN
  updateFUN_label <- shape_updateFUN_out$updateFUN_label

  # labelの記録をnotesに貼り付ける
  D$notes <- c(D$notes,
               selectFUN = selectFUN_label,
               updateFUN = updateFUN_label,
               stopFUN = stopFUN_label)

  # すべてのFUNのargsを取得する
  selectFUN <- selectFUN_shaped
  selectFUN_args <- suppressWarnings(as.list(formals(selectFUN_shaped)))
  stopFUN <- stopFUN_shaped
  stopFUN_args <- as.list(formals(stopFUN_shaped))
  updateFUN <- updateFUN_shaped
  updateFUN_args <- as.list(formals(updateFUN_shaped))

  # selectFUNがNULLではないときには、updateFUNに入れ込む
  if(!is.null(selectFUN)){
    body(updateFUN) <- as.call(append(as.list(body(updateFUN)),
                                expression(selected_agent <- do.call(selectFUN, args = selectFUN_args)), after = 1))
    formals(updateFUN) <- c(formals(updateFUN), alist(selectFUN = selectFUN, selectFUN_args = selectFUN_args))
    updateFUN_args <- as.list(formals(updateFUN))
  }

  # current time
  cat(paste0("current time: ", D$time, " "))

  # save_log = FALSEの場合
  if(save_log == FALSE){
    repeat{
      # 時間を更新
      D$time <- D$time + 1
      cat(paste0(D$time, " "))
      # update
      do.call(updateFUN, args = updateFUN_args)
      # 終了条件を確認
      if(do.call(stopFUN, stopFUN_args)){break}
    }
    #---save_log = Fここまで--------------
  }else{
    repeat{
      # 時間を更新
      D$time <- D$time + 1
      cat(paste0(D$time, " "))
      # update
      do.call(updateFUN, args = updateFUN_args)
      # 現状を記録
      D$.save()
      # 終了条件を確認
      if(do.call(stopFUN, stopFUN_args)){break}
    }
  }#----save_log = Tここまで--------------
  # リターン
  D
}

