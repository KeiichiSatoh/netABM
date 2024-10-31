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
#' - \code{"all"}: selecting all agents.
#' - \code{"one"}: Randomly selecting one agent in each time.
#'
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

runABM <- function(D, schedule = NULL,
                   remove_field = NULL, rename_field = NULL, keep_field = NULL,
                   update_FUN_name = NULL,
                   stop_FUN_name = NULL,
                   times = 1, save_log = FALSE,
                   add_tryCatch = TRUE,
                   return_update_FUN = FALSE,
                   saveRDS_inbetween = FALSE,
                   temp_E = NULL,
                   RDS_file_name = "D_temp.rds"){
  # Dをdeep clone
  D <- D$clone(deep = TRUE)
  # DがABM_Dかをチェック
  stopifnot("D must be the class of ABM_D" = class(D)[1]=="ABM_D")

  # rename_fieldに対処
  if(!is.null(rename_field)){
    stopifnot("rename_field must be a character vector." = (is.vector(rename_field) & is.character(rename_field)))
    stopifnot("rename_field must be inputted as c('new_name' = 'old.name'." = !is.null(names(rename_field)))
    stopifnot("some elements in rename_field lacks name." = any(names(rename_field)=="")==FALSE)
    stopifnot("Meta data cannot be renamed." = any(rename_field %in% c("time", "log","notes"))==FALSE)
    rename_field_df <- data.frame(old_name = rename_field, new_name = names(rename_field))
    field_type <- D$.field_type()
    for(i in 1:NROW(rename_field_df)){
      if(field_type[rename_field_df[i,"old_name"]] %in% c("global_FUN", "select_FUN","stop_FUN","update_FUN")){
        D$.add_FUN(name = rename_field_df[i, "new_name"],
                   FUN = D[[rename_field_df[i,"old_name"]]], FUN_type = field_type[rename_field_df[i,"old_name"]]) # copy
        D$.remove_field(field_name = rename_field_df[i, "old_name"])
      }else if(field_type[rename_field_df[i, "old_name"]]=="agent"){
        D$.add_agent(name = rename_field_df[i, "new_name"],
                     agent = D[[rename_field_df[i, "old_name"]]])
        D$.remove_field(field_name = rename_field_df[i, "old_name"])
      }else{
        D$.add_stage(name = rename_field_df[i, "new_name"],
                     stage = D[[rename_field_df[i, "old_name"]]],
                     field_type = field_type[rename_field_df$old_name[i]])
        D$.remove_field(field_name = rename_field_df[i, "old_name"])
      }
    }
    # 処理をプリント
    cat("Renamed field: ")
    for(i in 1:NROW(rename_field_df)){
      cat(rename_field_df$old_name[i], " -> ", rename_field_df$new_name[i], "; ", sep = "")
    }
    cat("\n")
  } #----rename_field対処ここまで

  # keep_fieldに対処
  if(!is.null(keep_field)){
    stopifnot("keep_field must be a character vector." = (is.vector(keep_field) & is.character(keep_field)))
    stopifnot("keep_field and remove_field cannot be set at the same time." = !is.null(remove_field))
    field_list <- D$.field_list()
    remove_field <- setdiff(field_list$name, keep_field)
    for(X in remove_field){
      D$.remove_field(field_name = X)
    }
    cat("Kept field:", keep_field, sep = " ")
    cat("Removed field:", remove_field, sep = " ")
    cat("\n")
  } #-----keep_field対処ここまで

  # remove_fieldに対処
  if(!is.null(remove_field)){
    stopifnot("remove_field must be a character vector." = (is.vector(remove_field) & is.character(remove_field)))
    stopifnot("Meta data cannot be removed." = any(remove_field %in% c("time", "log","notes"))==FALSE)
    for(X in remove_field){
      D$.remove_field(field_name = X)
    }
    cat("Removed field:", remove_field, sep = " ")
    cat("\n")
  } #-----remove_field対処ここまで

  # E(tempral_environment)を新たに作る
  if(!is.null(temp_E)){
    E <- rlang::new_environment(temp_E)
  }else{
    E <- NULL
  }
  on.exit(rm(E, envir = environment()), add = TRUE)

  # update_FUN_nameとscheduleの状況に合わせて対処
  if(is.null(update_FUN_name) && is.null(schedule)){
    ## (1) update_FUN_nameとscheduleが両方投入されていない場合
    update_FUN_candid <- D$.FUN_list()$type %in% c("act_FUN","global_FUN", "update_FUN")
    update_FUN_candid <- update_FUN_candid[update_FUN_candid]
    if(length(update_FUN_candid)==0){
      ### (1)-1: 一つもupdate_FUNの候補がない場合
      stop("No update_FUN candidate found in D.")
    }else{
      ### (1)-2: update_FUNの候補がある場合、一番上のものを使用する
      update_FUN_decided <- D$.FUN_list()[update_FUN_candid, ][1, ]
      ### 決めたupdate_FUNをアナウンス
      cat("update_FUN:", update_FUN_decided$FUN_name,"\n")
      ###
      if(update_FUN_decided$type=="act_FUN"){
        #### 決めたupdate_FUNがact_FUNの場合
        update_FUN <- function(D = D, E = E){self <- self}
        #### tryCatchによる場合分け
        if(add_tryCatch){
          update_FUN_text <- parse(text = paste0(
            "lapply(sample(1:length(D$", update_FUN_decided$agent, ")), function(i){",
            "tryCatch(","D$", update_FUN_decided$agent, "[[i]]$", update_FUN_decided$FUN_name, "(D = D, E = E)",
            ", error = function(e){
            message(paste0('error occured for agent ', i))
            return(NULL)})", "})"
          ))[[1]]
        }else{
          update_FUN_text <- parse(text = paste0(
            "lapply(sample(1:length(D$", update_FUN_decided$agent, ")), function(i){D$",
            update_FUN_decided$agent, "[[i]]$", update_FUN_decided$FUN_name, "(D = D, E = E)})"
          ))[[1]]
        }
        #### bodyに貼り付ける
        body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                           update_FUN_text))
      }else{
        #### 決めたupdate_FUNがglobal_FUNもしくはupdate_FUNの場合
        update_FUN <- function(D = D, E = E){}
        #### tryCatchで分岐
        if(add_tryCatch){
          update_FUN_text <- parse(
            text = paste0("tryCatch(D$", update_FUN_decided$FUN_name, "(D = D, E = E)",
                          ", error = function(e){message(paste0(",
                          "'error occured for ",
                          update_FUN_decided$FUN_name,"'))
                          return(NULL)})"
            ))[[1]]
        }else{
          update_FUN_text <- parse(text = paste0("D$", update_FUN_decided$FUN_name, "(D = D, E = E)"))[[1]]
        }
        #### bodyに貼り付ける
        body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                           update_FUN_text))
      }
    }
    #-----update_FUN_nameもscheduleのいずれも投入されていない場合
  }else if(!is.null(update_FUN_name)){
    ## (2) update_FUN_nameが投入されている場合
    stopifnot("update_FUN_name must be a character with the length of one." = (is.character(update_FUN_name) && length(update_FUN_name)==1))
    FUN_list <- D$.FUN_list()
    stopifnot("Inputted update_FUN_name is not update_FUN type." = FUN_list[FUN_list$FUN_name == update_FUN_name,"type"]=="update_FUN")
    #### 決めたupdate_FUNにselfを組み合わせる
    update_FUN <- function(D = D, E = E){}
    ## tryCatchをするか
    if(add_tryCatch){
      update_FUN_text <- parse(
        text = paste0("tryCatch(D$", update_FUN_name, "(D = D, E = E)",
                      ", error = function(e){message(paste0(",
                      "'error occured for ",
                      update_FUN_name,"'))
                          return(NULL)})"
        ))[[1]]
    }else{
      update_FUN_text <- parse(text = paste0("D$", update_FUN_name, "(D = D, E = E)"))[[1]]
    }
    #### bodyに貼り付ける
    body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                       update_FUN_text))
    ## update_FUNをアナウンス
    cat("update_FUN:", update_FUN_name, "\n")
    # -----update_FUN_nameが投入されている場合
  }else{
    # (3) scheduleに基づく場合---------------------------------
    ## scheduleのフォーマットを確認
    stopifnot("schedule must be a vector." = (is.vector(schedule) & !is.list(schedule)))
    stopifnot("schedule must be character string(s)." = all(is.character(schedule)))

    # FUN_listを取得
    FUN_list <- D$.FUN_list()
    ## 実際に存在する名前か確認
    check_FUN_name <- setdiff(schedule, FUN_list$FUN_name)
    if(length(check_FUN_name)>0){
      stop(paste("The following name in schedule does not exist:", check_FUN_name))
    }

    # scheduleで指定されたFUNのみからなるリストを作成
    FUN_posit <- unlist(lapply(schedule, function(X){which(X==FUN_list$FUN_name)}))
    schedule_list <- FUN_list[FUN_posit, ]

    # 重複のある名前がないか確認
    duplicate_check <- table(schedule_list$FUN_name) > 1
    if(any(duplicate_check)){
      stop(paste("The following names are duplicated. Please rename them:", names(duplicate_check)[duplicate_check==TRUE]))
    }

    ## scheduleにglobal_FUN, agent, select_FUN, partial_update_FUN_body以外が入っている場合にはストップ
    stopifnot("schedule must be either global_FUN, act_FUN, select_FUN, or partial_update_FUN_body." = schedule_list$type %in% c("global_FUN","act_FUN","select_FUN","partial_update_FUN_body"))

    ## agentのアクションの前にselect_FUNがあるか確認し、ない場合には当該agent全員を選ぶようにscheduleを書き直す
    if(schedule_list$type[1]=="act_FUN"){
      schedule_list <- rbind(
        data.frame(FUN_name = "[select_all]", type = "select_FUN", agent = schedule_list$agent[1]),
        schedule_list)
    }
    temp <- vector("list", length = NROW(schedule_list))
    temp[[1]] <- schedule_list[1, ]
    if(nrow(schedule_list)>=2){
      for(i in 2:nrow(schedule_list)){
        if(schedule_list$type[i]=="act_FUN" & schedule_list$type[i-1]!="select_FUN"){
          temp[[i]] <- rbind(
            data.frame(FUN_name = "[select_all]", type = "select_FUN", agent = schedule_list$agent[i]),
            schedule_list[i, ])
        }else{
          temp[[i]] <- schedule_list[i, ]
        }
      }
    }
    schedule_list <- do.call(rbind, temp)

    ## select_FUNの位置をチェック
    select_FUN_posit <- which(schedule_list$type == "select_FUN")
    posit_check <- unlist(lapply(select_FUN_posit, function(posit){
      tryCatch(schedule_list[posit + 1, "type"]=="act_FUN",
               error = function(e){FALSE})}))
    stopifnot("All select_FUN must be followed by act_FUN." = all(posit_check)==TRUE)

    ## scheduleに基づきupdate_FUNの要素を作成する
    update_FUN_parts <- vector(mode = "list", NROW(schedule_list))
    for(j in 1:length(update_FUN_parts)){
      update_FUN_parts[[j]] <- switch(
        schedule_list$type[j],
        "select_FUN" = {NULL},
        "partial_update_FUN_body" = {
          D[[schedule_list$FUN_name[j]]]
        },
        "act_FUN" = {
          # select_FUN部の前処理
          if(schedule_list$FUN_name[j-1]=="[select_all]"){
            agent <- schedule_list$agent[j-1]
            text_select <- paste0("1:length(D$",agent,")")
          }else{
            FUN <- schedule_list$FUN_name[j-1]
            ## tryCatchによる分岐
            if(add_tryCatch){
              text_select <- paste0(
                "tryCatch(",
                "D$",FUN,"(D = D, E = E)",
                ", error = function(e){
                         message(paste0('error occured for ", FUN, ".'))
                return(NULL)})")
            }else{
              text_select <- paste0("D$",FUN,"(D = D, E = E)")
            }
          } #--select_FUN部の前処理
          # act_FUNの処理
          agent <- schedule_list$agent[j]
          FUN <- schedule_list$FUN_name[j]
          # tryCatchによる分岐
          if(add_tryCatch){
            act_FUN_text <- paste0(
              "lapply(sample(", text_select, "), function(i){",
              "tryCatch(","D$", agent, "[[i]]$", FUN, "(D = D, E = E)",
              ", error = function(e){
            message(paste0('error occured for agent i for ", FUN,"'))
            return(NULL)})", "})"
            )
          }else{
            act_FUN_text <- paste0(
              "lapply(sample(", text_select, "), function(i){D$", agent, "[[i]]$", FUN,"(D = D, E = E)})")
          }  #----add_tryCatchによる処理
          parse(text = act_FUN_text)[[1]]
        },
        "global_FUN" = {
          FUN <- schedule_list$FUN_name[j]
          if(add_tryCatch){
            FUN_text <- paste0("tryCatch(D$", FUN, "(D = D, E = E)",
                               ", error = function(e){message(paste0(",
                               "'error occured for ", FUN,"'))
                          return(NULL)})")
          }else{
            FUN_text <- paste0("D$", FUN,"(D = D, E = E)")
          }#---add_tryCatch処理
          parse(text = FUN_text)[[1]]
        })
    } #-----FUN要素を構成する

    ## 得られたパーツのうち、NULLの部分を外す
    update_FUN_parts <- Filter(Negate(is.null), update_FUN_parts)

    ## 基となるupdate_FUNを作成し、パーツを組み合わせる
    update_FUN <- function(D = D, E = E){}
    body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                       update_FUN_parts))
    ## スケジュールについて知らせる
    schedule_print <- cbind(schedule_list, colon = "")
    schedule_print[!is.na(schedule_print$agent),"colon"] <- ":"
    schedule_print[is.na(schedule_print$agent),"agent"] <- ""

    cat(c("update_FUN created from schedule: ","\n",
          "  ", schedule_print$agent[1], schedule_print$colon[1], schedule_print$FUN_name[1]), sep = "")
    if(NROW(schedule_print)>1){
      for(i in 2:NROW(schedule_print)){
        cat(" -> ", schedule_print$agent[i],
            schedule_print$colon[i], schedule_print$FUN_name[i], sep = "")}
    }
    cat("\n", "\n") # 次の表示と重ならないように行替え
  } #----scheduleに基づく場合

  # stop_FUN_name
  if(!is.null(stop_FUN_name)){
    stop_FUN <- D[[stop_FUN_name]]
    cat(paste0("stop_FUN:", stop_FUN_name))
  }else{
    ## timesに基づく
    stopifnot("times must be a positive integer." = (is.numeric(times) && times >= 1))
    sim_time <- D$time + times
    stop_FUN <- function(D, E = NULL){D$time >= sim_time}
    cat(paste0("stop_FUN: ", "[stop times at ", sim_time, "]"))
    cat("\n")
  }

  # RDS_file_nameの処理
  if(saveRDS_inbetween == TRUE){
    stopifnot("RDS_file_name must be character scaler." = (is.character(RDS_file_name) && length(RDS_file_name)==1))
  }

  # Ready to run
  cat(paste0("\n"))
  cat(paste0("Ready to run...", "\n"))

  # current time
  cat(paste0("  start time  : ", D$time, "\n"))

  # start_timeを記録
  start_time <- Sys.time()

  # save_log = FALSEの場合
  if(save_log == FALSE & saveRDS_inbetween==FALSE){
    repeat{
      # 時間を更新
      D$time <- D$time + 1
      cat(paste0("  current time: ", D$time, "\n"), sep = "")
      # update
      update_FUN(D = D, E = E)
      # 終了条件を確認
      if(stop_FUN(D = D, E = E)){break}
    }
    #---save_log = Fここまで--------------
  }else if(save_log == TRUE & saveRDS_inbetween==FALSE){
    repeat{
      # 時間を更新
      D$time <- D$time + 1
      cat(paste0("  current time: ", D$time, "\n"), sep = "")
      # update
      update_FUN(D = D, E = E)
      # 現状を記録
      D$.save()
      # 終了条件を確認
      if(stop_FUN(D = D, E = E)){break}
    }
    #----save_log = Tここまで--------------
  }else if(save_log == FALSE & saveRDS_inbetween==TRUE){
    repeat{
      # 時間を更新
      D$time <- D$time + 1
      cat(paste0("  current time: ", D$time, "\n"), sep = "")
      # update
      update_FUN(D = D, E = E)
      # saveRDS
      saveRDS(D, file = RDS_file_name)
      # 終了条件を確認
      if(stop_FUN(D = D, E = E)){break}
    }
    #---save_log = Fここまで--------------
  }else if(save_log == TRUE & saveRDS_inbetween==TRUE){
    repeat{
      # 時間を更新
      D$time <- D$time + 1
      cat(paste0("  current time: ", D$time, "\n"), sep = "")
      # update
      update_FUN(D = D, E = E)
      # 現状を記録
      D$.save()
      # saveRDS
      saveRDS(D, file = RDS_file_name)
      # 終了条件を確認
      if(stop_FUN(D = D, E = E)){break}
    }
    #----save_log = Tここまで--------------
  }else{
    stop("The setting about 'save_log' and 'saveRDS_inbetween' seems to be wrong.")
  }

  # end_timeを記録
  end_time <- Sys.time()

  # Finished
  cat("Finished.", "\n", "\n")

  # 実行時間を計算（秒単位で取得）
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  # 時、分、秒、ミリ秒に変換
  hours <- floor(time_taken / 3600)
  minutes <- floor((time_taken %% 3600) / 60)
  seconds <- floor(time_taken %% 60)
  milliseconds <- round((time_taken %% 1) * 1000)
  # ミリ秒までの時間を「時:分:秒.ミリ秒」の形式にフォーマット
  time_hms <- sprintf("%02d:%02d:%02d.%03d", hours, minutes, seconds, milliseconds)
  # 実行時間を表示
  cat(paste("Simulation took", time_hms, "(hh:mm:ss.mmm)", "\n"))

  # リターン
  if(return_update_FUN==TRUE){
    return(list(D = D, update_FUN = update_FUN))
  }
  D
}# 関数ここまで

