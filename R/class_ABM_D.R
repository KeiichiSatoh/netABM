#' ABM class
#' @title ABM_D Class
#' @docType class
#' @description \code{ABM_D} is the core class of \code{netABM} that bundle datasets for running ABM.
#' @field agent list of the agents
#' @field time current time of the dataset
#' @field log list of the past dataset
#' @field notes list of the notes. Default is \code{NULL}
#' @field .f A function that control the fields of ABM_D. Default is \code{NULL}
#' @examples
#' ABM_D$new()
#' @export
#'
ABM_D <- R6::R6Class("ABM_D", lock_objects = FALSE, cloneable = TRUE,
                         public = list(
                           log = NULL,
                           initialize = function(fields = list(), methods = list(), log = NULL) {
                             # フィールドをオブジェクトにセット
                             for (field_name in names(fields)) {
                               self[[field_name]] <- fields[[field_name]]
                             }
                             # メソッドをオブジェクトにセット
                             for (method_name in names(methods)) {
                               self[[method_name]] <- methods[[method_name]]
                               environment(self[[method_name]]) <- self$.__enclos_env__
                             }
                           },
                           #' @description
                           #' show the name of the active binding field
                           .active_binding_names = function(){
                             names(self$.__enclos_env__$.__active__)
                           },
                           #' @description
                           #' show the type of each field
                           .field_type = function(){
                             fields <- ls(self)[!ls(self) %in% c("clone","initialize","log","time","print")]
                             out <- sapply(fields, function(X){
                               attr(self[[X]], "field_type")})
                             out[names(out) %in% self$.active_binding_names()] <- "active_binding"
                             out
                           },
                           #' @description
                           #' remove field
                           .remove_field = function(field_name){
                             rm(list = field_name, envir = self)
                             if(field_name %in% self$.active_binding_names()){
                               self$.__enclos_env__$.__active__[[field_name]] <- NULL}
                           },
                           #' @description
                           #' add a set of agents
                           .add_agent = function(name, agent){
                             stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
                             stopifnot("All agent must be ABM_Agent class objects" = all(sapply(agent, function(X){class(X)[[1]]})=="ABM_Agent"))
                             attr(agent, "field_type") <- "agent"
                             self[[name]] <- agent
                           },
                           #' @description
                           #' @import Matrix
                           #' add stages
                           .add_stage = function(name, stage, field_type){
                             stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
                             switch(field_type,
                                    "net" = {
                                      stopifnot("stage must be matrix if this is added as net type" = is.matrix(stage))
                                      if(is.null(dimnames(stage))){
                                        message("Warning: dimnames lacks, which can cause problems when running the ABM.")
                                      }
                                      attr(stage, "field_type") <- "net"
                                      self[[name]] <- stage
                                    },
                                    "mat" = {
                                      stopifnot("stage must be either matrix or array if this is added as mat type" = (is.matrix(stage)|is.array(stage)))
                                      if(is.matrix(stage)){
                                        stage <- as(stage, "sparseMatrix")
                                      }
                                      attr(stage[[m]], "field_type") <- "mat"
                                      self[[name]] <- stage
                                    },
                                    "euc" = {
                                      stopifnot("stage must be data.frame if this is added as euc type" = is.data.frame(stage))
                                      if(is.null(colnames(stage))){
                                        message("Warning: colnames lacks, which can cause problems when running the ABM.")
                                      }
                                      attr(stage, "field_type") <- "euc"
                                      self[[name]] <- stage
                                    },
                                    "df" = {
                                      stopifnot("stage must be data.frame if this is added as df type" = is.data.frame(stage))
                                      if(is.null(colnames(stage))){
                                        message("Warning: colnames lacks, which can cause problems when running the ABM.")
                                      }
                                      attr(stage, "field_type") <- "df"
                                      self[[name]] <- stage
                                    },
                                    "other" = {
                                      attr(stage, "field_type") <- "other"
                                      self[[name]] <- stage
                                    },
                                    stop("field type must be either 'net', 'mat', 'euc', 'df', or 'other'.")
                                    )
                           },
                           #' @description
                           #' add FUN
                           .add_FUN = function(name, FUN, FUN_type){
                             stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
                             retrieved_FUN <- .get_FUN(FUN = FUN)
                             switch(FUN_type,
                                    "global_FUN" = {
                                      attr(retrieved_FUN, "field_type") <- "global_FUN"
                                    },
                                    "select_FUN" = {
                                      attr(retrieved_FUN, "field_type") <- "select_FUN"
                                    },
                                    "stop_FUN" = {
                                      attr(retrieved_FUN, "field_type") <- "stop_FUN"
                                    },
                                    "update_FUN" = {
                                      attr(retrieved_FUN, "field_type") <- "update_FUN"
                                    },
                                    stop("FUN_type must be either 'global_FUN', 'select_FUN', 'stop_FUN', or 'update_FUN'.")
                                    )
                             self[[name]] <- retrieved_FUN
                             environment(self[[name]]) <- self$.__enclos_env__
                           },
                           #' @description
                           #' add an active binding
                           .add_active_binding = function(name, FUN){
                             stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
                             # 関数かどうかを確認
                             stopifnot("FUN must be a function." = is.function(FUN))
                             # environmentを付加する
                             environment(FUN) <- self$.__enclos_env__
                             # active_bindingをする
                             makeActiveBinding(name, FUN, self$.__enclos_env__$self)
                             # 名前とセットにして環境に保存する
                             FUN_list <- list(FUN)
                             names(FUN_list) <- name
                             self$.__enclos_env__$.__active__ <- c(self$.__enclos_env__$.__active__,
                                                                   FUN_list)
                           },
                           #' @description
                           #' add an partial_update_FUN_body
                           .add_partial_update_FUN_body = function(name, partial_body){
                             stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
                             # フォーマットする
                             if(is.expression(partial_body)){
                               body <- partial_body[[1]]
                             }else if(is.character(partial_body)){
                               body <- parse(text = partial_body)[[1]]
                             }else{
                               stop("partial_body must be either an expression or a character string.")
                             }
                             attr(body, "field_type") <- "partial_update_FUN_body"
                             self[[name]] <- body
                           },
                           #' @description
                           #' Save the current status of the D into log
                           .save = function(){
                             # フィールド名を取得する
                             field <- self$.field_type()
                             field_agent <- names(field[field=="agent"])
                             field_other <- c(names(field[!field %in% c("agent", "global_FUN", "select_FUN", "stop_FUN", "update_FUN", "partial_update_FUN_body")]), "time")
                             D_values <- lapply(field_other, function(X){self[[X]]})
                             names(D_values) <- field_other
                             # agentの値を取得する
                             D_agent <- lapply(field_agent, function(field_agent_p){
                               Agent <- self[[field_agent_p]]
                               lapply(1:length(Agent), function(i){
                                 Agent[[i]]$.save()
                               })
                             })
                             names(D_agent) <- field_agent
                             # すべてをつなげてnew_logへ
                             D_values <- c(D_values, D_agent)
                             new_log <- list(D_values)
                             names(new_log) <- paste0("t", self$time)
                             if(is.null(self$log)){
                               self$log <- new_log
                             }else{
                               self$log <- c(self$log, new_log)
                             }
                           },
                           #' @description
                           #' get FUN list
                           .FUN_list = function(){
                             field_type <- self$.field_type()
                             # agent
                             agent_type <- names(field_type)[field_type=="agent"]
                             if(length(agent_type)>0){
                               act_FUN <- data.frame(FUN_name = NA, type = NA, agent = NA)
                               act_FUN <- act_FUN[-1, ]
                               for(agents in agent_type){
                                 act_FUN_names <- self[[agents]][[1]]$.act_FUN_names()
                                 if(!is.null(act_FUN_names)){
                                   new <- data.frame(FUN_name = act_FUN_names,
                                                     type = rep("act_FUN", length(act_FUN_names)),
                                                     agent = rep(agents, length(act_FUN_names)))
                                   act_FUN <- rbind(act_FUN, new)
                                 }
                               }
                             }else{
                               act_FUN <- NULL
                             }
                             # その他のFUN
                             FUN_type <- field_type[field_type %in% c("global_FUN","select_FUN","stop_FUN","update_FUN", "partial_update_FUN_body")]
                             if(length(FUN_type) > 0){
                               other_FUN <- data.frame(
                                 FUN_name = names(FUN_type),
                                 type = unlist(FUN_type),
                                 agent = NA
                               )
                             }else{
                               other_FUN <- NULL
                             }
                             # まとめる
                             out <- rbind(act_FUN, other_FUN)
                             rownames(out) <- NULL
                             out
                           },
                           #' @description
                           #' field list
                           .field_list = function(){
                             # field_type
                             field_type <- self$.field_type()
                             # agent
                             agent_names <- names(field_type[field_type=="agent"])
                             if(length(agent_names) > 0){
                               agent_list <- lapply(agent_names, function(agent){
                                 data.frame(FUN_name = self[[agent]][[1]]$.value_names(),
                                            type = "value",
                                            agent = agent)
                               })
                               agent_list <- do.call(rbind, agent_list)
                             }else{
                               agent_list <- NULL
                             }
                             # stage
                             stage_field <- field_type[field_type!="agent"]
                             if(length(stage_field)>0){
                               stage_list <- data.frame(FUN_name = names(stage_field),
                                                        type = unlist(stage_field),
                                                        agent = NA)
                             }else{
                               stage_list <- NULL
                             }
                             # まとめる
                             field_list <- do.call(rbind, list(agent_list, stage_list))
                             if(is.null(field_list)){
                               return(NULL)
                             }
                             rownames(field_list) <- NULL
                             colnames(field_list) <- c("name","type","agent")
                             field_list
                           },
                           #' @description
                           #' value list
                           .value_list = function(){
                             field_list <- self$.field_list()
                             value_list <- field_list[!field_list$type %in% c("global_FUN","select_FUN","stop_FUN","update_FUN", "partial_update_FUN_body"), ]
                             value_list
                           },
                           #' @description
                           #' get field value
                           .get_value = function(field_name, value_name = NULL, log = NULL){
                             # field_type
                             field_type <- self$.field_type()
                             field_type <- field_type[field_name]
                             # logによる場合分け
                             value <- if(is.null(log)){
                               ## log = NULLの場合---------
                               ## field_typeによる場合分け
                               switch(field_type[[1]],
                                      "agent" = {self$.agent_attr(agents = field_name, attr = value_name)},
                                      "df" = {
                                        if(is.null(value_name)){
                                          self[[field_name]]
                                        }else{
                                          self[[field_name]][ ,value_name]
                                        }
                                      },
                                      {
                                        temp <- self[[field_name]]
                                        if(!is.null(value_name)){
                                          temp <- temp[[value_name]]
                                        }
                                        temp
                                      }
                               )
                             }else{
                               ## log != NULLの場合-------
                               ## field_typeによる場合分け
                               switch(field_type[[1]],
                                      "agent" = {self$.agent_attr(agents = field_name, attr = value_name, log = log)[[1]]},
                                      "df" = {
                                        if(is.null(value_name)){
                                          self$log[[log]][[field_name]]
                                        }else{
                                          self$log[[log]][[field_name]][ ,value_name]
                                        }
                                      },
                                      {
                                        temp <- self$log[[log]][[field_name]]
                                        if(!is.null(value_name)){
                                          temp <- temp[[value_name]]
                                        }
                                        temp
                                        }
                               )
                             }
                             # 取得した値を返す
                             value
                           },
                           #' @description
                           #' print method of ABM_D
                           print = function(){
                             # field_typeを取得する
                             field_type <- self$.field_type()
                             # プリント
                             cat("<ABM_D>", "\n")
                             # agent
                             agent_type <- names(field_type)[field_type=="agent"]
                             if(length(agent_type)>0){
                               cat("[agent]", "\n")
                               for(X in agent_type){
                                 cat(X, " (n = ", length(self[[X]]), ")", "\n", sep = "")
                                 cat("  attr   :", c("ID", self[[X]][[1]]$.value_names()), "\n", sep = " ")
                                 cat("  act_FUN:", self[[X]][[1]]$.act_FUN_names(), "\n", sep = " ")
                                 if(!is.null(self[[X]][[1]]$.active_binding_names())){
                                   cat("  active_binding*:", self[[X]][[1]]$.active_binding_names(), "\n", sep = " ")
                                 }
                                 cat("\n")
                               }
                             }
                             # stage
                             cat("[stage]", "\n")
                             # net
                             net_type <- names(field_type)[field_type=="net"]
                             if(length(net_type)>0){
                               cat("net  :", net_type, "\n", sep = " ")
                             }
                             # mat
                             mat_type <- names(field_type)[field_type=="mat"]
                             if(length(mat_type)>0){
                               cat("mat  :", mat_type, "\n", sep = " ")
                             }
                             # euc
                             euc_type <- names(field_type)[field_type=="euc"]
                             if(length(euc_type)>0){
                               cat("euc  :", euc_type, "\n", sep = " ")
                             }
                             # df
                             df_type <- names(field_type)[field_type=="df"]
                             if(length(df_type)>0){
                               cat("df   :", df_type, "\n", sep = " ")
                             }
                             # other
                             other_type <- names(field_type)[field_type=="other"]
                             if(length(other_type)>0){
                               cat("other:", other_type, "\n", sep = " ")
                             }
                             # active binding
                             active_binding_type <- names(self$.__enclos_env__$.__active__)
                             if(!is.null(active_binding_type)){
                               cat("active_binding*:", active_binding_type, "\n", sep = " ")
                             }
                             cat("\n")
                             # Functions
                             cat("[FUN]", "\n")
                             # global_FUN
                             global_FUN_type <- names(field_type)[field_type=="global_FUN"]
                             if(length(global_FUN_type)>0){
                               cat("global_FUN:", global_FUN_type, "\n", sep = " ")
                             }
                             # select_FUN
                             select_FUN_type <- names(field_type)[field_type=="select_FUN"]
                             if(length(select_FUN_type)>0){
                               cat("select_FUN:", select_FUN_type, "\n", sep = " ")
                             }
                             # stop_FUN
                             stop_FUN_type <- names(field_type)[field_type=="stop_FUN"]
                             if(length(stop_FUN_type)>0){
                               cat("stop_FUN  :", stop_FUN_type, "\n", sep = " ")
                             }
                             # update_FUN
                             update_FUN_type <- names(field_type)[field_type=="update_FUN"]
                             if(length(update_FUN_type)>0){
                               cat("update_FUN:", update_FUN_type, "\n", sep = " ")
                             }
                             cat("\n")
                             # partial_update_FUN_body
                             partial_update_FUN_body_type <- names(field_type)[field_type=="partial_update_FUN_body"]
                             if(length(partial_update_FUN_body_type)>0){
                               cat("partial_update_FUN_body:", partial_update_FUN_body_type, "\n", sep = " ")
                             }
                             cat("\n")
                             # meta data
                             cat("[meta data]", "\n")
                             cat("time :", self$time, "\n")
                             cat("log  :", if(all(is.na(self$log))){"NULL"}else{length(self$log)}, "\n")
                             cat("notes:", names(self$notes), "\n")
                             },
                           #' @description
                           #' get agents' attributes as data.frame
                           .agent_attr = function(agents = NULL, attr = NULL, log = NULL, long_format = FALSE){
                             # もしもwhich_agentがNULLの場合にはfield_typeの最初の"agent" typeを取得する
                             if(is.null(agents)){
                               field <- self$.field_type()=="agent"
                               agents <- names(field)[field][1]
                               message(paste0("The attribute of ", agents, " has been retrieved."))
                             }
                             # もしもattrがNULLの場合にはすべてのattrを取得する
                             if(is.null(attr)){
                               attr <- c("ID", self[[agents]][[1]]$.value_names())
                             }
                             # log = NULLの場合：現在のデータから取得する
                             if(is.null(log)==TRUE){
                               dat <- lapply(self[[agents]], function(agent_i){
                                 data.frame(agent_i$.get_values(attr = attr))
                               })
                               df <- do.call(rbind, dat)
                               if(ncol(df)==1){
                                 label <- rownames(df)
                                 df <- unlist(df)
                                 names(df) <- label
                               }
                             }else{
                               # logから取得する場合
                               ## log = "all"の場合：すべてのlogを取得する
                               if(length(log)==1 && log == "all"){
                                 log <- 1:length(D$log)
                               }
                               ## dfを取得する
                               df <- lapply(log, function(t){
                                 df_t <- lapply(attr, function(X){
                                   unlist(lapply(self$log[[t]][[agents]], function(agent_i){
                                     agent_i[[X]]
                                   }))
                                 })
                                 df_t <- data.frame(df_t)
                                 rownames(df_t) <- paste0("ID", unlist(lapply(self$log[[t]][[agents]], function(X){X$ID})))
                                 colnames(df_t) <- attr
                                 if(ncol(df_t)==1){
                                   label <- rownames(df_t)
                                   df_t <- unlist(df_t)
                                   names(df_t) <- label
                                 }
                                 df_t
                               })
                               ### long_format = FALSEの場合
                               if(long_format==FALSE){
                                 names(df) <- paste0("t", log)
                               }else{
                                 ### long_format = TRUEの場合
                                 df <- lapply(log, function(t){
                                   data.frame(df[[log]], time = D$log[[t]]$time)
                                 })
                                 df <- do.call(rbind, df)
                               }
                             }
                             # output
                             df
                           },
                           #' @description
                           #' get the summary statistics
                           .summary = function(return_value = FALSE, log = NULL){
                             # logの長さは1とする
                             if(!is.null(log)){
                               stopifnot("Please select a single time point in the log." = length(log)==1)
                             }

                             # value_list
                             value_list <- self$.value_list()

                             # time
                             if(is.null(log)){
                               cat("<Summary as of time: ", D$time, ">", "\n", sep = "")
                             }else{
                               cat("<Summary as of time: ", D$log[[log]]$time, ">", "\n", sep = "")
                             }
                             # agent
                             agents <- unique(na.exclude(value_list$agent))
                             if(length(agents) > 0){
                               cat("[agent]", "\n")
                               desc_agent <- lapply(agents, function(agent){
                                 # 対象となる値を取得する
                                 temp_list <- na.exclude(value_list[value_list$agent==agent, ])
                                 desc_temp <- lapply(1:NROW(temp_list), function(i){
                                   value <- self$.get_value(field_name = temp_list$agent[i], value_name = temp_list$name[i], log = log)
                                   .summarise_value(X = value, name = temp_list$name[i])
                                 })
                                 desc_temp <- do.call(rbind, desc_temp)
                                 cat(agent, ":", "\n", sep = "")
                                 print(desc_temp)
                                 cat("\n")
                                 desc_temp
                               })
                               cat("\n", "\n")
                               names(desc_agent) <- agents
                             }else{
                               desc_agent <- NULL
                             }
                             # stage
                             cat("[stage]", "\n")
                             # net
                             net_list <- value_list[value_list$type=="net", ]
                             if(NROW(net_list)>0){
                               desc_net <- lapply(1:NROW(net_list), function(i){
                                 temp <- self$.get_value(field_name = net_list$name[i], log = log)
                                 .summarise_value(X = temp, name = net_list$name[i])
                               })
                               desc_net <- do.call(rbind, desc_net)
                               cat("net:", "\n")
                               print(desc_net)
                               cat("\n")
                             }else{
                               desc_net <- NULL
                             }

                             # mat
                             mat_list <- value_list[value_list$type=="mat", ]
                             if(NROW(mat_list)>0){
                               desc_mat <- lapply(1:NROW(mat_list), function(i){
                                 temp <- self$.get_value(field_name = mat_list$name[i], log = log)
                                 .summarise_value(X = temp, name = mat_list$name[i])
                               })
                               desc_mat <- do.call(rbind, desc_mat)
                               cat("mat:", "\n")
                               print(desc_mat)
                               cat("\n")
                             }else{
                               desc_mat <- NULL
                             }

                             # euc
                             euc_list <- value_list[value_list$type=="euc", ]
                             if(NROW(euc_list)>0){
                               desc_euc <- lapply(1:NROW(euc_list), function(i){
                                 temp <- self$.get_value(field_name = euc_list$name[i], log = log)
                                 .summarise_value(X = temp, name = euc_list$name[i])
                               })
                               desc_euc <- do.call(rbind, desc_euc)
                               cat("euc:", "\n")
                               print(desc_euc)
                               cat("\n")
                             }else{
                               desc_euc <- NULL
                             }

                             # df
                             df_list <- value_list[value_list$type=="df", ]
                             if(NROW(df_list)>0){
                               desc_df <- lapply(1:NROW(df_list), function(i){
                                 temp <- self$.get_value(field_name = df_list$name[i], log = log)
                                 temp_summary <- lapply(colnames(temp), function(X){
                                   .summarise_value(X = temp[[X]], name = X)
                                 })
                                 do.call(rbind, temp_summary)
                               })
                               desc_df <- do.call(rbind, desc_df)
                               cat("df:", "\n")
                               print(desc_df)
                               cat("\n")
                             }else{
                               desc_df <- NULL
                             }

                             # other
                             other_list <- value_list[value_list$type=="other", ]
                             if(NROW(other_list)>0){
                               desc_other <- lapply(1:NROW(other_list), function(i){
                                 temp <- self$.get_value(field_name = other_list$name[i], log = log)
                                 temp_summary <- lapply(colnames(temp), function(X){
                                   .summarise_value(X = temp[[X]], name = X)
                                 })
                                 do.call(rbind, temp_summary)
                               })
                               desc_other <- do.call(rbind, desc_other)
                               cat("other:", "\n")
                               print(desc_other)
                               cat("\n")
                             }else{
                               desc_other <- NULL
                             }

                             # active_bindings
                             active_list <- value_list[value_list$type=="active_binding", ]
                             if(NROW(active_list)>0){
                               desc_active <- lapply(1:NROW(active_list), function(i){
                                 temp <- self$.get_value(field_name = active_list$name[i], log = log)
                                 .summarise_value(X = temp, name = active_list$name[i])
                               })
                               desc_active <- do.call(rbind, desc_active)
                               cat("active_binding:", "\n")
                               print(desc_active)
                               cat("\n")
                             }else{
                               desc_active <- NULL
                             }

                             # return_object
                             if(return_value){
                               out <- c(desc_agent, list(net = desc_net),
                                        list(mat = desc_mat), list(euc = desc_euc),
                                        list(df = desc_df), list(other_field = desc_other),
                                        list(active_binding = desc_active))
                               out <- Filter(Negate(is.null), out)
                               if(is.null(log)){
                                 out <- c(out, time = D$time)
                               }else{
                                 out <- c(out, time = D$log[[log]]$time)
                               }
                               invisible(out)
                             }else{
                               invisible(NULL)
                             }
                           },
                           #' @description
                           #' plot method of ABM_D
                           #' @import sna
                           #' @import plot.matrix
                           .plot = function(
    x_field_name, x_dim = NULL,
    y_field_name = NULL, y_dim = NULL,
    x_value = NULL, y_value = NULL,
    x_FUN = NULL, y_FUN = NULL,
    log = NULL,
    time_series = FALSE,
    return_value = FALSE,
    ...){
                             # valueの値をチェック
                             stopifnot("x_value must be vector." = !is.list(x_value))
                             stopifnot("y_value must be vector." = !is.list(y_value))

                             # time_seriesによる分岐-------------------
                             if(time_series){
                               ## message
                               if(!is.null(y_field_name)||!is.null(y_value)||!is.null(y_FUN)){
                                 message("Note: Input related to y is ignored when time_series is set to TRUE.")
                               }
                               ## time_seriesである場合
                               log <- 1:length(self$log)
                               ## x_valueありの場合にはエラーを出す
                               stopifnot("x_value with time series is not supported." = is.null(x_value))
                               ## x_FUNが投入されているかどうか---
                               if(!is.null(x_FUN)){
                                 ### x_FUNが投入されている
                                 x_value <- lapply(log, function(t){x_FUN(D = self$log[[t]])})
                                 y_value <- lapply(log, function(t){t})
                                 x_label <- "x_FUN(D = D)"
                                 y_label <- "time"
                               }else{
                                 ### x_FUNではなく、x_fieldから取得する
                                 x_value <- lapply(log, function(t){
                                   self$.get_value(field_name = x_field_name, value_name = x_dim, log = t)
                                 })
                                 y_value <- 1:length(log)
                                 if(is.null(x_dim)){
                                   x_label <- x_field_name
                                 }else{
                                   x_label <- paste0(x_field_name, " : ", x_dim)
                                 }
                                 y_label <- "time"
                               }
                             }else{
                               ## time_seriesではない----------------
                               ## xの投入設定により値を取得
                               if(!is.null(x_value)){
                                 ### x_valueにすでに値がある場合
                                 x_value <- list(x_value)
                                 x_label <- "x_value"
                               }else if(!is.null(x_FUN)){
                                 ### x_FUNが投入されている場合
                                 ### 現在のデータから取る場合
                                 if(is.null(log)){
                                   x_value <- list(x_FUN(D = self))
                                   x_label <- "x_FUN(D = D)"
                                 }else{
                                   ### logから採る場合
                                   x_value <- lapply(log, function(t){x_FUN(D = self$log[[t]])})
                                   x_label <- "x_FUN(D = D)"
                                 }
                                 ### x_FUNが投入されている場合----------#
                               }else{
                                 ### xを名前に基づいて取得する場合
                                 ### 現在のデータからとる
                                 if(is.null(log)){
                                   x_value <- list(self$.get_value(field_name = x_field_name, value_name = x_dim))
                                 }else{
                                   ### 過去のデータからとる
                                   x_value <- lapply(log, function(t){
                                     self$.get_value(field_name = x_field_name, value_name = x_dim, log = t)
                                   })
                                 } ### 過去のデータから採る場合--#

                                 # x_label
                                 if(is.null(value_name)){
                                   x_label <- x_field_name
                                 }else{
                                   x_label <- paste0(x_field_name," : ", x_dim)
                                 }
                               } ## xを名前に基づいて取得する場合--#
                             } # time_serisではない----#　


                             # yの値を取得--------------------------------
                             if(time_series == F){
                               ## time_seriesではない場合のみ、yの値も取得する
                               if(!is.null(y_value)){
                                 ### y_valueが投入されている場合
                                 y_value <- list(y_value)
                                 y_label <- "y_value"
                               }else if(is.function(y_FUN)){
                                 ### y_FUNが投入されている場合
                                 if(is.null(log)){
                                   #### 現在の値から採る場合
                                   y_value <- list(y_FUN(D = D))
                                 }else{
                                   #### logから採る場合------
                                   y_value <- lapply(log, function(t){y_FUN(D = self$log[[t]])})
                                 }
                                 y_label <- "y_FUN(D = D)"
                               }else if(!is.null(y_field_name)){
                                 ### yの値をフィールド名に基づいて取得する場合
                                 if(is.null(log)){
                                   #### 現在のデータからとる場合
                                   y_value <- list(self$.get_value(field_name = y_field_name, value_name = y_dim))
                                 }else{
                                   #### 過去のデータからとる
                                   y_value <- lapply(log, function(t){
                                     self$.get_value(field_name = y_field_name, value_name = y_dim, log = t)
                                   })
                                 } #### 過去のデータから採る------------#

                                 ### y_label
                                 if(is.null(y_dim)){
                                   y_label <- y_field_name
                                 }else{
                                   y_label <- paste0(y_field_name, ":", y_dim)
                                 }
                                 ### yの値をフィールド名に基づいて取得--#
                               }else{
                                 ### yの取得の指定はない場合
                                 y_value <- NULL
                               }
                             } ## time_seriesではない場合----------------#

                             # field_typeがない場合には"NULL"を指定
                             if(is.null(attr(x_value[[1]], "field_type"))){
                               attr(x_value[[1]], "field_type") <- "NULL"
                             }

                             # field_typeごとにアクションを分ける
                             out <- switch(attr(x_value[[1]], "field_type"),
                                           "net" = {
                                             if(is.null(log)){
                                               main <- x_field_name
                                             }else{
                                               main <- paste0(x_field_name, " (t = ", unlist(lapply(log, function(t){D$log[[t]]$time})), ")")
                                             }
                                             lapply(1:length(x_value), function(t){
                                               sna::gplot(x_value[[t]],
                                                          main = main[t], ...)
                                             })
                                           },
                                           "mat" = {
                                             # old_parに戻すように設定しておく
                                             old_par <- par(no.readonly = TRUE)
                                             on.exit(par(old_par))
                                             # プロットマージンを設定
                                             par(mar = c(5.1, 4.1, 4.1, 4.1))
                                             # main
                                             if(is.null(log)){
                                               main <- x_field_name
                                             }else{
                                               main <- paste0(x_field_name, " (t = ", unlist(lapply(log, function(t){D$log[[t]]$time})), ")")
                                             }
                                             # plot
                                             lapply(1:length(x_value), function(t){
                                               mat <- as.matrix(x_value[[t]])
                                               plot(mat, main = main[t], ...)
                                             })
                                           },
                                           "euc" = {
                                             # main
                                             if(is.null(log)){
                                               main <- x_field_name
                                             }else{
                                               main <- paste0(x_field_name, " (t = ", unlist(lapply(log, function(t){D$log[[t]]$time})), ")")
                                             }
                                             # x_dim, y_dim
                                             if(is.null(x_dim)){
                                               x_dim <- 1
                                               x_label <- colnames(x_value[[1]])[1]
                                             }else{
                                               x_label <- x_dim
                                             }
                                             if(is.null(y_dim)){
                                               y_dim <- 2
                                               y_label <- colnames(x_value[[1]])[2]
                                             }else{
                                               y_label <- y_dim
                                             }
                                             # plot
                                             lapply(1:length(x_value), function(t){
                                               plot(x = x_value[[t]][ ,x_dim], y = x_value[[t]][ ,y_dim],
                                                    xlab = x_label, ylab = y_label,
                                                    main = main[t], ...)
                                             })
                                           },
                                           # 上記以外の場合------------------------------------
                                           {
                                             # x_value
                                             stopifnot("Please specify the variable name for x_dim."= all(unlist(lapply(x_value, function(X){NCOL(X)==1}))))
                                             stopifnot("Please specify the variable name for y_dim."= all(unlist(lapply(y_value, function(X){NCOL(X)==1}))))

                                             # xとyのベクトルの長さを確認し、前処理する
                                             x_value_list <- vector("list", length(x_value))
                                             y_value_list <- vector("list", length(y_value))

                                             if(is.null(y_value)){
                                               x_value_list <- x_value
                                               y_value_list <- NULL
                                             }else{
                                               for(t in 1:length(x_value)){
                                                 x_value_len <- length(x_value[[t]])
                                                 y_value_len <- length(y_value[[t]])
                                                 if(x_value_len==y_value_len){
                                                   x_value_list[[t]] <- x_value[[t]]
                                                   y_value_list[[t]] <- y_value[[t]]
                                                 }else if(x_value_len > y_value_len){
                                                   stopifnot("The length of y does not match the length of x." = length(y_value_len)==1)
                                                   x_value_list[[t]] <- x_value[[t]]
                                                   y_value_list[[t]] <- rep(y_value[[t]], x_value_len)
                                                 }else{
                                                   stopifnot("The length of x does not match the length of y." = length(x_value_len)==1)
                                                   x_value_list[[t]] <- rep(x_value[[t]], y_value_len)
                                                   y_value_list[[t]] <- y_value[[t]]
                                                 }
                                               }
                                             }

                                             # time_seriesか否かによる分岐
                                             if(time_series){
                                               ## time_seriesの場合--------------------------
                                               # unlistする
                                               x_value <- unlist(x_value_list)
                                               y_value <- unlist(y_value_list)

                                               ## mainのラベル
                                               if(is.null(x_dim)){
                                                 main <- x_field_name
                                               }else{
                                                 main <- paste0(x_field_name, " : ", x_dim)
                                               }

                                               # x_valueとlogの長さの関係による分岐
                                               if(length(x_value)==length(log)){
                                                 ### x_valueとlogの長さが同じ場合
                                                 plot(y = x_value, x = y_value, type = "l",
                                                      xlab = y_label, ylab = x_label,
                                                      main = main, ...)
                                               }else{
                                                 ### x_valueとlogの長さが異なる場合
                                                 if(is.character(x_value)){
                                                   #### xが文字型
                                                   plot(prop.table(table(y_value, x_value)),
                                                        xlab = y_label, ylab = x_label, main = main, ...)
                                                 }else{
                                                   #### xが数値型
                                                   boxplot(x_value ~ y_value, main = main,
                                                           xlab = y_label, ylab = x_label, ...)
                                                 }
                                               }
                                             }else{
                                               ## time_seriesでない場合----------------
                                               lapply(1:length(x_value_list), function(t){
                                                 if(is.null(y_value_list[[t]])){
                                                   ## 1変量統計の場合--------------------
                                                   ### mainラベルの処理
                                                   if(is.null(x_dim)){
                                                     main <- x_field_name
                                                   }else{
                                                     main <- paste(x_field_name, " : ", x_dim)
                                                   }
                                                   ### logありの場合の処理
                                                   if(!is.null(log)){
                                                     main <- paste0(main, " (t=", self$log[[t]]$time, ")")
                                                   }
                                                   ### 値の型ごとに処理
                                                   if(is.character(x_value_list[[t]])|is.factor(x_value_list[[t]])){
                                                     barplot(table(x_value_list[[t]]),
                                                             ylab = "Frequency",
                                                             main = main, ...)
                                                   }else{
                                                     hist(x_value_list[[t]], main = main, xlab = "", ...)
                                                   }
                                                   ## 1変量統計の場合-------
                                                 }else{
                                                   ## 2変量統計の場合-------
                                                   ## メインタイトルの処理
                                                   if(is.null(log)){
                                                     main <- ""
                                                   }else{
                                                     main <- paste0("t=", self$log[[t]]$time, "")
                                                   }
                                                   ## 数値型ごとの分岐
                                                   if(is.numeric(x_value_list[[t]]) && is.numeric(y_value_list[[t]])){
                                                     ## x:数値型、y:数値型の場合
                                                     plot(x = x_value_list[[t]], y = y_value_list[[t]],
                                                          xlab = x_label, ylab = y_label, main = main, ...)
                                                   }else if(is.numeric(x_value_list[[t]]) && !is.numeric(y_value_list[[t]])){
                                                     ## x:数値型、y:文字型の場合
                                                     stripchart(x_value_list[[t]] ~ y_value_list[[t]],
                                                                method = "jitter",
                                                                xlab = x_label,
                                                                ylab = y_label,
                                                                main = main,
                                                                ...)
                                                   }else if(!is.numeric(x_value_list[[t]]) && is.numeric(y_value_list[[t]])){
                                                     ## x:文字型、y:数値型の場合
                                                     boxplot(y_value_list[[t]] ~ x_value_list[[t]],
                                                             xlab = x_label,
                                                             ylab = y_label,
                                                             main = main,
                                                             ...)
                                                   }else if(!is.numeric(x_value_list[[t]]) && !is.numeric(y_value_list[[t]])){
                                                     ## x:文字型、y:文字型の場合
                                                     tab <- table(x_value_list[[t]], y_value_list[[t]])
                                                     mosaicplot(tab,
                                                                xlab = x_label,
                                                                ylab = y_label,
                                                                main = main,
                                                                ...)
                                                   }
                                                 } ### 2変量統計の場合------------#
                                               })
                                             } ## time_seriesでない場合-------#
                                           } # 上記以外の場合----------------------------------
                             )

                             # リターン
                             if(return_value){
                               return(out)
                             }else{
                               invisible(NULL)
                             }
                           } # .plot関数ここまで
                           ),
                     private = list(
                       deep_clone = function(name, value){
                         # agentに関してはdeep_cloneしたものを付加する.
                         field_type <- self$.field_type()
                         agent_type <- names(field_type)[field_type=="agent"]
                         if(name %in% agent_type){
                           agent_cloned <- lapply(self[[name]], function(agent_i){
                             agent_i$clone(deep = TRUE)
                           })
                           names(agent_cloned) <- names(self[[name]])
                           attr(agent_cloned, "field_type") <- "agent"
                           agent_cloned
                         }else{
                           value
                         }
                       }
                     )
)
