#' @title ABM_Agent Class
#' @docType class
#' @description \code{ABM_Agent} is the core class of \code{netABM} for bundle the data of each agent and its behavior function.
#' @field .f A function that define the behavior of the agent. Default is \code{NULL}
#' @examples
#' ABM_Agent$new()
#' @export
ABM_Agent <- R6::R6Class("ABM_Agent", lock_objects = FALSE, cloneable = TRUE,
                         public = list(
                           initialize = function(fields = list(), methods = list()) {
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
                           .add_method = function(name, method){
                             self[[name]] <- method
                             environment(self[[name]]) <- self$.__enclos_env__
                           },
                           .add_field = function(name, value){
                             self[[name]] <- value
                           },
                           #' @description
                           #' add an active binding
                           .add_active_binding = function(name, FUN){
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
                           #' remove a field from the object
                           .remove_field = function(field_name){
                             rm(list = field_name, envir = self)
                             if(field_name %in% self$.active_binding_names()){
                               self$.__enclos_env__$.__active__[[field_name]] <- NULL
                             }
                           },
                           #' @description
                           #' Return the the current attributes of the agent
                           .save = function(){
                             value_names <- self$.value_names()
                             values <- lapply(X = c("ID", value_names), function(X){self[[X]]})
                             names(values) <- c("ID", value_names)
                             values},
                           #' @description
                           #' Return the name of the act_FUN fields.
                           .act_FUN_names = function(){
                             fields <- ls(self)[!ls(self) %in% c("initialize","clone", "print")]
                             act_FUN <- sapply(fields, function(x){is.function(self[[x]])})
                             if(all(act_FUN==FALSE)){
                               return(NULL)
                             }else{
                               names(act_FUN)[act_FUN]
                             }
                           },
                           #' @description
                           #' Return the name of the act_FUN fields.
                           .active_binding_names = function(){
                             names(self$.__enclos_env__$.__active__)
                           },

                           #' @description
                           #' Return the name of value fields.
                           .value_names = function(){
                             fields <- ls(self)[!ls(self) %in% c("initialize","clone", "print", "ID")]
                             act_FUN <- sapply(fields, function(x){is.function(self[[x]])})
                             fields[act_FUN==FALSE]
                           },

                           #' @description
                           #' get the value in value fields.
                           .get_values = function(attr = NULL){
                             if(is.null(attr)){
                               which_attr <- self$.value_names()
                             }
                             values <- lapply(attr, function(X){self[[X]]})
                             names(values) <- attr
                             values
                           },

                           #' @description
                           #' Print the agent's values
                           print = function(){
                             cat("<ABM_Agent>", "\n")
                             value_names <- self$.value_names()
                             cat("ID:", self$ID, "\n", sep = " ")
                             lapply(value_names, function(X){
                               if(X %in% self$.active_binding_names()){
                                 cat(X, "*", ": ", self[[X]], "\n", sep = "")
                               }else{
                                 cat(X, ": ", self[[X]], "\n", sep = "")
                               }
                             })
                             cat("act_FUN:", self$.act_FUN_names(), "\n", sep = " ")
                           }
                         )
)

