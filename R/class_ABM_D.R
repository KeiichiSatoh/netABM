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

ABM_D <- R6Class(
  "ABM_D",
  public = list(
    agent = NA,
    time = 1,
    log = list(NULL),
    notes = NULL,
    .f = NULL,
    #' @description
    #' Save the current status of the fields into log
    .save = function(){
      agent <- lapply(X = 1:length(self$agent), function(X){self$agent[[X]]$.save()})
      names(agent) <- names(self$agent)
      D_field <- names(self)[!names(self) %in% c(".f", ".save", "clone",".__enclos_env__","agent","log","print")]
      D_values <- lapply(X = D_field, get, envir = self$.__enclos_env__$self)
      names(D_values) <- D_field
      D_values$agent <- agent
      new_log <- list(D_values)
      names(new_log) <- paste0("t",self$time)
      self$log <- c(self$log, new_log)
    },
    #' @description
    #' print method of the \code{ABM_D} class
    print = function(){
      field_type <- unlist(lapply(self, function(X){attr(X, "field_type")}))
      cat("<ABM_D>", "\n")
      cat("[agent(n = ", length(self$agent), ")]", "\n", sep = "")
      cat("  attr :", names(self$agent[[1]])[!names(self$agent[[1]]) %in% c(".__enclos_env__", "clone", ".f",".save","print")], "\n")
      cat("\n")
      cat("[field]", "\n")
      if(length(field_type[field_type=="net"])>0){
        cat("  net  :", names(field_type[field_type=="net"]), "\n")
      }
      if(length(field_type[field_type=="ca"])>0){
        cat("  ca   :", names(field_type[field_type=="ca"]), "\n")
      }
      if(length(field_type[field_type=="euc"])>0){
        cat("  euc  :", names(field_type[field_type=="euc"]), "\n")
      }
      if(length(field_type[field_type=="other"])>0){
        cat("  other:", names(field_type[field_type=="other"]), "\n")
      }
      cat("\n")
      cat("[meta data]", "\n")
      cat("  time :", self$time, "\n")
      cat("  log  :", if(all(is.na(self$log))){"NULL"}else{length(self$log)}, "\n")
      cat("  notes:", names(self$notes), "\n")
    },
    #' @description
    #' get the field type of the \code{ABM_D} class
    .field_type = function(){
      unlist(lapply(self, function(X){attr(X, "field_type")}))
    },
    #' @description
    #' get the attribute of the agents in \code{ABM_D} class
    #' @param from_log logical. If the attribute is retrieved from log.
    #' @param attr_long logical. If the attribute from the log is converted into the long format.
    .agent_attr = function(from_log = FALSE, attr_long = FALSE){
      attr <- names(self$agent[[1]])[!names(self$agent[[1]]) %in% c(".__enclos_env__", "clone", ".f",".save","print")]
      if(from_log==FALSE){
        d <- do.call(
          data.frame,
          lapply(attr, function(X){
            sapply(1:length(self$agent), function(i){self$agent[[i]][[X]]})
          }))
        colnames(d) <- attr
      }else{
        d <- lapply(1:length(self$log), function(t){
          d_t <- do.call(
            data.frame,
            lapply(attr, function(X){
              sapply(1:length(self$agent), function(i){self$log[[t]]$agent[[i]][[X]]})
            }))
          colnames(d_t) <- attr
          d_t
          })
        if(attr_long==FALSE){
          names(d) <- names(self$log)
        }else{
          d <- lapply(1:length(d), function(t){
            data.frame(d[[t]], time = t)
            })
          d <- do.call(rbind, d)
        }
      }
      # output
      d
    }
  ),
  private = list(
    deep_clone = function(name, value){
      # agentに関してはdeep_cloneしたものを付加する.
      if(name == "agent"){
        agent_cloned <- lapply(1:length(self$agent), function(i){
          self$agent[[i]]$clone(deep = TRUE)
        })
        names(agent_cloned) <- names(self$agent)
        agent_cloned
      }else{
        value
      }
    }
  ),
  lock_objects = FALSE,
  cloneable = TRUE
)
