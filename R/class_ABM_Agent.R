#' @title ABM_Agent Class
#' @docType class
#' @description \code{ABM_Agent} is the core class of \code{netABM} for bundle the data of each agent and its behavior function.
#' @field .f A function that define the behavior of the agent. Default is \code{NULL}
#' @examples
#' ABM_Agent$new()
#' @export

ABM_Agent <- R6Class(
  "ABM_Agent",
  public = list(
    .f = NULL,

    #' @description
        #' Return the the current attributes of the agent
    .save = function(){
      field <- names(self)[!names(self) %in% c(".f", ".save", "clone",".__enclos_env__","print")]
      values <- lapply(X = field, get, envir = self$.__enclos_env__$self)
      names(values) <- field
      values},

    #' @description
        #' print the list of field names and the status of the dataset
    print = function(){
      attr_name <- names(self)[!names(self) %in% c(".__enclos_env__", "clone", ".f",".save","print")]
      cat("<ABM_Agent>", "\n")
      lapply(attr_name, function(X){
        cat(X, ": ", self[[X]], "\n", sep = "")
      })
    }
  ),
  lock_objects = FALSE,
  cloneable = TRUE
)

