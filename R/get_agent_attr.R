#' @title Get A Agents' Attributes From A ABM_D Object.
#' @description \code{get_agent_attr} creates a vector and data.frame of
#' agents' attributes.
#' @param D An object of class \code{ABM_D}.
#' @param from_log logical. Whether the agent attributes should be extracted from the log.
#' @param attr_long logical. Whether the agent attributes in the log shold be organized in a long format.
#' @returns a data.frame of agent attributes.
#' @import memoise
#' @export
#' @examples
#' # Create the ABM_D object
#' agent_attr <- data.frame(
#'    age = 1:5,
#'    sex = c("m","m","m","f","f"))
#' D <- setABM(agent_n = 5, agent_attr = agent_attr)
#'
#' # Extract the agent attributes
#' get_agent_attr(D)

get_agent_attr <- memoise(function(D, from_log = FALSE, attr_long = FALSE){
  # 現在のDから取得
  if(from_log==FALSE){
    temp <- lapply(X = 1:length(D$agent), function(X){
      data.frame(D$agent[[X]]$.save())})
    attr <- do.call(rbind, temp)
  }else{
    # timeを取得
    times <- unlist(lapply(D$log, function(X){X$time}))
    # logから取得
    agent_timewise <- lapply(1:length(D$log), FUN = function(t){D$log[[t]]$agent})
    attr <- lapply(agent_timewise, function(X){
      temp <- lapply(1:length(X), function(i){data.frame(X[[i]])})
      do.call(rbind, temp)})
    ## attr_long = FALSEの場合
    if(attr_long == FALSE){
      names(attr) <- names(times)
    }else{
      ## attr_long = TRUEの場合
      attr <- lapply(1:length(attr), function(t){
        attr[[t]] <- data.frame(attr[[t]], time = times[[t]])
        attr[[t]]})
      attr <- do.call(rbind, attr)
    }
  }
  attr
})
