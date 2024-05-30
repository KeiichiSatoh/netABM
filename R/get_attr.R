#' @title Get A Agents' Attributes From A netABM Object.
#' @description \code{get_attr} creates a vector and data.frame of agents' attributes.
#' @param D An object of class \code{netABM}.
#' @param which_attr An integer or character string indicating the agents' attribute(s).
#' The default is \code{NULL}, resulting in selecting the whole attributes.
#' @details Upon writing the function of the agent's action, there is often the case
#' that the action depends on the agents' attribute(s). For example,
#' if the agent's probability to form an edge with an alter depends on the alters' attributes,
#' the user firstly need to get the vector or data.frame of the agents' attributes as an input.
#' This function can be used for such purpose among others, but can be used
#' for other purposes.
#'
#' @returns A vector if only one attribute is queried and a data.frame if multiple attributes.
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @family f_tools
#' @export
#' @examples
#' # Create the netABM_network object
#' node_attr <- data.frame(
#'   age = 1:5,
#'   sex = c("m","m","m","f","f"))
#' D <- setABM_network(n = 5,
#'                     node_attr = node_attr)
#'
# example 1: get all agents' attributes
#' get_attr(D)
#'
#' # example 2: get a selected attribute
#' get_attr(D, "sex")
#'
#' # example 3: get a selected attribute by its position
#' get_attr(D, 2)
#'
#' # example 4: get multiple selected attributes
#' get_attr(D, c("age","sex"))
#'
#'

get_attr <- memoise::memoise(
  function(D, which_attr = NULL, from_log = FALSE, attr_long = FALSE){
    # 投入Dの状況を取得する
    n <- length(D$agent)
    # which_attr = NULLの場合
    if(is.null(which_attr)){
      which_attr <- names(D$agent[[1]]$a)
    }
    # IF: from_log = FALSE
    if(from_log == FALSE){
      # attrを取得
      temp_attr <- lapply(X = D$agent, function(X){
        data.frame(X$a[which_attr], ID = X$ID)
      })
      # rbindする
      attr <- do.call(rbind, temp_attr)
      rownames(attr) <- NULL
    }else{
      ## from_log == TRUEの場合
      attr <- vector("list", length = length(D$log))
      for(t in 1:length(D$log)){
        temp_attr <- lapply(X = D$log[[t]]$agent, function(X){
          data.frame(X$a[which_attr], ID = X$ID)
        })
        temp_attr <- do.call(rbind, temp_attr)
        temp_attr <- data.frame(temp_attr, time = D$log[[t]]$time)
        rownames(temp_attr) <- NULL
        attr[[t]] <- temp_attr
      }

      ## attr_long == FALSEの場合
      if(attr_long == FALSE){
        names(attr) <- names(D$log)
      }else{
        ### attr_long == TRUEの場合
        attr <- do.call(rbind, attr)
      }
    } ## from_log=Tここまで
    # return
    attr
  }, cache = cachem::cache_mem(max_age = 900))


