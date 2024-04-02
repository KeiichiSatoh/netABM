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
#' @family act_tools
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

get_attr <- memoise::memoise(
  function(D, which_attr = NULL){
    # 投入Dの状況を取得する
    n <- length(D$agents)
    agent_names <- names(D$agents)
    # which_attr = NULLの場合
    if(is.null(which_attr)){
      which_attr <- names(D$agents[[1]]$a)
    }
    # attrを取得
    temp_attr <- lapply(X = D$agents, function(X){
      data.frame(X$a[which_attr])
    })
    # rbindする
    node_attr <- do.call(rbind, temp_attr)
    # もしも列数が1の場合にはベクトルとして返す
    if(ncol(node_attr)==1){
      node_attr <- node_attr[[1]]
    }
    # return
    node_attr
  }, cache = cachem::cache_mem(max_age = 900))
