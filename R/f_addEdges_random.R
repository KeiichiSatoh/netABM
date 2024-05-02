#' @title Adds Edges by Randomly Selecting Alters
#' @description
#' \code{f_addEdges_random} is a function with which user can make agents randomly select alters and add  edges with a defined weight.
#' @param D \code{netABM_network} class object
#' @param which_level integer, (index of the level in the \code{stage}) or character (the name of the level in the \code{stage})(default is 1)
#' @param which_net integer (index of the network) or character (the name of network) to which edges will be added (default is 1)
#' @param num_edges integer, number of edges to add (default is 1).
#' @param alter_candidate character, how to determine the candidates for adding edges (possibilities: \code{"new"} (default), \code{"known"}, or \code{"all"}).
#' Alternatively, the ID of the candidates can be directly supplied.
#' @param value numeric, value of the new edges (default is 1).
#' @param .valueFunction function: function to generate edge values (optional. Default is \code{Null}).
#'
#' @details
#' \code{f_addEdges_random} is agent-behavior function which can be used for setting the agent behavior (e.g., agent_f) argument.
#' Notably, if user want to use the similar function as the part of the function which users write by themselves, use \code{addEdges_random} instead.
#'
#' As for the \code{alter_candidate}  argument, following options are available:
#' - "new": The candidate of new partner will be selected from those with whom the agent does not have edges.
#' - "known": The new partner candidate will be selected from theose with whom the agent already had edges.
#' - "all": Any other alters, regardless whether the agent has already has edges or not with them.
#' - directly supplying the candidate IDs as character. For multiple candidates, use \code{c( )}.
#'
#' \code{which_net} can be specified either with the integer (e.g., 1) or the name of the network (e.g., "net1").
#' If the user has only one network in \code{D} object, the user does not have to care about this argument.
#'
#' Similarly, \code{which_level} can be specified either with the integer (e.g., 1) or the name of the level in the \code{stage} (e.g., "agent", which is agent network).
#'
#' With \code{.valueFunction} argument, the user can supply a function with which the edge value will be generated.
#' Be ensure that the supplied function only generate one value and does not generate any other values,
#' because \code{.valueFunction} will be repeated for each selected new agent. If a function generating a random value is supplied,
#' the resulted edge weight will be different for each new alter. When \code{.valueFunction} is supplied,
#' the \code{value} argument is ignored. See the Examples below to get the idea about how to supply \code{.valueFunction}.
#'
#' @returns  The network of the active agent is updated in place through the \code{R6}'s reference system.
#' @family f_
#' @author Keiichi Satoh
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @import R6
#' @export
#' @examples
#' # Example 1: Make agents select their partner randomly (leave all arguments as default)
#' D <- setABM_network(agent_n = 5, agent_f = f_addEdges_random)
#' D$agent$A1$net                    # No edges exists
#' D$agent$A1$.f(D)                  # Agent adds one edges
#' D$stage$agent$net                 # One edge added
#' D$agent$A1                        # And reflected to the A1's profile
#'
#' # Example 2: Set edge weight from the normal distribution supplied directly.
#' D <- setABM_network(
#'      agent_n = 5,
#'      agent_f = list(f_addEdges_random(.valueFunction = rnorm(n = 1, mean = 0, sd = 1)))
#'      )
#' D$agent$A1                    # No edges exists
#' D$agent$A1$.f(D)             # Agent adds one edges
#' D$stage$agent$net            # One edge added
#' D$agent$A1                   # and reflected to the A1's profile
#'
#' # Example 3: Set edge weight from the normal distribution supplied via substituted object.
#' random2 <- substitute(f_addEdges_random(.valueFunction = rnorm(n = 1, mean = 0, sd = 1)))
#' D <- setABM_network(
#'    agent_n = 5,
#'    agent_f = random2)
#' D$agent$A1
#' D$agent$A1$.f(D)
#' D$stage$agent$net
#' D$agent$A1

f_addEdges_random <- function(D,
                              which_level = 1,
                              which_net = 1,
                              num_edges = 1,
                              alter_candidate = "new",
                              value = 1,
                              .valueFunction = NULL){
  # network name
  net_name <- names(D$stage[[which_level]])[!names(D$stage[[which_level]]) %in% c(".__enclos_env__","clone","print")]

  # 使用するネットワークの現状のエッジリスト
  current_network <- D$stage[[which_level]][[net_name[1]]]
  all_node_ID <- rownames(current_network)[rownames(current_network)!=self$ID]
  connected_node_ID <- self[[net_name]]

  # alter_candidateの設定ごとに対処
  if(alter_candidate[1] %in% c("new", "known", "all")){
    switch(alter_candidate,
           "new" = {
             candid <- setdiff(all_node_ID, connected_node_ID)
           },
           "known" = {
             candid <- connected_node_ID},
           "all"   = {candid <- all_node_ID}
    )
  }else{
    candid <- alter_candidate
  }

  # 新しい候補者をリスト化
  if(num_edges <= length(candid)){
    new_alter <- sample(candid, size = num_edges, replace = F)
  }else{
    new_alter <- sample(candid, size = length(candid), replace = F)
  }

  # new_alterが何もない場合にはNULLを返す
  if(length(new_alter)==0){
    return(NULL)
  }

  # 新しいalterのvalue
  # Functionが提供されている場合
  if(!is.null(.valueFunction)){
    ## Funcの形に応じてnameとargsを取得
    if(is.function(.valueFunction)){
      call_name <- as.character(substitute(.valueFunction))
      call_args <- NULL
    }else{
      func_temp <- substitute(.valueFunction)
      func_name <- call_name(func_temp)
      func_args <- call_args(func_temp)
    }
    ## new_alter分valueを生成する
    new_alter_value <- unlist(lapply(1:length(new_alter), function(i){
      do.call(func_name, func_args)
    }))
  }else{
    # Functionが提供されていない場合
    new_alter_value <- rep(value, length(new_alter))
  }

  # 新しいエッジを足す
  D$stage[[which_level]][[net_name]][self$ID, new_alter] <- new_alter_value
}

