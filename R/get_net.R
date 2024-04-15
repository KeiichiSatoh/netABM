#' @title Get A Whole Network From A netABM Object.
#' @description \code{get_net} creates a whole network matrix from the input \code{netABM} class D object.
#' @param D An object of class \code{netABM}.
#' @param which_net An integer or character string indicating the network to convert.
#' @param space logical, whether network to get locates at space level (\code{TRUE}).
#' The default is \code{FALSE} and the agent level network is returned.
#' @details Upon writing the function of the agent's action, there is often the case
#' that the action depends on the condition of the whole network. For example,
#' if the agent's probability to form an edge with an alter depends on the indegree of the
#' alter, the user firstly calculate the indegree of the alters which typically
#' needs an whole network as an input.
#' This function can be used for such purpose among others, but can be used
#' for other purposes.
#'
#' @returns A whole network matrix created from \code{D}
#' @importFrom vctrs vec_rbind
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @importFrom sna rgraph
#' @export
#' @family act_tools
#' @examples
#' # Make a sample dataset
#' D <- setABM_network(n = 5,
#'                     networks = list(net1 = sna::rgraph(5, tprob = 0.2),
#'                                     net2 = sna::rgraph(5, tprob = 0.5)))
#' # Example 1: get the whole network(default)
#' get_net(D)
#'
#' # Example 2: get the 2nd whole network
#' get_net(D, 2)
#'
#' # Example 3: get the 2nd whole network by its name
#' get_net(D, "net2")
#'
get_net <- memoise(
  function(D, which_net = 1, space = FALSE){
    if(space == TRUE){
      level <- "space"
    }else{
      level <- "agents"
    }
    # 投入Dの状況を取得する
    n <- length(D[[level]])
    node_names <- names(D[[level]])
    # 結合整形用に、すべてのnodeが入った空のvectorを作成
    temp_vec <- rep(0, n)
    names(temp_vec) <- node_names
    # 各アクターのネットワークを取得
    net_temp <- lapply(X = D[[level]], function(X){
      X$e[[which_net]]
    })
    # ネットワークがNULLになっているIDを取得し、NAに置き換える
    null_ID <- names(net_temp)[unlist(lapply(net_temp, is.null))]
    net_temp[null_ID] <- NA
    # すべてのnet_tempを結合する
    mat <- do.call(vec_rbind, c(list(temp_vec), net_temp))
    # 1行目を外し、row側にも名前を付ける
    mat <- mat[-1, ]
    mat[is.na(mat)] <- 0
    rownames(mat) <- node_names
    # リターン
    mat
  }, cache = cache_mem(max_age = 900))

