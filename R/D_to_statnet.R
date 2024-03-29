#' Convert netABM Object to statnet
#'
#' \code{D_to_statnet} converts \code{netABM} object to a statnet network object.
#'
#' @param D A \code{netABM} object.
#' @param log Logical, indicating whether to convert the log of \code{netABM} object into a list of \code{statnet} network objects. Default is \code{FALSE}.
#' @param duplicated_edges Character string indicating how duplicated edges should be handled. Options are \code{"sum"}, \code{"min"}, \code{"max"}, or \code{"mean"}. Default is \code{"sum"}.
#' @param which_net Integer or character string specifying which network to convert if there are multiple networks in the ABM object. Default is \code{1}.
#'
#' @return A statnet network object if \code{log} is \code{FALSE}, or a list of statnet network objects if \code{log} is \code{TRUE}.
#' @details
#' \code{statnet}(see: https://statnet.org/)  is a popular suite package for statistical social network analysis which takes \code{network} class object as an input.
#' The \code{D_to_statnet} function internally calls \code{agrABM_network} to create an aggregated dataset of D and then passes
#' it to \code{statnet}'s \code{network} function to create an \code{network} class object.
#' @seealso [agrABM_network()], [network::network()]
#' @importFrom network network
#' @import network
#' @export
#' @examples
#' # preparing the dataset
#' D <- setABM_network(n = 5,
#' .act = actAgent_addEdges_random)
#' D <- runABM_network(D,
#'                     .stopCondition = stopABM_times(simTimes = 5),
#'                     save_log = TRUE)
#'
#' # Example 1: Convert the current D object into statnet's network object
#' D_net <- D_to_statnet(D)
#'
#' # Example 2: Convert the log of D object into the list of statnet's network object
#' D_net <- D_to_statnet(D, log = TRUE)


# 関数
D_to_statnet <- function(D, log = F, duplicated_edges = "sum", which_net = 1){
  # logを対象とするか
  if(log == F){
    # DATAの方を変換する場合
    ## ひとまず通常のフォーマットに変換
    temp_convert <- agrABM_network(D = D,
                                   duplicated_edges = duplicated_edges,
                                   agr_log = F,
                                   node_attr_long = F)

    ## statnetのオブジェクトを作成
    D_net <- network::network(x = temp_convert$networks[[which_net]],
                              matrix.type = "adjacency",
                              vertex.attr = as.list(temp_convert$node_attr),
                              ignore.eval=FALSE,
                              names.eval='weight')
  }else{
    # logの方を変換する場合
    ## ひとまず通常のフォーマットに変換
    temp_convert <- agrABM_network(D = D,
                                   duplicated_edges = duplicated_edges,
                                   agr_log = T, node_attr_long = F)
    ## 時間の数を計算
    n_times <- length(temp_convert)
    ## 各時間ごとに処理
    D_net <- vector("list", n_times)
    for(t in 1:n_times){
      temp_net <- network::network(x = temp_convert[[t]]$networks[[which_net]],
                                   matrix.type = "adjacency",
                                   vertex.attr = as.list(temp_convert[[t]]$node_attr),
                                   ignore.eval = FALSE,
                                   names.eval = "weight")
      temp_net %n% "time" <- D$log[[t]]$time
      D_net[[t]] <- temp_net
    }
    names(D_net) <- paste0("t", 1:n_times)
  } # log=T終わり
  # アウトプット
  D_net
}
