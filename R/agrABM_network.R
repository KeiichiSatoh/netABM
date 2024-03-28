#' @title Aggregate Data From A netABM Object.
#' @description \code{agrABM_network} create node attribute data.frame and network matrices, 
#' which are the normal dataset format for the data analysis. The log file can be also converted 
#' either as a separate file for each time slice or aggregated so-called long-format of the panel dataset.
#' @param D An object of class \code{netABM}, representing the network ABM simulation data.
#' @param agr_log A logical value indicating whether to aggregate the log data. Default is \code{FALSE}.
#' @param node_attr_long A logical value indicating whether to format node attributes in long format. Default is \code{FALSE}.
#' @param duplicated_edges A character string specifying how to handle duplicated edges. 
#' Possible values are "sum", "min", "max", or "mean". Default is \code{"sum"}.
#' 
#' @details This function aggregates data from a network ABM simulation stored in the 'netABM' class object.
#' If \code{agr_log} is \code{FALSE}, it aggregates the most recent data from 
#' the simulation. If \code{agr_log} is \code{TRUE}, it aggregates data from 
#' the simulation logs. If the log data is not available, it aggregates the most recent data instead.
#' The \code{node_attr_long} argument specifies whether to format node attributes in long format. When TRUE, it rearranges 
#' node attributes into long format, suitable for further analysis.
#' 
#' The \code{duplicated_edges} argument determines how duplicated edges are handled 
#' during aggregation. It can be set to \code{"sum"} (summing duplicated edges), 
#' \code{"min"} (taking the minimum value), \code{"max"} (taking the maximum value), 
#' or \code{"mean"} (calculating the mean value).
#' 
#' @returns A list containing aggregated data from the network ABM simulation, 
#' including node attributes and aggregated networks.
#' @export
#' @examples
#' # Preparing the data
#' D <- setABM_network(n = 5,
#'                     .act = actAgent_addEdges_random)
#' D <- runABM_network(D = D, 
#'                   save_log = TRUE, 
#'                   .stopCondition = stopABM_times(simTimes = 3))
#'                     
#' # Example 1: Simple aggregation of the current dataset
#' D_agr <- agrABM_network(D)
#' D_agr
#' 
#' # Example 2: Aggregation of the log
#' D_agr <- agrABM_network(D, agr_log = TRUE)
#' D_agr
#' 
#' # Example 3: Aggregation of the log with long-format node attributes
#' D_agr <- agrABM_network(D, agr_log = TRUE, node_attr_long = TRUE)
#' D_agr

agrABM_network <- function(
    D, 
    agr_log = F,
    node_attr_long = F,
    duplicated_edges = "sum"
){
  # netABMクラスではない場合には警告を出す。
  if(!any(class(D) %in% "netABM")){
    warnings("D is supposed to be the class of: netABM")
  }
  
  # 関数内関数:一回分のDから全体レベルデータを作成する関数を定義-----
  .make_agr_D <- function(D, duplicated_edges = "sum"){
    # node_attrをまとめる-------
    node_attr_name <- names(D$agents[[1]]$a) 
    node_attr <- as.data.frame(matrix(NA, length(D$agents), length(node_attr_name)))
    colnames(node_attr) <- node_attr_name
    x <- lapply(D$agents, FUN = function(i){unlist(i$a)})
    for(i in 1:length(x)){
      node_attr[i, ] <- unlist(x[[i]])
    }
    # networkをまとめる-------
    network_name <- names(D$agents[[1]]$e)
    networks <- vector("list", length(network_name))
    names(networks) <- network_name
    ## それぞれのnetworkごとにまとめる
    for(p in 1:length(network_name)){
      # 空のマトリクスを作る
      mat <- matrix(0, length(D$agents), length(D$agents))
      dimnames(mat) <- list(names(D$agents), names(D$agents))
      # matrixにそれぞれの回答をつける
      for(i in 1:length(D$agents)){
        # agentのエッジを取り出す
        agent_edges <- D$agents[[i]]$e[[network_name[p]]]
        agent_edges <- data.frame(ID = names(agent_edges), val = agent_edges)
        # エッジが存在する場合
        if(nrow(agent_edges)!=0){
          # 複数のエッジがある場合にどう処理するのかに従って処理
          agent_edges <- switch(duplicated_edges, 
                                "sum" = {tapply(agent_edges$val, agent_edges$ID, sum)},
                                "max" = {tapply(agent_edges$val, agent_edges$ID, max)},
                                "min" = {tapply(agent_edges$val, agent_edges$ID, min)},
                                "mean" = {tapply(agent_edges$val, agent_edges$ID, mean)},
                                stop("This method for the duplicated_edges is not available")
          )
          mat[i,names(agent_edges)] <- agent_edges  
        } # エッジが存在する場合
      }
      # networkにまとめる
      networks[[p]] <- mat
    }
    
    ## 結果を出力する
    out <- list(node_attr = node_attr,
                networks = networks)
    out
  }
  # 関数内関数ここまで-----
  
  # 実際の処理
  if(agr_log == FALSE){
    # 一番最新のDをまとめる場合
    D_agr <- .make_agr_D(D = D, 
                         duplicated_edges = duplicated_edges)
  }else{
    # logをaggregateする場合
    if(all(is.na(D$log))){
      warning("NO log found in the input D.")
      ## もしもlogが見つからない場合
      D_agr <- .make_agr_D(D = D,
                           duplicated_edges = duplicated_edges)

    }else{
      ## logがきちんと見つかる場合
      ## 空の入れ物を用意
      D_agr <- vector("list", length(D$log))
      names(D_agr) <- names(D$log)
      ## データを貼り付ける
      for(m in 1:length(D$log)){
        D_agr[[m]] <- .make_agr_D(D = D$log[[m]],
                                  duplicated_edges = duplicated_edges)
      }# aggregate終了
      
      ## node_attr_long = Tの場合
      if(node_attr_long == TRUE){
        ### nodeをまとめなおす
        node_attr_long_format <- cbind(
          D_agr[[1]]$node_attr,
          time = D$log[[1]]$time)
        for(m in 2:length(D_agr)){
          new_time <- cbind(D_agr[[m]]$node_attr, 
                            time = D$log[[m]]$time)
          node_attr_long_format <- rbind(node_attr_long_format, 
                                         new_time)
        }
        ### networksをまとめなおす
        network_log <- vector("list", length(D_agr$t1$networks))
        names(network_log) <- names(D_agr$t1$networks)
        for(p in 1:length(D_agr$t1$network)){
          network_log[[p]] <- vector("list", length(D_agr))
          names(network_log[[p]]) <- names(D$log)
          for(t in 1:length(D_agr)){
            network_log[[p]][[t]] <- D_agr[[t]]$networks[[p]]
          }
        }
        ### まとめなおしたものを付け替える
        D_agr <- list(node_attr = node_attr_long_format,
                      networks = network_log)
      } # node_attr_longここまで
    } # logがきちんと見つかった場合ここまで
  } # log_aggregateここまで
  
  # アウトプット
  D_agr
}
