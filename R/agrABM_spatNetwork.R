#' @title Aggregate Data From A netABM_spatNetwork Object.
#' @description \code{agrABM_spatNetwork} create node attribute data.frame and network matrices,
#' which are the normal dataset format for the data analysis. The log file can be also converted
#' either as a separate file for each time slice or aggregated so-called long-format of the panel dataset.
#' @param D An object of class \code{netABM_spatNetwork}, representing the network ABM simulation data.
#' @param agr_log A logical value indicating whether to aggregate the log data. Default is \code{FALSE}.
#' @param attr_long A logical value indicating whether to format node attributes in long format. Default is \code{FALSE}.
#'
#' @details This function aggregates data from a network ABM simulation stored in the 'netABM_network' class object.
#' If \code{agr_log} is \code{FALSE}, it aggregates the most recent data from
#' the simulation. If \code{agr_log} is \code{TRUE}, it aggregates data from
#' the simulation logs. If the log data is not available, it aggregates the most recent data instead.
#' The \code{attr_long} argument specifies whether to format node attributes in long format.
#' When \code{TRUE}, it rearranges node attributes into long format, suitable for further analysis.
#'
#' @returns A list containing aggregated data from the network ABM simulation,
#' including node attributes and aggregated networks.
#' @import Matrix
#' @export
#' @examples
#' # Preparing the data
#' agent_attr <- data.frame(
#'    age = c(0, 1, 2, 3, 4),
#'    sex = c("m","m","m","f","f"))
#' net <- matrix(1, 5, 5)
#' move_to_P1 <- function(D){
#'   D$stage$location$loc[self$ID, self$loc] <- 0
#'   D$stage$location$loc[self$ID, 1] <- 1
#'   }
#' D <- setABM_spatNetwork(agent_n = 5,
#'                         agent_attr = agent_attr,
#'                         agent_f = move_to_P1,
#'                         place_n = 10)
#' D <- runABM_spatNetwork(D = D, save_log = T)
#'
#' # Example 1: Simple aggregation of the current dataset
#' D_agr <- agrABM_spatNetwork(D = D)
#'
#' # Example 2: Aggregation of the log
#' D_agr <- agrABM_spatNetwork(D = D, agr_log = TRUE)
#' D_agr
#'
#' # Example 3: Aggregation of the log with long-format node attributes
#' D_agr <- agrABM_spatNetwork(D = D, agr_log = TRUE, attr_long = TRUE)
#' D_agr$location_network$loc$t2

agrABM_spatNetwork <- function(
    D,
    agr_log = F,
    attr_long = F
){
  # netABMクラスではない場合には警告を出す。
  if(!any(class(D) %in% "netABM_spatNetwork")){
    warnings("D is supposed to be the class of: netABM_spatNetwork")
  }

  # 関数内関数:一回分のDから全体レベルデータを作成する関数を定義-----
  .make_agr_D <- function(D){
    # agent_attrをまとめる-------
    agent_attr_name <- names(D$agent[[1]]$a)
    agent_attr <- as.data.frame(matrix(NA, length(D$agent), length(agent_attr_name)))
    colnames(agent_attr) <- agent_attr_name
    x <- lapply(D$agent, FUN = function(i){unlist(i$a)})
    for(i in 1:length(x)){
      agent_attr[i, ] <- unlist(x[[i]])
    }
    agent_attr <- data.frame(ID = names(D$agent), agent_attr)
    # place_attrをまとめる------
    place_attr_name <- names(D$place[[1]]$a)
    place_attr <- as.data.frame(matrix(NA, length(D$place), length(place_attr_name)))
    colnames(place_attr) <- place_attr_name
    x <- lapply(D$place, FUN = function(i){unlist(i$a)})
    for(i in 1:length(x)){
      place_attr[i, ] <- unlist(x[[i]])
    }
    place_attr <- data.frame(ID = names(D$place), place_attr)
    # networkをまとめる-------
    agent_network <- as.list(D$stage$agent)[!names(D$stage$agent) %in% c(".__enclos_env__","clone","print")]
    place_network <- as.list(D$stage$place)[!names(D$stage$place) %in% c(".__enclos_env__","clone","print")]
    location_network <- as.list(D$stage$location)[!names(D$stage$location) %in% c(".__enclos_env__","clone","print")]
    ## 結果を出力する
    out <- list(agent_attr = agent_attr,
                place_attr = place_attr,
                agent_network = agent_network,
                place_network = place_network,
                location_network = location_network)
    out
  }
  # 関数内関数ここまで-----

  # 実際の処理
  if(agr_log == FALSE){
    # 一番最新のDをまとめる場合
    D_agr <- .make_agr_D(D = D)
  }else{
    # logをaggregateする場合
    if(all(is.na(D$log))){
      warning("No log found in the input D.")
      ## もしもlogが見つからない場合
      D_agr <- .make_agr_D(D = D)
    }else{
      ## logがきちんと見つかる場合
      ## 空の入れ物を用意
      D_agr <- vector("list", length(D$log))
      names(D_agr) <- names(D$log)
      ## データを貼り付ける
      for(m in 1:length(D$log)){
        D_agr[[m]] <- .make_agr_D(D = D$log[[m]])
      }# aggregate終了

      ## node_attr_long = Tの場合
      if(attr_long == TRUE){
        ### nodeをまとめなおす
        agent_attr_long_format <- data.frame(
          D_agr[[1]]$agent_attr,
          time = D$log[[1]]$time)
        place_attr_long_format <- data.frame(
          D_agr[[1]]$place_attr,
          time = D$log[[1]]$time)
        for(m in 2:length(D_agr)){
          agent_new_time <- data.frame(D_agr[[m]]$agent_attr,
                            time = D$log[[m]]$time)
          agent_attr_long_format <- rbind(agent_attr_long_format,
                                         agent_new_time)
          place_new_time <- data.frame(D_agr[[m]]$place_attr,
                                       time = D$log[[m]]$time)
          place_attr_long_format <- rbind(place_attr_long_format,
                                          place_new_time)
        }
        ### networksをまとめなおす
        agent_network_log <- vector("list", length(D_agr$t1$agent_network))
        names(agent_network_log) <- names(D_agr$t1$agent_network)
        place_network_log <- vector("list", length(D_agr$t1$place_network))
        names(place_network_log) <- names(D_agr$t1$place_network)
        location_network_log <- vector("list", length(D_agr$t1$location_network))
        names(location_network_log) <- names(D_agr$t1$location_network)
        for(p in 1:length(D_agr$t1$agent_network)){
          agent_network_log[[p]] <- vector("list", length(D_agr))
          names(agent_network_log[[p]]) <- names(D$log)
          for(t in 1:length(D_agr)){
            agent_network_log[[p]][[t]] <- D_agr[[t]]$agent_network[[p]]
          }
        }
        for(p in 1:length(D_agr$t1$place_network)){
          place_network_log[[p]] <- vector("list", length(D_agr))
          names(place_network_log[[p]]) <- names(D$log)
          for(t in 1:length(D_agr)){
            place_network_log[[p]][[t]] <- D_agr[[t]]$place_network[[p]]
          }
        }
        for(p in 1:length(D_agr$t1$location_network)){
          location_network_log[[p]] <- vector("list", length(D_agr))
          names(location_network_log[[p]]) <- names(D$log)
          for(t in 1:length(D_agr)){
            location_network_log[[p]][[t]] <- D_agr[[t]]$location_network[[p]]
          }
        }
        ### まとめなおしたものを付け替える
        D_agr <- list(agent_attr = agent_attr_long_format,
                      place_attr = place_attr_long_format,
                      agent_network = agent_network_log,
                      place_network = place_network_log,
                      location_network = location_network_log)
      } # attr_longここまで
    } # logがきちんと見つかった場合ここまで
  } # log_aggregateここまで

  # アウトプット
  D_agr
}
