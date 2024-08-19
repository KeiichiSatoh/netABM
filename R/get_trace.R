#' @title Trace The Change in Agent's Attributes And Fields From A Log
#' @description How each agent changes their attributes and positions in the filed
#' is one of the focus in ABM. This function extracts such data from the simulated
#' log and convert them into the data.frame where agents are in the row and time is in the column.
#'
#' @param which_attr A character vector specifying the names of agent attributes to extract over time.
#' If \code{NULL}, no agent attributes are extracted. Default is \code{NULL}.
#' @param which_ca A character vector specifying the names of cellular automaton (CA)
#'  to track. Default is \code{NULL}.
#' @param ID_ca A single value indicating the CA in which the cell in which agent ID is shown in the relevant position.
#' This must be provided if \code{which_attr_ca} is specified; otherwise the function does not know the position of the agent in CA.
#' @param which_attr_ca A character vector specifying CA from which the relevant agent's attribute is retrieved.
#' The cell must correspond to the agent location indicated by \code{ID_ca}.
#' @param ID A vector of agent IDs for which the traces are to be extracted.
#' If \code{NULL}, traces for all agents are extracted. Default is \code{NULL}.
#' @param D A list containing the simulation log (i.e., \code{D$log}) in the \code{ABM_D} class.
#' If \code{NULL}, the function attempts to retrieve \code{D} from the parent environment.
#' Default is \code{NULL}.
#'
#' @return A list where each element is a data frame containing the traces of the specified attributes or actions
#' over time. The list keys correspond to the requested attributes or actions.
#' @export
#'
#' @examples
#' agent_get_older <- function(){self$age <- self$age + 1}
#' attr_ca1 <- matrix(1:9, 3, 3)
#' attr_ca2 <- matrix(rev(1:9), 3, 3)
#' ID_ca <- matrix(sample(c(rep(0, 4), 1:5)), 3, 3)
#' D <- setABM(agent_n = 5,
#'             agent_attr = data.frame(age = 1:5, sex = c("m", "m", "m", "f", "f")),
#'             ca = list(ID_ca = ID_ca, attr_ca1 = attr_ca1, attr_ca2 = attr_ca2),
#'             agent_f = agent_get_older)
#' D <- runABM(D, save_log = TRUE, stopFUN = 3)
#'
#'
#' get_trace(which_attr = c("age"),
#'           ID_ca = "ID_ca",
#'           which_attr_ca = c("attr_ca1", "attr_ca2"),
#'           D = D)

get_trace <- function(
  which_attr = NULL, which_ca = NULL,
  ID_ca = NULL, which_attr_ca = NULL,
  ID = NULL, D = NULL){
  # Dがない場合にはparent.frameより取得
  if(is.null(D)){D <- parent.frame()$D}
  # IDがない場合にはすべてのagent
  if(is.null(ID)){ID <- 1:length(D$agent)}

  # which_attrから取得する場合
  trace_attr <- NULL
  if(!is.null(which_attr)){
    trace_attr <- lapply(which_attr, function(X){
      trace_X <- data.frame(
        lapply(1:length(D$log), function(t){
          sapply(ID, function(i){
            D$log[[t]]$agent[[i]][[X]]
          })
        })
      )
      dimnames(trace_X) <- list(ID, names(D$log))
      trace_X
    })
    names(trace_attr) <- which_attr
  }

  # caから取得する場合
  trace_ca <- NULL
  if(!is.null(which_ca)){
      trace_ca <- lapply(which_ca, function(X){
        trace_X <- data.frame(lapply(1:length(D$log), function(t){
          sapply(ID, function(i){
            which(D$log[[t]][[X]]==i)
          })
        }))
        dimnames(trace_X) <- list(ID, names(D$log))
        trace_X
      })
      names(trace_ca) <- which_ca
  }

  # ID_ca & attr_caから採る場合
  trace_attr_ca <- NULL
  if(!is.null(ID_ca)){
    stopifnot("ID_ca must be length of 1." = length(ID_ca)==1)
    stopifnot("The length of attr_ca must be at least 1."= length(which_attr_ca)>=1)
    # ID_ca上のそれぞれの時点の位置を取得する
    ca_posit <- data.frame(lapply(1:length(D$log), function(t){
      sapply(ID, function(i){
        which(D$log[[t]][[ID_ca]]==i)
      })
    }))
    dimnames(ca_posit) <- list(ID, names(D$log))
    # それぞれの時点におけるattr_caを取得する
    trace_attr_ca <- lapply(which_attr_ca, function(X){
      attr_ca_X <- data.frame(lapply(1:length(D$log), function(t){
        D$log[[t]][[X]][ca_posit[,t]]
      })
      )
      dimnames(attr_ca_X) <- list(ID, names(D$log))
      attr_ca_X
    })
    names(trace_attr_ca) <- which_attr_ca
  }

  # すべての結果をまとめる
  out <- c(trace_attr, trace_ca, trace_attr_ca)
  out
}
