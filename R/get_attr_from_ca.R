#' @title Retrieve Attributes from CA
#' @description
#' This function retrieves specific attributes from a \code{ca} (cellular automaton) type field
#' within the given \code{D} object.
#' The attributes can be retrieved from the current data or from log data,
#' depending on the parameters provided.
#'
#' @param D The \code{ABM_D} object containing \code{ca} type field.
#' @param ID_ca The identifier for the \code{ca} type field within the \code{D}.
#' @param attr_ca A vector of attribute names to retrieve.
#' If \code{NULL}, all \code{ca} type fields within \code{D} except for \code{ID_ca}
#' will be used.
#' @param with_agent_attr A logical value indicating whether to include agent attributes in the output.
#' @param incl_posit A logical value indicating whether to include the position information of the \code{ca}.
#' @param from_log A logical value indicating whether to retrieve data from the log.
#' @param attr_long A logical value indicating whether to output the attributes in long format.
#' @description
#' In some cases, the user firstly defines the attribute of the {ca} by using the \code{ca} type field
#' and wants to extract this \code{ca} field attribute of the cell in which agents locates.
#' This function can be used for such scenario.
#' @return A data frame containing the retrieved CA attributes.
#' If \code{from_log} is \code{TRUE}, a list of data frames
#' is returned, one for each time point in the log.
#' If \code{attr_long} is \code{TRUE}, a single data frame in long format
#' is returned.
#' @seealso [agrABM()]
#' @import memoise
#' @export
#' @examples
#' # define ca attributes
#' zone <- matrix(c(1,1,
#'                  2,2), 2, 2, byrow = TRUE)
#' D <- setABM(agent_n = 3, ca = list(ca = 2, zone = zone))
#' # View where each agent locates
#' D$ca
#' # Extract the zone number where each agent locates
#' get_attr_from_ca(D = D, ID_ca = "ca", attr_ca = "zone")

get_attr_from_ca <- memoise(function(
    D, ID_ca, attr_ca = NULL, with_agent_attr = FALSE, incl_posit = FALSE,
    from_log = FALSE, attr_long = FALSE){
  # attr_caがNULLの場合
  if(is.null(attr_ca)){
    field_name <- names(D)[!names(D) %in% c(".__enclos_env__",".f","notes","log","time","agent","clone",".save")]
    field_type <- unlist((lapply(field_name, function(X){attr(D[[X]], "field_type")})))
    field_name <- field_name[(field_name != "ID_ca") & (field_type == "ca")]
    if(length(field_name)==0){
      warning("No ca found in D")
      return(NULL)
    }else{
      attr_ca <- field_name
    }
  }
  # agent_nを取得する
  agent_n <- length(D$agent)
  # 現在のデータの場合
  if(from_log == FALSE){
    # データを取得する
    ID_ca_retrievd <- D[[ID_ca]]
    attr_ca_retrieved <- lapply(attr_ca, function(p){D[[p]]})
    names(attr_ca_retrieved) <- attr_ca
    # positを取得
    posit <- unlist(lapply(1:agent_n, function(i){which(i==ID_ca_retrievd)}))
    # attr_caから各agentのattrを取得
    out <- data.frame(lapply(attr_ca_retrieved, function(X){X[posit]}))
    # IDを含める
    out <- data.frame(ID = 1:agent_n, out)
    # positを含めるか
    if(incl_posit==TRUE){
      out <- data.frame(out, posit = posit)
    }
    # agent_attrを含めるか
    if(with_agent_attr==TRUE){
      out <- data.frame(get_agent_attr(D), out[ ,-1])
    }
  }else{
  # logデータからの場合
    ## timeを取得する
    times <- unlist(lapply(D$log, function(X){X$time}))
    n_times <- length(D$log)
    ## データを取得する
    ID_ca_retrieved <- lapply(1:n_times, function(t){D$log[[t]][[ID_ca]]})
    ## positを取得
    posit <- lapply(ID_ca_retrieved, function(ca){
      unlist(lapply(1:n_times, function(i){which(i == ca)}))
    })
    ## それぞれの時点ごとに、各ca_attrを取得する
    out <- lapply(1:n_times, function(t){
      D_timewise <- D$log[[t]]
      posit_timewise <- posit[[t]]
      attr_timewise <- data.frame(lapply(attr_ca, function(p){
        temp_attr_ca <- D_timewise[[p]]
        unlist(lapply(1:agent_n, function(i){temp_attr_ca[i]}))}
      ))
      names(attr_timewise) <- attr_ca
      data.frame(ID = 1:agent_n, attr_timewise)
    })
    names(out) <- names(times)
    ## incl_posit = Tの場合
    if(incl_posit==TRUE){
      out <- lapply(1:n_times, function(t){
        X_t <- out[[t]]
        posit_t <- posit[[t]]
        X_t <- data.frame(X_t, posit = posit_t)
        X_t
      })
      names(out) <- names(times)
    }
    ## agent_attrを含める場合
    if(with_agent_attr==TRUE){
      attr <- get_agent_attr(D, from_log = TRUE)
      out <- lapply(1:length(out), function(t){
        data.frame(attr[[t]], out[[t]][ ,-1])
      })
      names(out) <- names(times)
    }
    ## attr_long = Tの場合
    if(attr_long==TRUE){
      out <- lapply(1:length(out), function(t){
        X <- data.frame(out[[t]], time = times[[t]])
        X
      })
      out <- do.call(rbind, out)
    }
  } # log==TRUEここまで
  # リターン
  out
})
