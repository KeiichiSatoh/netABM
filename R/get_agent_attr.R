#' @title Get A Agents' Attributes From A ABM_D Object.
#' @description \code{get_agent_attr} creates a vector and data.frame of
#' agents' attributes.
#' @param D An object of class \code{ABM_D}.
#' @param which_attr character. If \code{NULL}, retrieve all agent's attribute.
#' @param from_log logical. Whether the agent attributes should be extracted from the log.
#' @param attr_long logical. Whether the agent attributes in the log shold be organized in a long format.
#' @returns a data.frame of agent attributes.
#' @import memoise
#' @export
#' @examples
#' # Create the ABM_D object
#' agent_attr <- data.frame(
#'     sex = c("m","m","m","f","f"))
#' D <- setABM(agent_n = 5, agent_attr = agent_attr)
#'
#' # Extract the agent attributes
#' get_agent_attr(D)


get_agent_attr <- function(D, which_attr = NULL,
                           from_log = FALSE, attr_long = FALSE){
  # 前処理
  if(is.null(which_attr)){
    which_attr <- c("ID","f_label",
                    setdiff(names(D$agent[[1]]), c("print","clone",".save",".f",".__enclos_env__","ID","f_label")))
  }

  # 本処理
  if(from_log==FALSE){
    ## 現在のDから採る場合
    out <- .get_agent_attr(D = D, which_attr = which_attr)
  }else{
    ## logから採る場合
    out <- lapply(D$log, function(D_t){
      .get_agent_attr(D = D_t, which_attr = which_attr)
    })

    if(attr_long==TRUE){
      ## attr_longの場合
      out <- lapply(1:length(out), function(t){
        data.frame(out[[t]], time = t)
      })
      out <- do.call(rbind, out)
    }
  } # from_log = TRUEここまで

  # アウトプット
  out
}

