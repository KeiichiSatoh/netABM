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
#' @param which_attr Charactor vector of attributes. The default \code{NULL} retrieves all agent's attributes.
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

# ログあり版
get_attr_from_ca <- function(
    D, ID_ca, attr_ca = NULL, with_agent_attr = FALSE, incl_posit = FALSE,
    from_log = FALSE, attr_long = FALSE, which_attr = NULL){
  # 前処理---------------------------------
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

  # 本処理
  if(from_log==FALSE){
    ## 現在のデータから
    out <- .get_attr_from_ca(D = D, ID_ca = ID_ca,
                             attr_ca = attr_ca,
                             with_agent_attr = with_agent_attr,
                             incl_posit = incl_posit,
                             which_attr = which_attr)
  }else{
    ## from_logから
    out <- lapply(D$log, function(D_t){
      .get_attr_from_ca(D = D_t, ID_ca = ID_ca, attr_ca = attr_ca,
                        with_agent_attr = with_agent_attr,
                        incl_posit = incl_posit,
                        which_attr = which_attr)
    })
    ## attr_long = Tの場合
    if(attr_long==TRUE){
      out2 <- lapply(1:length(out), function(t){
        data.frame(out[[t]], time = t)
      })
      out <- do.call(rbind, out2)
    }
  } ## from_log = TRUEここまで
  # アウトプット
  out
}
