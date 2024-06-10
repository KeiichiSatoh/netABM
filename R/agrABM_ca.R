#' @title Aggregate the CA and attributes from the ABM data
#' @description The `agrABM_ca` function processes a given data object `D` to extract and
#' organize its attributes and cellular automata (CA) components.
#' It provides different methods of extracting these components based on the specified parameters.
#'
#' @param D A \code{netABM} class objectxto be processed.
#' @param from_log A logical value indicating whether to extract data from the log \code{TRUE} or directly from the current state \code{FALSE}. Default is \code{FALSE}.
#' @param by_element A logical value that, when \code{from_log} is \code{TRUE}, specifies whether to group data by individual elements \code{TRUE} or by time points \code{FALSE}. Default is \code{FALSE}.
#' @param attr_long A logical value that, when \code{from_log} is \cpde{TRUE} and \cpde{by_element} is \code{TRUE}, specifies whether to extract attributes in a long format \code{TRUE} or not \code{FALSE}. Default is \code{FALSE}.
#'
#' @details The function processes the data object \code{D} and extracts its attributes and CA components. The behavior of the function changes based on the values of \code{from_log} and \code{by_element}
#' parameters:
#' \itemize{
#'   \item When \code{from_log} is \code{FALSE}, the function extracts the current attributes and CA components directly from \code{D}.
#'   \item When \code{from_log} is \code{TRUE}:
#'   \itemize{
#'     \item If \code{by_element} is \code{FALSE}, the function extracts data for each time point in the log, organizing the output by time.
#'     \item If \code{by_element} is \code{TRUE}, the function organizes the data by elements, optionally using a long format for attributes if \code{attr_long} is \code{TRUE}.
#'   }
#' }
#'
#' @return A list containing the extracted attributes and CA components. The structure of the list depends on the specified parameters:
#' \itemize{
#'   \item If \code{from_log} is \code{FALSE}, the list contains the current attributes and CA components.
#'   \item If \code{from_log} is \code{TRUE} and \code{by_element} is \code{FALSE}, the list contains sublists for each time point in the log.
#'   \item If \code{from_log} is \code{TRUE} and \code{by_element} is \code{TRUE}, the list contains data grouped by individual elements, with attributes optionally in a long format.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage of agrABM_ca function
#' result <- agrABM_ca(D = some_data_object, from_log = FALSE, by_element = FALSE, attr_long = FALSE)
#' }
#'
#' @seealso \code{\link{get_attr}} for the helper function used within \code{agrABM_ca} to extract attributes from the data object.
#'
#' @export

agrABM_ca <- function(D, from_log = FALSE, by_element = FALSE, attr_long = FALSE){
  # from_log == FALSEの場合
  if(from_log == FALSE){
    # attribute
    attr <- get_attr(D, which_attr = NULL, from_log = FALSE)
    # CA
    ca_name <- names(D$ca)[!names(D$ca) %in% c("clone",".__enclos_env__")]
    ca_temp <- lapply(ca_name, function(X){
      D$ca[[X]]
    })
    names(ca_temp) <- ca_name
    # まとめる
    out <- c(attr = list(attr), ca_temp)
  }else{
    ## from_log = TRUEの場合
    if(by_element == FALSE){
      ## by_element == FALSEのとき：各時点ごとにリストにまとめる
      out <- vector("list", length(D$log))
      names(out) <- names(D$log)
      for(t in 1:length(D$log)){
        temp_D <- D$log[[t]]
        attr <- get_attr(D = temp_D, which_attr = NULL, from_log = FALSE)
        # CA
        ca_name <- names(temp_D$ca)[!names(temp_D$ca) %in% c("clone",".__enclos_env__")]
        ca_temp <- lapply(ca_name, function(X){
          temp_D$ca[[X]]
        })
        names(ca_temp) <- ca_name
        # まとめる
        out[[t]] <- c(attr = list(attr), ca_temp)
      }
    }else{
      ## by_element == TRUEのとき：要素ごとにまとめる
      ## attr
      if(attr_long == FALSE){
        ### attr_long == FALSEの時
        attr <- get_attr(D = D, from_log = TRUE, attr_long = FALSE)
      }else{
        ### attr_long == TRUEの時
        attr <- get_attr(D = D, from_log = TRUE, attr_long = TRUE)
      }
      ## ca
      ca_name <- names(temp_D$ca)[!names(temp_D$ca) %in% c("clone",".__enclos_env__")]
      ca_list <- vector("list", length(ca_name))
      names(ca_list) <- ca_name
      for(i in 1:length(ca_name)){
        ca_list[[i]] <- vector("list", length(D$log))
        names(ca_list[[i]]) <- names(D$log)
      }
      for(t in 1:length(D$log)){
        for(i in 1:length(ca_name)){
          ca_list[[i]][[t]] <- D$log[[t]]$ca[[i]]
        }
      }
      # すべての結果をまとめる
      out <- c(list(attr = attr), ca_list)
    }
  }
  # アウトプット
  out
}




