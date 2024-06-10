#' @title Get Cellular Automata by A Specified Attribute
#'
#' @description The \code{get_ca_by_attr} function retrieves cellular automata (CA) based on specified attributes
#' from a data object \code{D} or directly from provided CA and attribute list.
#' The function can also extract CA from a \code{log} if specified.
#'
#' @param D A \code{netABM} class object containing CA and attributes. This parameter is required if \code{CA} and \code{attr_list} are not provided directly.
#' @param which_ca Either an index or name specifying which CA to extract from \code{D}. Default is \code{1}.
#' @param which_attr Either an index or name specifying which attribute to use for extraction. Default is \code{1}.
#' @param from_log A logical value indicating whether to extract data from the log (\code{TRUE}) or from the current state (\code{FALSE}). Default is \code{FALSE}.
#' @param CA A list or array of CA data. If provided, this CA data is used directly, and \code{D} is not required.
#' @param attr_list A list of attribute vectors. If provided, this attribute list is used directly, and \code{D} is not required.
#' @param exclude_value A value in the CA data that should be excluded and replaced with \code{NA}. Default is \code{0}.
#'
#' @details The function processes the data object \code{D} or the provided \code{CA} and \code{attr_list}
#' to retrieve the CA data based on the specified attributes. The function allows for flexibility in how the CA data and attributes are specified,
#' either directly or via the data object \code{D}.
#'
#' @return A list or array of CA data with the specified attributes applied. If only one CA data set is retrieved, it is returned as an array.
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @examples
#' \dontrun{
#' # Example usage of get_ca_by_attr function with a data object
#' result <- get_ca_by_attr(D = some_data_object)
#'
#' # Example usage with data from log
#' result <- get_ca_by_attr(D = some_data_object, from_log = TRUE)
#'
#' # Example usage with direct CA and attribute list
#' result <- get_ca_by_attr(CA = some_data_object$ca$room, D = some_data_object)
#' result <- get_ca_by_attr(CA = some_data_object$ca$room, attr_list = list(c("F", "F", "F", "J", "J")))
#' }
#'
#' @seealso \code{\link{agrABM_ca}} for a related function to process and extract attributes and CA components.
#'
#' @export
get_ca_by_attr <- memoise::memoise(
  function(
    D = NULL, which_ca = 1, which_attr = 1, from_log = FALSE,
    CA = NULL, attr_list = NULL, exclude_value = 0){
    # CAが直接投入されている場合にはそちらを利用
    # そうでない場合にはDとwhich_caから取得
    if(is.null(CA)){
      # Dが必要となるため、チェックをかける
      stopifnot("D must be supplied, if you do not supply CA directly." = is.null(D)==FALSE)
      # CAを特定する
      if(is.numeric(which_ca)){
        ca_namelist <- names(D$ca)[!names(D$ca) %in% c(".__enclos_env__","clone","print")]
        which_ca <- ca_namelist[which_ca]
      }
      if(from_log == FALSE){
        # from_log==TRUE:現状のデータからCAを構成
        CA <- D$ca[[which_ca]]
      }else{
        # from_log==FALSE:logからCAを構成
        CA <- lapply(1:length(D$log), function(t){
          D$log[[t]]$ca[[which_ca]]
        })
      }
    }

    # すべてのCAをリスト形式に揃える
    if(is.list(CA)==FALSE){
      CA <- list(CA)
    }

    # attr
    if(!is.null(attr_list)){
      # attr_vecが投入されている場合にはそちらを使用
      stopifnot("attr_list must be a list. If you want to supply a vector, set imput as list(vector)."=is.list(attr_list))
      for(t in 1:length(attr_list)){
        if(is.vector(attr_list[[t]])==FALSE){
          attr_list[[t]] <- attr_list[[t]][[which_attr]]
        }
      }
    }else{
      stopifnot("D must be supplied, if you do not supply attr_list directly." = is.null(D)==FALSE)
      # それ以外の場合にはwhich_attrから取得
      if(from_log==FALSE){
        # logから取らない場合
        attr_list <- list(unlist(lapply(D$agent, function(X){X$a[[which_attr]]})))
      }else{
        # logからとる場合
        attr_list <- lapply(D$log, function(X){
          unlist(lapply(X$agent, function(Y){Y$a[[which_attr]]}))
        })
      }
    }

    # CAとattr_vec_listの次元が会うかどうかをテスト
    stopifnot("The length of CA and attr_vec_list does not match." = length(CA)==length(attr_list))

    # CAのIDをattr_vec_listに置き換える
    out <- mapply(function(X, Y){
      X_dim <- dim(X)
      X_vec <- as.vector(X)
      X_vec[X_vec %in% exclude_value] <- NA
      array(Y[X_vec], dim = X_dim)
    }, X = CA, Y = attr_list, SIMPLIFY = FALSE)

    # アウトプットのリスト数が1ならば、リストを外す
    if(length(out)==1){
      out <- out[[1]]
    }
    # アウトプット
    out
  }, cache = cachem::cache_mem(max_age = 900))
