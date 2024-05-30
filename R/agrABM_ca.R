
#' agrD <- agrABM_ca(D)
#' agrD <- agrABM_ca(D, from_log = TRUE, attr_long = FALSE)
#' agrD <- agrABM_ca(D, from_log = TRUE, attr_long = TRUE)

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




