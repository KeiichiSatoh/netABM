

#' library(rlang)
#'
#' # INPUT
#' mat <- list(matrix(1:9,3,3),
#'            matrix(11:19, 3,3),
#'            matrix(21:29, 3,3))
#'
#'mapplyly(sum(mat, na.rm = FALSE))


mapplyly <- function(FUN, which_list = 1, .combine = NULL){
  # substituteする
  FUN <- substitute(FUN)
  name_FUN <- call_name(FUN)
  args_FUN <- call_args(FUN)
  args_name <- names(args_FUN)

  # argumentのうちリスト化するものと、それ以外を分ける
  which_list_modifed <- c()
  for(i in 1:length(which_list)){
    if(is.character(which_list[i])){
      which_list_modifed[i] <- which(args_name==which_list[i])
    }else{
      which_list_modifed[i] <- which_list[i]
    }
  }

  args_FUN_main <- args_FUN[which_list_modifed]
  args_FUN_rest <- args_FUN[!1:length(args_FUN) %in% which_list_modifed]

  # リストとして取得されてしまった場合にはそれを外す
  if(is.list(args_FUN_main)){
    args_FUN_main <- args_FUN_main[[1]]
  }

  # mapplyをかける
  temp_out <- mapply(FUN = name_FUN, eval(args_FUN_main), args_FUN_rest,
                     SIMPLIFY = FALSE)

  # .combineを整理する
  .combine <- as.character(substitute(.combine))

  if(length(.combine)==0){
    out <- temp_out
  }else{
    if(.combine == "c"){
      .combine == "unlist"
    }
    out <- do.call(what = .combine, args = temp_out)
  }

  # output
  out
}
