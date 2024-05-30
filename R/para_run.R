
#' library(doParallel)
#' library(doRNG)
#' library(foreach)
#' library(rlang)
#'
#' out <- para_run(FUN = setABM_ca(agent_n = 6), cores = 3, n_sim = 3, seed = 1)

para_run <- function(FUN, cores = 2, n_sim = 3, seed = NULL, .export = NULL){
  # インプットされた関数および引数を取得
  FUN <- substitute(FUN)
  call_name_FUN <- call_name(FUN)
  call_args_FUN <- call_args(FUN)

  # クラスターを作成する
  cl <- makeCluster(cores)

  # registerする
  registerDoParallel(cl)

  # シード値を設定
  if(!is.null(seed)){
    set.seed(seed)
  }

  # foreachを行う
  out <- foreach(i = 1:n_sim, .packages = "netABM", .export = .export) %dorng% {
    do.call(what = call_name_FUN, args = call_args_FUN)
  }

  # クラスターを止める
  on.exit(stopCluster(cl))

  # アウトプット
  out
}

