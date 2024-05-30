#' @title netABM_network
#' @param D A netABM_network object
#' @export
netABM_network <- function(D){
  UseMethod("netABM_network")
}

#' print.netABM_network - Print Method for netABM_network
#'
#' @param D A netABM_network object
#' @method print netABM_network
#' @export
print.netABM_network <- function(D){
  cat("agent:", "\n")
  cat("  Number of Agents     :", length(D$agent), "\n")
  cat("  Agent attributes(a)  :", names(D$agent[[1]]$a), "\n")
  cat("  agent network(edges) :", names(D$stage$agent)[!names(D$stage$agent) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("stage:", "\n")
  cat("  agent(network)       :", names(D$stage$agent)[!names(D$stage$agent) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("time:", D$time, "\n")
  cat("log(length):", if(is.na(D$log)){"NULL"}else{length(D$log)}, "\n")
}
