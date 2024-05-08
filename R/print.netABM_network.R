#' @title Printing the netABM_network class
#' @param D A netABM_netork object
#' @export
#' @examples
#' mat <- sna::rgraph(5)
#' D <- setABM_network(agent_n = 5, agent_network = mat)
#' print.netABM_network(D)
#'
#' print(D)    # do the same

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
