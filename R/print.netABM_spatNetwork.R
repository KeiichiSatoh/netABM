#' @title Printing the netABM_spatNetwork class
#' @param D A netABM_spatNetwork object
#' @export
#' @examples
#'
#' D <- setABM_spatNetwork(agent_n = 5, place_n = 10)
#' print.netABM_spatNetwork(D)
#'
#' print(D)    # do the same

print.netABM_spatNetwork <- function(D){
  cat("agent:", "\n")
  cat("  Number of Agents     :", length(D$agent), "\n")
  cat("  Agent attributes(a)  :", names(D$agent[[1]]$a), "\n")
  cat("  Agent networks(edges):", names(D$stage$agent)[!names(D$stage$agent) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("  Agent location(edges):", names(D$stage$location)[!names(D$stage$location) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("place:", "\n")
  cat("  Number of Places     :", length(D$place), "\n")
  cat("  Place attributes(a)  :", names(D$place[[1]]$a), "\n")
  cat("  Place networks(edges):", names(D$stage$place)[!names(D$stage$place) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("stage:", "\n")
  cat("  agent(network)       :", names(D$stage$agent)[!names(D$stage$agent) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("  place(network)       :", names(D$stage$place)[!names(D$stage$place) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("  location(network)    :", names(D$stage$location)[!names(D$stage$location) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("\n")
  cat("time:", D$time, "\n")
  cat("log(length):", if(is.na(D$log)){"NULL"}else{length(D$log)}, "\n")
}



