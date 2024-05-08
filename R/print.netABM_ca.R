#' @title Printing the netABM_ca class
#' @param D A netABM_ca object
#' @export
#' @examples
#' D <- setABM_ca(agent_n = 5)
#' print.netABM_network(D)
#'
#' print(D)    # do the same

print.netABM_ca <- function(D){
  cat("agent:", "\n")
  cat("  Number of Agents     :", length(D$agent), "\n")
  cat("  Agent attributes(a)  :", names(D$agent[[1]]$a), "\n")
  cat("  ca(agent location)   :", names(D$stage$ca)[!names(D$stage$ca) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("stage:", "\n")
  cat("  ca                   :", names(D$stage$ca)[!names(D$stage$ca) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("time:", D$time, "\n")
  cat("log(length):", if(is.na(D$log)){"NULL"}else{length(D$log)}, "\n")
  cat("\n")
  cat("Note: The address and the row-column index of the agent's location in the CA can be obtained with adr and rc, respectively (e.g., D$agent$A1$ca_adr.)")
}

