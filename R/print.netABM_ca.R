#' @title Printing the netABM_ca class
#' @param D A netABM_ca object
#' @method print netABM_ca
#' @export
#' @examples
#' D <- setABM_ca(agent_n = 5)
#' print.netABM_ca(D)
#'
#' print(D)    # do the same

print.netABM_ca <- function(D){
  cat("agent:", "\n")
  cat("  Number of Agents     :", length(D$agent), "\n")
  cat("  Agent attributes(a)  :", names(D$agent[[1]]$a), "\n")
  cat("ca:", "\n")
  cat(" ", names(D$ca)[!names(D$ca) %in% c(".__enclos_env__", "clone", "print")], "\n")
  cat("\n")
  cat("time:", D$time, "\n")
  cat("log(length):", if(all(is.na(D$log))){"NULL"}else{length(D$log)}, "\n")
}

D
