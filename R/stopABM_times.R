#' @title Stop ABM Simulation by Runtime
#' @description
#' \code{stopABM_times} returns TRUE when the ABM simulation continues after the defined time.
#' @param D The D object. Note that this will be supplied internally within \code{runABM}. Therefore, user needs to leave this argument as it is.
#' @param simTimes Number of run (Default: 1)
#' @returns  TRUE (if the simulation has run for the defined number of times specified by simTimes)
#' @family stopABM
#' @author Keiichi Satoh
#' @export
#' @examples
#' # This function is supposed to be used in tandem with runABM family function.
#'
#' # D <- setABM_network(n = 5)
#' # runABM_network(D=D, .stopCondition = stopABM_simTimes(simTimes = 5))

stopABM_times <- function(D, simTimes = 1){
  D$time >= (simTimes + 1)
}
