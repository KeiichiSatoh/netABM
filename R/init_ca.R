#' @title Initialize a Cellular Automaton (CA)
#' @description This function creates a matrix or array as Cellular Automaton object
#' that can be used as an input of \code{setABM_ca}.
#'
#' @param data The initial value to populate the CA grid. Default is \code{0}.
#' @param dim A vector specifying the dimensions of the CA grid.
#' @param agent_n The number of agents to place in the CA grid. Default is \code{NULL}.
#' @param byrow A logical value indicating whether the matrix should be filled by row. Default is \code{FALSE}.
#' @param dimnames A list of character vectors giving the names to the dimensions. Default is \code{NULL}.
#' @param alloc_method A string specifying the method for allocating agents.
#' Options are \code{"one"}, \code{"all"}, and \code{"one_at_least"} (see the details).
#' Default is \code{"one"}.
#'
#' @details
#' This function creates a matrix or an array based on the specified dimensions
#' and fills it with the given initial data. If \code{agent_n} is provided,
#' it places agents in the CA grid according to the specified allocation method:
#' \itemize{
#'   \item \code{"one"}: Each agent is placed in a unique location.
#'   \item \code{"all"}: Agents are randomly assigned to all locations with replacement. In other words, each cell is occupied by one of the agents.
#'   \item \code{"one_at_least"}: Each agent is placed in a unique location first, then remaining locations are filled with agents chosen randomly with replacement.
#' }
#'
#' @return A matrix or an array representing the initialized cellular automaton.
#' @family initialization ca_tools
#' @author Keiichi Satoh
#' @export
#' @examples
#' # Initialize a 3x3 CA grid with default values
#' init_ca(dim = c(3, 3))
#'
#' # Initialize a 3x3 CA grid with agents placed using the "one" method
#' init_ca(dim = c(3, 3), agent_n = 4, alloc_method = "one")

init_ca <- function(data = 0, dim,
                    agent_n = NULL,
                    byrow = FALSE,
                    dimnames = NULL,
                    alloc_method = "one"){
  # dimensionを計算
  dim_length <- length(dim)
  # CAを作成
  if(dim_length == 1){
    CA <- matrix(data = data, nrow = dim[1], byrow = byrow, dimnames = dimnames)
  }else if(dim_length==2){
    CA <- matrix(data = data, nrow = dim[1], ncol = dim[2], byrow = byrow, dimnames = dimnames)
  }else{
    CA <- array(data = data, dim = dim, dimnames = dimnames)
  }
  # agent_nが投入されている場合には、agentを貼り付ける
  if(!is.null(agent_n)){
    CA <- switch(
      alloc_method,
      "one" = {
        place <- sample(x = 1:length(CA), size = agent_n)
        CA[place] <- 1:agent_n
        CA
      },
      "all" = {
        locating_agent <- sample(x = 1:agent_n, size = length(CA), replace = TRUE)
        CA[1:length(CA)] <- locating_agent
        CA
      },
      "one_at_least" = {
        place <- sample(x = 1:length(CA), size = agent_n)
        locating_agent <- sample(x = 1:agent_n, size = (length(CA) - agent_n), replace = TRUE)
        CA[place] <- 1:agent_n
        CA[setdiff(1:length(CA), place)] <- locating_agent
        CA
      },
      stop("Incorrect alloc_method supplied.")
    )
  }
  # output
  CA
}
