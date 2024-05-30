
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
