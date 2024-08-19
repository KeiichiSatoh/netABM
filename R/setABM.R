#' @title Setting ABM_D Objects
#' @description
#' \code{setABM} constructs a \code{ABM_D} object, which is the main data object of the /code{netABM} package.
#' @param agent_n integer for setting the number of agents.
#' @param agent_attr vector/data.frame/list of attributes of agents
#'  (default: \code{NULL}).
#' @param agent_f a user-defined or built-in function object or
#' list of them representing agent's actions.
#' @param net an integer/matrix/array or list of them for constructing the network field.
#' @param ca an integer/matrix/array or list of them for constructing
#' ca (i.e., Cellular Automaton) field.
#' @param euc an integer/matrix/data.frame or list of them for constructing
#' objects that representing agents' position in a euclidean space.
#' @param other_field A vector/matrix/array or list of them for constructing
#' the fields other than \code{net}, \code{ca}, and \code{euc}.
#' @param log a list that stores the log of the simulation. For the normal usage, leave this input \code{NULL}.
#' @param time an integer of the current time of the data. For the normal usage, leave this input \code{NULL}.
#' @param notes a vector or list of them for storing the any meta data.
#'
#' @details
#' \code{setABM} is a constructor of \code{setABM_D} object,
#' which has \code{agent}, \code{time}(set as 1), \code{log} (set as NULL),
#' and \code{notes} as default.
#'
#' Each agent in the \code{agent} has their attribute set by \code{agent_attr}
#' and the action as \code{.f} (i.e., "function").
#' In addition to the user-defined \code{agent_attr}, each agent automatically get its \code{ID} and \code{f_label} and store them
#'  as their attributes. The latter \code{f_label} is taken from the object name of \code{agent_f}.
#'
#' There are two ways to set \code{agnet_f}.
#' The first way is to write the user's own function of agent's
#' action and supply this object to \code{agent_f}.
#' Do not write the function directly to \code{agent_f} because this will not be properly parsed.
#' Upon writing an original function, be sure to aware the following two reserved name:
#' - \code{self} is a reserved for indicating the agent themselves.
#' - \code{D} is a reserved for indicating the {ABM_D} object.
#'
#' The second way of setting \code{agent_f} is to use a built-in function of this package.
#' This second way actually has further three variations. First, the easiest one,
#' just supply the function object to \code{agent_f} (e.g., agent_f = function_name).
#' Second, if user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
#' Third, if user wants to put another name to this modified function object, assign it with substitute().
#' Then supply this substituted object to \code{agnet_f}. The last method may be useful when the modification
#' of the function is very long.
#' For getting the ideas more concretely about how to supply a function to \code{agent_f},
#' see the examples below.
#'
#' If \code{net} is an integer, it creates the relevant number of \code{net} field
#' in the resulted \code{ABM_D} object, wherein there are no ties among agents.
#' Notably, if the integer is supplied as list, each value is used for filling each cell in each matrix.
#'
#' Likewise, if \code{ca} is an integer, it creates the relevant number of \code{ca} field,
#'  wherein the position of each agent is randomly assigned in a \code{sqrt(agent_n + 1)*sqrt(agent_n + 1)} matrix.
#'  Uf the integer is supplied as list, each value is used for determining the \code{nrow} and \code{ncol} of each matrix.
#'
#'  Similarly, if \code{euc} is an integer, it creates the relevant number of \code{euc} field,
#'  wherein the position of each agent is randomly assigned in a two-dimensional space
#'   via normal distribution that has the mean value of 0 and the standard deviation of 1.
#'
#' \code{notes} can be used, for example, to store the parameter of constructing
#'  fields. The values in the notes can be later converted into a variable of agent * variable data.frame
#'  via the data-format transformer \code{agrABM}.
#'
#' @returns  an \code{ABM_D} class object
#' @author Keiichi Satoh
#' @importFrom rlang parse_expr
#' @importFrom rlang call_name
#' @importFrom rlang call_args
#' @import R6
#' @import Matrix
#' @export
#' @examples
#' Data for the agent attributes and agent behavior
#' agent_attr <- data.frame(
#' age = c(0, 1, 2, 3, 4),
#' sex = c("m","m","m","f","f"))
#'
#' # Behavior 1: Agent gets older by beta in each time in a simulation.
#' agent_get_older <- function(beta = 1){self$age <- self$age + beta}
#'
#' # Example 1-1: Set the default behavior
#' D1_1 <- setABM(agent_n = 3, agent_f = agent_get_older)
#'
#' # Example 1-2: Modifying the behavior parameter beta to 2
#' D1_2 <- setABM(agent_n = 3, agent_f = agent_get_older(b = 2))
#'
#' # Example 2: Set a net field and set a agent behavior on this field
#' # Behavior 2: Agent add an edge randomly
#' add_an_edge <- function(){
#'   alters_candid <- as.character(setdiff(1:length(D$agent), self$ID))
#'   alter_decided <- sample(alters_candid, size = 1)
#'   D$net[self$ID, alter_decided] <- 1}
#'
#' D2 <- setABM(agent_n = 3, net = 1, agent_f = add_an_edge)

setABM <- function(
    agent_n,
    agent_attr = NULL,
    agent_f = list(NULL),
    net = NULL,
    ca = NULL,
    euc = NULL,
    other_field = NULL,
    log = NULL,
    time = NULL,
    notes = NULL,
    init = list(agent_n = NULL, agent_attr = NULL, agent_f = NULL,
                net = NULL, ca =NULL, euc = NULL,
                other_field = NULL, log = NULL,
                time = NULL, notes = NULL)){
  # インプットの形態を確認する------------
  ## agent_n
  if(!is.null(init$agent_n)){agent_n <- init$agent_n}
  ## 数値データであることを確認
  stopifnot("agent_n must be numeric." = is.numeric(agent_n))

  ## agent_attr
  if(!is.null(init$agent_attr)){
    agent_attr <- init$agent_attr
    agent_attr_sbs <- substitute(agent_attr)
  }else{
    agent_attr_sbs <- substitute(agent_attr)
  }
  agent_attr <- .shape_agent_attr(agent_attr_sbs = agent_attr_sbs, agent_n = agent_n)
  if(any(colnames(agent_attr)=="ID")){
    colnames(agent_attr)[which(colnames(agent_attr)=="ID")] <- "user_ID"
  }

  ## agent_f
  if(!is.null(init$agent_f)){
    agent_f <- init$agent_f
    agent_f_sbs <- substitute(agent_f)
  }else{
    agent_f_sbs <- substitute(agent_f)
  }
  agent_f_list <- .shape_agent_f(agent_f_sbs = agent_f_sbs, agent_n = agent_n)
  ## agent_f_listの冒頭にself <- selfを挿入する
  agent_f_list <- lapply(agent_f_list, .insert_line_to_function)

  ## net
  if(!is.null(init$net)){
    net <- init$net
    net_sbs <- substitute(net)
  }else{
    net_sbs <- substitute(net)
  }
  net <- .shape_net(net_sbs = net_sbs, agent_n = agent_n)

  ## ca
  if(!is.null(init$ca)){
    ca <- init$ca
    ca_sbs <- substitute(ca)
  }else{
    ca_sbs <- substitute(ca)
  }
  ca <- .shape_ca(ca_sbs = ca_sbs, agent_n = agent_n)

  ## euc
  if(!is.null(init$euc)){
    euc <- init$euc
    euc_sbs <- substitute(euc)
  }else{
    euc_sbs <- substitute(euc)
  }
  euc <- .shape_euc(euc_sbs = euc_sbs, agent_n = agent_n)

  ## other_field
  if(!is.null(init$other_field)){
    other_field <- init$other_field
    other_field_sbs <- substitute(other_field)
  }else{
    other_field_sbs <- substitute(other_field)
  }
  other_field <- .shape_other_field(other_field_sbs = other_field_sbs)

  ## log
  if(!is.null(init$log)){log <- init$log}
  if(is.null(log)){
    log <- NA
  }else{
    log <- log
  }

  ## time
  if(!is.null(init$time)){time <- init$time}
  if(is.null(time)){
    time <- 1
  }else{
    time <- time
  }

  ## notes
  if(!is.null(init$notes)){notes <- init$notes}
  if(is.null(notes)){
    notes <- NULL
  }else{
    if(is.data.frame(notes)){
      notes <- as.list(notes)
    }else if(!is.list(notes)){
      notes <- as.list(notes)
    }
  }

  # Dのフィールドを作成する
  D_field <- c(net, ca, euc, other_field)
  # Dを生成
  D <- .generator_D(field = D_field, time = time, log = log, notes = notes)
  # agentを生成
  agent <- .generator_Agent(agent_n = agent_n, agent_attr = agent_attr, agent_f = agent_f_list)
  # Dにagentを貼り付ける
  D$agent <- agent
  # logがNAの場合には初期値を貼り付ける
  if(is.na(D$log)){
    agent <- lapply(X = 1:length(D$agent), function(X){D$agent[[X]]$.save()})
    names(agent) <- names(D$agent)
    D_field <- names(D)[!names(D) %in% c(".f", ".save", "clone",".__enclos_env__","agent","log")]
    D_values <- lapply(X = D_field, get, envir = D$.__enclos_env__$self)
    names(D_values) <- D_field
    D_values$agent <- agent
    new_log <- list(D_values)
    names(new_log) <- paste0("t",D$time)
    D$log <- new_log
  }
  # Dを返却する
  D
}

