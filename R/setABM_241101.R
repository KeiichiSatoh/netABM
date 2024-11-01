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
    agents = NULL,
    net = NULL,
    mat = NULL,
    euc = NULL,
    df = NULL,
    other_field = NULL,
    active_binding_field = NULL,
    global_FUN = NULL,
    select_FUN = NULL,
    stop_FUN = NULL,
    update_FUN = NULL,
    partial_update_FUN_body = NULL,
    log = NULL,
    time = NULL,
    notes = NULL,
    init = list(agents = NULL,
                net = NULL, mat = NULL, euc = NULL, df = NULL, other_field = NULL,
                active_binding_field = NULL,
                global_FUN = NULL, select_FUN = NULL, stop_FUN = NULL, update_FUN = NULL,
                partial_update_FUN_body = NULL,
                log = NULL, time = NULL, notes = NULL)){
  # agent----------
  if(!is.null(init$agents)){
    agents <- init$agents
    agents_sbs <- substitute(agents)
  }else{
    agents_sbs <- substitute(agents)
  }
  agents_formatted <- .shape_agent(agents_sbs = agents_sbs)

  # net------------
  if(!is.null(init$net)){
    net <- init$net
    net_sbs <- substitute(net)
  }else{
    net_sbs <- substitute(net)
  }
  net_formatted <- .shape_net(net_sbs = net_sbs)

  # mat-----------
  if(!is.null(init$mat)){
    mat <- init$mat
    mat_sbs <- substitute(mat)
  }else{
    mat_sbs <- substitute(mat)
  }
  mat_formatted <- .shape_mat(mat_sbs = mat_sbs)

  # euc-----------
  if(!is.null(init$euc)){
    euc <- init$euc
    euc_sbs <- substitute(euc)
  }else{
    euc_sbs <- substitute(euc)
  }
  euc_formatted <- .shape_euc(euc_sbs = euc_sbs)

  # df-----------
  if(!is.null(init$df)){
    df <- init$df
    df_sbs <- substitute(df)
  }else{
    df_sbs <- substitute(df)
  }
  df_formatted <- .shape_df(df_sbs = df_sbs)

  # other_field----
  if(!is.null(init$other_field)){
    other_field <- init$df
    other_sbs <- substitute(other_field)
  }else{
    other_sbs <- substitute(other_field)
  }
  other_formatted <- .shape_other(other_sbs = other_sbs)

  # active_binding_field---
  if(!is.null(init$active_binding_field)){
    active_binding_field <- init$active_binding_field
    active_binding_field_sbs <- substitute(active_binding_field)
  }else{
    active_binding_field_sbs <- substitute(active_binding_field)
  }
  active_binding_field_formatted <- .shape_active_binding_field(active_binding_field_sbs = active_binding_field_sbs)

  # global_FUN----
  if(!is.null(init$global_FUN)){
    global_FUN <- init$global_FUN
    global_FUN_sbs <- substitute(global_FUN)
  }else{
    global_FUN_sbs <- substitute(global_FUN)
  }
  global_FUN_formatted <- .shape_global_FUN(global_FUN = global_FUN,
                                            global_FUN_sbs = global_FUN_sbs)

  # select_FUN----
  if(!is.null(init$select_FUN)){
    select_FUN <- init$select_FUN
    select_FUN_sbs <- substitute(select_FUN)
  }else{
    select_FUN_sbs <- substitute(select_FUN)
  }
  select_FUN_formatted <- .shape_select_FUN(select_FUN = select_FUN,
                                            select_FUN_sbs = select_FUN_sbs)

  # stop_FUN----
  if(!is.null(init$stop_FUN)){
    stop_FUN <- init$stop_FUN
    stop_FUN_sbs <- substitute(stop_FUN)
  }else{
    stop_FUN_sbs <- substitute(stop_FUN)
  }
  stop_FUN_formatted <- .shape_stop_FUN(stop_FUN = stop_FUN,
                                        stop_FUN_sbs = stop_FUN_sbs)

  # update_FUN----
  if(!is.null(init$update_FUN)){
    update_FUN <- init$update_FUN
    update_FUN_sbs <- substitute(update_FUN)
  }else{
    update_FUN_sbs <- substitute(update_FUN)
  }
  update_FUN_formatted <- .shape_update_FUN(update_FUN = update_FUN,
                                            update_FUN_sbs = update_FUN_sbs)

  # partial_FUN_expr---
  if(!is.null(init$partial_update_FUN_body)){
    partial_update_FUN_body <- init$partial_update_FUN_body
    partial_update_FUN_body_sbs <- substitute(partial_update_FUN_body)
  }else{
    partial_update_FUN_body_sbs <- substitute(partial_update_FUN_body)
  }
  partial_update_FUN_body_formatted <- .shape_partial_update_FUN_body(
    partial_update_FUN_body = partial_update_FUN_body,
    partial_update_FUN_body_sbs = partial_update_FUN_body_sbs)

  ## log---------
  if(!is.null(init$log)){log <- init$log}
  if(is.null(log)){
    log <- list(log = NULL)
  }else if(is.list(log)){
    log <- log
  }else{
    stop("log must be NULL or list.")
  }

  ## time--------
  if(!is.null(init$time)){time <- init$time}
  if(is.null(time)){
    time <- list(time = 1)
  }else if(rlang::is_scalar_atomic(time) && time >= 1){
    time <- list(time = time)
  }else{
    stop("time must be integer greater than or equal to 1.")
  }

  ## notes------
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

  ## field_type
  fields <- c(agents_formatted,
      net_formatted, mat_formatted, euc_formatted,
      df_formatted, other_formatted,
      partial_update_FUN_body_formatted,
      global_FUN_formatted, select_FUN_formatted,
      stop_FUN_formatted, update_FUN_formatted)
  field_type <- sapply(fields, function(x){attr(x, "field_type")})

  ## Dを生成する
  D <- ABM_D$new(fields = c(agents_formatted,
                            net_formatted, mat_formatted, euc_formatted,
                            df_formatted, other_formatted,
                            partial_update_FUN_body_formatted,
                            time, notes = notes),
                 methods = c(global_FUN_formatted, select_FUN_formatted,
                             stop_FUN_formatted, update_FUN_formatted),
                 log = log,
                 field_type = field_type)

  # active_bindingを処理する(NULLではない場合)
  if(!is.null(active_binding_field_formatted)){
    active <- assign_func_envs(active_binding_field_formatted, D$.__enclos_env__)
    for(name in names(active)){
      makeActiveBinding(name, active[[name]], D$.__enclos_env__$self)
    }
    D$.__enclos_env__$.__active__ <- active
  }

  # logがNULLの場合には初期値を貼り付ける
  if(is.null(D$log)){
    D$.save()}

  # 同じ名前のfieldがないかを確認し、あればストップをかける
  check_name_tb <- table(ls(D))>1
  if(any(check_name_tb)){
    stop(paste0("The following field has a duplicated name. Please give each field a unique name: ", names(check_name_tb[check_name_tb])))
  }

  # Dを返却する
  D
}
