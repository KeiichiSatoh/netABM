#' @title Replace Agent IDs on a Stage Field with an Agent Attribute
#' @description Replaces the agent IDs on a stage field with a
#' specified agent attribute. The function modifies the stage field in `G` object
#' by substituting agent IDs with the values of a given attribute from the agents.
#'
#' @param stage_name A character string specifying the name of the stage
#' in the `ABM_G` object.
#' @param agents_name A character string specifying the name of the agents
#' object in the `ABM_G` object.
#' @param attr_name A character string specifying the name of the attribute
#' to replace the agent IDs with.
#' @param G An ABM object containing the agents and stage.
#' If `NULL`, it uses `self` to refer to the ABM environment. For example,
#' if users uses this function as an `active_binding_field` in `G` object,
#' leave this field as default.
#'
#' @return A matrix where agent IDs in the specified stage have been replaced with the values of the specified attribute.
#'
#' @examples
#' persons <- init_agent(n = 3, attr_df = data.frame(sex = c("f","f","m")))
#' stage <- matrix(c(1:3, 0), 2, 2)
#' G <- setABM(agents = persons, stage = stage)
#' stage_by_attr(stage_name = "stage", agents_name = "persons", attr_name = "sex", G = G)
#'
#' @export
stage_by_attr <- function(stage_name, agents_name, attr_name, G = NULL){
  if(!is.null(G)){
    self <- G
  }
  stage <- self[[stage_name]]
  attr <- unlist(self$.agent_attr(agents = agents_name, attr = attr_name))
  agent_ID <- unlist(self$.agent_attr(agents = agents_name, attr = "ID"))

  stage_eval <- stage %in% agent_ID
  posit <- stage[stage_eval]
  stage[stage_eval] <- attr[posit]
  stage
}

