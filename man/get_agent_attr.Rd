% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_agent_attr.R
\name{get_agent_attr}
\alias{get_agent_attr}
\title{Get A Agents' Attributes From A ABM_D Object.}
\usage{
get_agent_attr(D, which_attr = NULL, from_log = FALSE, attr_long = FALSE)
}
\arguments{
\item{D}{An object of class \code{ABM_D}.}

\item{which_attr}{character. If \code{NULL}, retrieve all agent's attribute.}

\item{from_log}{logical. Whether the agent attributes should be extracted from the log.}

\item{attr_long}{logical. Whether the agent attributes in the log shold be organized in a long format.}
}
\value{
a data.frame of agent attributes.
}
\description{
\code{get_agent_attr} creates a vector and data.frame of
agents' attributes.
}
\examples{
# Create the ABM_D object
agent_attr <- data.frame(
    sex = c("m","m","m","f","f"))
D <- setABM(agent_n = 5, agent_attr = agent_attr)

# Extract the agent attributes
get_agent_attr(D)
}
