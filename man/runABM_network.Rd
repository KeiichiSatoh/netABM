% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runABM_network.R
\name{runABM_network}
\alias{runABM_network}
\title{Run the simulation based on the netABM_network object}
\usage{
runABM_network(D, .stopCondition = NULL, .selectAgent = NULL, save_log = FALSE)
}
\arguments{
\item{D}{a \code{netABM} class object.}

\item{.selectAgent}{A user-defined or built-in function object about which agents to select.
The default value \code{NULL} will result in selecting all agents, meaning that all agents do their action each time.}

\item{save_log}{logical; if the log of each run shold be saved. The default is \code{FALSE}.}

\item{.stopCondition.}{A user-defined or built-in function object that determines when the simulation to stop.
The default value \code{NULL} will result in running one simulation.}
}
\value{
a \code{netABM_network} class object
}
\description{
\code{runABM_network} let agents their action defined by \code{.act} in the
\code{netABM_network} object.
}
\details{
\code{runAgent_network} is a simulator based on \code{netABM_network} object (D).
In each run the selected agents act their action defined \code{.act} of \code{netABM_network}
object.

Because the counting system of \code{R} starts from 1, \code{runABM} counts the initial time starts from 1,
which means that the one run of the simulation corresponds to time 2.

For setting \code{.stopCondition} and \code{.selectAgent} condition, there are two different ways.
The first way is to write the user's own function.
Upon writing an original function, be sure to set \code{D} as the first argument without any default;
otherwise agent's action does not reflect dynamically to the changing \code{D} object during the simulation.
\code{self} is a reserved for indicating the agent themselves.

The second way is to use a built-in function of this package.
This second way actually has further two variations. First, the easiest one,
just supply the function object to \code{.stopCondition} and \code{.selectAgent} (e.g., .act = function_name).
Second, if user wants to modify some argument, supply it as a form: \code{function_name(x = a new value)}.
}
\examples{
node_attr <- data.frame(
 age = c(0, 0, 0, 0, 0),
 sex = c("m","m","m","f","f"))
network <- matrix(0, 5, 5)
agent_get_older <- function(D){self$a$age <- self$a$age + 1}

# Create the netABM_network object
D <- setABM_network(n = 5,
                    node_attr = node_attr,
                    .act = list(agent_get_older))
# run the simulation
D <- runABM_network(D = D,
                    .stopCondition = stopABM_times(simTimes = 10))
# result
D
}
\author{
Keiichi Satoh
}
\concept{runABM}
