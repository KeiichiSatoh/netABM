% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ca_move.R
\name{ca_move}
\alias{ca_move}
\title{Move an Agent in a Cellular Automaton (CA)}
\usage{
ca_move(which_ca = NULL, where_to, ID = NULL, D = NULL)
}
\arguments{
\item{which_ca}{A name specifying which CA to use from the \code{D}.
The Default is \code{NULL}, resulting in selecting the first CA found by
\code{D$.field_type()} method.}

\item{where_to}{An integer of vector index or matrix of array index
specifying the new location for the agent within the CA.}

\item{ID}{The ID of the agent. The default \code{NULL} will result in
selecting the current agent from the parent.frame,
which may be the usual usage.}

\item{D}{A \code{ABM_D} class object set by \code{setABM} function.
The default \code{NULL} will result in selecting the \code{D} object
from the parent.frame, which may be the usual usage.}
}
\value{
The function updates the CA in place and does not return a value.
}
\description{
This function moves an agent from its current location
to a specified new location within a cellular automaton (CA).
}
\details{
The function identifies the current location of the specified agent ID in the CA.
It then attempts to move the agent to the specified new location.
If the destination is already occupied by another agent, the move is not performed,
and a message is displayed. Otherwise, the agent is moved,
and its original location is cleared.
}
\examples{
# Example 1
# prepare an example dataset
set.seed(1)
ca <- init_ca(dim = c(3, 3), agent_n = 5)
D <- setABM(agent_n = 5, ca = ca)
# ID1 locates at the place 9
D$ca
ca_move(which_ca = "ca", where_to = 5, D = D, ID = 1)
# ID1 moves to the place 5 (i.e. row = 2, col = 2)
D$ca

# Example 2: a more practical usage

f <- function(){
 ca_move(which_ca = "ca", where_to = 5)
}

set.seed(1)
ca <- init_ca(dim = c(3, 3), agent_n = 1)
D <- setABM(agent_n = 1, ca = ca, agent_f = f)
D$ca
D <- runABM(D = D)
D$ca
}
\seealso{
Other ca_tools: 
\code{\link{ca_2d_to_3d}()},
\code{\link{ca_loc}()},
\code{\link{get_ca}()}

Other f_tools: 
\code{\link{ca_2d_to_3d}()},
\code{\link{ca_loc}()},
\code{\link{calc_logistic}()},
\code{\link{calc_softmax}()},
\code{\link{f_nothing}()},
\code{\link{get_ca}()}
}
\concept{ca_tools}
\concept{f_tools}
