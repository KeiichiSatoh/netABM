% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_nothing.R
\name{f_nothing}
\alias{f_nothing}
\title{Agent does nothing}
\usage{
f_nothing()
}
\description{
\code{f_nothing} returns \code{NULL} regardless of any \code{D}.
}
\details{
\code{f_nothing} is intended to express the agent's non-action.
While this may seem like a trivial function, it is functionally necessary
for \code{runABM} because it mandates setting an action
even if the user does not intend to specify any action.
The primary purpose of this function is to assign a non-action state to
certain agents while allowing others to act.
}
\examples{
D <- setABM(agent_n = 3, agent_f = f_nothing)
D$agent$ID1$.f(D = D)
}
\seealso{
Other f_tools: 
\code{\link{ca_2d_to_3d}()},
\code{\link{ca_loc}()},
\code{\link{ca_move}()},
\code{\link{calc_logistic}()},
\code{\link{calc_softmax}()},
\code{\link{get_ca}()}
}
\author{
Keiichi Satoh
}
\concept{f_tools}
