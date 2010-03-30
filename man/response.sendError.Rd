\name{response.sendError}
\Rdversion{1.1}
\alias{response.sendError}

\title{
response.sendError
}
\description{
Shows a formatted error message and aborts operations.
}
\usage{
response.sendError(errStr)
}

\arguments{
  \item{errStr}{
A description for error.
}
}

\author{
Mehmet Hakan Satman
}



\seealso{
\link{response.print}
\link{response.println}
}
\examples{
	response.sendError("Please login!")
}
