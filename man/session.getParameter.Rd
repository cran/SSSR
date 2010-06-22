\name{session.getParameter}
\Rdversion{1.1}
\alias{session.getParameter}

\title{
session.getParameter
}
\description{
Returns value of a given session parameter
}
\usage{
session.getParameter(paramName)
}

\arguments{
  \item{paramName}{
Session parameter name
}
}
\details{
Pages that uses session.getParameter and session.setParameter must include a
line of codes of session.start()
}

\author{
Mehmet Hakan Satman
}



\seealso{
\link{session.setParameter}
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

session.setParameter("x",rnorm(3))
response.println("x is ",session.getParameter("x"))
}

