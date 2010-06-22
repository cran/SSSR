\name{session.deleteParameter}
\Rdversion{1.1}
\alias{session.deleteParameter}

\title{
session.deleteParameter
}
\description{
Deletes given session parameter.
}
\usage{
session.deleteParameter(paramName)
}
\arguments{
  \item{paramName}{
Parameter name to delete
}
}

\author{
Mehmet Hakan Satman
}



\seealso{
\link{session.delete}
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

#Logging out
response.println("You are logging out")
session.deleteParameter("username")
}

