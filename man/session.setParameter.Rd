\name{session.setParameter}
\Rdversion{1.1}
\alias{session.setParameter}

\title{
session.setParameter
}
\description{
Sets a session variable for current user
}
\usage{
session.setParameter(paramName, paramValue)
}

\arguments{
  \item{paramName}{
Parameter name to set
}
  \item{paramValue}{
Value of a given session parameter name
}
}

\author{
Mehmet Hakan Satman
}
\note{
session.getParameter
}



\seealso{
\link{session.getParameter}
\link{session.getAllParameters}
\link{application.getParameter}
\link{application.getAllParameters}
}
\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

name<-request.getGETParameter("name")
surname<-request.getGETParameter("surname")

if (nchar(name)>0 && nchar(surname)>0){
	session.setParameter("name",name)
	session.setParameter("surname",surname)
	response.print("Session variables were set to ",name," and ", surname)
}
}

