\name{request.getPOSTParameter}
\Rdversion{1.1}
\alias{request.getPOSTParameter}

\title{
request.getPOSTParameter
}
\description{
Returns value of a given parameter that sent from web forms using post method.
}
\usage{
request.getPOSTParameter(paramName)
}

\arguments{
  \item{paramName}{
Parameter name
}
}
\details{
request.getPOSTParameter and request.getGETParameter functions are wrappers for request.getParameter function.
}

\author{
Mehmet Hakan Satman
}



\seealso{
\link{request.getParameter}
\link{request.getGETParameter}
}
\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
response.startDocument()

response.print("<form method=post>")
response.print("<input type=hidden name=formvar1 value=\"hello\">")
response.print("<input type=submit>")

myVar<-request.getPOSTParameter("formvar1")
response.println("Form variable is: ",myVar)
}
