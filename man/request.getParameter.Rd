\name{request.getParameter}
\Rdversion{1.1}
\alias{request.getParameter}

\title{
request.getParameter
}
\description{
Returns value of a given parameter from form variables.
}
\usage{
request.getParameter(aQueryString, paramName, delimiter = "&")
}

\arguments{
  \item{aQueryString}{
Received form data using post or get method
}
  \item{paramName}{
Parameter name
}
  \item{delimiter}{
Delimiter for parsing variables.
}
}

\author{
Mehmet Hakan Satman
}
\note{
This function is a proxy for handling form data. Use request.getPOSTParameter or 
request.getGETParameter, instead.
}



\seealso{
\link{request.getPOSTParameter}
\link{request.getGETParameter}
}
\examples{
	myVar<-request.getParameter("num=8&str=ABC","str","&")
}

