\name{application.getAllParameters}
\Rdversion{1.1}
\alias{application.getAllParameters}

\title{
application.getAllParameters
}
\description{
Returns a list of all application variables.
}
\usage{
application.getAllParameters()
}
%- maybe also 'usage' for other objects documented here.

\author{
Mehmet Hakan Satman
}



\seealso{
\link{application.getParameter}
}

\examples{
	myVars<-application.getAllParameters()
	response.println("<pre>",myVars,"</pre>")
}

