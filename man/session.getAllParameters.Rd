\name{session.getAllParameters}
\Rdversion{1.1}
\alias{session.getAllParameters}

\title{
session.getAllParameters
}
\description{
Returns a list of all session parameters for current user.
}
\usage{
session.getAllParameters()
}


\author{
Mehmet Hakan Satman
}



\seealso{
\link{application.getAllParameters}
\link{session.getParameter}
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

session.setParameter("x",rnorm(3))

response.println("All session variables are:")
response.println("<pre>")
response.println(session.getAllParameters())
response.println("</pre>")
}

