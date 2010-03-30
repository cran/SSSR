\name{session.delete}
\Rdversion{1.1}
\alias{session.delete}

\title{
session.delete
}
\description{
Deletes all of the session entries for current user.
}
\usage{
session.delete()
}
%- maybe also 'usage' for other objects documented here.

\author{
Mehmet Hakan Satman
}


\seealso{
\link{session.setParameter}
\link{session.getParameter}
\link{application.delete}
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

#Logging out
response.println("You are logging out")
session.delete()
}

