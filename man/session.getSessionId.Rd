\name{session.getSessionId}
\Rdversion{1.1}
\alias{session.getSessionId}

\title{
session.getSessionId
}
\description{
Returns the session id for current user
}
\usage{
session.getSessionId()
}


\author{
Mehmet Hakan Satman
}




\seealso{
\link{session.getRandomSessionID}
\link{session.start}
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

sess<-session.getSessionId()
response.println("id is ",sess)
}

