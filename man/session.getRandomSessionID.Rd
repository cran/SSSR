\name{session.getRandomSessionID}
\Rdversion{1.1}
\alias{session.getRandomSessionID}

\title{
session.getRandomSessionID
}
\description{
Returns a randomly generated string length of 20 chars.
}
\usage{
session.getRandomSessionID()
}

\details{
Function controls if generated session id was used before. If used, re-generates another session id.
Function iterates this step while last generated session id is unique. This function is called by
session.start, so normal user should not use this.
}

\author{
Mehmet Hakan Satman
}




\seealso{
\link{session.start}
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

sess<-session.getRandomSessionID()
response.println("id is ",sess)
}

