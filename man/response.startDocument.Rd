\name{response.startDocument}
\Rdversion{1.1}
\alias{response.startDocument}

\title{
response.startDocument
}
\description{
Finalizes HTTP headers and let the document starts
}
\usage{
response.startDocument()
}



\author{
Mehmet Hakan Satman
}



\seealso{
\link{response.sendRedirect}
\link{response.setCookie}
\link{session.start}
}
\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
response.startDocument()
}

