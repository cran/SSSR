\name{request.getCookie}
\Rdversion{1.1}
\alias{request.getCookie}

\title{
request.getCookie
}
\description{
Returns value of a given cookie
}
\usage{
request.getCookie(paramName)
}

\arguments{
  \item{paramName}{
Cookie name
}
}
\details{
request.getCookie function is used for handling session operations. Default cookie name for 
session operations is RSESSID. 
}

\author{
Mehmet Hakan Satman
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
response.startDocument()

response.println("Cookie is ",request.getCookie("RSESSID"))
}

