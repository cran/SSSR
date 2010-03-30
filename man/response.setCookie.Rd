\name{response.setCookie}
\Rdversion{1.1}
\alias{response.setCookie}

\title{
response.setCookie
}
\description{
Sets value of a given cookie name
}
\usage{
response.setCookie(cookieName, cookieValue)
}

\arguments{
  \item{cookieName}{
Cookie name 
}
  \item{cookieValue}{
Value for given cookie name
}
}
\details{
Must be called before response.startDocument function.
}

\author{
Mehmet Hakan Satman
}


\seealso{
\link{request.getCookie}
}
\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
response.setCookie("name","Misra")
response.startDocument()

#Refresh page after cookie value set
response.println(request.getCookie("name"))
}

